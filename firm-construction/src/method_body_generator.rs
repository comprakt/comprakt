use super::{
    firm_program::FirmProgram,
    type_translation::{get_firm_mode, ty_from_checked_type},
    Runtime,
};
use crate::{
    asciifile::{Span, Spanned},
    ast::{self, BinaryOp},
    strtab::{StringTable, Symbol},
    type_checking::{
        type_analysis::{RefInfo, TypeAnalysis},
        type_system::{
            BuiltinMethodBody, CheckedType, ClassFieldDef, ClassMethodBody, ClassMethodDef,
            TypeSystem,
        },
    },
};
use libfirm_rs::{
    bindings,
    nodes::{NodeTrait, *},
    types::*,
    Entity, Graph, Mode, Tarval,
};
use std::{collections::HashMap, rc::Rc};
use strum_macros::EnumDiscriminants;

pub struct MethodBodyGenerator<'ir, 'src, 'ast> {
    program: &'ir FirmProgram<'src, 'ast>,
    graph: Graph,
    method_def: Rc<ClassMethodDef<'src, 'ast>>,
    local_vars: HashMap<Symbol<'src>, (usize, Mode)>,
    num_vars: usize,
    runtime: &'ir Runtime,
    type_system: &'ir TypeSystem<'src, 'ast>,
    type_analysis: &'ir TypeAnalysis<'src, 'ast>,
    strtab: &'ir StringTable<'src>,
    pub spans: HashMap<Node, Span<'src>>,
}

enum ActiveBlock {
    None,
    Some(Block),
}

impl<'a, 'ir, 'src, 'ast> MethodBodyGenerator<'ir, 'src, 'ast> {
    pub(super) fn new(
        graph: Graph,
        program: &'ir FirmProgram<'src, 'ast>,
        method_def: Rc<ClassMethodDef<'src, 'ast>>,
        type_system: &'ir TypeSystem<'src, 'ast>,
        type_analysis: &'ir TypeAnalysis<'src, 'ast>,
        runtime: &'ir Runtime,
        strtab: &'ir StringTable<'src>,
    ) -> Self {
        Self {
            graph,
            program,
            local_vars: HashMap::new(),
            num_vars: 0,
            method_def,
            runtime,
            type_analysis,
            type_system,
            strtab,
            spans: HashMap::new(),
        }
    }

    fn with_spanned<T: NodeTrait + Into<Node> + Copy, TAst>(
        &mut self,
        ast_node: &'ast Spanned<'src, TAst>,
        node: T,
    ) -> T {
        self.spans.insert(node.into(), ast_node.span);
        node
    }

    fn with_span<T: NodeTrait + Into<Node> + Copy>(&mut self, span: Span<'src>, node: T) -> T {
        self.spans.insert(node.into(), span);
        node
    }

    pub fn get_const_bool(&mut self, val: bool) {
        self.graph
            .new_const(Tarval::mj_int(if val { 1 } else { 0 }));
    }

    pub fn gen_method(&mut self, body: &'ast Spanned<'src, ast::Block<'src>>) {
        let act_block = self.graph.start_block();
        self.set_graph_arg_vals(act_block);

        let act_block = self.gen_block(act_block, body);

        if let ActiveBlock::Some(act_block) = act_block {
            let ret = act_block.new_return(act_block.cur_store(), &[]);
            self.graph.end_block().imm_add_pred(ret);
        }

        self.graph.end_block().mature();
    }

    fn set_graph_arg_vals(&mut self, block: Block) {
        let args = self.graph.args();

        if !self.method_def.is_static {
            let this_symbol = self.strtab.this_symbol();
            let this_var = self.new_local_var(this_symbol, Mode::P());
            block.set_value(this_var, args.new_proj(0, Mode::P()));
        }

        let method_def = Rc::clone(&self.method_def);
        for (idx, param) in method_def.params.iter().enumerate() {
            let mode = get_firm_mode(&param.ty).expect("parmeter cannot be void");
            let var = self.new_local_var(param.name, mode);
            block.set_value(
                var,
                args.new_proj(
                    if self.method_def.is_static { 0 } else { 1 } + idx as u32,
                    mode,
                ),
            );
        }
    }

    fn gen_block(
        &mut self,
        act_block: Block,
        block: &'ast Spanned<'src, ast::Block<'src>>,
    ) -> ActiveBlock {
        block
            .data
            .statements
            .iter()
            .fold(
                ActiveBlock::Some(act_block),
                |act_block, stmt| match act_block {
                    ActiveBlock::Some(act_block) => self.gen_stmt(act_block, &stmt),
                    ActiveBlock::None => ActiveBlock::None,
                },
            )
    }

    fn gen_stmt(
        &mut self,
        act_block: Block,
        stmt: &'ast Spanned<'src, ast::Stmt<'src>>,
    ) -> ActiveBlock {
        use self::ast::Stmt::*;
        match &stmt.data {
            Block(block) => self.gen_block(act_block, block),
            Expression(expr) => self.gen_expr(act_block, expr).active_block(),
            If(cond, then_arm, else_arm) => self.gen_if(act_block, cond, then_arm, else_arm),
            While(cond, body) => self.gen_while(act_block, cond, body),
            Return(res_expr) => self.gen_return(act_block, stmt.span, res_expr),
            LocalVariableDeclaration(_ty, _name, init_expr) => {
                self.gen_var_decl(act_block, stmt, init_expr)
            }
            Empty => ActiveBlock::Some(act_block),
        }
    }

    fn gen_var_decl(
        &mut self,
        act_block: Block,
        stmt: &'ast Spanned<'src, ast::Stmt<'src>>,
        init_expr: &'ast Option<Box<Spanned<'src, ast::Expr<'src>>>>,
    ) -> ActiveBlock {
        let local_var_def = self
            .type_analysis
            .local_var_def(stmt)
            .expect("Was set by type_analysis. Stmt is local var def.");

        let mode = get_firm_mode(&local_var_def.ty).expect("parmeter cannot be void");

        let var_slot = self.new_local_var(local_var_def.name, mode);

        let (act_block, initial_val) = if let Some(init_expr) = init_expr {
            self.gen_value(act_block, init_expr)
        } else {
            (act_block, self.graph.new_const(Tarval::zero(mode)).into())
        };

        act_block.set_value(var_slot, initial_val);

        ActiveBlock::Some(act_block)
    }

    fn gen_while(
        &mut self,
        act_block: Block,
        cond: &'ast Spanned<'src, ast::Expr<'src>>,
        body: &'ast Spanned<'src, ast::Stmt<'src>>,
    ) -> ActiveBlock {
        // We evaluate the condition
        let header_block = self.graph.new_imm_block(&[act_block.new_jmp().into()]);
        let CondProjection { tr, fls } = self
            .gen_expr(header_block, cond)
            .enforce_cond(self, self.graph);

        // Run body if cond is true
        let body_block = self.graph.new_block(&[tr]);
        if let ActiveBlock::Some(body_block) = self.gen_stmt(body_block, &*body) {
            // We jump back to the condition-check
            header_block.imm_add_pred(body_block.new_jmp());
        }
        header_block.mature();
        header_block.keep_alive(); // to keep endless loops

        // Leave loop if cond is false
        let after_loop_block = self.graph.new_block(&[fls]);

        ActiveBlock::Some(after_loop_block)
    }

    fn gen_if(
        &mut self,
        act_block: Block,
        cond: &'ast Spanned<'src, ast::Expr<'src>>,
        then_arm: &'ast Spanned<'src, ast::Stmt<'src>>,
        else_arm: &'ast Option<Box<Spanned<'src, ast::Stmt<'src>>>>,
    ) -> ActiveBlock {
        use self::ActiveBlock::*;
        // We evaluate the condition
        let CondProjection { tr, fls } = self
            .gen_expr(act_block, cond)
            .enforce_cond(self, self.graph);

        // If its true, we take the then_arm
        let then_block = self.gen_stmt(self.graph.new_block(&[tr]), &*then_arm);

        // If its false, we take the else_arm
        let else_block = self.graph.new_block(&[fls]);
        let else_block = if let Option::Some(else_arm) = else_arm {
            self.gen_stmt(else_block, &**else_arm)
        } else {
            ActiveBlock::Some(else_block)
        };

        match (then_block, else_block) {
            (Some(then_block), Some(else_block)) => {
                let next_block = self
                    .graph
                    .new_block(&[then_block.new_jmp().into(), else_block.new_jmp().into()]);
                Some(next_block)
            }
            (Some(then_block), None) => Some(then_block),
            (None, Some(else_block)) => Some(else_block),
            (None, None) => None,
        }
    }

    fn gen_return(
        &mut self,
        act_block: Block,
        span: Span<'src>,
        res_expr: &'ast Option<Box<Spanned<'src, ast::Expr<'src>>>>,
    ) -> ActiveBlock {
        let mut res = vec![];
        let act_block = if let Some(res_expr) = res_expr {
            let (act_block, val) = self.gen_value(act_block, &*res_expr);
            res.push(val);
            act_block
        } else {
            act_block
        };

        let return_node = self.with_span(span, act_block.new_return(act_block.cur_store(), &res));
        self.graph.end_block().imm_add_pred(return_node);

        ActiveBlock::None
    }

    fn gen_static_fn_call(
        &mut self,
        act_block: Block,
        span: Span<'src>,
        func: Entity,
        return_type: Option<Mode>,
        args: &[Node],
    ) -> ExprResult<'src> {
        use self::ExprResult::*;
        let call = act_block.new_call(
            act_block.cur_store(),
            self.graph.new_address(func),
            &args,
            func.ty(),
        );

        let call = self.with_span(span, call);

        act_block.set_store(call.new_proj_m());

        match return_type {
            Some(mode) => Value(act_block, call.new_proj_t_result().new_proj(0, mode).into()),
            None => Void(act_block),
        }
    }

    fn gen_expr_list<I>(&mut self, act_block: Block, exprs: I) -> (Block, Vec<Node>)
    where
        I: Iterator<Item = &'ast Spanned<'src, ast::Expr<'src>>>,
    {
        let mut result = vec![];
        let act_block = exprs.fold(act_block, |act_block, arg| {
            let (act_block, arg) = self.gen_value(act_block, arg);
            result.push(arg);
            act_block
        });
        (act_block, result)
    }

    fn gen_expr(
        &mut self,
        act_block: Block,
        expr: &'ast Spanned<'src, ast::Expr<'src>>,
    ) -> ExprResult<'src> {
        use self::{ast::Expr::*, ExprResult::*};
        match &expr.data {
            Int(literal) => {
                let val = literal.parse().expect("Integer literal has to be valid");
                let tarval = Tarval::mj_int(val);
                Value(
                    act_block,
                    self.with_spanned(expr, self.graph.new_const(tarval).into()),
                )
            }
            NegInt(literal) => {
                let val = literal
                    .parse::<i32>()
                    .map_or_else(|_| -2_147_483_648, |v| -v);
                let tarval = Tarval::mj_int(i64::from(val));
                Value(
                    act_block,
                    self.with_spanned(expr, self.graph.new_const(tarval)).into(),
                )
            }
            Boolean(value) => Value(act_block, gen_const_bool(*value, self.graph)),
            This => Value(act_block, self.this(act_block)),
            Null => Value(
                act_block,
                self.with_spanned(expr, self.graph.new_const(Tarval::zero(Mode::P())))
                    .into(),
            ),
            Var(name) => {
                use self::RefInfo::*;
                match self
                    .type_analysis
                    .expr_info(expr)
                    .ref_info
                    .as_ref()
                    .expect("Variable access expr is always a ref")
                {
                    Var(_) | Param(_) => self.local_var(act_block, name),
                    Field(field_def) => self.gen_field(
                        act_block,
                        expr.span,
                        self.this(act_block),
                        Rc::clone(field_def),
                    ),
                    GlobalVar(..) | This(..) | ArrayAccess | Method(..) => {
                        unreachable!("Variable access expr is always var, param or field")
                    }
                }
            }
            FieldAccess(obj, _field) => {
                let field_def = match &self.type_analysis.expr_info(expr).ref_info {
                    Some(RefInfo::Field(field_def)) => Rc::clone(field_def),
                    other => unreachable!("Got unexpected: {:?}", other),
                };
                let (act_block, obj) = self.gen_value(act_block, obj);
                self.gen_field(act_block, expr.span, obj, field_def)
            }
            Binary(op, lhs, rhs) => self.gen_binary_expr(act_block, expr.span, *op, lhs, rhs),
            Unary(ast::UnaryOp::Neg, expr) => {
                let (act_block, expr) = self.gen_value(act_block, expr);
                Value(act_block, act_block.new_minus(expr).into())
            }
            Unary(ast::UnaryOp::Not, expr) => {
                use self::ExprResult::*;
                let expr = self.gen_expr(act_block, expr);
                let graph = self.graph;
                let inverse_val = |(act_block, val): (Block, Node)| {
                    // booleans are mode::Bu, hence XOR does the job.
                    // could also use mode::Bi and -1 for true:
                    // => could use Neg / Not, but would rely on 2's complement
                    debug_assert_eq!(val.mode(), Mode::Bu());
                    Value(
                        act_block,
                        act_block.new_eor(val, gen_const_bool(true, graph)).into(),
                    )
                };

                match expr {
                    Void(_) => panic!("type system should not allow !void"),
                    Assignable(act_block, lvalue) => inverse_val(lvalue.gen_eval(self, act_block)),
                    Value(act_block, val) => inverse_val((act_block, val)),
                    Cond(proj) => Cond(proj.flip()),
                }
            }
            MethodInvocation(_, _, arguments) | ThisMethodInvocation(_, arguments) => {
                let class_method_def = match &self.type_analysis.expr_info(expr).ref_info {
                    Some(RefInfo::Method(class_method_def)) => Rc::clone(class_method_def),
                    _ => panic!("type analysis inconsistent"),
                };

                if let ClassMethodBody::Builtin(builtin) = &class_method_def.body {
                    let (act_block, args) = self.gen_expr_list(act_block, arguments.data.iter());
                    // ENHANCEMENT: @hediet: dedup builtin type definitions
                    return match builtin {
                        BuiltinMethodBody::SystemOutPrintln => self.gen_static_fn_call(
                            act_block,
                            expr.span,
                            self.runtime.system_out_println,
                            None,
                            &args,
                        ),
                        BuiltinMethodBody::SystemOutWrite => self.gen_static_fn_call(
                            act_block,
                            expr.span,
                            self.runtime.system_out_write,
                            None,
                            &args,
                        ),
                        BuiltinMethodBody::SystemOutFlush => self.gen_static_fn_call(
                            act_block,
                            expr.span,
                            self.runtime.system_out_flush,
                            None,
                            &args,
                        ),
                        BuiltinMethodBody::SystemInRead => self.gen_static_fn_call(
                            act_block,
                            expr.span,
                            self.runtime.system_in_read,
                            get_firm_mode(&CheckedType::Int),
                            &args,
                        ),
                    };
                } else {
                    let method = self.program.method(class_method_def).unwrap();

                    let (act_block, target_obj) = match &expr.data {
                        MethodInvocation(target_obj, ..) => self.gen_value(act_block, target_obj),
                        ThisMethodInvocation(..) => (act_block, self.this(act_block)),
                        _ => unreachable!(),
                    };
                    let (act_block, mut args) =
                        self.gen_expr_list(act_block, arguments.data.iter());
                    args.insert(0, target_obj); // IMPROVEMENT: get rid of insert
                    let return_type = get_firm_mode(&method.borrow().def.return_ty);
                    let entity = method.borrow().entity;
                    self.gen_static_fn_call(act_block, expr.span, entity, return_type, &args)
                }
            }
            ArrayAccess(target, idx_expr) => {
                let item_ty = &self.type_analysis.expr_info(expr).ty;
                let item_ty = ty_from_checked_type(item_ty, self.type_system, self.program)
                    .expect("not void");
                let arr_ty = item_ty.array().into();
                let (act_block, target) = self.gen_value(act_block, target);
                let (act_block, idx_node) = self.gen_value(act_block, idx_expr);
                let sel = self.with_spanned(expr, act_block.new_sel(target, idx_node, arr_ty));
                Assignable(
                    act_block,
                    LValue::ArrayOrField {
                        span: expr.span,
                        sel_or_mem: sel.into(),
                        item_ty,
                    },
                )
            }

            NewObject(_ty_name) => {
                let class_def = if let CheckedType::TypeRef(class_def_id) =
                    self.type_analysis.expr_info(expr).ty
                {
                    self.type_system.class(class_def_id)
                } else {
                    unreachable!();
                };

                let class = self.program.class(class_def).unwrap();
                let class_ty = class.borrow().entity.ty();
                let call = self.with_spanned(
                    expr,
                    act_block.new_call(
                        act_block.cur_store(),
                        self.graph.new_address(self.runtime.new),
                        // TODO: Mode::Ls corresponds to PrimitiveTy::i64 required for mjrt_new
                        &[self.graph.new_size(Mode::Ls(), class_ty).into()],
                        self.runtime.new.ty(),
                    ),
                );
                act_block.set_store(call.new_proj_m());

                Value(
                    act_block,
                    call.new_proj_t_result().new_proj(0, Mode::P()).into(),
                )
            }
            NewArray(_, num_expr, _) => {
                let checked_elem_ty = &self
                    .type_analysis
                    .expr_info(expr)
                    .ty
                    .inner_type()
                    .expect("type of array must have inner type");
                let elem_ty = ty_from_checked_type(checked_elem_ty, self.type_system, self.program)
                    .expect("To be a valid type");
                let (act_block, num_elts) = self.gen_value(act_block, num_expr);
                // TODO refactor: Mode::Ls corresponds to PrimitiveTy::i64 required for mjrt_new
                let num_elts = act_block.new_conv(num_elts, Mode::Ls());
                let elt_size = self.graph.new_size(Mode::Ls(), elem_ty);

                let alloc_size = act_block.new_mul(num_elts, elt_size); // Is Mode::Ls()?
                let call = act_block.new_call(
                    act_block.cur_store(),
                    self.graph.new_address(self.runtime.new),
                    &[alloc_size.into()],
                    self.runtime.new.ty(),
                );
                let call = self.with_spanned(expr, call);
                act_block.set_store(call.new_proj_m());
                Value(
                    act_block,
                    call.new_proj_t_result().new_proj(0, Mode::P()).into(),
                )
            }
        }
    }

    fn gen_field(
        &mut self,
        act_block: Block,
        span: Span<'src>,
        target: Node,
        field_def: Rc<ClassFieldDef<'src>>,
    ) -> ExprResult<'src> {
        let field = self.program.field(Rc::clone(&field_def)).unwrap();
        let field_entity = field.borrow().entity;
        let member = self.with_span(span, act_block.new_member(target, field_entity));
        let lvalue = LValue::ArrayOrField {
            span,
            sel_or_mem: member.into(),
            item_ty: field_entity.ty(),
        };
        ExprResult::Assignable(act_block, lvalue)
    }

    fn relation(op: BinaryOp) -> Option<bindings::ir_relation::Type> {
        match op {
            BinaryOp::Equals => Some(bindings::ir_relation::Equal),
            BinaryOp::NotEquals => Some(bindings::ir_relation::LessGreater),
            BinaryOp::LessThan => Some(bindings::ir_relation::Less),
            BinaryOp::GreaterThan => Some(bindings::ir_relation::Greater),
            BinaryOp::LessEquals => Some(bindings::ir_relation::LessEqual),
            BinaryOp::GreaterEquals => Some(bindings::ir_relation::GreaterEqual),
            _ => None,
        }
    }

    fn gen_value(
        &mut self,
        act_block: Block,
        expr: &'ast Spanned<'src, ast::Expr<'src>>,
    ) -> (Block, Node) {
        self.gen_expr(act_block, expr)
            .enforce_value(self, self.graph)
    }

    fn gen_binary_expr(
        &mut self,
        act_block: Block,
        span: Span<'src>,
        op: BinaryOp,
        lhs: &'ast Spanned<'src, ast::Expr<'src>>,
        rhs: &'ast Spanned<'src, ast::Expr<'src>>,
    ) -> ExprResult<'src> {
        use self::ExprResult::*;
        let pinned = bindings::op_pin_state::Pinned as i32;
        match op {
            op if Self::relation(op).is_some() => {
                let relation = Self::relation(op).unwrap();
                let (act_block, lhs) = self.gen_value(act_block, lhs);
                let (act_block, rhs) = self.gen_value(act_block, rhs);
                let cmp = self.with_span(span, act_block.new_cmp(lhs, rhs, relation));
                let cond = self.with_span(span, act_block.new_cond(cmp));
                Cond(CondProjection::new(cond))
            }
            BinaryOp::Add | BinaryOp::Sub | BinaryOp::Mul => {
                let (act_block, lhs) = self.gen_value(act_block, lhs);
                let (act_block, rhs) = self.gen_value(act_block, rhs);
                let node: Node = match op {
                    BinaryOp::Add => act_block.new_add(lhs, rhs).into(),
                    BinaryOp::Sub => act_block.new_sub(lhs, rhs).into(),
                    BinaryOp::Mul => act_block.new_mul(lhs, rhs).into(),
                    _ => unreachable!(),
                };
                Value(act_block, self.with_span(span, node))
            }
            BinaryOp::Div | BinaryOp::Mod => {
                let (act_block, lhs) = self.gen_value(act_block, lhs);
                let (act_block, rhs) = self.gen_value(act_block, rhs);
                let mem = act_block.cur_store();

                let lhs = self.with_span(span, act_block.new_conv(lhs, Mode::Ls()));
                let rhs = self.with_span(span, act_block.new_conv(rhs, Mode::Ls()));

                let (result, mem) = match op {
                    BinaryOp::Div => {
                        let node = self.with_span(span, act_block.new_div(mem, lhs, rhs, pinned));
                        (node.new_proj_res(Mode::Ls()), node.new_proj_m())
                    }
                    BinaryOp::Mod => {
                        let node = self.with_span(span, act_block.new_mod(mem, lhs, rhs, pinned));
                        (node.new_proj_res(Mode::Ls()), node.new_proj_m())
                    }
                    _ => unreachable!(),
                };

                let result = self.with_span(span, act_block.new_conv(result, Mode::Is()));
                act_block.set_store(mem);
                Value(act_block, result.into())
            }
            BinaryOp::LogicalOr => {
                let lhs = self.gen_expr(act_block, lhs).enforce_cond(self, self.graph);
                let false_block = self.graph.new_block(&[lhs.fls]);
                let rhs = self
                    .gen_expr(false_block, rhs)
                    .enforce_cond(self, self.graph);
                // IMPROVEMENT: this block is unneccessary if we could return multiple tr nodes
                let both_true_block = self.graph.new_block(&[lhs.tr, rhs.tr]);
                Cond(CondProjection {
                    tr: both_true_block.new_jmp().into(),
                    fls: rhs.fls,
                })
            }
            BinaryOp::LogicalAnd => {
                let lhs = self.gen_expr(act_block, lhs).enforce_cond(self, self.graph);
                let lhs_true_block = self.graph.new_block(&[lhs.tr]);
                let rhs = self
                    .gen_expr(lhs_true_block, rhs)
                    .enforce_cond(self, self.graph);
                // IMPROVEMENT: this block is unneccessary if we could return multiple fls nodes
                let any_false_block = self.graph.new_block(&[lhs.fls, rhs.fls]);
                Cond(CondProjection {
                    tr: rhs.tr,
                    fls: any_false_block.new_jmp().into(),
                })
            }
            BinaryOp::Assign => {
                let (act_block, lvalue) = self.gen_expr(act_block, lhs).expect_lvalue();
                let (act_block, assign_value) = self.gen_value(act_block, rhs);
                ExprResult::value_tuple(lvalue.gen_assign(span, self, act_block, assign_value))
            }
            _ => unreachable!(),
        }
    }

    /// Allocate a new local variable in the next free slot
    fn new_local_var(&mut self, name: Symbol<'src>, mode: Mode) -> usize {
        let slot = self.num_vars;
        self.num_vars += 1;
        self.local_vars.insert(name, (slot, mode));
        slot
    }

    /// Get name and mode of previously allocated local var
    fn local_var(
        &mut self,
        act_block: Block,
        name: &'ast Spanned<'src, Symbol<'src>>,
    ) -> ExprResult<'src> {
        match self.local_vars.get(&name) {
            Some(&(slot_idx, mode)) => ExprResult::Assignable(
                act_block,
                LValue::Var {
                    span: name.span,
                    slot_idx,
                    mode,
                },
            ),
            None => panic!("undefined variable '{}'", name),
        }
    }

    fn this(&self, act_block: Block) -> Node {
        act_block.value(0, Mode::P())
    }
}

/// Result of `MethodBodyGenerator::gen_expr`
#[derive(EnumDiscriminants)]
enum ExprResult<'src> {
    /// No Result (e.g. call of void method)
    Void(Block),
    /// Result is a single value (e.g. method call, variable, integer
    /// arithmetic)
    Value(Block, Node),
    /// Result is two mode::X nodes (i.e. control flow) that can be branched on
    /// (e.g. result of short-circuiting binary expr, `||`, `==`, `&&`, ...)
    Cond(CondProjection),

    /// An assignable lvalue, such as parameter / local var or array access
    Assignable(Block, LValue<'src>),
}

/// An If-diamond
struct CondProjection {
    /// This is the true exec flow ([`mode::X`] node) of the cond
    tr: Node, // Jmp or Proj Cond
    /// This is the false exec flow ([`mode::X`] node) of the cond
    fls: Node, // Jmp or Proj Cond
}

impl CondProjection {
    fn new(cond: Cond) -> Self {
        Self {
            tr: cond.new_proj_true().into(),
            fls: cond.new_proj_false().into(),
        }
    }

    fn flip(self) -> Self {
        let Self { tr, fls } = self;
        Self { tr: fls, fls: tr }
    }
}

fn gen_const_bool(val: bool, graph: Graph) -> Node {
    graph
        .new_const(Tarval::val(if val { 1 } else { 0 }, Mode::Bu()))
        .into()
}

impl<'src> ExprResult<'src> {
    fn value_tuple(tuple: (Block, Node)) -> ExprResult<'static> {
        ExprResult::Value(tuple.0, tuple.1)
    }

    fn active_block(self) -> ActiveBlock {
        use self::ExprResult::*;
        match self {
            Void(block) | Value(block, ..) | Assignable(block, ..) => ActiveBlock::Some(block),
            Cond(..) => ActiveBlock::None,
        }
    }

    /// Enforce that the result is variant Cond. If self is a boolean `Value`,
    /// convert it to a selector that projects the two cases. If it is a
    /// non-boolean value, libfirm will complain.
    fn enforce_cond(
        self,
        span_storage: &mut MethodBodyGenerator<'_, 'src, '_>,
        graph: Graph,
    ) -> CondProjection {
        use self::ExprResult::*;
        match self {
            Void(..) => panic!("Tried to branch on result of void expr"),
            Value(act_block, node) => {
                let cond = act_block.new_cond(act_block.new_cmp(
                    node,
                    gen_const_bool(true, graph),
                    bindings::ir_relation::Equal,
                ));

                CondProjection::new(cond)
            }
            Assignable(act_block, lval) => {
                Self::value_tuple(lval.gen_eval(span_storage, act_block))
                    .enforce_cond(span_storage, graph)
            }
            Cond(cp) => cp,
        }
    }

    /// Enforce that the result is a single value. If self is a `Cond`,
    /// convert it to boolean value
    fn enforce_value(
        self,
        span_storage: &mut MethodBodyGenerator<'_, 'src, '_>,
        graph: Graph,
    ) -> (Block, Node) {
        use self::ExprResult::*;
        match self {
            Void(_) => panic!("Tried to get result value of void expr"),
            Value(act_block, val) => (act_block, val),
            Assignable(act_block, lval) => lval.gen_eval(span_storage, act_block),
            Cond(cp) => {
                let CondProjection { tr, fls } = cp;

                let false_ = gen_const_bool(false, graph);
                let true_ = gen_const_bool(true, graph);

                let phi_block = graph.new_block(&[fls, tr]);
                let phi = phi_block.new_phi(&[false_, true_], false_.mode());

                (phi_block, phi.into())
            }
        }
    }

    /// Assert that self is an lvalue and return it
    fn expect_lvalue(self) -> (Block, LValue<'src>) {
        use self::ExprResult::*;
        match self {
            Assignable(act_block, lvalue) => (act_block, lvalue),
            _ => panic!("cannot assign to {:?}", ExprResultDiscriminants::from(self)),
        }
    }
}

/// An assignable (or evaluatable) lvalue
/// The Idea is, that upon encountering a possible lvalue
/// when looking at an expression in `gen_expr`, we defer the decision
/// on wether to treat it as a simple value or treating it as a location.
/// Then, upon encountering an assignment, we still have the information
/// needed to assign to the location described by the LHS.
enum LValue<'src> {
    // Local variable. This includes parameters
    Var {
        slot_idx: usize,
        mode: Mode,
        span: Span<'src>,
    },
    // Array or field access
    ArrayOrField {
        sel_or_mem: Node,
        item_ty: Ty,
        span: Span<'src>,
    },
}

impl<'src> LValue<'src> {
    /// Evaluate the lvalue just as if it were handled as a normal expression
    fn gen_eval(
        self,
        span_storage: &mut MethodBodyGenerator<'_, 'src, '_>,
        act_block: Block,
    ) -> (Block, Node) {
        use self::LValue::*;

        match self {
            Var {
                slot_idx,
                mode,
                span,
            } => {
                let mut val = act_block.value(slot_idx, mode);
                if Node::is_phi(val) {
                    val = span_storage.with_span(span, val);
                }
                (act_block, val)
            }
            ArrayOrField {
                sel_or_mem,
                item_ty,
                span,
            } => {
                let load = span_storage.with_span(
                    span,
                    act_block.new_load(
                        act_block.cur_store(),
                        sel_or_mem,
                        item_ty.mode(),
                        item_ty,
                        bindings::ir_cons_flags::None,
                    ),
                );
                act_block.set_store(load.new_proj_m());
                (
                    act_block,
                    span_storage
                        .with_span(span, load.new_proj_res(item_ty.mode()))
                        .into(),
                )
            }
        }
    }

    /// Store the given value at the location described by this lvalue
    fn gen_assign(
        self,
        span: Span<'src>,
        span_storage: &mut MethodBodyGenerator<'_, 'src, '_>,
        act_block: Block,
        value: Node,
    ) -> (Block, Node) {
        use self::LValue::*;
        match self {
            Var { slot_idx, .. } => {
                act_block.set_value(slot_idx, value);
                (act_block, value)
            }
            ArrayOrField {
                sel_or_mem,
                item_ty,
                ..
            } => {
                let store = span_storage.with_span(
                    span,
                    act_block.new_store(
                        act_block.cur_store(),
                        sel_or_mem,
                        value,
                        item_ty,
                        bindings::ir_cons_flags::None,
                    ),
                );
                act_block.set_store(store.new_proj_m());
                (act_block, value)
            }
        }
    }
}
