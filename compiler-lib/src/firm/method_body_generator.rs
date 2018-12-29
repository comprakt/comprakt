use super::{
    firm_program::FirmProgram,
    type_translation::{get_firm_mode, size_of, ty_from_checked_type},
    Runtime,
};
use crate::{
    asciifile::Spanned,
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
use libfirm_rs::{bindings, entity::Entity, nodes::*, tarval::*, types::*, Graph, Mode};
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
    strtab: &'ir mut StringTable<'src>,
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
        strtab: &'ir mut StringTable<'src>,
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
        }
    }

    pub fn get_const_bool(&mut self, val: bool) {
        self.graph
            .new_const(Tarval::mj_int(if val { 1 } else { 0 }));
    }

    pub fn gen_method(&mut self, body: &'ast ast::Block<'src>) {
        let act_blck = self.graph.start_block();
        self.set_graph_arg_vals(act_blck);

        let act_blck = self.gen_block(act_blck, body);

        if let ActiveBlock::Some(act_blck) = act_blck {
            let ret = act_blck.new_return(act_blck.cur_store(), &[]);
            self.graph.end_block().imm_add_pred(ret);
        }

        self.graph.end_block().mature();
    }

    fn set_graph_arg_vals(&mut self, block: Block) {
        let args = self.graph.args();

        if !self.method_def.is_static {
            let this_symbol = self.strtab.intern("this");
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

    fn gen_block(&mut self, act_blck: Block, block: &'ast ast::Block<'src>) -> ActiveBlock {
        block.statements.iter().fold(
            ActiveBlock::Some(act_blck),
            |act_blck, stmt| match act_blck {
                ActiveBlock::Some(act_blck) => self.gen_stmt(act_blck, &stmt),
                ActiveBlock::None => ActiveBlock::None,
            },
        )
    }

    fn gen_stmt(&mut self, act_blck: Block, stmt: &'ast ast::Stmt<'src>) -> ActiveBlock {
        use self::ast::Stmt::*;
        match &stmt {
            Block(block) => self.gen_block(act_blck, block),
            Expression(expr) => self.gen_expr(act_blck, expr).active_block(),
            If(cond, then_arm, else_arm) => self.gen_if(act_blck, cond, then_arm, else_arm),
            While(cond, body) => self.gen_while(act_blck, cond, body),
            Return(res_expr) => self.gen_return(act_blck, res_expr),
            LocalVariableDeclaration(_ty, _name, init_expr) => {
                self.gen_var_decl(act_blck, stmt, init_expr)
            }
            Empty => ActiveBlock::Some(act_blck),
        }
    }

    fn gen_var_decl(
        &mut self,
        act_blck: Block,
        stmt: &'ast ast::Stmt<'src>,
        init_expr: &'ast Option<Box<Spanned<'src, ast::Expr<'src>>>>,
    ) -> ActiveBlock {
        let local_var_def = self
            .type_analysis
            .local_var_def(stmt)
            .expect("Was set by type_analysis. Stmt is local var def.");

        let mode = get_firm_mode(&local_var_def.ty).expect("parmeter cannot be void");

        let var_slot = self.new_local_var(local_var_def.name, mode);

        let (act_blck, initial_val) = if let Some(init_expr) = init_expr {
            self.gen_expr(act_blck, init_expr).enforce_value(self.graph)
        } else {
            (act_blck, self.graph.new_const(Tarval::zero(mode)).into())
        };

        act_blck.set_value(var_slot, initial_val);

        ActiveBlock::Some(act_blck)
    }

    fn gen_while(
        &mut self,
        act_blck: Block,
        cond: &'ast ast::Expr<'src>,
        body: &'ast ast::Stmt<'src>,
    ) -> ActiveBlock {
        // We evaluate the condition
        let header_block = self.graph.new_imm_block(&[act_blck.new_jmp().into()]);
        let CondProjection { tr, fls } = self.gen_expr(header_block, cond).enforce_cond(self.graph);

        // Run body if cond is true
        let body_block = self.graph.new_block(&[tr.into()]);
        if let ActiveBlock::Some(body_block) = self.gen_stmt(body_block, &*body) {
            // We jump back to the condition-check
            header_block.imm_add_pred(body_block.new_jmp());
        }
        header_block.mature();
        header_block.keep_alive(); // to keep endless loops

        // Leave loop if cond is false
        let after_loop_block = self.graph.new_block(&[fls.into()]);

        ActiveBlock::Some(after_loop_block)
    }

    fn gen_if(
        &mut self,
        act_blck: Block,
        cond: &'ast ast::Expr<'src>,
        then_arm: &'ast ast::Stmt<'src>,
        else_arm: &'ast Option<Box<Spanned<'src, ast::Stmt<'src>>>>,
    ) -> ActiveBlock {
        // We evaluate the condition
        let CondProjection { tr, fls } = self.gen_expr(act_blck, cond).enforce_cond(self.graph);

        // If its true, we take the then_arm
        let then_block = self.gen_stmt(self.graph.new_block(&[tr]), &*then_arm);

        // If its false, we take the else_arm
        let else_block = self.graph.new_block(&[fls]);
        let else_block = if let Option::Some(else_arm) = else_arm {
            self.gen_stmt(else_block, &**else_arm)
        } else {
            ActiveBlock::Some(else_block)
        };

        use self::ActiveBlock::*;
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
        act_blck: Block,
        res_expr: &'ast Option<Box<Spanned<'src, ast::Expr<'src>>>>,
    ) -> ActiveBlock {
        let mut res = vec![];
        let act_blck = if let Some(res_expr) = res_expr {
            let (act_blck, val) = self
                .gen_expr(act_blck, &*res_expr)
                .enforce_value(self.graph);
            res.push(val);
            act_blck
        } else {
            act_blck
        };

        let ret = act_blck.new_return(act_blck.cur_store(), &res);
        self.graph.end_block().imm_add_pred(ret);

        ActiveBlock::None
    }

    fn gen_static_fn_call(
        &mut self,
        act_blck: Block,
        func: Entity,
        return_type: Option<Mode>,
        args: &[Node],
    ) -> ExprResult {
        let call = act_blck.new_call(
            act_blck.cur_store(),
            self.graph.new_address(func),
            &args,
            func.ty(),
        );

        act_blck.set_store(call.new_proj_m());

        use self::ExprResult::*;
        match return_type {
            Some(mode) => Value(act_blck, call.new_proj_t_result().new_proj(0, mode).into()),
            None => Void(act_blck),
        }
    }

    fn gen_expr_list<I>(&mut self, act_blck: Block, exprs: I) -> (Block, Vec<Node>)
    where
        I: Iterator<Item = &'ast ast::Expr<'src>>,
    {
        let mut result = vec![];
        let act_blck = exprs.fold(act_blck, |act_blck, arg| {
            let (act_blck, arg) = self.gen_expr(act_blck, arg).enforce_value(self.graph);
            result.push(arg);
            act_blck
        });
        (act_blck, result)
    }

    fn gen_expr(&mut self, act_blck: Block, expr: &'ast ast::Expr<'src>) -> ExprResult {
        use self::{ast::Expr::*, ExprResult::*};
        match &expr {
            Int(literal) => {
                let val = literal.parse().expect("Integer literal has to be valid");
                Value(act_blck, self.graph.new_const(Tarval::mj_int(val)).into())
            }
            NegInt(literal) => {
                let val = literal
                    .parse::<i32>()
                    .map_or_else(|_| -2_147_483_648, |v| -v);
                Value(
                    act_blck,
                    self.graph.new_const(Tarval::mj_int(i64::from(val))).into(),
                )
            }
            Boolean(value) => Value(act_blck, gen_const_bool(*value, self.graph)),
            This => Value(act_blck, self.this(act_blck)),
            Null => Value(
                act_blck,
                self.graph.new_const(Tarval::zero(Mode::P())).into(),
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
                    Var(_) | Param(_) => {
                        log::debug!("gen assignable for var or param {}", **name);
                        let (slot, mode) = self.local_var(**name);
                        Assignable(
                            act_blck,
                            LValue::Var {
                                slot_idx: slot,
                                mode,
                            },
                        )
                    }
                    Field(field_def) => {
                        log::debug!("gen assignable for field {}", **name);
                        Assignable(
                            act_blck,
                            self.gen_field(self.this(act_blck), Rc::clone(field_def)),
                        )
                    }
                    GlobalVar(..) | This(..) | ArrayAccess | Method(..) => {
                        unreachable!("Variable access expr is always var, param or field")
                    }
                }
            }
            FieldAccess(object, _field) => {
                let field_def = match &self.type_analysis.expr_info(expr).ref_info {
                    Some(RefInfo::Field(field_def)) => Rc::clone(field_def),
                    other => unreachable!("Got unexpected: {:?}", other),
                };
                let (act_blck, object_ptr) =
                    self.gen_expr(act_blck, object).enforce_value(self.graph);
                Assignable(act_blck, self.gen_field(object_ptr, field_def))
            }
            Binary(op, lhs, rhs) => self.gen_binary_expr(act_blck, *op, lhs, rhs),
            Unary(ast::UnaryOp::Neg, expr) => {
                let (act_blck, expr) = self.gen_expr(act_blck, expr).enforce_value(self.graph);
                log::debug!("pre new_neg");
                Value(act_blck, act_blck.new_minus(expr).into())
            }
            Unary(ast::UnaryOp::Not, expr) => {
                log::debug!("unary op");
                let expr = self.gen_expr(act_blck, expr);
                use self::ExprResult::*;
                match expr {
                    Void(_) => panic!("type system should not allow !void"),
                    Assignable(act_blck, _) | Value(act_blck, _) => {
                        let (act_blck, val) = match expr {
                            Assignable(act_blck, lvalue) => lvalue.gen_eval(act_blck),
                            Value(act_blk, n) => (act_blk, n),
                            Void(_) | Cond(..) => unreachable!(),
                        };

                        // booleansare mode::Bu, hence XOR does the job.
                        // could also use mode::Bi and -1 for true:
                        // => could use Neg / Not, but would rely on 2's complement
                        // TODO assert_eq!(get_irn_mode(n), Mode::Bu().libfirm_mode());
                        Value(
                            act_blck,
                            act_blck
                                .new_eor(val, gen_const_bool(true, self.graph))
                                .into(),
                        )
                    }
                    Cond(proj) => Cond(proj.flip()),
                }
            }
            MethodInvocation(_, _method, argument_list)
            | ThisMethodInvocation(_method, argument_list) => {
                let class_method_def = match &self.type_analysis.expr_info(expr).ref_info {
                    Some(RefInfo::Method(class_method_def)) => Rc::clone(class_method_def),
                    _ => panic!("type analysis inconsistent"),
                };

                if let ClassMethodBody::Builtin(builtin) = &class_method_def.body {
                    let (act_blck, args) =
                        self.gen_expr_list(act_blck, argument_list.data.iter().map(|a| &a.data));
                    // ENHANCEMENT: @hediet: dedup builtin type definitions
                    return match builtin {
                        BuiltinMethodBody::SystemOutPrintln => self.gen_static_fn_call(
                            act_blck,
                            self.runtime.system_out_println,
                            None,
                            &args,
                        ),
                        BuiltinMethodBody::SystemOutWrite => self.gen_static_fn_call(
                            act_blck,
                            self.runtime.system_out_write,
                            None,
                            &args,
                        ),
                        BuiltinMethodBody::SystemOutFlush => self.gen_static_fn_call(
                            act_blck,
                            self.runtime.system_out_flush,
                            None,
                            &args,
                        ),
                        BuiltinMethodBody::SystemInRead => self.gen_static_fn_call(
                            act_blck,
                            self.runtime.system_in_read,
                            get_firm_mode(&CheckedType::Int),
                            &args,
                        ),
                    };
                } else {
                    let method = self.program.method(class_method_def).unwrap();

                    let (act_blck, target_obj) = match expr {
                        MethodInvocation(target_obj, ..) => self
                            .gen_expr(act_blck, target_obj)
                            .enforce_value(self.graph),
                        ThisMethodInvocation(..) => (act_blck, self.this(act_blck)),
                        _ => unreachable!(),
                    };
                    let (act_blck, mut args) =
                        self.gen_expr_list(act_blck, argument_list.data.iter().map(|a| &a.data));
                    args.insert(0, target_obj);
                    // IMPROVEMENT: get rid of insert

                    let return_type = get_firm_mode(&method.borrow().def.return_ty);

                    let entity = method.borrow().entity;
                    self.gen_static_fn_call(act_blck, entity, return_type, &args)
                }
            }
            ArrayAccess(target_expr, idx_expr) => {
                let elt_type = ty_from_checked_type(&self.type_analysis.expr_info(expr).ty)
                    .expect("array element type must have firm equivalent");

                let array_type = &self.type_analysis.expr_info(target_expr).ty;
                let firm_array_type = PointerTy::from(
                    ty_from_checked_type(array_type).expect("array type must have firm equivalent"),
                )
                .expect("must be pointer type")
                .points_to();

                let (act_blck, target_expr) = self
                    .gen_expr(act_blck, target_expr)
                    .enforce_value(self.graph);
                let (act_blck, idx_expr) =
                    self.gen_expr(act_blck, idx_expr).enforce_value(self.graph);

                Assignable(
                    act_blck,
                    LValue::Array {
                        sel: act_blck.new_sel(target_expr, idx_expr, firm_array_type),
                        elt_type,
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
                let size = i64::from(class.borrow().entity.ty().size());

                let call = act_blck.new_call(
                    act_blck.cur_store(),
                    self.graph.new_address(self.runtime.new),
                    &[self.graph.new_const(Tarval::mj_int(size)).into()],
                    self.runtime.new.ty(),
                );

                act_blck.set_store(call.new_proj_m());

                Value(
                    act_blck,
                    call.new_proj_t_result().new_proj(0, Mode::P()).into(),
                )
            }
            NewArray(_, num_expr, _) => {
                let new_array_type = &self
                    .type_analysis
                    .expr_info(expr)
                    .ty
                    .inner_type()
                    .expect("type of array must have inner type");

                let (act_blck, num_elts) =
                    self.gen_expr(act_blck, num_expr).enforce_value(self.graph);

                let elt_size = self.graph.new_const(Tarval::mj_int(
                    size_of(new_array_type)
                        .map(i64::from)
                        .expect("cannot allocate array of unsized type"),
                ));

                let alloc_size = act_blck.new_mul(num_elts, elt_size);

                let call = act_blck.new_call(
                    act_blck.cur_store(),
                    self.graph.new_address(self.runtime.new),
                    &[alloc_size.into()],
                    self.runtime.new.ty(),
                );
                act_blck.set_store(call.new_proj_m());

                Value(
                    act_blck,
                    call.new_proj_t_result().new_proj(0, Mode::P()).into(),
                )
            }
        }
    }

    fn gen_field(&mut self, ptr: Node, field_def: Rc<ClassFieldDef<'src>>) -> LValue {
        let field = self.program.field(Rc::clone(&field_def)).unwrap();
        let field_mode =
            get_firm_mode(&field_def.ty).expect("Type `void` is not a valid field type");
        let field_entity = field.borrow().entity;
        LValue::Field {
            object: ptr,
            field_entity,
            field_mode,
        }
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

    fn gen_binary_expr(
        &mut self,
        act_blck: Block,
        op: BinaryOp,
        lhs: &'ast ast::Expr<'src>,
        rhs: &'ast ast::Expr<'src>,
    ) -> ExprResult {
        use self::ExprResult::*;
        let pinned = bindings::op_pin_state::Pinned as i32;
        match op {
            op if Self::relation(op).is_some() => {
                let relation = Self::relation(op).unwrap();
                let (act_blck, lhs) = self.gen_expr(act_blck, lhs).enforce_value(self.graph);
                let (act_blck, rhs) = self.gen_expr(act_blck, rhs).enforce_value(self.graph);
                let cond = act_blck.new_cond(act_blck.new_cmp(lhs, rhs, relation));
                Cond(CondProjection::new(cond))
            }
            BinaryOp::Add | BinaryOp::Sub | BinaryOp::Mul => {
                let (act_blck, lhs) = self.gen_expr(act_blck, lhs).enforce_value(self.graph);
                let (act_blck, rhs) = self.gen_expr(act_blck, rhs).enforce_value(self.graph);
                let node: Node = match op {
                    BinaryOp::Add => act_blck.new_add(lhs, rhs).into(),
                    BinaryOp::Sub => act_blck.new_sub(lhs, rhs).into(),
                    BinaryOp::Mul => act_blck.new_mul(lhs, rhs).into(),
                    _ => unreachable!(),
                };
                Value(act_blck, node)
            }
            BinaryOp::Div | BinaryOp::Mod => {
                let (act_blck, lhs) = self.gen_expr(act_blck, lhs).enforce_value(self.graph);
                let (act_blck, rhs) = self.gen_expr(act_blck, rhs).enforce_value(self.graph);
                let mem = act_blck.cur_store();

                let lhs = act_blck.new_conv(lhs, Mode::Ls());
                let rhs = act_blck.new_conv(rhs, Mode::Ls());

                let (res, mem) = match op {
                    BinaryOp::Div => {
                        log::debug!("Mode lhs: {:?}, rhs: {:?}", lhs.mode(), rhs.mode());
                        let node = act_blck.new_div(mem, lhs, rhs, pinned);
                        (node.new_proj_res(Mode::Ls()), node.new_proj_m())
                    }
                    BinaryOp::Mod => {
                        let node = act_blck.new_mod(mem, lhs, rhs, pinned);
                        (node.new_proj_res(Mode::Ls()), node.new_proj_m())
                    }
                    _ => unreachable!(),
                };

                let res = act_blck.new_conv(res, Mode::Is());
                act_blck.set_store(mem);
                Value(act_blck, res.into())
            }
            BinaryOp::LogicalOr => {
                let lhs = self.gen_expr(act_blck, lhs).enforce_cond(self.graph);
                let false_block = self.graph.new_block(&[lhs.fls]);
                let rhs = self.gen_expr(false_block, rhs).enforce_cond(self.graph);
                // IMPROVEMENT: this block is unneccessary if we could return multiple tr nodes
                let both_true_block = self.graph.new_block(&[lhs.tr, rhs.tr]);
                Cond(CondProjection {
                    tr: both_true_block.new_jmp().into(),
                    fls: rhs.fls,
                })
            }
            BinaryOp::LogicalAnd => {
                let lhs = self.gen_expr(act_blck, lhs).enforce_cond(self.graph);
                let lhs_true_block = self.graph.new_block(&[lhs.tr]);
                let rhs = self.gen_expr(lhs_true_block, rhs).enforce_cond(self.graph);
                // IMPROVEMENT: this block is unneccessary if we could return multiple fls nodes
                let any_false_block = self.graph.new_block(&[lhs.fls, rhs.fls]);
                Cond(CondProjection {
                    tr: rhs.tr,
                    fls: any_false_block.new_jmp().into(),
                })
            }
            BinaryOp::Assign => {
                let (act_blck, lvalue) = self.gen_expr(act_blck, lhs).expect_lvalue();
                let (act_blck, value) = self.gen_expr(act_blck, rhs).enforce_value(self.graph);
                ExprResult::value_tuple(lvalue.gen_assign(act_blck, value))
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
    fn local_var(&mut self, name: Symbol<'src>) -> (usize, Mode) {
        match self.local_vars.get(&name) {
            Some(local) => *local,
            None => panic!("undefined variable '{}'", name),
        }
    }

    fn this(&self, act_blck: Block) -> Node {
        act_blck.value(0, Mode::P())
    }
}

/// Result of `MethodBodyGenerator::gen_expr`
#[derive(EnumDiscriminants)]
enum ExprResult {
    /// No Result (e.g. call of void method)
    Void(Block),
    /// Result is a single value (e.g. method call, variable, integer
    /// arithmetic)
    Value(Block, Node),
    /// Result is two mode::X nodes (i.e. control flow) that can be branched on
    /// (e.g. result of short-circuiting binary expr, `||`, `==`, `&&`, ...)
    Cond(CondProjection),

    /// An assignable lvalue, such as parameter / local var or array access
    Assignable(Block, LValue),
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
        CondProjection {
            tr: cond.new_proj_true().into(),
            fls: cond.new_proj_false().into(),
        }
    }

    fn flip(self) -> CondProjection {
        let CondProjection { tr, fls } = self;
        CondProjection { tr: fls, fls: tr }
    }
}

fn gen_const_bool(val: bool, graph: Graph) -> Node {
    graph
        .new_const(Tarval::val(if val { 1 } else { 0 }, Mode::Bu()))
        .into()
}

impl ExprResult {
    fn value_tuple(tuple: (Block, Node)) -> ExprResult {
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
    fn enforce_cond(self, graph: Graph) -> CondProjection {
        use self::ExprResult::*;
        match self {
            Void(..) => panic!("Tried to branch on result of void expr"),
            Value(act_blck, node) => {
                let cond = act_blck.new_cond(act_blck.new_cmp(
                    node,
                    gen_const_bool(true, graph),
                    bindings::ir_relation::Equal,
                ));

                CondProjection::new(cond)
            }
            Assignable(act_blck, lval) => {
                Self::value_tuple(lval.gen_eval(act_blck)).enforce_cond(graph)
            }
            Cond(cp) => cp,
        }
    }

    /// Enforce that the result is a single value. If self is a `Cond`,
    /// convert it to boolean value
    fn enforce_value(self, graph: Graph) -> (Block, Node) {
        use self::ExprResult::*;
        match self {
            Void(_) => panic!("Tried to get result value of void expr"),
            Value(act_blck, val) => (act_blck, val),
            Assignable(act_blck, lval) => lval.gen_eval(act_blck),
            Cond(cp) => {
                let CondProjection { tr, fls } = cp;

                let false_ = gen_const_bool(false, graph);
                let true_ = gen_const_bool(true, graph);

                let phi_block = graph.new_block(&[fls.into(), tr.into()]);
                let phi = phi_block.new_phi(&[false_, true_], false_.mode());

                (phi_block, phi.into())
            }
        }
    }

    /// Assert that self is an lvalue and return it
    fn expect_lvalue(self) -> (Block, LValue) {
        use self::ExprResult::*;
        match self {
            Assignable(act_blck, lvalue) => (act_blck, lvalue),
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
enum LValue {
    // Local variable. This includes parameters
    Var {
        slot_idx: usize,
        mode: Mode,
    },
    // Array access
    Array {
        sel: Sel,
        elt_type: Ty,
    },
    // Class field access
    Field {
        object: Node,
        field_mode: Mode,
        field_entity: Entity,
    },
}

impl LValue {
    /// Evaluate the lvalue just as if it were handled as a normal expression
    fn gen_eval(self, act_blck: Block) -> (Block, Node) {
        use self::LValue::*;

        match self {
            Var { slot_idx, mode } => (act_blck, act_blck.value(slot_idx, mode)),
            Array { sel, elt_type } => {
                let load = act_blck.new_load(
                    act_blck.cur_store(),
                    sel,
                    elt_type.mode(),
                    elt_type,
                    bindings::ir_cons_flags::None,
                );
                act_blck.set_store(load.new_proj_m());
                (act_blck, load.new_proj_res(elt_type.mode()).into())
            }
            Field {
                object,
                field_mode,
                field_entity,
            } => {
                let member = act_blck.new_member(object, field_entity);
                let load = act_blck.new_load(
                    act_blck.cur_store(),
                    member,
                    field_mode,
                    field_entity.ty(),
                    bindings::ir_cons_flags::None,
                );
                act_blck.set_store(load.new_proj_m());
                (act_blck, load.new_proj_res(field_mode).into())
            }
        }
    }

    /// Store the given value at the location described by this lvalue
    fn gen_assign(self, act_blck: Block, value: Node) -> (Block, Node) {
        use self::LValue::*;
        match self {
            Var { slot_idx, mode: _ } => {
                act_blck.set_value(slot_idx, value);
                (act_blck, value)
            }
            Array { sel, elt_type } => {
                let store = act_blck.new_store(
                    act_blck.cur_store(),
                    sel,
                    value,
                    elt_type,
                    bindings::ir_cons_flags::None,
                );
                act_blck.set_store(store.new_proj_m());
                (act_blck, value)
            }
            Field {
                object,
                field_entity,
                ..
            } => {
                let member = act_blck.new_member(object, field_entity);
                let store = act_blck.new_store(
                    act_blck.cur_store(),
                    member,
                    value,
                    field_entity.ty(),
                    bindings::ir_cons_flags::None,
                );
                act_blck.set_store(store.new_proj_m());
                (act_blck, value)
            }
        }
    }
}
