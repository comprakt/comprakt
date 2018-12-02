use super::{get_firm_mode, size_of, ty_from_checked_type, Class, Runtime};
use crate::{
    asciifile::Spanned,
    ast::{self, BinaryOp},
    strtab::{StringTable, Symbol},
    type_checking::{
        type_analysis::{RefInfo, TypeAnalysis},
        type_system::{CheckedType, ClassMethodDef},
    },
};
use libfirm_rs::{bindings::*, *};
use std::{cell::RefCell, collections::HashMap, rc::Rc};
use strum_macros::EnumDiscriminants;

pub struct MethodBodyGenerator<'ir, 'src, 'ast> {
    graph: Graph,
    class: &'ir Class<'src, 'ast>,
    classes: &'ir HashMap<Symbol<'src>, Rc<RefCell<Class<'src, 'ast>>>>,
    method_def: Rc<ClassMethodDef<'src, 'ast>>,
    local_vars: HashMap<Symbol<'src>, (usize, mode::Type)>,
    num_vars: usize,
    runtime: &'ir Runtime,
    type_analysis: &'ir TypeAnalysis<'src, 'ast>,
    strtab: &'ir mut StringTable<'src>,
}

impl<'a, 'ir, 'src, 'ast> MethodBodyGenerator<'ir, 'src, 'ast> {
    pub(super) fn new(
        graph: Graph,
        class: &'ir Class<'src, 'ast>,
        classes: &'ir HashMap<Symbol<'src>, Rc<RefCell<Class<'src, 'ast>>>>,
        method_def: Rc<ClassMethodDef<'src, 'ast>>,
        type_analysis: &'ir TypeAnalysis<'src, 'ast>,
        runtime: &'ir Runtime,
        strtab: &'ir mut StringTable<'src>,
    ) -> Self {
        Self {
            graph,
            class,
            classes,
            local_vars: HashMap::new(),
            num_vars: 0,
            method_def,
            runtime,
            type_analysis,
            strtab,
        }
        .gen_args()
    }

    fn gen_args(mut self) -> Self {
        let args = self.graph.args_node();

        if !self.method_def.is_static {
            let mode_ptr = unsafe { mode::P };
            let this_symbol = self.strtab.intern("this");
            let this_var = self.new_local_var(this_symbol, mode_ptr);
            self.graph.set_value(this_var, &args.project(mode_ptr, 0));

            let method_def = Rc::clone(&self.method_def);
            for (i, p) in method_def.params.iter().enumerate() {
                let mode = get_firm_mode(&p.ty).expect("parmeter cannot be void");
                self.graph
                    .set_value(self.new_local_var(p.name, mode), &args.project(mode, i + 1));
            }
        }

        self
    }

    /// Generate IR for a method body
    pub fn gen_method(&mut self, body: &ast::Block<'src>) {
        self.graph.set_cur_block(self.graph.start_block());
        self.gen_block(body);

        // Void functions have an implicit return in the end
        if self.method_def.return_ty == CheckedType::Void {
            let mem = self.graph.cur_store();
            let ret = self.graph.cur_block().new_return(mem, None);
            self.graph.end_block().add_pred(&ret);
        }

        self.graph.cur_block().mature();
        self.graph.end_block().mature();
    }

    /// Generate IR for a whole block
    fn gen_block(&mut self, block: &ast::Block<'src>) {
        for stmt in &block.statements {
            self.gen_stmt(&stmt);
        }
    }

    /// Generate IR for a single statement
    fn gen_stmt(&mut self, stmt: &ast::Stmt<'src>) {
        use self::ast::Stmt::*;
        match &stmt {
            Block(block) => self.gen_block(block),
            Expression(expr) => {
                self.gen_expr(expr);
            }
            If(cond, then_arm, else_arm) => self.gen_if(cond, then_arm, else_arm),
            While(cond, body) => self.gen_while(cond, body),
            Return(res_expr) => self.gen_return(res_expr),
            LocalVariableDeclaration(_ty, _name, init_expr) => self.gen_var_decl(stmt, init_expr),
            Empty => (),
        }
    }

    fn gen_var_decl(
        &mut self,
        stmt: &ast::Stmt<'src>,
        init_expr: &Option<Box<Spanned<'src, ast::Expr<'src>>>>,
    ) {
        let local_var_def = self
            .type_analysis
            .local_var_def(stmt)
            .expect("Was set by type_analysis. Stmt is local var def.");

        let mode = get_firm_mode(&local_var_def.ty).expect("parmeter cannot be void");

        let var_slot = self.new_local_var(local_var_def.name, mode);
        if let Some(init_expr) = init_expr {
            self.graph.set_value(
                var_slot,
                &self.gen_expr(init_expr).enforce_value(self.graph),
            );
        } else {
            self.graph.set_value(var_slot, &self.gen_zero(mode));
        }
    }

    fn gen_zero(&mut self, mode: mode::Type) -> libfirm_rs::Const {
        self.gen_const(0, mode)
    }

    fn gen_const(&mut self, value: i64, mode: mode::Type) -> libfirm_rs::Const {
        self.graph
            .new_const(unsafe { new_tarval_from_long(value, mode) })
    }

    fn gen_while(&mut self, cond: &ast::Expr<'src>, body: &ast::Stmt<'src>) {
        // TODO DRY beginning nearly the same as If-case
        let prev_block = self.graph.cur_block();

        let incoming_jmp = prev_block.new_jmp();
        let header_block = self.graph.new_imm_block(&incoming_jmp);

        prev_block.mature(); // This block is done now
        self.graph.set_cur_block(header_block);

        // We evaluate the condition
        let CondProjection { tr, fls } = &self.gen_expr(cond).enforce_cond(self.graph);

        // Run body if cond is true
        let body_block = self.graph.new_imm_block(tr);
        {
            self.graph.set_cur_block(body_block);
            self.gen_stmt(&*body);

            // We jump back to the condition-check
            header_block.add_pred(&body_block.new_jmp());
        }

        // Leave loop if cond is false
        let next_block = self.graph.new_imm_block(fls);
        self.graph.set_cur_block(next_block);

        header_block.mature();
        body_block.mature();
    }

    fn gen_if(
        &mut self,
        cond: &ast::Expr<'src>,
        then_arm: &ast::Stmt<'src>,
        else_arm: &Option<Box<Spanned<'src, ast::Stmt<'src>>>>,
    ) {
        let prev_block = self.graph.cur_block();

        // TODO Do we really need an additional header_block? Can't we just put the
        // new_cond in the prev_block?
        let incoming_jmp = prev_block.new_jmp();
        let header_block = self.graph.new_imm_block(&incoming_jmp);

        prev_block.mature(); // This block is done now
        self.graph.set_cur_block(header_block);

        // We evaluate the condition
        let CondProjection { tr, fls } = &self.gen_expr(cond).enforce_cond(self.graph);

        // If its true, we take the then_arm
        let then_block = self.graph.new_imm_block(tr);
        {
            self.graph.set_cur_block(then_block);
            self.gen_stmt(&*then_arm);
        }

        // If its false, we take the else_arm
        // We generate an else_block in any case, when there is no `else`, it's empty
        let else_block = self.graph.new_imm_block(fls);
        if let Some(else_arm) = else_arm {
            self.graph.set_cur_block(else_block);
            self.gen_stmt(&**else_arm);
        }

        header_block.mature();

        let from_then_jmp = then_block.new_jmp();
        let from_else_jmp = else_block.new_jmp();

        // Now we close the if-diamond
        let next_block = self.graph.new_imm_block(&from_then_jmp);
        next_block.add_pred(&from_else_jmp);
        self.graph.set_cur_block(next_block);

        // Those blocks are finished now
        then_block.mature();
        else_block.mature();
    }

    fn gen_return(&mut self, res_expr: &Option<Box<Spanned<'src, ast::Expr<'src>>>>) {
        let mem = self.graph.cur_store();
        let res = res_expr
            .as_ref()
            .map(|res_expr| self.gen_expr(&*res_expr).enforce_value(self.graph));

        let ret = self.graph.cur_block().new_return(mem, res);

        self.graph.end_block().add_pred(&ret);
    }

    fn gen_static_fn_call(
        &mut self,
        func: Entity,
        return_type: Option<mode::Type>,
        args: &[*mut ir_node],
    ) -> ExprResult {
        let call = self.graph.cur_block().new_call(
            self.graph.cur_store(),
            self.graph.new_addr(func),
            &args,
        );

        self.graph.set_store(call.project_mem());

        use self::ExprResult::*;
        match return_type {
            Some(mode) => Value(call.project_result_tuple().project(mode, 0).as_value_node()),
            None => Void,
        }
    }

    /// Return a node that evaluates the given expression
    ///
    /// TODO non-raw-ptr abstraction for ret type; Box<dyn ValueNode> might
    /// work, but unnecessary box
    fn gen_expr(&mut self, expr: &ast::Expr<'src>) -> ExprResult {
        use self::{ast::Expr::*, ExprResult::*};
        match &expr {
            Int(literal) => {
                let val = literal.parse().expect("Integer literal has to be valid");
                Value(self.gen_const(val, unsafe { mode::Is }).as_value_node())
            }
            NegInt(literal) => {
                let val = literal
                    .parse::<i32>()
                    .map_or_else(|_| -2_147_483_648, |v| -v);
                Value(
                    self.gen_const(i64::from(val), unsafe { mode::Is })
                        .as_value_node(),
                )
            }
            Boolean(value) => {
                let as_bit = if *value { 1 } else { 0 };
                Value(self.gen_const(as_bit, unsafe { mode::Bu }).as_value_node())
            }
            Var(name) => {
                match self
                    .type_analysis
                    .expr_info(expr)
                    .ref_info
                    .as_ref()
                    .expect("Variable access expr is always a ref")
                {
                    RefInfo::Var(_) | RefInfo::Param(_) => {
                        log::debug!("gen assignable for var or param {}", **name);
                        let (slot, mode) = self.local_var(**name);
                        Assignable(LValue::Var {
                            slot_idx: slot,
                            mode,
                        })
                    }
                    RefInfo::Field(_) => {
                        log::debug!("gen assignable for field {}", **name);
                        let this = self.this().as_value_node();
                        Assignable(self.gen_field(this, self.class_name(), **name))
                    }
                    _ => unreachable!("Variable access expr is always var, param or field"),
                }
            }
            Binary(op, lhs, rhs) => self.gen_binary_expr(*op, lhs, rhs),
            Unary(ast::UnaryOp::Neg, expr) => {
                let expr = self.gen_expr(expr).enforce_value(self.graph);
                log::debug!("pre new_neg");
                let neg = self.graph.cur_block().new_minus(&expr);
                Value(neg.as_value_node())
            }
            Unary(ast::UnaryOp::Not, expr) => {
                log::debug!("unary op");
                let expr = self.gen_expr(expr);
                use self::ExprResult::*;
                match expr {
                    Void => panic!("type system should not allow !void"),
                    Assignable(_) | Value(_) => {
                        let n = match expr {
                            Assignable(lvalue) => lvalue.gen_eval(self.graph),
                            Value(n) => n,
                            _ => unreachable!(),
                        };

                        // booleansare mode::Bu, hence XOR does the job.
                        // could also use mode::Bi and -1 for true:
                        // => could use Neg / Not, but would rely on 2's complement
                        let bu1 = unsafe {
                            assert_eq!(get_irn_mode(n), mode::Bu);
                            self.graph.new_const(new_tarval_from_long(1, mode::Bu))
                        };
                        Value(self.graph.cur_block().new_xor(&n, &bu1).into())
                    }
                    Cond(proj) => Cond(proj.flip()),
                }
            }
            This => Value(self.this()),
            ThisMethodInvocation(method, argument_list) => {
                let method = self
                    .class
                    .methods
                    .get(&method)
                    .unwrap_or_else(|| {
                        panic!(
                            "invocation of unknown method {} on class {} using implicit `this`",
                            method,
                            self.class_name()
                        )
                    })
                    .borrow();

                let this = self.this();
                let mut args = vec![this];

                for arg in argument_list.iter() {
                    args.push(self.gen_expr(arg).enforce_value(self.graph));
                }

                let return_type = get_firm_mode(&method.def.return_ty);

                self.gen_static_fn_call(method.entity, return_type, &args)
            }
            MethodInvocation(object, method, argument_list) => {
                log::debug!(
                    "gen method invocation for object={:?}, method={:?}",
                    object.span.as_str(),
                    method.span.as_str()
                );

                let object_expr_info = self.type_analysis.expr_info(object);

                let class_id = match object_expr_info.ty {
                    CheckedType::TypeRef(class_def) => class_def.id(),
                    _ => panic!("method invocations can only be done on type references"),
                };

                // we only have one global variable: 'System'. Since implementing
                // global variables just to support this single type seems unnecessary,
                // we just check if a method invocation targets the runtime dollar types (which
                // are singletons, and can therefore be statically dispatched.)
                match class_id.as_str() {
                    // TODO: symbol vergleich, kein string vergleich ;)
                    "$Writer" => {
                        let mut args = vec![];

                        for arg in argument_list.iter() {
                            args.push(self.gen_expr(arg).enforce_value(self.graph));
                        }

                        return match method.data.as_str() {
                            // TODO: symbol vergleich, kein string vergleich ;)
                            "println" => self.gen_static_fn_call(
                                self.runtime.system_out_println,
                                None,
                                &args,
                            ),
                            "write" => {
                                self.gen_static_fn_call(self.runtime.system_out_write, None, &args)
                            }
                            "flush" => {
                                self.gen_static_fn_call(self.runtime.system_out_flush, None, &args)
                            }
                            method_name => {
                                panic!("unknown runtime function System.out.{}", method_name)
                            }
                        };
                    }
                    "$Reader" => {
                        return match method.data.as_str() {
                            // TODO: symbol vergleich, kein string vergleich ;)
                            "read" => self.gen_static_fn_call(
                                self.runtime.system_in_read,
                                get_firm_mode(&CheckedType::Int),
                                &[],
                            ),
                            method_name => {
                                panic!("unknown runtime function System.in.{}", method_name)
                            }
                        };
                    }
                    _ => (),
                }

                // at this point, we known that the invocation is targeting a user defined
                // class and method.

                let class = self
                    .classes
                    .get(&class_id)
                    .expect("method invocation on inexistent class")
                    .borrow();

                let method = class
                    .methods
                    .get(&method)
                    .expect("invocation of unknown method")
                    .borrow();

                // object could be any expression that has to be
                // evaluated, e.g. (this.get_object()).foo()
                let this = self.gen_expr(object).enforce_value(self.graph);

                let mut args = vec![this];

                for arg in argument_list.iter() {
                    args.push(self.gen_expr(arg).enforce_value(self.graph));
                }

                let return_type = get_firm_mode(&method.def.return_ty);

                self.gen_static_fn_call(method.entity, return_type, &args)
            }
            FieldAccess(object, field) => {
                let object_type = match self.type_analysis.expr_info(object).ty {
                    CheckedType::TypeRef(object_type) => object_type,
                    _ => panic!("Only classes have fields"),
                };

                let object_ptr = self.gen_expr(object).enforce_value(self.graph);

                Assignable(self.gen_field(object_ptr, object_type.id(), **field))
            }

            ArrayAccess(target_expr, idx_expr) => {
                let elt_type = ty_from_checked_type(&self.type_analysis.expr_info(expr).ty)
                    .expect("array element type must have firm equivalent");

                Assignable(self.gen_array_access(target_expr, idx_expr, elt_type))
            }

            Null => Value(self.gen_const(0, unsafe { mode::P }).as_value_node()),

            NewObject(ty_name) => {
                let class = self
                    .classes
                    .get(ty_name)
                    .expect("creating non-existing class");

                let size = i64::from(class.borrow().entity.ty().size());

                let call = self.graph.cur_block().new_call(
                    self.graph.cur_store(),
                    self.graph.new_addr(self.runtime.new),
                    &[self.gen_const(size, unsafe { mode::Is }).as_value_node()],
                );

                self.graph.set_store(call.project_mem());

                Value(
                    call.project_result_tuple()
                        .project(unsafe { mode::P }, 0)
                        .as_value_node(),
                )
            }
            NewArray(_, num_expr, _) => {
                let new_array_type = &self
                    .type_analysis
                    .expr_info(expr)
                    .ty
                    .inner_type()
                    .expect("type of array must have inner type");

                let num_elts = self.gen_expr(num_expr).enforce_value(self.graph);
                let elt_size = self.gen_const(
                    size_of(new_array_type)
                        .map(i64::from)
                        .expect("cannot allocate array of unsized type"),
                    unsafe { mode::Is },
                );

                let alloc_size = self.graph.cur_block().new_mul(&num_elts, &elt_size);

                let call = self.graph.cur_block().new_call(
                    self.graph.cur_store(),
                    self.graph.new_addr(self.runtime.new),
                    &[alloc_size.as_value_node()],
                );
                self.graph.set_store(call.project_mem());

                Value(
                    call.project_result_tuple()
                        .project(unsafe { mode::P }, 0)
                        .as_value_node(),
                )
            }
        }
    }

    fn gen_array_access(
        &mut self,
        target_expr: &ast::Expr<'src>,
        idx_expr: &ast::Expr<'src>,
        elt_type: Ty,
    ) -> LValue {
        let array_type = &self.type_analysis.expr_info(target_expr).ty;
        let firm_array_type =
            ty_from_checked_type(array_type).expect("array type must have firm equivalent");

        let target_expr = self.gen_expr(target_expr).enforce_value(self.graph);
        let idx_expr = self.gen_expr(idx_expr).enforce_value(self.graph);

        LValue::Array {
            sel: self.graph.cur_block().new_sel(
                &target_expr,
                &idx_expr,
                firm_array_type.points_to(),
            ),
            elt_type,
        }
    }

    fn gen_field(
        &mut self,
        ptr: *mut ir_node,
        class_name: Symbol<'src>,
        field_name: Symbol<'src>,
    ) -> LValue {
        let class = self
            .classes
            .get(&class_name)
            .expect("Class has to be registered")
            .borrow();
        let field = class
            .fields
            .get(&field_name)
            .expect("Field must exist in class after type checking")
            .borrow();
        let mode = get_firm_mode(&field.def.ty).expect("Type `void` is not a valid field type");

        LValue::Field {
            object: ptr,
            field_entity: field.entity,
            field_mode: mode,
        }
    }

    fn gen_binary_expr(
        &mut self,
        op: BinaryOp,
        lhs: &ast::Expr<'src>,
        rhs: &ast::Expr<'src>,
    ) -> ExprResult {
        macro_rules! enforce {
            (value, $lhs: ident, $rhs: ident) => {
                let lhs = self.gen_expr(lhs);
                let rhs = self.gen_expr(rhs);
                let $lhs = lhs.enforce_value(self.graph);
                let $rhs = rhs.enforce_value(self.graph);
            };
        }

        macro_rules! relation {
            ($lhs: ident, $rhs: ident, $relation: expr) => {{
                enforce!(value, $lhs, $rhs);
                let cmp = self.graph.cur_block().new_cmp(&$lhs, &$rhs, $relation);
                let cond = self.graph.cur_block().new_cond(&cmp);
                Cond(cond.into())
            }};
        }

        macro_rules! arithemtic_op_with_exception {
            ($lhs: ident, $rhs: ident, $op: ident) => {{
                enforce!(value, $lhs, $rhs);
                let mem = self.graph.cur_store();
                log::debug!("pre {}", stringify!($op));
                let op = self
                    .graph
                    .cur_block()
                    .$op(mem, &$lhs, &$rhs, op_pin_state::Pinned as i32);
                self.graph.set_store(op.project_mem());
                log::debug!("pre project res {}", stringify!($op));
                let res = op.project_res();
                log::debug!("pre as_value_node {}", stringify!($op));
                Value(res.as_value_node())
            }};
        }

        macro_rules! arithemtic_op {
            ($lhs: ident, $rhs: ident, $op: ident) => {{
                enforce!(value, $lhs, $rhs);
                let op = self.graph.cur_block().$op(&$lhs, &$rhs);
                Value(op.as_value_node())
            }};
        }

        use self::ExprResult::*;
        match op {
            BinaryOp::Add => arithemtic_op!(lhs, rhs, new_add),
            BinaryOp::Sub => arithemtic_op!(lhs, rhs, new_sub),
            BinaryOp::Mul => arithemtic_op!(lhs, rhs, new_mul),
            BinaryOp::Div => arithemtic_op_with_exception!(lhs, rhs, new_div),
            BinaryOp::Mod => arithemtic_op_with_exception!(lhs, rhs, new_mod),
            BinaryOp::LogicalOr => {
                let lhs = self.gen_expr(lhs);
                let CondProjection {
                    tr: lhs_tr,
                    fls: lhs_fls,
                } = lhs.enforce_cond(self.graph);
                self.graph.cur_block().mature();

                let false_block = self.graph.new_imm_block(&lhs_fls);
                self.graph.set_cur_block(false_block);
                let rhs = self.gen_expr(rhs);
                let CondProjection {
                    tr: rhs_tr,
                    fls: rhs_fls,
                } = rhs.enforce_cond(self.graph);
                false_block.mature();

                // FIXME: we're constructing an empty block here because we are only
                // allowed to return a CondProjection with one X node for true and false,
                // whereas libfirm would allow us to just add multiple predecessors
                // to the target node (2 true X projections in this case)

                let both_true_block = self.graph.new_imm_block(&lhs_tr);
                self.graph.set_cur_block(both_true_block);
                both_true_block.add_pred(&rhs_tr);
                let true_out = both_true_block.new_jmp();
                both_true_block.mature();

                // TODO check if this is necessary, if and while do a similar thing
                // the previous block is done, give a new block to outer exprs
                // The idea is that the outer expr may expect to have a cur block...
                let successor_block = unsafe {
                    Block::from(new_r_immBlock(self.graph.into())) // FIXME API
                };
                self.graph.set_cur_block(successor_block);

                Cond(CondProjection {
                    tr: true_out,
                    fls: rhs_fls,
                })
            }
            BinaryOp::LogicalAnd => {
                let lhs = self.gen_expr(lhs);
                let CondProjection {
                    tr: lhs_tr,
                    fls: lhs_fls,
                } = lhs.enforce_cond(self.graph);
                self.graph.cur_block().mature();

                let lhs_true_block = self.graph.new_imm_block(&lhs_tr);
                self.graph.set_cur_block(lhs_true_block);
                let rhs = self.gen_expr(rhs);
                let CondProjection {
                    tr: rhs_tr,
                    fls: rhs_fls,
                } = rhs.enforce_cond(self.graph);
                lhs_true_block.mature();

                // FIXME: we're constructing an empty block here because we are only
                // allowed to return a CondProjection with one X node for true and false,
                // whereas libfirm would allow us to just add multiple predecessors
                // to the target node (2 true X projections in this case)

                let any_false_block = self.graph.new_imm_block(&lhs_fls);
                self.graph.set_cur_block(any_false_block);
                any_false_block.add_pred(&rhs_fls);
                let false_out = any_false_block.new_jmp();
                any_false_block.mature();

                // TODO check if this is necessary, if and while do a similar thing
                // the previous block is done, give a new block to outer exprs
                // The idea is that the outer expr may expect to have a cur block...
                let successor_block = unsafe {
                    Block::from(new_r_immBlock(self.graph.into())) // FIXME API
                };
                self.graph.set_cur_block(successor_block);

                Cond(CondProjection {
                    tr: rhs_tr,
                    fls: false_out,
                })
            }

            BinaryOp::Assign => Value(
                self.gen_expr(lhs)
                    .expect_lvalue()
                    .gen_assign(self.graph, &self.gen_expr(rhs).enforce_value(self.graph)),
            ),

            BinaryOp::Equals => relation!(lhs, rhs, ir_relation::Equal),
            BinaryOp::NotEquals => relation!(lhs, rhs, ir_relation::LessGreater),
            BinaryOp::LessThan => relation!(lhs, rhs, ir_relation::Less),
            BinaryOp::GreaterThan => relation!(lhs, rhs, ir_relation::Greater),
            BinaryOp::LessEquals => relation!(lhs, rhs, ir_relation::LessEqual),
            BinaryOp::GreaterEquals => relation!(lhs, rhs, ir_relation::GreaterEqual),
        }
    }

    /// Allocate a new local variable in the next free slot
    fn new_local_var(&mut self, name: Symbol<'src>, mode: mode::Type) -> usize {
        let slot = self.num_vars;
        self.num_vars += 1;
        self.local_vars.insert(name, (slot, mode));
        slot
    }

    /// Get name and mode of previously allocated local var
    fn local_var(&mut self, name: Symbol<'src>) -> (usize, mode::Type) {
        match self.local_vars.get(&name) {
            Some(local) => *local,
            None => panic!("undefined variable '{}'", name),
        }
    }

    fn this(&self) -> *mut ir_node {
        self.graph.value(0, unsafe { mode::P }).as_value_node()
    }

    fn class_name(&self) -> Symbol<'src> {
        self.class.def.name
    }
}

/// Result of `MethodBodyGenerator::gen_expr`
#[derive(EnumDiscriminants)]
enum ExprResult {
    /// No Result (e.g. call of void method)
    Void,
    /// Result is a single value (e.g. method call, variable, integer
    /// arithmetic)
    Value(*mut ir_node),
    /// Result is two mode::X nodes (i.e. control flow) that can be branched on
    /// (e.g. result of short-circuiting binary expr, `||`, `==`, `&&`, ...)
    Cond(CondProjection),

    /// An assignable lvalue, such as parameter / local var or array access
    Assignable(LValue),
}

struct CondProjection {
    tr: Jmp,
    fls: Jmp,
}

impl CondProjection {
    fn flip(self) -> CondProjection {
        let CondProjection { tr, fls } = self;
        CondProjection { tr: fls, fls: tr }
    }
}

impl From<Cond> for CondProjection {
    fn from(cond: Cond) -> CondProjection {
        CondProjection {
            tr: cond.project_true(),
            fls: cond.project_false(),
        }
    }
}

impl ExprResult {
    /// Enforce that the result is variant Cond. If self is a boolean `Value`,
    /// convert it to a selector that projects the two cases. If it is a
    /// non-boolean value, libfirm will complain.
    fn enforce_cond(self, graph: Graph) -> CondProjection {
        use self::ExprResult::*;
        match self {
            Void => panic!("Tried to branch on result of void expr"),
            Value(val) => {
                let one = graph.new_const(unsafe { new_tarval_from_long(1, mode::Bu) });

                let sel = graph
                    .cur_block()
                    .new_cmp(&val, &one, ir_relation::Equal)
                    .as_selector();
                let cond = graph.cur_block().new_cond(&sel);
                CondProjection::from(cond)
            }
            Assignable(lval) => Value(lval.gen_eval(graph)).enforce_cond(graph),
            Cond(cp) => cp,
        }
    }

    /// Enforce that the result is a single value. If self is a `Cond`,
    /// convert it to boolean value
    fn enforce_value(self, graph: Graph) -> *mut ir_node {
        use self::ExprResult::*;
        match self {
            Void => panic!("Tried to get result value of void expr"),
            Value(val) => val,
            Assignable(lval) => lval.gen_eval(graph),
            Cond(cp) => {
                let CondProjection { tr, fls } = cp;
                let zero = graph.new_const(unsafe { new_tarval_from_long(0, mode::Bu) });
                let one = graph.new_const(unsafe { new_tarval_from_long(1, mode::Bu) });

                let phi_block = unsafe {
                    let phi_block = new_r_immBlock(graph.into());
                    add_immBlock_pred(phi_block, fls.into());
                    add_immBlock_pred(phi_block, tr.into());
                    phi_block
                };
                let phi = unsafe {
                    let inputs = [zero.into(), one.into()];
                    new_r_Phi(phi_block, 2, inputs.as_ptr(), mode::Bu)
                };
                let phi_block = Block::from(phi_block);
                phi_block.mature();
                graph.set_cur_block(phi_block);
                phi
            }
        }
    }

    /// Assert that self is an lvalue and return it
    fn expect_lvalue(self) -> LValue {
        use self::ExprResult::*;
        match self {
            Assignable(lvalue) => lvalue,
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
        mode: mode::Type,
    },
    // Array access
    Array {
        sel: Sel,
        elt_type: Ty,
    },
    // Class field access
    Field {
        object: *mut ir_node,
        field_mode: mode::Type,
        field_entity: Entity,
    },
}

impl LValue {
    /// Evaluate the lvalue just as if it were handled as a normal expression
    fn gen_eval(self, graph: Graph) -> *mut ir_node {
        use self::LValue::*;
        match self {
            Var { slot_idx, mode } => graph.value(slot_idx, mode).as_value_node(),
            Array { sel, elt_type } => sel.gen_load(graph, elt_type).as_value_node(),
            Field {
                object,
                field_mode,
                field_entity,
            } => {
                let member = graph.cur_block().new_member(&object, field_entity);
                let load = graph.cur_block().new_load(
                    graph.cur_store(),
                    &member,
                    field_mode,
                    field_entity.ty(),
                    ir_cons_flags::None,
                );
                graph.set_store(load.project_mem());
                load.project_res(field_mode).as_value_node()
            }
        }
    }

    /// Store the given value at the location described by this lvalue
    fn gen_assign<V: ValueNode>(self, graph: Graph, value: &V) -> *mut ir_node {
        use self::LValue::*;
        match self {
            Var { slot_idx, mode } => {
                graph.set_value(slot_idx, value);
                graph.value(slot_idx, mode).as_value_node()
            }
            Array { sel, elt_type } => {
                sel.gen_store(graph, value);
                sel.gen_load(graph, elt_type).as_value_node()
            }
            Field {
                object,
                field_entity,
                ..
            } => {
                let member = graph.cur_block().new_member(&object, field_entity);
                let store = graph.cur_block().new_store(
                    graph.cur_store(),
                    &member,
                    value,
                    field_entity.ty(),
                    ir_cons_flags::None,
                );
                graph.set_store(store.project_mem());
                value.as_value_node()
            }
        }
    }
}
