use super::{get_firm_mode, size_of, Class, Runtime};
use crate::{
    asciifile::Spanned,
    ast::{self, BinaryOp},
    strtab::Symbol,
    type_checking::{
        method_body_type_checker::RefInfo,
        type_analysis::TypeAnalysis,
        type_system::{CheckedType, ClassMethodDef},
    },
};
use libfirm_rs::{bindings::*, *};
use std::{cell::RefCell, collections::HashMap, rc::Rc};

pub struct MethodBodyGenerator<'ir, 'src, 'ast> {
    graph: Graph,
    class: &'ir Class<'src, 'ast>,
    classes: &'ir HashMap<Symbol<'src>, Rc<RefCell<Class<'src, 'ast>>>>,
    method_def: Rc<ClassMethodDef<'src, 'ast>>,
    local_vars: HashMap<Symbol<'src>, (usize, mode::Type)>,
    num_vars: usize,
    runtime: &'ir Runtime,
    type_analysis: &'ir TypeAnalysis<'src, 'ast>,
}

impl<'a, 'ir, 'src, 'ast> MethodBodyGenerator<'ir, 'src, 'ast> {
    pub(super) fn new(
        graph: Graph,
        class: &'ir Class<'src, 'ast>,
        classes: &'ir HashMap<Symbol<'src>, Rc<RefCell<Class<'src, 'ast>>>>,
        method_def: Rc<ClassMethodDef<'src, 'ast>>,
        type_analysis: &'ir TypeAnalysis<'src, 'ast>,
        runtime: &'ir Runtime,
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
        }
        .gen_args()
    }

    fn gen_args(mut self) -> Self {
        let args = self.graph.args_node();

        if !self.method_def.is_static {
            // TODO `this`-ptr graph.set_value(0, &args.project(unsafe { mode::P }, 0));
            self.num_vars += 1;

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
            LocalVariableDeclaration(ty, name, init_expr) => self.gen_var_decl(ty, name, init_expr),
            Empty => (),
        }
    }

    fn gen_var_decl(
        &mut self,
        ty: &ast::Type<'src>,
        name: &Symbol<'src>,
        init_expr: &Option<Box<Spanned<'src, ast::Expr<'src>>>>,
    ) {
        // TODO: move mode conversion to its own function
        let mode = unsafe {
            match &ty.basic.data {
                ast::BasicType::Int => mode::Is,
                ast::BasicType::Boolean => mode::Bu,
                ast::BasicType::Custom(_) => mode::P,
                ast::BasicType::Void => panic!("type analysis should prohibit void local var"),
                ast::BasicType::MainParam => {
                    panic!("type analysis should prohbiit MainParam local var")
                }
            }
        };

        let var_slot = self.new_local_var(*name, mode);
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
        let cond = header_block.new_cond(&self.gen_expr(cond).enforce_selector(self.graph));

        // Run body if cond is true
        let body_block = self.graph.new_imm_block(&cond.project_true());
        {
            self.graph.set_cur_block(body_block);
            self.gen_stmt(&*body);

            // We jump back to the condition-check
            header_block.add_pred(&body_block.new_jmp());
        }

        // Leave loop if cond is false
        let next_block = self.graph.new_imm_block(&cond.project_false());
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
        let cond = header_block.new_cond(&self.gen_expr(cond).enforce_selector(self.graph));

        // If its true, we take the then_arm
        let then_block = self.graph.new_imm_block(&cond.project_true());
        {
            self.graph.set_cur_block(then_block);
            self.gen_stmt(&*then_arm);
        }

        // If its false, we take the else_arm
        // We generate an else_block in any case, when there is no `else`, it's empty
        let else_block = self.graph.new_imm_block(&cond.project_false());
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
                        let (slot, mode) = self.local_var(**name);
                        Value(self.graph.value(slot, mode).as_value_node())
                    }
                    RefInfo::Field(_) => {
                        // this pointer is in slot 0
                        let pre_ptr = unsafe { new_r_Proj(self.this(), mode::P, 0) };
                        Value(self.gen_field(pre_ptr, self.class_name(), **name))
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
            Unary(_, _expr) => unimplemented!(),
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
                let object_ir_node = self.gen_expr(object);
                match self.type_analysis.expr_info(object).ty {
                    CheckedType::TypeRef(object_type) => {
                        let object_ptr = object_ir_node.enforce_value(self.graph);
                        Value(self.gen_field(object_ptr, object_type.id(), **field))
                    }
                    _ => panic!("Only classes have fields"),
                }
            }
            ArrayAccess(_expr, _index_expr) => unimplemented!(),
            Null => Value(self.gen_const(0, unsafe { mode::P }).as_value_node()),
            NewObject(ty_name) => {
                // TODO classes should be hash map for efficient lookup
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

    fn gen_field(
        &mut self,
        ptr: *mut ir_node,
        class_name: Symbol<'src>,
        field_name: Symbol<'src>,
    ) -> *mut ir_node {
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
        let member = self.graph.cur_block().new_member(&ptr, field.entity);
        let mode = get_firm_mode(&field.def.ty).expect("Type `void` is not a valid field type");
        let load = self.graph.cur_block().new_load(
            self.graph.cur_store(),
            &member.ptr(),
            mode,
            field.entity.ty(),
            ir_cons_flags::None,
        );
        self.graph.set_store(load.project_mem());
        load.project_res(mode).as_value_node()
    }

    fn gen_binary_expr(
        &mut self,
        op: BinaryOp,
        lhs: &ast::Expr<'src>,
        rhs: &ast::Expr<'src>,
    ) -> ExprResult {
        let lhs = self.gen_expr(lhs);
        let rhs = self.gen_expr(rhs);

        macro_rules! enforce {
            (value, $lhs: ident, $rhs: ident) => {
                let $lhs = lhs.enforce_value(self.graph);
                let $rhs = rhs.enforce_value(self.graph);
            };
        }

        macro_rules! relation {
            ($lhs: ident, $rhs: ident, $relation: expr) => {{
                enforce!(value, $lhs, $rhs);
                let cmp = self.graph.cur_block().new_cmp(&$lhs, &$rhs, $relation);
                Selector(cmp.as_selector())
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
            BinaryOp::LogicalOr => unimplemented!(),
            BinaryOp::LogicalAnd => unimplemented!(),
            BinaryOp::Assign => unimplemented!(),
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
enum ExprResult {
    /// No Result (i.e. call of void method)
    Void,
    /// Result is a single value (i.e. method call, variable, integer
    /// arithmetic)
    Value(*mut ir_node),
    /// Result is control flow that can be branched on (i.e. result of
    /// short-circuiting binary expr, `||`, `==`, `&&`, ...)
    Selector(Selector),
}

impl ExprResult {
    /// Enforce that the result is selector. If self is a boolean `Value`,
    /// convert it to a selector that projects the two cases. If it is a
    /// non-boolean value, libfirm will complain.
    fn enforce_selector(self, graph: Graph) -> Selector {
        use self::ExprResult::*;
        match self {
            Void => panic!("Tried to branch on result of void expr"),
            Value(val) => {
                let one = graph.new_const(unsafe { new_tarval_from_long(1, mode::Bu) });

                graph
                    .cur_block()
                    .new_cmp(&val, &one, ir_relation::Equal)
                    .as_selector()
            }
            Selector(sel) => sel,
        }
    }

    /// Enforce that the result is a single value. If self is a `Selector`,
    /// convert it to boolean value
    fn enforce_value(self, graph: Graph) -> *mut ir_node {
        use self::ExprResult::*;
        match self {
            Void => panic!("Tried to get result value of void expr"),
            Value(val) => val,
            Selector(sel) => {
                let cond = graph.cur_block().new_cond(&sel);
                let t = cond.project_true();
                let f = cond.project_false();
                let zero = graph.new_const(unsafe { new_tarval_from_long(0, mode::Bu) });
                let one = graph.new_const(unsafe { new_tarval_from_long(1, mode::Bu) });

                let phi_block = unsafe {
                    let phi_block = new_r_immBlock(graph.into());
                    add_immBlock_pred(phi_block, f.into());
                    add_immBlock_pred(phi_block, t.into());
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
}
