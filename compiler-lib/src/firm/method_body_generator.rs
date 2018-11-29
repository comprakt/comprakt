use super::{get_firm_mode, Class, Runtime};
use crate::{
    asciifile::Spanned,
    ast::{self, BinaryOp},
    strtab::Symbol,
    type_checking::{
        type_analysis::TypeAnalysis,
        type_system::{CheckedType, ClassMethodDef},
    },
};
use libfirm_rs::{bindings::*, *};
use std::{cell::RefCell, collections::HashMap, ptr, rc::Rc};

pub struct MethodBodyGenerator<'ir, 'src, 'ast> {
    graph: Graph,
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
        classes: &'ir HashMap<Symbol<'src>, Rc<RefCell<Class<'src, 'ast>>>>,
        method_def: Rc<ClassMethodDef<'src, 'ast>>,
        type_analysis: &'ir TypeAnalysis<'src, 'ast>,
        runtime: &'ir Runtime,
    ) -> Self {
        Self {
            graph,
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
        unsafe { self.graph.set_cur_block(self.graph.start_block()) };
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
            self.graph.set_value(var_slot, &self.gen_expr(init_expr));
        }
    }

    fn gen_while(&mut self, cond: &ast::Expr<'src>, body: &ast::Stmt<'src>) {
        // TODO DRY beginning nearly the same as If-case
        let prev_block = self.graph.cur_block();

        let incoming_jmp = prev_block.new_jmp();
        let header_block = self.graph.new_imm_block(&incoming_jmp);

        prev_block.mature(); // This block is done now
        unsafe { self.graph.set_cur_block(header_block) };

        // We evaluate the condition
        let cond = header_block.new_cond(&self.gen_cond_expr(cond));

        // Run body if cond is true
        let body_block = self.graph.new_imm_block(&cond.project_true());
        {
            unsafe { self.graph.set_cur_block(body_block) };
            self.gen_stmt(&*body);

            // We jump back to the condition-check
            header_block.add_pred(&body_block.new_jmp());
        }

        // Leave loop if cond is false
        let next_block = self.graph.new_imm_block(&cond.project_false());
        unsafe { self.graph.set_cur_block(next_block) };

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
        unsafe { self.graph.set_cur_block(header_block) };

        // We evaluate the condition
        let cond = header_block.new_cond(&self.gen_cond_expr(cond));

        // If its true, we take the then_arm
        let then_block = self.graph.new_imm_block(&cond.project_true());
        {
            unsafe { self.graph.set_cur_block(then_block) };
            self.gen_stmt(&*then_arm);
        }

        // If its false, we take the else_arm
        // We generate an else_block in any case, when there is no `else`, it's empty
        let else_block = self.graph.new_imm_block(&cond.project_false());
        if let Some(else_arm) = else_arm {
            unsafe { self.graph.set_cur_block(else_block) };
            self.gen_stmt(&**else_arm);
        }

        header_block.mature();

        let from_then_jmp = then_block.new_jmp();
        let from_else_jmp = else_block.new_jmp();

        // Now we close the if-diamond
        let next_block = self.graph.new_imm_block(&from_then_jmp);
        next_block.add_pred(&from_else_jmp);
        unsafe { self.graph.set_cur_block(next_block) };

        // Those blocks are finished now
        then_block.mature();
        else_block.mature();
    }

    fn gen_return(&mut self, res_expr: &Option<Box<Spanned<'src, ast::Expr<'src>>>>) {
        let mem = self.graph.cur_store();
        let res = res_expr.as_ref().map(|res_expr| self.gen_expr(&*res_expr));

        let ret = self.graph.cur_block().new_return(mem, res);

        self.graph.end_block().add_pred(&ret);
    }

    /// Return a node that evaluates the given expression
    ///
    /// TODO non-raw-ptr abstraction for ret type; Box<dyn ValueNode> might
    /// work, but unnecessary box
    fn gen_expr(&mut self, expr: &ast::Expr<'src>) -> *mut ir_node {
        use self::ast::Expr::*;
        match &expr {
            Int(literal) => {
                let val = unsafe { new_tarval_from_long(literal.parse().unwrap(), mode::Is) };
                self.graph.new_const(val).as_value_node()
            }
            NegInt(literal) => {
                let val = unsafe {
                    new_tarval_from_long(
                        literal
                            .parse::<i64>()
                            .map_or_else(|_| -2_147_483_648, |v| -v),
                        mode::Is,
                    )
                };
                self.graph.new_const(val).as_value_node()
            }
            Boolean(value) => {
                let val = unsafe { new_tarval_from_long(if *value { 1 } else { 0 }, mode::Bu) };
                self.graph.new_const(val).as_value_node()
            }
            Var(name) => {
                let (slot, mode) = self.local_var(**name);
                self.graph.value(slot, mode).as_value_node()
            }
            Binary(op, lhs, rhs) => self.gen_binary_expr(*op, lhs, rhs),
            Unary(ast::UnaryOp::Neg, expr) => {
                let expr = self.gen_expr(expr);
                log::debug!("pre new_neg");
                let neg = self.graph.cur_block().new_minus(&expr);
                neg.as_value_node()
            }
            Unary(_, _expr) => unimplemented!(),
            MethodInvocation(object, method, argument_list) => {
                let class_id = match self.type_analysis.expr_info(object).ty {
                    CheckedType::TypeRef(class_ref) => class_ref.id(),
                    _ => panic!("method invocations can only be done on type references"),
                };

                let class = self
                    .classes
                    .get(&class_id)
                    .expect("method invocation on inexistent class")
                    .borrow();

                let method = class
                    .methods
                    .get(&method)
                    .expect("invocation of unknown method");

                // object could be any expression that has to be
                // evaluated, e.g. (this.get_object()).foo()
                let this = self.gen_expr(object);

                let mut args = vec![this];

                for arg in argument_list.iter() {
                    args.push(self.gen_expr(arg));
                }

                let call = self.graph.cur_block().new_call(
                    self.graph.cur_store(),
                    self.graph.new_addr(method.borrow().entity),
                    &args,
                );

                self.graph.set_store(call.project_mem());

                let return_type = &method.borrow().def.return_ty;

                match get_firm_mode(&return_type) {
                    Some(mode) => call.project_result_tuple().project(mode, 0).as_value_node(),
                    None => {
                        // TODO: we are fucked here! this is possible (e.g. function returns void).
                        // But we cannot satisfy the return type of gen_expr!!! Requires refactor
                        // nullptr is an arbitrary value
                        ptr::null_mut()
                    }
                }
            }
            FieldAccess(expr, symbol) => {
                self.gen_expr(expr);
                match self.type_analysis.expr_info(expr).ty {
                    CheckedType::TypeRef(pre_expr_ty) => {
                        let class = self
                            .classes
                            .get(&pre_expr_ty.id())
                            .expect("Class has to be registered")
                            .borrow();
                        let field = class
                            .fields
                            .get(symbol)
                            .expect("Field must exist in class after type checking")
                            .borrow();
                        let field_addr = self.graph.new_addr(field.entity);
                        let mode = get_firm_mode(&field.def.ty)
                            .expect("Type `void` is not a valid field type");
                        let load = self.graph.cur_block().new_load(
                            self.graph.cur_store(),
                            &field_addr,
                            mode,
                            field.entity.ty(),
                            ir_cons_flags::None,
                        );
                        load.project_mem();
                        load.project_res(mode).as_value_node()
                    }
                    _ => panic!("Only classes have fields"),
                }
            }
            ArrayAccess(_expr, _index_expr) => unimplemented!(),
            Null => unimplemented!(),
            ThisMethodInvocation(_symbol, _argument_list) => unimplemented!(),
            This => unimplemented!(),
            NewObject(ty_name) => {
                // TODO classes should be hash map for efficient lookup
                let class = self
                    .classes
                    .get(ty_name)
                    .expect("creating non-existing class");

                let size = unsafe {
                    self.graph.new_const(new_tarval_from_long(
                        i64::from(class.borrow().entity.ty().size()),
                        mode::Iu,
                    ))
                };

                let call = self.graph.cur_block().new_call(
                    self.graph.cur_store(),
                    self.graph.new_addr(self.runtime.new),
                    &[size.as_value_node()],
                );

                self.graph.set_store(call.project_mem());

                call.project_result_tuple()
                    .project(unsafe { mode::P }, 0)
                    .as_value_node()
            }
            NewArray(_basic_type, _expr, _dimension) => unimplemented!(),
        }
    }

    fn gen_binary_expr(
        &mut self,
        op: BinaryOp,
        lhs: &ast::Expr<'src>,
        rhs: &ast::Expr<'src>,
    ) -> *mut ir_node {
        let lhs = self.gen_expr(lhs);
        let rhs = self.gen_expr(rhs);
        match op {
            BinaryOp::Add => {
                let add = self.graph.cur_block().new_add(&lhs, &rhs);
                add.as_value_node()
            }
            BinaryOp::Sub => {
                let sub = self.graph.cur_block().new_sub(&lhs, &rhs);
                sub.as_value_node()
            }
            BinaryOp::Mul => {
                let mul = self.graph.cur_block().new_mul(&lhs, &rhs);
                mul.as_value_node()
            }
            BinaryOp::Div => {
                let mem = self.graph.cur_store();
                log::debug!("pre new_div");
                let div = self.graph.cur_block().new_div(mem, &lhs, &rhs, 0);
                self.graph.set_store(div.project_mem());
                log::debug!("pre project_res");
                let res = div.project_res();
                log::debug!("pre as_value_node");
                res.as_value_node()
            }
            BinaryOp::Mod => {
                let mem = self.graph.cur_store();
                log::debug!("pre new_mod");
                let mod_node = self.graph.cur_block().new_mod(mem, &lhs, &rhs, 0);
                self.graph.set_store(mod_node.project_mem());
                log::debug!("pre project_res");
                let res = mod_node.project_res();
                log::debug!("pre as_value_node");
                res.as_value_node()
            }
            BinaryOp::LogicalOr => unimplemented!(),
            BinaryOp::LogicalAnd => unimplemented!(),
            BinaryOp::Assign => unimplemented!(),
            BinaryOp::Equals => unimplemented!(),
            BinaryOp::NotEquals => unimplemented!(),
            BinaryOp::LessThan => unimplemented!(),
            BinaryOp::GreaterThan => unimplemented!(),
            BinaryOp::LessEquals => unimplemented!(),
            BinaryOp::GreaterEquals => unimplemented!(),
        }
    }

    /// Assume an expression can be evaluated as a boolean and generate a
    /// `Selector` for it
    fn gen_cond_expr(&mut self, expr: &ast::Expr<'src>) -> Selector {
        use self::ast::Expr::*;
        match &expr {
            Var(name) => {
                let (slot, mode) = self.local_var(**name);
                let val = self.graph.value(slot, mode);

                let zero = self
                    .graph
                    .new_const(unsafe { new_tarval_from_long(0, mode) });

                // cond is true iff. val != 0
                self.graph
                    .cur_block()
                    // TODO ir_relation::NotEqual does not exist. Need ALU-neg?
                    .new_cmp(&val, &zero, ir_relation::Greater)
                    .as_selector()
            }
            Int(_literal) => unimplemented!(),
            Binary(_op, _expr_left, _expr_right) => unimplemented!(),
            Unary(_op, _expr) => unimplemented!(),
            MethodInvocation(_expr, _symbol, _argument_list) => unimplemented!(),
            FieldAccess(_expr, _symbol) => unimplemented!(),
            ArrayAccess(_expr, _index_expr) => unimplemented!(),
            Null => unimplemented!(),
            Boolean(_val) => unimplemented!(),
            NegInt(_literal) => unimplemented!(),
            ThisMethodInvocation(_symbol, _argument_list) => unimplemented!(),
            This => unimplemented!(),
            NewObject(_symbol) => unimplemented!(),
            NewArray(_basic_type, _expr, _dimension) => unimplemented!(),
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
        *self.local_vars.get(&name).expect("undefined variable")
    }
}
