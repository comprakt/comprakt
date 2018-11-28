use super::Runtime;
use crate::{
    asciifile::Spanned,
    ast,
    strtab::Symbol,
    type_checking::{
        type_analysis::TypeAnalysis,
        type_system::{CheckedType, ClassMethodDef},
    },
};
use libfirm_rs::{bindings::*, *};
use std::{collections::HashMap, rc::Rc};

pub struct MethodBodyGenerator<'ir, 'src, 'ast> {
    graph: Graph,
    method_def: Rc<ClassMethodDef<'src, 'ast>>,
    local_vars: HashMap<Symbol<'src>, (usize, mode::Type)>,
    num_vars: usize,
    runtime: &'ir Runtime,
    type_analysis: &'ir TypeAnalysis<'src, 'ast>,
}

impl<'a, 'ir, 'src, 'ast> MethodBodyGenerator<'ir, 'src, 'ast> {
    pub fn new(
        graph: Graph,
        method_def: Rc<ClassMethodDef<'src, 'ast>>,
        type_analysis: &'ir TypeAnalysis<'src, 'ast>,
        runtime: &'ir Runtime,
    ) -> Self {
        Self {
            graph,
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
                let mode = get_firm_mode(&p.ty);
                self.graph
                    .set_value(self.new_local_var(p.name, mode), &args.project(mode, i + 1));
            }
        }

        self
    }

    /// Generate IR for a method body
    pub fn gen_method(&mut self, body: &Spanned<'src, ast::Block<'src>>) {
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
    fn gen_block(&mut self, block: &Spanned<'src, ast::Block<'src>>) {
        for stmt in &block.statements {
            self.gen_stmt(&stmt);
        }
    }

    /// Generate IR for a single statement
    fn gen_stmt(&mut self, stmt: &Spanned<'src, ast::Stmt<'src>>) {
        use self::ast::Stmt::*;
        match &**stmt {
            Block(block) => self.gen_block(block),
            If(cond, then_arm, else_arm) => {
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
                    self.gen_stmt(&**then_arm);
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

            While(cond, body) => {
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
                    self.gen_stmt(&**body);

                    // We jump back to the condition-check
                    header_block.add_pred(&body_block.new_jmp());
                }

                // Leave loop if cond is false
                let next_block = self.graph.new_imm_block(&cond.project_false());
                unsafe { self.graph.set_cur_block(next_block) };

                header_block.mature();
                body_block.mature();
            }

            Expression(expr) => {
                self.gen_expr(expr);
            }

            Return(res_expr) => {
                let mem = self.graph.cur_store();
                let res = res_expr.as_ref().map(|res_expr| self.gen_expr(&*res_expr));

                let ret = self.graph.cur_block().new_return(mem, res);

                self.graph.end_block().add_pred(&ret);
            }

            LocalVariableDeclaration(_ty, name, init_expr) => {
                // TODO here we need hennings type_analysis, because _ty is not a CheckedType.
                // For now, just assume i32
                //let mode = get_firm_mode(_ty).expect(&format!("var '{}' is void", name));
                let mode = unsafe { mode::Is };
                let var_slot = self.new_local_var(**name, mode);
                if let Some(init_expr) = init_expr {
                    self.graph.set_value(var_slot, &self.gen_expr(init_expr));
                }
            }

            Empty => (),
        }
    }

    /// Return a node that evaluates the given expression
    ///
    /// TODO non-raw-ptr abstraction for ret type; Box<dyn ValueNode> might
    /// work, but unnecessary box
    fn gen_expr(&mut self, expr: &Spanned<'src, ast::Expr<'src>>) -> *mut ir_node {
        use self::ast::Expr::*;
        match &**expr {
            Int(literal) => {
                let val = unsafe { new_tarval_from_long(literal.parse().unwrap(), mode::Is) };
                self.graph.new_const(val).as_value_node()
            }
            Var(name) => {
                let (slot, mode) = self.local_var(**name);
                self.graph.value(slot, mode).as_value_node()
            }
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

    /// Assume an expression can be evaluated as a boolean and generate a
    /// `Selector` for it
    fn gen_cond_expr(&mut self, expr: &Spanned<'src, ast::Expr<'src>>) -> Selector {
        use self::ast::Expr::*;
        match &**expr {
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

fn get_firm_mode(ty: &CheckedType<'_>) -> mode::Type {
    match ty {
        CheckedType::Int => unsafe { mode::Is },
        CheckedType::Boolean => unsafe { mode::Bu },
        CheckedType::TypeRef(_) | CheckedType::Array(_) | CheckedType::Null => unsafe { mode::P },
        // MUST NOT be void or unknown type after semantic analysis phase.
        CheckedType::Void | CheckedType::UnknownType(_) => unreachable!(),
    }
}

fn get_firm_type(ty: &CheckedType<'_>) -> Option<Ty> {
    match ty {
        CheckedType::Int => Some(PrimitiveType::i32()),
        CheckedType::Boolean => Some(PrimitiveType::bool()),
        CheckedType::TypeRef(name) => Some(ClassType::new_class_type(name.as_str())),
        CheckedType::Array(checked_type) => Some(
            get_firm_type(checked_type)
                .expect("Arrays are never of type `void` or `null`")
                .pointer(),
        ),
        // Not possible
        // TODO: use unreachable as soon as this is no longer "unused"!
        CheckedType::Void | CheckedType::Null | CheckedType::UnknownType(_) => None,
    }
}
