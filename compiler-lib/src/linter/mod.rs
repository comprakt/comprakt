//! How to write a lint:
//!
//! ```rust
//! use asciifile::Spanned;
//! use compiler_lib::linter::AstLintPass;
//! use compiler_shared::context::Context;
//! use diagnostics::declare_lint;
//! use parser::ast::*;
//!
//! declare_lint!(
//!     pub MY_COOL_LINT,
//!     Warning,
//!     "do this the right way"
//! );
//!
//! pub struct MyLintPass;
//!
//! impl<'f> AstLintPass<'f> for MyLintPass {
//!     fn check_program(&mut self, cx: &Context<'_>, program: &Spanned<'_, Program<'_>>) {
//!         cx.diagnostics.lint(MY_COOL_LINT, program.span, "RIIR".to_string());
//!     }
//! }
//! ```
use asciifile::Spanned;
use compiler_shared::context::Context;
use parser::{ast, visitor::*};

mod bools;
mod unused_argument;

#[derive(Default)]
pub struct Linter<'f> {
    ast_lint_passes: Vec<Box<dyn AstLintPass<'f> + 'f>>,
}

pub trait AstLintPass<'f> {
    fn check_program(&mut self, _cx: &Context<'_>, _program: &Spanned<'f, ast::Program<'f>>) {}
    fn check_class_decl(
        &mut self,
        _cx: &Context<'_>,
        _class_decl: &Spanned<'f, ast::ClassDeclaration<'f>>,
    ) {
    }
    fn check_class_member(
        &mut self,
        _cx: &Context<'_>,
        _class_member: &Spanned<'f, ast::ClassMember<'f>>,
    ) {
    }
    fn check_parameter_list(
        &mut self,
        _cx: &Context<'_>,
        _parameter_list: &Spanned<'f, ast::ParameterList<'f>>,
    ) {
    }
    fn check_parameter(&mut self, _cx: &Context<'_>, _parameter: &Spanned<'f, ast::Parameter<'f>>) {
    }
    fn check_block(&mut self, _cx: &Context<'_>, _block: &Spanned<'f, ast::Block<'f>>) {}
    fn check_stmt(&mut self, _cx: &Context<'_>, _stmt: &Spanned<'f, ast::Stmt<'f>>) {}
    fn check_expr(&mut self, _cx: &Context<'_>, _expr: &Spanned<'f, ast::Expr<'f>>) {}
    fn check_binop(&mut self, _cx: &Context<'_>, _expr: ast::BinaryOp) {}
    fn check_unop(&mut self, _cx: &Context<'_>, _expr: ast::UnaryOp) {}
    fn check_type(&mut self, _cx: &Context<'_>, _ty: &Spanned<'f, ast::Type<'f>>) {}
    fn check_basic_type(&mut self, _cx: &Context<'_>, _basic_ty: &Spanned<'f, ast::BasicType<'f>>) {
    }

    fn check_program_post(&mut self, _cx: &Context<'_>, _program: &Spanned<'f, ast::Program<'f>>) {}
    fn check_class_decl_post(
        &mut self,
        _cx: &Context<'_>,
        _class_decl: &Spanned<'f, ast::ClassDeclaration<'f>>,
    ) {
    }
    fn check_class_member_post(
        &mut self,
        _cx: &Context<'_>,
        _class_member: &Spanned<'f, ast::ClassMember<'f>>,
    ) {
    }
}

impl<'f> Linter<'f> {
    pub fn register_ast_passes(&mut self) {
        self.register_ast_lint(box unused_argument::UnusedArgumentPass);
        self.register_ast_lint(box bools::BoolPass);
    }

    pub fn check(self, cx: &'_ Context<'f>, ast: &ast::AST<'f>) {
        for pass in self.ast_lint_passes {
            let mut lint_visitor = LintVisitor::new(cx, pass);
            lint_visitor.visit_ast(&NodeKind::AST(ast));
            lint_visitor.visit_ast_post(&NodeKind::AST(ast));
        }
    }

    fn register_ast_lint(&mut self, lint: Box<dyn AstLintPass<'f> + 'f>) {
        self.ast_lint_passes.push(lint);
    }
}

struct LintVisitor<'a, 'f> {
    lint_pass: Box<dyn AstLintPass<'f> + 'f>,
    cx: &'a Context<'f>,
}

impl<'a, 'f> LintVisitor<'a, 'f> {
    fn new(cx: &'a Context<'f>, lint_pass: Box<dyn AstLintPass<'f> + 'f>) -> Self {
        Self { lint_pass, cx }
    }

    fn visit_ast(&mut self, node: &NodeKind<'a, 'f>) {
        use self::NodeKind::*;

        node.for_each_child(&mut |child| {
            match child {
                AST(_) => (),
                Program(program) => self.lint_pass.check_program(self.cx, program),
                ClassDeclaration(class_decl) => {
                    self.lint_pass.check_class_decl(self.cx, class_decl)
                }
                ClassMember(class_member) => {
                    self.lint_pass.check_class_member(self.cx, class_member)
                }
                Parameter(param) => self.lint_pass.check_parameter(self.cx, param),
                ParameterList(param_list) => {
                    self.lint_pass.check_parameter_list(self.cx, param_list)
                }
                Type(ty) => self.lint_pass.check_type(self.cx, ty),
                BasicType(basic_ty) => self.lint_pass.check_basic_type(self.cx, basic_ty),
                Block(block) => self.lint_pass.check_block(self.cx, block),
                Stmt(stmt) => self.lint_pass.check_stmt(self.cx, stmt),
                Expr(expr) => self.lint_pass.check_expr(self.cx, expr),
                BinaryOp(binop) => self.lint_pass.check_binop(self.cx, *binop),
                UnaryOp(unop) => self.lint_pass.check_unop(self.cx, *unop),
            }

            self.visit_ast(&child)
        });
    }

    fn visit_ast_post(&mut self, node: &NodeKind<'a, 'f>) {
        use self::NodeKind::*;

        node.for_each_child(&mut |child| {
            match child {
                AST(_) => (),
                Program(program) => self.lint_pass.check_program_post(self.cx, program),
                ClassDeclaration(class_decl) => {
                    self.lint_pass.check_class_decl_post(self.cx, class_decl)
                }
                ClassMember(class_member) => self
                    .lint_pass
                    .check_class_member_post(self.cx, class_member),
                _ => return,
            }

            self.visit_ast_post(&child)
        });
    }
}
