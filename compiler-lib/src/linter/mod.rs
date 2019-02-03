//! How to write a lint:
//!
//! ```rust
//! use asciifile::Spanned;
//! use compiler_lib::linter::{AstLintPass, LintContext, LintPass};
//! use diagnostics::{declare_lint, lint::LintArray, lint_array};
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
//! impl LintPass for MyLintPass {
//!     fn get_lints(&self) -> LintArray {
//!         lint_array!(MY_COOL_LINT)
//!     }
//! }
//!
//! impl<'f> AstLintPass<'f> for MyLintPass {
//!     fn check_program(&mut self, cx: &LintContext<'_>, program: &Spanned<'_, Program<'_>>) {
//!         cx.struct_lint(MY_COOL_LINT, program.span, "RIIR");
//!     }
//! }
//! ```
use self::lint_level_builder::LintLevelBuilder;
use asciifile::{MaybeSpanned, Span, Spanned};
use compiler_shared::context::Context;
use diagnostics::{
    lint::{Lint, LintArray, LintId},
    Diagnostics, MessageLevel, Printable,
};
use failure::Fail;
use parser::{ast, visitor::*};
use std::collections::HashMap;

mod bools;
mod lint_level_builder;
mod unused_argument;

#[derive(Debug, Clone, Fail)]
#[fail(display = "unknown lint name: `{}`", name)]
pub struct UnknownLint {
    name: String,
}

impl<'a, 'b> Printable<'a, 'b> for UnknownLint {
    fn as_maybe_spanned(&'b self) -> MaybeSpanned<'a, &'b dyn std::fmt::Display> {
        MaybeSpanned::WithoutSpan(self)
    }
}

#[derive(Default)]
pub struct Linter<'f> {
    lints: Vec<&'static Lint>,
    by_name: HashMap<String, LintId>,
    ast_lint_passes: Vec<Box<dyn AstLintPass<'f> + 'f>>,
}

pub trait LintPass {
    fn get_lints(&self) -> LintArray;
}

pub trait AstLintPass<'f>: LintPass {
    fn check_program(&mut self, _cx: &LintContext<'_>, _program: &Spanned<'f, ast::Program<'f>>) {}
    fn check_class_decl(
        &mut self,
        _cx: &LintContext<'_>,
        _class_decl: &Spanned<'f, ast::ClassDeclaration<'f>>,
    ) {
    }
    fn check_class_member(
        &mut self,
        _cx: &LintContext<'_>,
        _class_member: &Spanned<'f, ast::ClassMember<'f>>,
    ) {
    }
    fn check_parameter_list(
        &mut self,
        _cx: &LintContext<'_>,
        _parameter_list: &Spanned<'f, ast::ParameterList<'f>>,
    ) {
    }
    fn check_parameter(
        &mut self,
        _cx: &LintContext<'_>,
        _parameter: &Spanned<'f, ast::Parameter<'f>>,
    ) {
    }
    fn check_block(&mut self, _cx: &LintContext<'_>, _block: &Spanned<'f, ast::Block<'f>>) {}
    fn check_stmt(&mut self, _cx: &LintContext<'_>, _stmt: &Spanned<'f, ast::Stmt<'f>>) {}
    fn check_expr(&mut self, _cx: &LintContext<'_>, _expr: &Spanned<'f, ast::Expr<'f>>) {}
    fn check_binop(&mut self, _cx: &LintContext<'_>, _expr: ast::BinaryOp) {}
    fn check_unop(&mut self, _cx: &LintContext<'_>, _expr: ast::UnaryOp) {}
    fn check_type(&mut self, _cx: &LintContext<'_>, _ty: &Spanned<'f, ast::Type<'f>>) {}
    fn check_basic_type(
        &mut self,
        _cx: &LintContext<'_>,
        _basic_ty: &Spanned<'f, ast::BasicType<'f>>,
    ) {
    }

    fn check_program_post(
        &mut self,
        _cx: &LintContext<'_>,
        _program: &Spanned<'f, ast::Program<'f>>,
    ) {
    }
    fn check_class_decl_post(
        &mut self,
        _cx: &LintContext<'_>,
        _class_decl: &Spanned<'f, ast::ClassDeclaration<'f>>,
    ) {
    }
    fn check_class_member_post(
        &mut self,
        _cx: &LintContext<'_>,
        _class_member: &Spanned<'f, ast::ClassMember<'f>>,
    ) {
    }
}

impl<'f> Linter<'f> {
    pub fn check(&mut self, cx: &'_ Context<'_>, ast: &ast::AST<'f>) -> Result<(), UnknownLint> {
        self.register_ast_passes();
        let mut cx = LintContext {
            diagnostics: &cx.diagnostics,
            builder: LintLevelBuilder::new(&self.by_name),
        };
        let mut lint_visitor = LintVisitor::new(&mut cx);
        for pass in self.ast_lint_passes.iter_mut() {
            lint_visitor.visit_ast(&NodeKind::AST(ast), pass.as_mut())?;
            lint_visitor.visit_ast_post(&NodeKind::AST(ast), pass.as_mut())?;
        }

        Ok(())
    }

    pub fn register_ast_passes(&mut self) {
        self.register_ast_lint(box unused_argument::UnusedArgumentPass);
        self.register_ast_lint(box bools::BoolPass);
    }

    fn register_ast_lint(&mut self, pass: Box<dyn AstLintPass<'f> + 'f>) {
        for lint in pass.get_lints() {
            let id = LintId::of(lint);
            let name = id.to_string();

            self.by_name.insert(name, id);
            self.lints.push(lint);
        }

        self.ast_lint_passes.push(pass);
    }
}

pub struct LintContext<'a> {
    diagnostics: &'a Diagnostics,
    builder: LintLevelBuilder,
}

impl<'a> LintContext<'a> {
    pub fn struct_lint(&self, lint: &'static Lint, span: Span<'_>, msg: &str) {
        let lvl = self
            .builder
            .lint_level(LintId::of(lint))
            .unwrap_or(lint.level);
        if lvl == MessageLevel::Allow {
            return;
        }
        self.diagnostics
            .emit(lvl, Spanned::new(span, msg).as_maybe_spanned());
    }
}

struct LintVisitor<'a> {
    cx: &'a mut LintContext<'a>,
}

impl<'a, 'f> LintVisitor<'a> {
    fn new(cx: &'a mut LintContext<'a>) -> Self {
        Self { cx }
    }

    fn visit_ast(
        &mut self,
        node: &NodeKind<'a, 'f>,
        lint_pass: &mut (dyn AstLintPass<'f> + 'f),
    ) -> Result<(), UnknownLint> {
        use self::NodeKind::*;

        node.for_each_child(&mut |child| {
            let mut push = None;
            match child {
                AST(_) => (),
                Program(program) => {
                    push = Some(self.cx.builder.push(&program.attrs)?);
                    lint_pass.check_program(self.cx, program);
                }
                ClassDeclaration(class_decl) => {
                    push = Some(self.cx.builder.push(&class_decl.attrs)?);
                    lint_pass.check_class_decl(self.cx, class_decl);
                }
                ClassMember(class_member) => {
                    push = Some(self.cx.builder.push(&class_member.attrs)?);
                    lint_pass.check_class_member(self.cx, class_member);
                }
                Parameter(param) => lint_pass.check_parameter(self.cx, param),
                ParameterList(param_list) => lint_pass.check_parameter_list(self.cx, param_list),
                Type(ty) => lint_pass.check_type(self.cx, ty),
                BasicType(basic_ty) => lint_pass.check_basic_type(self.cx, basic_ty),
                Block(block) => lint_pass.check_block(self.cx, block),
                Stmt(stmt) => lint_pass.check_stmt(self.cx, stmt),
                Expr(expr) => lint_pass.check_expr(self.cx, expr),
                BinaryOp(binop) => lint_pass.check_binop(self.cx, *binop),
                UnaryOp(unop) => lint_pass.check_unop(self.cx, *unop),
            }

            self.visit_ast(&child, lint_pass)?;
            if let Some(push) = push {
                self.cx.builder.pop(push);
            }
            Ok(())
        })
        .unwrap_or(Ok(()))
    }

    fn visit_ast_post(
        &mut self,
        node: &NodeKind<'a, 'f>,
        lint_pass: &mut (dyn AstLintPass<'f> + 'f),
    ) -> Result<(), UnknownLint> {
        use self::NodeKind::*;

        node.for_each_child(&mut |child| {
            let mut push = None;
            match child {
                AST(_) => (),
                Program(program) => {
                    push = Some(self.cx.builder.push(&program.attrs)?);
                    lint_pass.check_program_post(self.cx, program);
                }
                ClassDeclaration(class_decl) => {
                    push = Some(self.cx.builder.push(&class_decl.attrs)?);
                    lint_pass.check_class_decl_post(self.cx, class_decl);
                }
                ClassMember(class_member) => {
                    push = Some(self.cx.builder.push(&class_member.attrs)?);
                    lint_pass.check_class_member_post(self.cx, class_member);
                }
                _ => return Ok(()),
            }

            self.visit_ast_post(&child, lint_pass)?;
            if let Some(push) = push {
                self.cx.builder.pop(push);
            }
            Ok(())
        })
        .unwrap_or(Ok(()))
    }
}
