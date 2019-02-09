//! How to write an early lint:
//!
//! ```rust
//! use asciifile::Spanned;
//! use compiler_lib::linter::{AstLintPass, EarlyContext, LintContext, LintPass};
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
//! impl<'a, 'f> AstLintPass<'a, 'f> for MyLintPass {
//!     fn check_program(&mut self, cx: &EarlyContext<'_, '_>, program: &Spanned<'_, Program<'_>>) {
//!         cx.struct_lint(MY_COOL_LINT, program.span, "RIIR");
//!     }
//! }
//! ```
//!
//! How to write a late lint:
//!
//! ```rust
//! use asciifile::Spanned;
//! use compiler_lib::linter::{SemanticLintPass, LateContext, LintContext, LintPass};
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
//! impl<'a, 'f> SemanticLintPass<'a, 'f> for MyLintPass {
//!     fn check_program(&mut self, cx: &LateContext<'_, '_>, program: &Spanned<'_, Program<'_>>) {
//!         cx.struct_lint(MY_COOL_LINT, program.span, "RIIR");
//!     }
//! }
//! ```
//!
//! Don't forget to register your lint in one of
//! ```rust,ignore
//! // Early lints
//! Linter::register_ast_passes();
//! // Late lints
//! Linter::register_semantic_passes();
//! ```
//!
//! Every lint module needs also to be registered in `lints/mod.rs`.
use self::{lint_level_builder::LintLevelBuilder, locator::Locator};
use asciifile::{MaybeSpanned, Span, Spanned};
use compiler_shared::context::Context;
use diagnostics::{
    lint::{Lint, LintArray, LintId},
    Diagnostics, MessageLevel, Printable,
};
use failure::Fail;
use parser::{ast, visitor::*};
use std::collections::HashMap;
use type_checking::type_analysis::TypeAnalysis;

mod lint_level_builder;
mod lints;
mod locator;

use self::lints::*;

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

pub struct Linter<'a, 'f> {
    lints: Vec<&'static Lint>,
    by_name: HashMap<String, LintId>,
    ast_lint_passes: Vec<Box<dyn AstLintPass<'a, 'f> + 'a>>,
    semantic_lint_passes: Vec<Box<dyn SemanticLintPass<'a, 'f> + 'a>>,
}

pub trait LintPass {
    fn get_lints(&self) -> LintArray;
}

pub trait AstLintPass<'a, 'f>: LintPass {
    fn check_program(
        &mut self,
        _cx: &EarlyContext<'a, 'f>,
        _program: &Spanned<'f, ast::Program<'f>>,
    ) {
    }
    fn check_class_decl(
        &mut self,
        _cx: &EarlyContext<'a, 'f>,
        _class_decl: &Spanned<'f, ast::ClassDeclaration<'f>>,
    ) {
    }
    fn check_class_member(
        &mut self,
        _cx: &EarlyContext<'a, 'f>,
        _class_member: &Spanned<'f, ast::ClassMember<'f>>,
    ) {
    }
    fn check_parameter_list(
        &mut self,
        _cx: &EarlyContext<'a, 'f>,
        _parameter_list: &Spanned<'f, ast::ParameterList<'f>>,
    ) {
    }
    fn check_parameter(
        &mut self,
        _cx: &EarlyContext<'a, 'f>,
        _parameter: &Spanned<'f, ast::Parameter<'f>>,
    ) {
    }
    fn check_block(&mut self, _cx: &EarlyContext<'a, 'f>, _block: &Spanned<'f, ast::Block<'f>>) {}
    fn check_stmt(&mut self, _cx: &EarlyContext<'a, 'f>, _stmt: &Spanned<'f, ast::Stmt<'f>>) {}
    fn check_expr(&mut self, _cx: &EarlyContext<'a, 'f>, _expr: &Spanned<'f, ast::Expr<'f>>) {}
    fn check_binop(&mut self, _cx: &EarlyContext<'a, 'f>, _expr: ast::BinaryOp) {}
    fn check_unop(&mut self, _cx: &EarlyContext<'a, 'f>, _expr: ast::UnaryOp) {}
    fn check_type(&mut self, _cx: &EarlyContext<'a, 'f>, _ty: &Spanned<'f, ast::Type<'f>>) {}
    fn check_basic_type(
        &mut self,
        _cx: &EarlyContext<'a, 'f>,
        _basic_ty: &Spanned<'f, ast::BasicType<'f>>,
    ) {
    }

    fn check_program_post(
        &mut self,
        _cx: &EarlyContext<'a, 'f>,
        _program: &Spanned<'f, ast::Program<'f>>,
    ) {
    }
    fn check_class_decl_post(
        &mut self,
        _cx: &EarlyContext<'a, 'f>,
        _class_decl: &Spanned<'f, ast::ClassDeclaration<'f>>,
    ) {
    }
    fn check_class_member_post(
        &mut self,
        _cx: &EarlyContext<'a, 'f>,
        _class_member: &Spanned<'f, ast::ClassMember<'f>>,
    ) {
    }
}

pub trait SemanticLintPass<'a, 'f>: LintPass {
    fn check_program(
        &mut self,
        _cx: &LateContext<'a, 'f>,
        _program: &Spanned<'f, ast::Program<'f>>,
    ) {
    }
    fn check_class_decl(
        &mut self,
        _cx: &LateContext<'a, 'f>,
        _class_decl: &Spanned<'f, ast::ClassDeclaration<'f>>,
    ) {
    }
    fn check_class_member(
        &mut self,
        _cx: &LateContext<'a, 'f>,
        _class_member: &Spanned<'f, ast::ClassMember<'f>>,
    ) {
    }
    fn check_parameter_list(
        &mut self,
        _cx: &LateContext<'a, 'f>,
        _parameter_list: &Spanned<'f, ast::ParameterList<'f>>,
    ) {
    }
    fn check_parameter(
        &mut self,
        _cx: &LateContext<'a, 'f>,
        _parameter: &Spanned<'f, ast::Parameter<'f>>,
    ) {
    }
    fn check_block(&mut self, _cx: &LateContext<'a, 'f>, _block: &Spanned<'f, ast::Block<'f>>) {}
    fn check_stmt(&mut self, _cx: &LateContext<'a, 'f>, _stmt: &Spanned<'f, ast::Stmt<'f>>) {}
    fn check_expr(&mut self, _cx: &LateContext<'a, 'f>, _expr: &Spanned<'f, ast::Expr<'f>>) {}
    fn check_binop(&mut self, _cx: &LateContext<'a, 'f>, _expr: ast::BinaryOp) {}
    fn check_unop(&mut self, _cx: &LateContext<'a, 'f>, _expr: ast::UnaryOp) {}
    fn check_type(&mut self, _cx: &LateContext<'a, 'f>, _ty: &Spanned<'f, ast::Type<'f>>) {}
    fn check_basic_type(
        &mut self,
        _cx: &LateContext<'a, 'f>,
        _basic_ty: &Spanned<'f, ast::BasicType<'f>>,
    ) {
    }

    fn check_program_post(
        &mut self,
        _cx: &LateContext<'a, 'f>,
        _program: &Spanned<'f, ast::Program<'f>>,
    ) {
    }
    fn check_class_decl_post(
        &mut self,
        _cx: &LateContext<'a, 'f>,
        _class_decl: &Spanned<'f, ast::ClassDeclaration<'f>>,
    ) {
    }
    fn check_class_member_post(
        &mut self,
        _cx: &LateContext<'a, 'f>,
        _class_member: &Spanned<'f, ast::ClassMember<'f>>,
    ) {
    }
}

impl<'a, 'f: 'a> Linter<'a, 'f> {
    pub fn check_ast(
        &mut self,
        cx: &'a Context<'f>,
        ast: &'a ast::AST<'f>,
    ) -> Result<(), UnknownLint> {
        let cx = EarlyContext {
            diagnostics: &cx.diagnostics,
            builder: LintLevelBuilder::new(&self.by_name),
            locator: Locator::default(),
        };
        let mut lint_visitor = EarlyVisitor::new(cx);
        for pass in self.ast_lint_passes.iter_mut() {
            lint_visitor.visit_ast(&NodeKind::AST(ast), pass.as_mut())?;
            lint_visitor.visit_ast_post(&NodeKind::AST(ast), pass.as_mut())?;
        }

        Ok(())
    }

    pub fn check_semantic(
        &mut self,
        cx: &'a Context<'f>,
        ast: &'a ast::AST<'f>,
        type_analysis: &'a TypeAnalysis<'f, 'a>,
    ) -> Result<(), UnknownLint> {
        let cx = LateContext {
            diagnostics: &cx.diagnostics,
            builder: LintLevelBuilder::new(&self.by_name),
            locator: Locator::default(),
            type_analysis,
        };
        let mut lint_visitor = LateVisitor::new(cx);
        for pass in self.semantic_lint_passes.iter_mut() {
            lint_visitor.visit_ast(&NodeKind::AST(ast), pass.as_mut())?;
            lint_visitor.visit_ast_post(&NodeKind::AST(ast), pass.as_mut())?;
        }

        Ok(())
    }

    pub fn register_ast_passes(&mut self) {
        self.register_ast_lint(box bools::BoolPass);
        self.register_ast_lint(box too_many_arguments::TooManyArgumentsPass);
        self.register_ast_lint(box unused_arguments::UnusedArgumentsPass);
        self.register_ast_lint(box unused_class::UnusedClassPass::default());
    }

    fn register_ast_lint(&mut self, pass: Box<dyn AstLintPass<'a, 'f> + 'a>) {
        for lint in pass.get_lints() {
            let id = LintId::of(lint);
            let name = id.to_string();

            self.by_name.insert(name, id);
            self.lints.push(lint);
        }

        self.ast_lint_passes.push(pass);
    }

    pub fn register_semantic_passes(&mut self) {
        self.register_semantic_lint(box unused_functions::UnusedFunctionsPass::default());
        self.register_semantic_lint(box unused_fields::UnusedFieldsPass::default());
    }

    fn register_semantic_lint(&mut self, pass: Box<dyn SemanticLintPass<'a, 'f> + 'a>) {
        for lint in pass.get_lints() {
            let id = LintId::of(lint);
            let name = id.to_string();

            self.by_name.insert(name, id);
            self.lints.push(lint);
        }

        self.semantic_lint_passes.push(pass);
    }
}

impl<'a, 'f: 'a> Default for Linter<'a, 'f> {
    fn default() -> Self {
        let mut linter = Self {
            lints: vec![],
            by_name: HashMap::new(),
            ast_lint_passes: vec![],
            semantic_lint_passes: vec![],
        };
        linter.register_ast_passes();
        linter.register_semantic_passes();
        linter
    }
}

pub trait LintContext {
    fn struct_lint(&self, lint: &'static Lint, span: Span<'_>, msg: &str);
}

pub struct EarlyContext<'a, 'f> {
    diagnostics: &'a Diagnostics,
    builder: LintLevelBuilder,
    locator: Locator<'f>,
}

impl<'a> LintContext for EarlyContext<'a, '_> {
    fn struct_lint(&self, lint: &'static Lint, span: Span<'_>, msg: &str) {
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

pub struct LateContext<'a, 'f> {
    diagnostics: &'a Diagnostics,
    builder: LintLevelBuilder,
    locator: Locator<'f>,
    type_analysis: &'a TypeAnalysis<'f, 'a>,
}

impl<'a, 'f> LintContext for LateContext<'a, 'f> {
    fn struct_lint(&self, lint: &'static Lint, span: Span<'_>, msg: &str) {
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

struct EarlyVisitor<'a, 'f> {
    cx: EarlyContext<'a, 'f>,
}

impl<'a, 'f> EarlyVisitor<'a, 'f> {
    fn new(cx: EarlyContext<'a, 'f>) -> Self {
        EarlyVisitor { cx }
    }

    fn visit_ast(
        &mut self,
        node: &NodeKind<'a, 'f>,
        lint_pass: &mut (dyn AstLintPass<'a, 'f> + 'a),
    ) -> Result<(), UnknownLint> {
        use self::NodeKind::*;

        node.for_each_child(&mut |child| {
            let mut push = None;
            let mut location_pushed = false;
            match child {
                AST(_) => (),
                Program(program) => {
                    push = Some(self.cx.builder.push(&program.attrs)?);
                    lint_pass.check_program(&self.cx, program);
                }
                ClassDeclaration(class_decl) => {
                    push = Some(self.cx.builder.push(&class_decl.attrs)?);
                    self.cx.locator.push(class_decl.name.data);
                    location_pushed = true;
                    lint_pass.check_class_decl(&self.cx, class_decl);
                }
                ClassMember(class_member) => {
                    push = Some(self.cx.builder.push(&class_member.attrs)?);
                    self.cx.locator.push(class_member.name);
                    location_pushed = true;
                    lint_pass.check_class_member(&self.cx, class_member);
                }
                Parameter(param) => lint_pass.check_parameter(&self.cx, param),
                ParameterList(param_list) => lint_pass.check_parameter_list(&self.cx, param_list),
                Type(ty) => lint_pass.check_type(&self.cx, ty),
                BasicType(basic_ty) => lint_pass.check_basic_type(&self.cx, basic_ty),
                Block(block) => lint_pass.check_block(&self.cx, block),
                Stmt(stmt) => lint_pass.check_stmt(&self.cx, stmt),
                Expr(expr) => lint_pass.check_expr(&self.cx, expr),
                BinaryOp(binop) => lint_pass.check_binop(&self.cx, *binop),
                UnaryOp(unop) => lint_pass.check_unop(&self.cx, *unop),
            }

            self.visit_ast(&child, lint_pass)?;
            if let Some(push) = push {
                self.cx.builder.pop(push);
            }
            if location_pushed {
                self.cx.locator.pop();
            }
            Ok(())
        })
        .unwrap_or(Ok(()))
    }

    fn visit_ast_post(
        &mut self,
        node: &NodeKind<'a, 'f>,
        lint_pass: &mut (dyn AstLintPass<'a, 'f> + 'a),
    ) -> Result<(), UnknownLint> {
        use self::NodeKind::*;

        node.for_each_child(&mut |child| {
            let mut push = None;
            let mut location_pushed = false;
            match child {
                AST(_) => (),
                Program(program) => {
                    push = Some(self.cx.builder.push(&program.attrs)?);
                    lint_pass.check_program_post(&self.cx, program);
                }
                ClassDeclaration(class_decl) => {
                    push = Some(self.cx.builder.push(&class_decl.attrs)?);
                    self.cx.locator.push(class_decl.name.data);
                    location_pushed = true;
                    lint_pass.check_class_decl_post(&self.cx, class_decl);
                }
                ClassMember(class_member) => {
                    push = Some(self.cx.builder.push(&class_member.attrs)?);
                    self.cx.locator.push(class_member.name);
                    location_pushed = true;
                    lint_pass.check_class_member_post(&self.cx, class_member);
                }
                _ => return Ok(()),
            }

            self.visit_ast_post(&child, lint_pass)?;
            if let Some(push) = push {
                self.cx.builder.pop(push);
            }
            if location_pushed {
                self.cx.locator.pop();
            }
            Ok(())
        })
        .unwrap_or(Ok(()))
    }
}

struct LateVisitor<'a, 'f> {
    cx: LateContext<'a, 'f>,
}

impl<'a, 'f> LateVisitor<'a, 'f> {
    fn new(cx: LateContext<'a, 'f>) -> Self {
        Self { cx }
    }

    fn visit_ast(
        &mut self,
        node: &NodeKind<'a, 'f>,
        lint_pass: &mut (dyn SemanticLintPass<'a, 'f> + 'a),
    ) -> Result<(), UnknownLint> {
        use self::NodeKind::*;

        node.for_each_child(&mut |child| {
            let mut push = None;
            let mut location_pushed = false;
            match child {
                AST(_) => (),
                Program(program) => {
                    push = Some(self.cx.builder.push(&program.attrs)?);
                    lint_pass.check_program(&self.cx, program);
                }
                ClassDeclaration(class_decl) => {
                    push = Some(self.cx.builder.push(&class_decl.attrs)?);
                    self.cx.locator.push(class_decl.name.data);
                    location_pushed = true;
                    lint_pass.check_class_decl(&self.cx, class_decl);
                }
                ClassMember(class_member) => {
                    push = Some(self.cx.builder.push(&class_member.attrs)?);
                    self.cx.locator.push(class_member.name);
                    location_pushed = true;
                    lint_pass.check_class_member(&self.cx, class_member);
                }
                Parameter(param) => lint_pass.check_parameter(&self.cx, param),
                ParameterList(param_list) => lint_pass.check_parameter_list(&self.cx, param_list),
                Type(ty) => lint_pass.check_type(&self.cx, ty),
                BasicType(basic_ty) => lint_pass.check_basic_type(&self.cx, basic_ty),
                Block(block) => lint_pass.check_block(&self.cx, block),
                Stmt(stmt) => lint_pass.check_stmt(&self.cx, stmt),
                Expr(expr) => lint_pass.check_expr(&self.cx, expr),
                BinaryOp(binop) => lint_pass.check_binop(&self.cx, *binop),
                UnaryOp(unop) => lint_pass.check_unop(&self.cx, *unop),
            }

            self.visit_ast(&child, lint_pass)?;
            if let Some(push) = push {
                self.cx.builder.pop(push);
            }
            if location_pushed {
                self.cx.locator.pop();
            }
            Ok(())
        })
        .unwrap_or(Ok(()))
    }

    fn visit_ast_post(
        &mut self,
        node: &NodeKind<'a, 'f>,
        lint_pass: &mut (dyn SemanticLintPass<'a, 'f> + 'a),
    ) -> Result<(), UnknownLint> {
        use self::NodeKind::*;

        node.for_each_child(&mut |child| {
            let mut push = None;
            let mut location_pushed = false;
            match child {
                AST(_) => (),
                Program(program) => {
                    push = Some(self.cx.builder.push(&program.attrs)?);
                    lint_pass.check_program_post(&self.cx, program);
                }
                ClassDeclaration(class_decl) => {
                    push = Some(self.cx.builder.push(&class_decl.attrs)?);
                    self.cx.locator.push(class_decl.name.data);
                    location_pushed = true;
                    lint_pass.check_class_decl_post(&self.cx, class_decl);
                }
                ClassMember(class_member) => {
                    push = Some(self.cx.builder.push(&class_member.attrs)?);
                    self.cx.locator.push(class_member.name);
                    location_pushed = true;
                    lint_pass.check_class_member_post(&self.cx, class_member);
                }
                _ => return Ok(()),
            }

            self.visit_ast_post(&child, lint_pass)?;
            if let Some(push) = push {
                self.cx.builder.pop(push);
            }
            if location_pushed {
                self.cx.locator.pop();
            }
            Ok(())
        })
        .unwrap_or(Ok(()))
    }
}
