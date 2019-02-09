use super::{AstLintPass, EarlyContext, LintContext, LintPass};
use asciifile::Spanned;
use diagnostics::{declare_lint, lint::LintArray, lint_array};
use parser::ast::*;
use std::collections::HashSet;
use strtab::Symbol;

declare_lint!(
    pub UNUSED_CLASS,
    Warning,
    "unused class"
);

#[derive(Default)]
pub struct UnusedClassPass<'f> {
    constructed_classes: HashSet<Symbol<'f>>,
}

impl<'f> LintPass for UnusedClassPass<'f> {
    fn get_lints(&self) -> LintArray {
        lint_array!(UNUSED_CLASS)
    }
}

impl<'a, 'f> AstLintPass<'a, 'f> for UnusedClassPass<'f> {
    fn check_expr(&mut self, _: &EarlyContext<'a, 'f>, expr: &Spanned<'_, Expr<'f>>) {
        if let Expr::NewObject(name) = &expr.data {
            self.constructed_classes.insert(name.data);
        }
    }

    fn check_class_decl_post(
        &mut self,
        cx: &EarlyContext<'_, '_>,
        class_decl: &Spanned<'_, ClassDeclaration<'_>>,
    ) {
        if class_decl.name.as_str().starts_with('_') {
            return;
        }
        if self.constructed_classes.get(&class_decl.name).is_none() {
            for member in &class_decl.members {
                if let ClassMemberKind::MainMethod(..) = member.kind {
                    return;
                }
            }
            cx.struct_lint(
                UNUSED_CLASS,
                class_decl.span,
                &format!(
                    "This class is never constructed: `{0}`. Consider using `_{0}` instead",
                    class_decl.name.data,
                ),
            );
        }
    }
}
