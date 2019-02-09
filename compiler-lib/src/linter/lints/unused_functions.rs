use super::{LateContext, LintContext, LintPass, SemanticLintPass};
use asciifile::Spanned;
use diagnostics::{declare_lint, lint::LintArray, lint_array};
use parser::ast::*;
use std::collections::HashSet;
use strtab::Symbol;
use type_checking::type_system::CheckedType;

declare_lint!(
    pub UNUSED_FUNCTIONS,
    Warning,
    "unused functions"
);

#[derive(Default)]
pub struct UnusedFunctionsPass<'f> {
    used_methods: HashSet<(Symbol<'f>, Symbol<'f>)>,
}

impl<'f> LintPass for UnusedFunctionsPass<'f> {
    fn get_lints(&self) -> LintArray {
        lint_array!(UNUSED_FUNCTIONS)
    }
}

impl<'a, 'f> SemanticLintPass<'a, 'f> for UnusedFunctionsPass<'f> {
    fn check_expr(&mut self, cx: &LateContext<'a, 'f>, expr: &Spanned<'_, Expr<'f>>) {
        match &expr.data {
            Expr::MethodInvocation(prev, name, ..) => {
                let expr_info = cx.type_analysis.expr_info(prev);
                if let CheckedType::TypeRef(class_id) = expr_info.ty {
                    self.used_methods.insert((class_id.id(), name.data));
                } else {
                    unreachable!()
                }
            }
            Expr::ThisMethodInvocation(name, ..) => {
                self.used_methods.insert((cx.locator.class(), name.data));
            }
            _ => (),
        }
    }

    fn check_class_member_post(
        &mut self,
        cx: &LateContext<'_, '_>,
        class_member: &Spanned<'_, ClassMember<'_>>,
    ) {
        if class_member.name.as_str().starts_with('_') {
            return;
        }
        if let ClassMemberKind::Method(..) = class_member.kind {
            if self
                .used_methods
                .get(&(cx.locator.class(), class_member.name))
                .is_none()
            {
                cx.struct_lint(
                    UNUSED_FUNCTIONS,
                    class_member.span,
                    &format!(
                        "unused function: `{0}`. Consider using `_{0}` instead",
                        class_member.name,
                    ),
                );
            }
        }
    }
}
