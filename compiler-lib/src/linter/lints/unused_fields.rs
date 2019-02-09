use super::{LateContext, LintContext, LintPass, SemanticLintPass};
use asciifile::Spanned;
use diagnostics::{declare_lint, lint::LintArray, lint_array};
use parser::ast::*;
use std::collections::HashMap;
use strtab::Symbol;
use type_checking::{type_analysis::RefInfo, type_system::CheckedType};

declare_lint!(
    pub UNUSED_FIELDS,
    Warning,
    "unused fields"
);

#[derive(Default)]
pub struct UnusedFieldsPass<'f> {
    used_fields: HashMap<(Symbol<'f>, Symbol<'f>), usize>,
    fields_in_assignment: HashMap<(Symbol<'f>, Symbol<'f>), usize>,
}

impl<'f> LintPass for UnusedFieldsPass<'f> {
    fn get_lints(&self) -> LintArray {
        lint_array!(UNUSED_FIELDS)
    }
}

impl<'a, 'f> SemanticLintPass<'a, 'f> for UnusedFieldsPass<'f> {
    fn check_expr(&mut self, cx: &LateContext<'a, 'f>, expr: &Spanned<'_, Expr<'f>>) {
        match &expr.data {
            Expr::FieldAccess(prev, name) => {
                let expr_info = cx.type_analysis.expr_info(prev);
                if let CheckedType::TypeRef(class_id) = expr_info.ty {
                    *self
                        .used_fields
                        .entry((class_id.id(), name.data))
                        .or_default() += 1;
                } else {
                    unreachable!()
                }
            }
            Expr::Var(name) => {
                let expr_info = cx.type_analysis.expr_info(&expr.data);
                if let Some(RefInfo::Field(_)) = expr_info.ref_info {
                    *self
                        .used_fields
                        .entry((cx.locator.class(), name.data))
                        .or_default() += 1;
                }
            }
            Expr::Binary(BinaryOp::Assign, lhs, _) => match &lhs.data {
                Expr::FieldAccess(prev, name) => {
                    let expr_info = cx.type_analysis.expr_info(&prev.data);
                    if let CheckedType::TypeRef(class_id) = expr_info.ty {
                        *self
                            .fields_in_assignment
                            .entry((class_id.id(), name.data))
                            .or_default() += 1;
                    } else {
                        unreachable!()
                    }
                }
                Expr::Var(name) => {
                    let expr_info = cx.type_analysis.expr_info(&lhs.data);
                    if let Some(RefInfo::Field(_)) = expr_info.ref_info {
                        *self
                            .fields_in_assignment
                            .entry((cx.locator.class(), name.data))
                            .or_default() += 1;
                    }
                }
                _ => (),
            },
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
        let class = cx.locator.class();
        if let ClassMemberKind::Field(..) = class_member.kind {
            match self.used_fields.get(&(class, class_member.name)) {
                Some(num_usage) => {
                    if let Some(num_assigned) =
                        self.fields_in_assignment.get(&(class, class_member.name))
                    {
                        debug_assert!(num_assigned <= num_usage);
                        if num_usage == num_assigned {
                            cx.struct_lint(
                                UNUSED_FIELDS,
                                class_member.span,
                                &format!(
                                    "this field is assigned to but never used: `{0}`",
                                    class_member.name.as_str()
                                ),
                            );
                        }
                    }
                }
                None => {
                    cx.struct_lint(
                        UNUSED_FIELDS,
                        class_member.span,
                        &format!(
                            "unused field: `{0}`. Consider using `_{0}` instead",
                            class_member.name,
                        ),
                    );
                }
            }
        }
    }
}
