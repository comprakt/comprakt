use super::AstLintPass;
use asciifile::Spanned;
use compiler_shared::context::Context;
use diagnostics::declare_lint;
use parser::{ast::*, visitor::NodeKind};
use std::collections::HashMap;
use strtab::Symbol;

declare_lint!(
    pub UNUSED_ARGUMENTS,
    Warning,
    "unused arguments of a function"
);

pub struct UnusedArgumentPass;

impl<'f> AstLintPass<'f> for UnusedArgumentPass {
    fn check_class_member(
        &mut self,
        cx: &Context<'_>,
        class_member: &Spanned<'_, ClassMember<'_>>,
    ) {
        if let ClassMemberKind::Method(_, param_list, block) = &class_member.kind {
            let mut method_body_visitor = MethodBodyVisitor::default();
            method_body_visitor.visit_block(&NodeKind::Block(block));

            for param in &param_list.data {
                if param.name.as_str().starts_with('_') {
                    return;
                }
                match method_body_visitor.used_vars.get(&param.data.name) {
                    Some(num_usage) => {
                        if let Some(num_assigned) =
                            method_body_visitor.vars_in_assignment.get(&param.data.name)
                        {
                            debug_assert!(num_assigned <= num_usage);
                            if num_usage == num_assigned {
                                cx.diagnostics.lint(
                                    UNUSED_ARGUMENTS,
                                    param.span,
                                    format!(
                                        "unused argument: `{0}`. Consider using `_{0}` instead",
                                        param.name.as_str()
                                    ),
                                );
                            }
                        }
                    }
                    None => cx.diagnostics.lint(
                        UNUSED_ARGUMENTS,
                        param.span,
                        format!(
                            "unused argument: `{0}`. Consider using `_{0}` instead",
                            param.name.as_str()
                        ),
                    ),
                }
            }
        }
    }
}

#[derive(Default)]
struct MethodBodyVisitor<'f> {
    used_vars: HashMap<Symbol<'f>, usize>,
    vars_in_assignment: HashMap<Symbol<'f>, usize>,
}

impl<'a, 'f> MethodBodyVisitor<'f> {
    fn visit_block(&mut self, node: &NodeKind<'a, 'f>) {
        node.for_each_child(&mut |child| {
            if let NodeKind::Expr(expr) = child {
                match &expr.data {
                    Expr::Var(var) => {
                        *self.used_vars.entry(var.data).or_default() += 1;
                    }
                    Expr::Binary(BinaryOp::Assign, lhs, _) => {
                        if let Expr::Var(var) = &lhs.data {
                            *self.vars_in_assignment.entry(var.data).or_default() += 1;
                        }
                    }
                    _ => (),
                }
            }

            self.visit_block(&child)
        });
    }
}
