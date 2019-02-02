use super::AstLintPass;
use asciifile::Spanned;
use compiler_shared::context::Context;
use diagnostics::declare_lint;
use parser::ast::*;

declare_lint!(
    pub BOOL_LITERAL_COMPARISON,
    Warning,
    "comparison with a bool literal"
);

declare_lint!(
    pub NOT_ON_BOOL_LITERAL,
    Warning,
    "comparison with a bool literal"
);

pub struct BoolPass;

impl<'f> AstLintPass<'f> for BoolPass {
    fn check_expr(&mut self, cx: &Context<'_>, expr: &Spanned<'_, Expr<'_>>) {
        match &expr.data {
            Expr::Binary(BinaryOp::Equals, lhs, rhs)
            | Expr::Binary(BinaryOp::NotEquals, lhs, rhs) => match (&lhs.data, &rhs.data) {
                (Expr::Boolean(x), Expr::Boolean(y)) => {
                    let msg = if x == y {
                        "comparison of identical boolean literals. Consider using `true`"
                    } else {
                        "comparison of differing boolean literals. Consider using `false`"
                    };
                    cx.diagnostics
                        .lint(BOOL_LITERAL_COMPARISON, expr.span, msg.to_string());
                }
                (Expr::Boolean(val), _) => {
                    let prefix = if *val { "" } else { "!" };
                    cx.diagnostics.lint(
                        BOOL_LITERAL_COMPARISON,
                        expr.span,
                        format!(
                            "comparison with a boolean literal. Consider using `{}{}`",
                            prefix,
                            rhs.span.as_str()
                        ),
                    );
                }
                (_, Expr::Boolean(val)) => {
                    let prefix = if *val { "" } else { "!" };
                    cx.diagnostics.lint(
                        BOOL_LITERAL_COMPARISON,
                        expr.span,
                        format!(
                            "comparison with a boolean literal. Consider using `{}{}`",
                            prefix,
                            lhs.span.as_str()
                        ),
                    );
                }
                _ => (),
            },
            Expr::Unary(UnaryOp::Not, e) => {
                if let Expr::Boolean(val) = e.data {
                    let val = if expr.span.as_str().matches('!').count() % 2 == 0 {
                        !val
                    } else {
                        val
                    };
                    let inverse = if val { "false" } else { "true" };
                    cx.diagnostics.lint(
                        NOT_ON_BOOL_LITERAL,
                        expr.span,
                        format!(
                            "inverting a bool literal. This can be simplified to `{}`",
                            inverse
                        ),
                    );
                }
            }
            _ => (),
        }
    }
}
