use super::{AstLintPass, EarlyContext, LintContext, LintPass};
use asciifile::Spanned;
use diagnostics::{declare_lint, lint::LintArray, lint_array};
use parser::ast::*;

declare_lint!(
    pub TOO_MANY_ARGUMENTS,
    Warning,
    "too many arguments in a function"
);

pub struct TooManyArgumentsPass;

impl LintPass for TooManyArgumentsPass {
    fn get_lints(&self) -> LintArray {
        lint_array!(TOO_MANY_ARGUMENTS)
    }
}

impl<'a, 'f> AstLintPass<'a, 'f> for TooManyArgumentsPass {
    fn check_parameter_list(
        &mut self,
        cx: &EarlyContext<'_, '_>,
        parameter_list: &Spanned<'_, ParameterList<'_>>,
    ) {
        if parameter_list.len() > 6 {
            cx.struct_lint(
                TOO_MANY_ARGUMENTS,
                parameter_list.span,
                "too many arguments. Try to use 6 or less arguments",
            );
        }
    }
}
