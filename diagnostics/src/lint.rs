use super::MessageLevel;
use std::hash::{Hash, Hasher};

pub type LintArray = Vec<&'static Lint>;

#[derive(Debug, Copy, Clone)]
pub struct Lint {
    pub name: &'static str,
    pub level: MessageLevel,
    pub desc: &'static str,
}

#[macro_export]
macro_rules! declare_lint {
    ($vis:vis $NAME:ident, $Level:ident, $desc:expr) => {
        $vis static $NAME: &::diagnostics::lint::Lint = &::diagnostics::lint::Lint {
            name: stringify!($NAME),
            level: ::diagnostics::MessageLevel::$Level,
            desc: $desc,
        };
    }
}

/// Declare a static `LintArray` and return it as an expression.
#[macro_export]
macro_rules! lint_array {
    ($( $lint:expr ),* ,) => { lint_array!( $($lint),* ) };
    ($( $lint:expr ),*) => {{
        vec![$($lint),*]
    }}
}

#[derive(Debug, Copy, Clone)]
pub struct LintId {
    lint: &'static Lint,
}

impl PartialEq for LintId {
    fn eq(&self, other: &Self) -> bool {
        std::ptr::eq(self.lint, other.lint)
    }
}

impl Eq for LintId {}

impl Hash for LintId {
    fn hash<H: Hasher>(&self, state: &mut H) {
        let ptr = self.lint as *const Lint;
        ptr.hash(state);
    }
}

impl LintId {
    pub fn of(lint: &'static Lint) -> Self {
        Self { lint }
    }

    pub fn to_string(self) -> String {
        self.lint.name.to_ascii_lowercase()
    }
}
