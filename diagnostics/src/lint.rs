use super::MessageLevel;

pub type LintArray = Vec<Lint>;

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
