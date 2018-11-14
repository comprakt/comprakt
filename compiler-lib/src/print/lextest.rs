use crate::lexer::TokenKind::{self, *};
use std::fmt;
///! Our default `Display` output for each `TokenKind` is less ambiguous
///! than the format required by `--lextest` and therefore improves error
///! messages.

pub struct Output<'token> {
    token: &'token TokenKind<'token>,
}

impl<'token> Output<'token> {
    pub fn new(token: &'token TokenKind<'token>) -> Self {
        Self { token }
    }
}

impl<'token> fmt::Display for Output<'token> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> Result<(), std::fmt::Error> {
        match self.token {
            Keyword(keyword) => write!(f, "{}", keyword),
            Operator(operator) => write!(f, "{}", operator),
            Identifier(symbol) => write!(f, "identifier {}", symbol),
            IntegerLiteral(symbol) => write!(f, "integer literal {}", symbol),
            Comment(body) => write!(f, "/*{}*/", body),
            Whitespace => write!(f, ""),
        }
    }
}
