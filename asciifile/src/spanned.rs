use crate::{Position, Span};
use std::fmt;

#[derive(Debug, Clone)]
pub struct Spanned<'f, T> {
    pub span: Span<'f>,
    pub data: T,
}

impl<T> fmt::Display for Spanned<'_, T>
where
    T: fmt::Display,
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{} at {}", self.data, self.span)
    }
}

impl<'f, T> Spanned<'f, T> {
    pub fn new(start: Position<'f>, end: Position<'f>, value: T) -> Self {
        Spanned {
            span: Span { start, end },
            data: value,
        }
    }

    pub fn map<U, F>(&self, f: F) -> Spanned<'f, U>
    where
        F: FnOnce(&T) -> U,
    {
        Spanned {
            span: self.span.clone(),
            data: f(&self.data),
        }
    }
}
