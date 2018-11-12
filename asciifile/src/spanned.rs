use crate::{MaybeSpanned, Span};
use std::{fmt, ops::Deref};

#[derive(Debug, Clone)]
pub struct Spanned<'f, T> {
    pub span: Span<'f>,
    pub data: T,
}

impl<'f, T> Eq for Spanned<'f, T> where T: Eq {}
impl<'f, T> PartialEq for Spanned<'f, T>
where
    T: PartialEq,
{
    /// This only compares the `data`! I.e. two `Spanned`s are equal even if
    /// they point to two different spans in the source file, as long as the
    /// content is the same.
    fn eq(&self, other: &Self) -> bool {
        self.data == other.data
    }
}

impl<'f, T> Deref for Spanned<'f, T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        &self.data
    }
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
    pub fn new(span: Span<'f>, value: T) -> Self {
        Spanned { span, data: value }
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

impl<'a, 'b, T: 'b> From<&'b Spanned<'a, T>> for MaybeSpanned<'a, &'b dyn fmt::Display>
where
    T: fmt::Display,
{
    fn from(spanned: &'b Spanned<'a, T>) -> Self {
        MaybeSpanned::WithSpan(Spanned {
            span: spanned.span.clone(),
            data: &spanned.data,
        })
    }
}
