use super::Span;
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

    pub fn map<U, F>(self, f: F) -> Spanned<'f, U>
    where
        F: FnOnce(T) -> U,
    {
        Spanned {
            span: self.span,
            data: f(self.data),
        }
    }

    pub fn map_ref<U, F>(&self, f: F) -> Spanned<'f, U>
    where
        F: FnOnce(&T) -> U,
    {
        Spanned {
            span: self.span,
            data: f(&self.data),
        }
    }

    /// Combine two `Spanned`s, aplying `f` to compute the contents of the new
    /// `Spanned`. There are also variants of this where LHS/Self, RHS, or both
    /// are boxed. These are necassary to avoid reallocation when calling this.
    pub fn combine_with<Rhs, Res, F>(self, rhs: Spanned<'f, Rhs>, f: F) -> Spanned<'f, Res>
    where
        F: FnOnce(Self, Spanned<'f, Rhs>) -> Res,
    {
        Spanned {
            span: Span::combine(&self.span, &rhs.span),
            data: f(self, rhs),
        }
    }

    pub fn combine_with_boxed<Rhs, Res, F>(
        self,
        rhs: Box<Spanned<'f, Rhs>>,
        f: F,
    ) -> Spanned<'f, Res>
    where
        F: FnOnce(Self, Box<Spanned<'f, Rhs>>) -> Res,
    {
        Spanned {
            span: Span::combine(&self.span, &rhs.span),
            data: f(self, rhs),
        }
    }

    pub fn combine_boxed_with<Rhs, Res, F>(
        self: Box<Self>,
        rhs: Spanned<'f, Rhs>,
        f: F,
    ) -> Spanned<'f, Res>
    where
        F: FnOnce(Box<Self>, Spanned<'f, Rhs>) -> Res,
    {
        rhs.combine_with_boxed(self, |lhs, rhs| f(rhs, lhs))
    }

    pub fn combine_boxed_with_boxed<Rhs, Res, F>(
        self: Box<Self>,
        rhs: Box<Spanned<'f, Rhs>>,
        f: F,
    ) -> Spanned<'f, Res>
    where
        F: FnOnce(Box<Self>, Box<Spanned<'f, Rhs>>) -> Res,
    {
        Spanned {
            span: Span::combine(&self.span, &rhs.span),
            data: f(self, rhs),
        }
    }
}
