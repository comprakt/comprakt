use crate::Spanned;
use std::ops::Deref;

#[derive(Debug)]
pub enum MaybeSpanned<'a, T> {
    WithoutSpan(T),
    WithSpan(Spanned<'a, T>),
}

impl<'a, T> Deref for MaybeSpanned<'a, T> {
    type Target = T;

    fn deref(&self) -> &T {
        match self {
            MaybeSpanned::WithoutSpan(data) => data,
            MaybeSpanned::WithSpan(spanned) => &spanned.data,
        }
    }
}
