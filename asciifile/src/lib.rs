#![feature(slice_patterns)]
///! Represents an input file that my only contain ASCII characters. The
///! abstractions `Position` and `Span` represent a type-safe index-less
///! wrapper around a ASCII character, respectively a range of characters
///! (a slice) with positional information.
pub mod file;
pub mod iter;
pub mod position;
pub mod span;
pub mod spanned;
pub mod maybe_spanned;

pub use self::{
    file::AsciiFile,
    iter::{PositionIterator, ReversePositionIterator},
    position::Position,
    span::Span,
    spanned::Spanned,
    maybe_spanned::MaybeSpanned,
};
