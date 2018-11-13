///! Represents an input file that my only contain ASCII characters. The
///! abstractions `Position` and `Span` represent a type-safe index-less
///! wrapper around a ASCII character, respectively a range of characters
///! (a slice) with positional information.
pub mod file;
pub mod iter;
pub mod maybe_spanned;
pub mod position;
pub mod span;
pub mod spanned;

pub use self::{
    file::AsciiFile,
    iter::{PositionIterator, ReversePositionIterator},
    maybe_spanned::MaybeSpanned,
    position::Position,
    span::Span,
    spanned::Spanned,
};
