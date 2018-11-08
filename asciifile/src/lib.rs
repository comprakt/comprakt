#![feature(slice_patterns)]
///! Represents an input file that my only contain ASCII characters. The
///! abstractions `Position` and `Span` represent a type-safe index-less
///! wrapper around a ASCII character, respectively a range of characters
///! (a slice) with positional information.
pub mod file;
// TODO: rename Position to Char
pub mod iter;
pub mod position;
pub mod span;
pub mod spanned;

// TODO: remove LinenTruncation. Will reduce performance but keep my sanity
pub use self::{
    file::{AsciiFile, LineTruncation},
    iter::PositionIterator,
    position::Position,
    span::Span,
    spanned::Spanned,
};
