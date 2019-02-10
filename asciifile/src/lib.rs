#![feature(slice_patterns)]
#![warn(
    clippy::print_stdout,
    clippy::unimplemented,
    clippy::doc_markdown,
    clippy::items_after_statements,
    clippy::match_same_arms,
    clippy::similar_names,
    clippy::single_match_else,
    clippy::use_self
)]

///! Represents an input file that my only contain ASCII characters. The
///! abstractions `Position` and `Span` represent a type-safe index-less
///! wrapper around a ASCII character, respectively a range of characters
///! (a slice) with positional information.
pub mod file;
pub mod iter;
pub mod line_number_cache;
pub mod maybe_spanned;
pub mod position;
pub mod span;
pub mod spanned;

pub use self::{
    file::AsciiFile,
    iter::{PositionIterator, ReversePositionIterator},
    line_number_cache::LineNumberCache,
    maybe_spanned::MaybeSpanned,
    position::Position,
    span::Span,
    spanned::Spanned,
};
