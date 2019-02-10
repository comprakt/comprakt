#![feature(box_syntax)]
#![feature(duration_as_u128)]
#![warn(
    clippy::print_stdout,
    clippy::unimplemented,
    clippy::doc_markdown,
    clippy::items_after_statements,
    clippy::match_same_arms,
    clippy::similar_names,
    clippy::single_match_else,
    clippy::use_self,
    clippy::use_debug
)]

pub mod context;

#[macro_use]
pub mod timing;
