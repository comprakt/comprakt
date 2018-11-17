#![warn(rust_2018_idioms)]
#![feature(try_from)]
#![feature(if_while_or_patterns)]
#![feature(bind_by_move_pattern_guards)]
#![feature(const_str_as_bytes)]
#![feature(slice_patterns)]
#![feature(box_syntax)]
#![feature(repeat_generic_slice)]
#![feature(never_type)]
#![feature(nll)]
#![feature(core_intrinsics)]
#![feature(custom_attribute)]
#[macro_use]
extern crate derive_more;

pub mod asciifile;
#[macro_use]
mod utils;
pub mod ast;
mod color;
pub mod context;
pub mod diagnostics;
pub mod lexer;
pub mod parser;
#[macro_use]
pub mod visitor;
pub mod print;
pub mod semantics;
mod spantracker;
pub mod strtab;
pub mod symtab;
pub mod type_checking;
