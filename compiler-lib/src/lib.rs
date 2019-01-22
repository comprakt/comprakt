#![warn(rust_2018_idioms)]
#![warn(clippy::print_stdout)]
#![allow(clippy::unneeded_field_pattern)]
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
#![feature(result_map_or_else)]
#![feature(proc_macro_hygiene, decl_macro)]
#![feature(try_trait)]
#![feature(duration_as_u128)]
#[macro_use]
extern crate derive_more;

mod analysis;
pub use asciifile;
pub mod backend;
pub mod timing;
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
pub mod dot;
#[macro_use]
#[allow(dead_code)]
pub mod debugging;
pub mod firm;
pub mod optimization;
pub mod print;
mod ref_eq;
pub mod semantics;
mod spantracker;
pub mod strtab;
pub use symtab;
pub mod type_checking;
pub use self::utils::OutputSpecification;
