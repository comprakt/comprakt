#![warn(rust_2018_idioms)]
#![warn(clippy::print_stdout)]
#![allow(clippy::unneeded_field_pattern)]
#![feature(try_from)]
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
#![feature(weak_ptr_eq)]
#![feature(type_ascription)]
#![feature(duration_as_u128)]

pub use asciifile;
pub mod backend;

#[macro_use]
extern crate utils;
pub use utils::ref_eq;

pub use diagnostics;

pub use compiler_shared::context;

pub use lexer;

#[macro_use]
extern crate parser;

pub use parser::{ast, visitor};

pub use firm_construction as firm;
pub mod print;

pub use self::utils::OutputSpecification;
pub use strtab;
pub use symtab;
pub use type_checking;

mod firm_context;
pub use crate::firm_context::FirmContext;
