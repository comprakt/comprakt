#![feature(result_map_or_else)]
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

//! Lowers the output of the semantic analysis phase (AST, Type System, Type
//! Analysis), into Firm for code generation.
//!
//!
//! # Generated Labels
//!
//! A dot (`.`) is a valid character in an ASM label, but not in `MiniJava`.
//! This is why it's used as a separator. As properties and methods live in
//! their own namespaces, fields have the additional segment `.F.`, methods have
//! the additional segment `.M.`.
//!
//! # Unused Struct Properties
//!
//! While building the firm graph, identifiers are created for Firm entities
//! using `CString`, which heap-allocates. However, firm only contains raw
//! pointers to the `CString` instances, hence the `CString` must be kept around
//! for the lifetime of the graph. Otherwise rust would de allocate the
//! `CString` to early!
pub mod firm_program;
pub mod method_body_generator;
pub mod program_generator;
pub mod runtime;
mod type_translation;

// hacks for legacy imports
pub use asciifile;
pub use parser::{self, ast};
pub use strtab;
pub use type_checking;
pub use utils::ref_eq;

#[macro_use]
extern crate derive_more;

pub use self::{
    firm_program::*, method_body_generator::MethodBodyGenerator,
    program_generator::ProgramGenerator, runtime::Runtime,
};
use failure::Fail;
use std::path::PathBuf;

/// Enable or disable behaviour during the lowering phase
#[derive(Debug, Clone, Default)]
pub struct Options {
    pub dump_firm_graph: bool,
    pub dump_class_layouts: bool,
}

#[derive(Debug, Fail)]
pub enum FirmError {
    #[fail(display = "failed to write assembly to file {:?}", path)]
    EmitAsmFailure { path: PathBuf },
}
