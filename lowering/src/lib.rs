#![feature(box_syntax)]
#![feature(type_ascription)]
#![feature(never_type)]
#![feature(core_intrinsics)]
#![feature(try_from)]

#[macro_use]
extern crate derive_more;

extern crate debugging;
extern crate utils;

pub mod amd64;
pub mod gen_instr;
pub mod lir;
#[macro_use]
pub mod lir_allocator;
// FIXME users should use re-export lir::debugging
pub mod lir_debugging;

// Compat uses (old imports)
pub(crate) mod lowering {
    pub(crate) use super::{amd64, lir, lir_allocator};
}
pub(crate) use firm_construction as firm;
pub(crate) use optimization;
pub(crate) use type_checking;
