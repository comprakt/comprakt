#![feature(uniform_paths)]
#![feature(box_syntax)]
#![feature(core_intrinsics)]

#[macro_use]
extern crate derive_more;

use firm_construction::FirmProgram;

mod alloc;
mod asm;
mod graph;
mod peephole;
mod regalloc;

pub(crate) use alloc::Ptr;

use alloc::Allocator;

#[derive(Default)]
pub struct Alloc {
    pub(crate) function: Allocator<graph::Function>,
    pub(crate) graph: Allocator<graph::BlockGraph>,
    pub(crate) block: Allocator<graph::Block>,
}

pub fn emit_asm(out: &mut std::io::Write, firm: &FirmProgram) -> std::io::Result<()> {
    let alloc = Alloc::default();
    let mut program = graph::Program::from_firm(&alloc, firm);
    program.register_allocation();
    program.peephole();
    program.emit_asm(out)
}
