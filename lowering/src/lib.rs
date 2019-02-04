#![feature(box_syntax)]
#![feature(type_ascription)]
#![feature(never_type)]
#![feature(core_intrinsics)]
#![feature(try_from)]
#![feature(uniform_paths)]
#![warn(clippy::print_stdout)]
#![warn(clippy::all)]

#[macro_use]
extern crate derive_more;

extern crate debugging;
extern crate utils;

pub mod amd64;
pub(crate) mod gen_instr;
pub(crate) mod lir;
#[macro_use]
pub(crate) mod lir_allocator;
// FIXME users should use re-export lir::debugging
pub(crate) mod lir_debugging;

// Compat uses (old imports)
pub(crate) mod lowering {
    pub(crate) use super::{amd64, lir, lir_allocator};
}
pub(crate) use firm_construction as firm;
pub(crate) use optimization;
pub(crate) use type_checking;

use firm::FirmProgram;
use lir::LIR;

pub fn run_backend(
    firm_program: &FirmProgram<'_, '_>,
    out: &mut impl std::io::Write,
) -> std::io::Result<()> {
    let mut lir = LIR::from(firm_program);
    crate::debugging::breakpoint!("LIR stage 1", lir, &|block: &lir::BasicBlock| {
        self::lir_debugging::default_lir_label(block)
    });

    writeln!(out, "\t.text")?;

    // TODO predictable order
    for f in &mut lir.functions {
        crate::amd64::basic_block_scheduling::basic_block_scheduling(f);
        let lva_result = crate::amd64::live_variable_analysis::live_variable_analysis(
            &f.graph.blocks_scheduled.as_ref().unwrap(),
            &lir.allocator,
        );

        let lsa_result = crate::amd64::linear_scan::register_allocation(f, lva_result);

        let codegen = amd64::codegen::begin_codegen(f, lsa_result);

        // TODO local peepholer

        let function_asm = codegen.emit_function();

        // TODO global peepholer

        for instr in function_asm {
            writeln!(out, "{}", instr)?;
        }
    }

    Ok(())
}
