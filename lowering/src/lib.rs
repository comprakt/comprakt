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

extern crate firm_construction;

pub(crate) mod allocator;
pub(crate) mod basic_block_scheduling;
pub(crate) mod codegen;
pub(crate) mod cycle_removal;
pub(crate) mod linear_scan;
pub(crate) mod lir;
pub(crate) mod live_variable_analysis;
pub(crate) mod peephole;
pub(crate) mod register;

use debugging;
use firm_construction::FirmProgram;
use lir::LIR;

pub fn run_backend(
    firm_program: &FirmProgram<'_, '_>,
    out: &mut impl std::io::Write,
) -> std::io::Result<()> {
    let mut lir = LIR::from(firm_program);
    debugging::breakpoint!("LIR stage 1", lir, &|block: &lir::BasicBlock| {
        lir::debugging::default_lir_label(block)
    });

    writeln!(out, "\t.text")?;

    // TODO predictable order
    for f in &mut lir.functions {
        basic_block_scheduling::basic_block_scheduling(f);
        let lva_result = live_variable_analysis::live_variable_analysis(
            &f.graph.blocks_scheduled.as_ref().unwrap(),
            &lir.allocator,
        );

        let lsa_result = linear_scan::register_allocation(f, lva_result);

        // TOOD debugging breakpoint with live ranges + register allocation

        let codegen = codegen::begin_codegen(f, lsa_result);

        // TODO local peepholer

        let mut function_asm = codegen.emit_function();

        peephole::global_peephole(&mut function_asm);

        for instr in function_asm {
            writeln!(out, "{}", instr)?;
        }
    }

    Ok(())
}
