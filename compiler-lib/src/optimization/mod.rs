use crate::firm::Program;
mod constant_folding;
use libfirm_rs::bindings;
use std::ffi::CString;

#[derive(
    strum_macros::EnumString,
    serde_derive::Deserialize,
    serde_derive::Serialize,
    Debug,
    Copy,
    Clone,
    Display,
)]
pub enum OptimizationKind {
    AlgebraicSimplification,
    ConstantFolding,
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum GlobalOptimizationFlag {
    DumpYcomp,
}

#[derive(Clone, Debug)]
pub struct Optimization {
    pub kind: OptimizationKind,
    pub flags: Vec<GlobalOptimizationFlag>,
}

/// run a list of optimizations on the given program
pub fn run_all(program: &Program<'_, '_>, optimizations: &[Optimization]) {
    for (i, optimization) in optimizations.iter().enumerate() {
        log::debug!("Running optimization #{}: {:?}", i, optimization.kind);
        optimization.run(program);
        if optimization
            .flags
            .iter()
            .any(|f| *f == GlobalOptimizationFlag::DumpYcomp)
        {
            unsafe {
                let suffix = CString::new(format!("{:?}", optimization.kind)).unwrap();
                log::debug!("Dumping graph with suffix {:?}", suffix);
                bindings::dump_all_ir_graphs(suffix.as_ptr());
            }
        }
        log::debug!("Finished optimization #{}: {:?}", i, optimization.kind);
    }
}

impl Optimization {
    fn run(&self, program: &Program<'_, '_>) {
        match self.kind {
            OptimizationKind::AlgebraicSimplification => {}
            OptimizationKind::ConstantFolding => constant_folding::run(program),
        };
    }
}
