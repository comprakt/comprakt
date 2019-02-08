#![feature(try_trait)]

use crate::{firm::FirmProgram, timing::Measurement};
use libfirm_rs::{bindings, Graph};
use std::ffi::CString;

#[macro_use]
extern crate debugging;
#[macro_use]
extern crate derive_more;

// compat uses
pub(crate) use compiler_shared::timing;
pub(crate) use debugging::dot;
pub(crate) use firm_construction as firm;
pub(crate) use utils::ref_eq;
pub(crate) mod optimization {
    pub(crate) use super::*;
}

pub(crate) mod analysis;

mod inlining;
use self::inlining::Inlining;
mod constant_folding;
use self::constant_folding::ConstantFolding;
mod control_flow;
use self::control_flow::ControlFlow;
mod code_placement;
use self::code_placement::{CodePlacement, CostMinimizingPlacement, EarliestPlacement};
mod remove_critical_edges;
pub use self::remove_critical_edges::RemoveCriticalEdges;
mod common_subexpr_elim;
pub use self::common_subexpr_elim::CommonSubExpr;
pub mod compile_time_assertions;
pub use self::compile_time_assertions::{CompileTimeAssertions, Phase};
mod lattices;

/// An optimization that optimizes the whole program by examining all function
/// graphs at once.
pub trait Interprocedural {
    fn optimize(program: &mut FirmProgram<'_, '_>) -> Outcome;
}

/// An optimization that only works on a single graph and therefore does not
/// optimize across function call boundaries.
pub trait Local {
    fn optimize_function(graph: Graph) -> Outcome;
}

impl<T> Interprocedural for T
where
    T: Local,
{
    fn optimize(program: &mut FirmProgram<'_, '_>) -> Outcome {
        let mut collector = OutcomeCollector::new();
        for method in program.methods.values() {
            if let Some(graph) = method.borrow().graph {
                collector.push(T::optimize_function(graph));
            }
        }
        collector.result()
    }
}

/// All available optimizations
#[derive(
    strum_macros::EnumString,
    serde_derive::Deserialize,
    serde_derive::Serialize,
    Debug,
    Copy,
    Clone,
    Display,
)]
pub enum Kind {
    ConstantFolding,
    Inline,
    ControlFlow,
    RemoveCriticalEdges,
    EarliestPlacement,
    CostMinimizingPlacement,
    CodePlacement,
    CommonSubExprElim,
}

impl Kind {
    fn run(self, program: &mut FirmProgram<'_, '_>) -> Outcome {
        match self {
            Kind::ConstantFolding => ConstantFolding::optimize(program),
            Kind::Inline => Inlining::optimize(program),
            Kind::ControlFlow => ControlFlow::optimize(program),
            Kind::RemoveCriticalEdges => RemoveCriticalEdges::optimize(program),
            Kind::EarliestPlacement => EarliestPlacement::optimize(program),
            Kind::CostMinimizingPlacement => CostMinimizingPlacement::optimize(program),
            Kind::CodePlacement => CodePlacement::optimize(program),
            Kind::CommonSubExprElim => CommonSubExpr::optimize(program),
        }
    }
}

/// These are predefined sequences of optimizations (in clang and gcc these are
/// called `-O1`, `-O3`, `-Os` and so forth).
#[derive(Debug, Clone)]
pub enum Level {
    /// This level compiles the fastest and generates the most debuggable code
    /// since it does not perform any optimization.
    None,
    /// Moderate level of optimization which enables most optimizations.
    Moderate,
    /// Enables optimizations that take longer to perform or that may generate
    /// larger code in an attempt to make the program run faster.
    Aggressive,
    /// A user-defined sequence of optimizations
    Custom(Vec<Optimization>),
}

impl Default for Level {
    fn default() -> Level {
        Level::None
    }
}

impl Level {
    fn sequence(&self) -> Vec<Optimization> {
        match self {
            Level::None => vec![],
            Level::Moderate => vec![
                Optimization::new(Kind::ConstantFolding),
                Optimization::new(Kind::ControlFlow),
                // block-local common subexpression elimination
                Optimization::new(Kind::CommonSubExprElim),
            ],
            Level::Aggressive => vec![
                // TODO: code placement in combination with inlining can be
                // very expensive
                Optimization::new(Kind::Inline),
                Optimization::new(Kind::ConstantFolding),
                Optimization::new(Kind::ControlFlow),
                // this sequence results in global common subexpression elimination
                // and loop invariant code motion
                Optimization::new(Kind::EarliestPlacement),
                Optimization::new(Kind::CommonSubExprElim),
                Optimization::new(Kind::CostMinimizingPlacement),
            ],
            Level::Custom(list) => list.clone(),
        }
    }
}

/// These are options that can be attached to any optimization
#[derive(Clone, Copy, Debug, PartialEq)]
pub enum Flag {
    /// Dump a 'Visualization of Compiler Graph' (VCG) file that can be viewed
    /// in yComp.
    DumpVcg,
    /// Start the interactive web-based graphical debugger
    Gui,
}

#[derive(Clone, Debug)]
pub struct Optimization {
    pub kind: Kind,
    pub flags: Vec<Flag>,
}

impl Optimization {
    fn new(kind: Kind) -> Self {
        Self {
            kind,
            flags: vec![],
        }
    }

    fn has_flag(&self, flag: Flag) -> bool {
        self.flags.iter().any(|f| *f == flag)
    }

    fn run(&self, program: &mut FirmProgram<'_, '_>) -> Outcome {
        let outcome = self.kind.run(program);
        self.apply_flags(program);
        outcome
    }

    fn apply_flags(&self, program: &FirmProgram<'_, '_>) {
        if self.has_flag(Flag::DumpVcg) {
            unsafe {
                let suffix = CString::new(format!("-{}", self.kind)).unwrap();
                bindings::dump_all_ir_graphs(suffix.as_ptr());
            }
        }

        if self.has_flag(Flag::Gui) {
            let label = format!("After running optimization '{}'", self.kind);
            breakpoint!(label, program);
        }
    }
}

#[derive(Clone, Copy, PartialEq, Debug)]
pub enum Outcome {
    Unchanged,
    Changed,
}

#[derive(Default)]
pub struct OutcomeCollector {
    results: Vec<Outcome>,
}

impl OutcomeCollector {
    pub fn new() -> OutcomeCollector {
        Self::default()
    }

    pub fn push(&mut self, res: Outcome) {
        self.results.push(res);
    }

    pub fn result(&self) -> Outcome {
        if self.results.iter().any(|x| *x == Outcome::Changed) {
            Outcome::Changed
        } else {
            Outcome::Unchanged
        }
    }
}

impl Level {
    /// run the list of optimizations defined by the optimization level
    /// on the given program
    pub fn run_all(&self, program: &mut FirmProgram<'_, '_>) {
        breakpoint!("before optimization sequence".to_string(), program);
        let measurement_all = Measurement::start("optimization phase");
        let compile_time_assertions = CompileTimeAssertions::new();

        for (i, optimization) in self.sequence().iter().enumerate() {
            log::info!("Running optimization #{}: {:?}", i, optimization);
            compile_time_assertions.check_program(
                program,
                Phase {
                    opt_idx: i,
                    opt_kind: optimization.kind,
                    is_already_applied: false,
                },
            );
            let measurement = Measurement::start(&format!("opt #{}: {}", i, optimization.kind));
            optimization.run(program);
            measurement.stop();
            compile_time_assertions.check_program(
                program,
                Phase {
                    opt_idx: i,
                    opt_kind: optimization.kind,
                    is_already_applied: true,
                },
            );
            log::debug!("Finished optimization #{}: {:?}", i, optimization.kind);
        }

        measurement_all.stop();

        breakpoint!("after optimization sequence".to_string(), program);
    }
}
