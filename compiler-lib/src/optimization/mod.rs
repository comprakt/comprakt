use crate::firm::Program;
use libfirm_rs::{bindings, graph::Graph};
use std::ffi::CString;

mod constant_folding;
use self::constant_folding::ConstantFolding;
mod unreachable_code_elimination;
use self::unreachable_code_elimination::UnreachableCodeElimination;

/// An optimization that optimizes the whole program by examining all function
/// graphs at once.
pub trait Interprocedural {
    fn optimize(program: &Program<'_, '_>) -> Outcome;
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
    fn optimize(program: &Program<'_, '_>) -> Outcome {
        let mut collector = OutcomeCollector::new();
        for class in program.classes.values() {
            for method in class.borrow().methods.values() {
                if let Some(graph) = method.borrow().graph {
                    collector.push(T::optimize_function(graph.into()));
                }
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
    UnreachableCodeElimination,
}

impl Kind {
    fn run(self, program: &Program<'_, '_>) -> Outcome {
        match self {
            Kind::ConstantFolding => ConstantFolding::optimize(program),
            Kind::UnreachableCodeElimination => UnreachableCodeElimination::optimize(program),
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
            Level::Moderate | Level::Aggressive => vec![Optimization::new(Kind::ConstantFolding)],
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

    fn run(&self, program: &Program<'_, '_>) -> Outcome {
        let outcome = self.kind.run(program);
        self.apply_flags(program);
        outcome
    }

    fn apply_flags(&self, program: &Program<'_, '_>) {
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
    pub fn run_all(&self, program: &Program<'_, '_>) {
        breakpoint!("before optimization sequence".to_string(), program);

        for (i, optimization) in self.sequence().iter().enumerate() {
            log::info!("Running optimization #{}: {:?}", i, optimization);
            optimization.run(program);
            log::debug!("Finished optimization #{}: {:?}", i, optimization.kind);
        }

        breakpoint!("after optimization sequence".to_string(), program);
    }
}