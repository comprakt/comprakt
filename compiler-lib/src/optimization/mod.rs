use crate::firm::Program;
mod constant_folding;

#[derive(
    strum_macros::EnumString,
    serde_derive::Deserialize,
    serde_derive::Serialize,
    Debug,
    Copy,
    Clone,
    Display,
)]
pub enum Optimization {
    AlgebraicSimplification,
    ConstantFolding,
}

/// run a list of optimizations on the given program
pub fn run_all(program: &Program<'_, '_>, optimizations: &[Optimization]) {
    for optimization in optimizations {
        optimization.run(program);
    }
}

impl Optimization {
    fn run(self, program: &Program<'_, '_>) {
        log::debug!("Running optimization: {:?}", self);

        match self {
            Optimization::AlgebraicSimplification => {}
            Optimization::ConstantFolding => constant_folding::run(program),
        };
    }
}
