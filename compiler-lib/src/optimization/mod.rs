use crate::firm::Program;
mod worklist;
pub use self::worklist::WorkList;

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
            Optimization::ConstantFolding => {
                for class in program.classes.values() {
                    for method in class.borrow().methods.values() {
                        if let Some(graph) = method.borrow().graph {
                            println!("Graph for Method: {:?}", method.borrow().entity.name());
                            let mut worklist = WorkList::new(graph.into());
                            worklist.fixpoint_iterate_const_folding();
                        }
                    }
                }
            }
        };
    }
}
