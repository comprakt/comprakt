use super::{Optimization, OptimizationResult};
use crate::firm::Program;
use log;

#[derive(Clone)]
pub struct Fixpoint<'a> {
    opts: &'a [Optimization],
}

impl<'a> Fixpoint<'a> {
    pub fn new(opts: &'a [Optimization]) -> Fixpoint<'a> {
        Fixpoint { opts }
    }

    pub fn run(&self, program: &Program<'_, '_>) -> OptimizationResult {
        if self.opts.is_empty() {
            return OptimizationResult::Unchanged;
        }

        let mut res = OptimizationResult::Unchanged;
        let mut unchanged_in_a_row = 0;
        for opt in self.opts.iter().cycle() {
            let opt_res = opt.run(program);
            if OptimizationResult::Unchanged == opt_res {
                log::debug!("unchanged: {}", opt.kind);

                unchanged_in_a_row += 1;
                log::debug!("unchanged in a row: {}", unchanged_in_a_row);
                if unchanged_in_a_row == self.opts.len() {
                    break;
                }
            } else {
                log::debug!("changed: {}", opt.kind);
                res = OptimizationResult::Changed;
                unchanged_in_a_row = 0;
            }
        }

        res
    }
}
