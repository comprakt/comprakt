//! Basic block scheduling heuristic.

use crate::lir;

pub fn basic_block_scheduling(func: &mut lir::Function) {
    // reverse postorder
    let ordered = func
        .graph
        .postorder_blocks()
        .into_iter()
        .rev()
        .collect::<Vec<_>>();
    func.graph.blocks_scheduled = Some(ordered);
}
