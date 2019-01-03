//! Replaces chains of blocks that only contain a jump with a single
//! jump. 
//!
//! > Compilers have been implementing Jump Threading for a long time, and little
//! > scientific attention has been devoted to it because it is a seemingly basic optimization.
//! > However, what compiler authors mean by Jump Threading differs wildly between
//! > compilers.
//!
//! We implement two very basic substitutions here. Removal of unnecessary conditional
//! jumps:
//!
//! ```
//!       Block A
//!         |
//!        Cond
//!       /    \                  Block A
//!    Proj   Proj      -->          |
//!    True   False               Block B
//!       \    /
//!       Block B
//!
//! ```
//!
//!
//! Removal of unnecessary unconditional jumps:
//! 
//! ```
//!        Block A
//!        |            -->       Block AB
//!        Jmp
//!        |
//!        Block B
//! ```
//!
//! # Things to Consider
//! - Firstly, duplicating blocks means introducing additional definitions of that block’s
//!   variables. In SSA-based CFGs, we have to take additional steps to preserve semantics.
//!
//! # References
//! 
//! - Blog entry "Jump Threading" by Andreas Zwickau:
//!   http://beza1e1.tuxen.de/articles/jump_threading.html
//! - Master thesis "Generalized Jump Threading in LibFirm" by
//!   Joachim Priesner
//!   https://pp.ipd.kit.edu/uploads/publikationen/priesner17masterarbeit.pdf
use super::{Outcome};
use std::collections::HashSet;
use crate::{dot::*, optimization};
use libfirm_rs::{Graph, nodes::Node};
use libfirm_rs::nodes::NodeTrait;

pub struct JumpThreading {
    graph: Graph
}

impl optimization::Local for JumpThreading {
    fn optimize_function(graph: Graph) -> Outcome {
        JumpThreading::new(graph).run()
    }
}

impl JumpThreading {
    fn new(graph: Graph) -> Self {
        graph.assure_outs();
        Self { graph }
    }

    fn run(&mut self) -> Outcome {
        // TODO: merging with the endblock might not be allowed
        let mut worklist = vec![Node::Block(self.graph.end_block())];
        let mut visited = HashSet::new();

        while let Some(current) = worklist.pop() {
            if visited.contains(&current) {
                breakpoint!("Jump Threading: break", self.graph, &|node: &Node| {
                    let mut label = default_label(node);

                    if node == &current {
                        label = label
                            .style(Style::Filled)
                            .fillcolor(X11Color::Yellow)
                            .fontcolor(X11Color::White);
                    }

                    label
                });
                continue;
            }

            breakpoint!("Jump Threading: iteration", self.graph, &|node: &Node| {
                let mut label = default_label(node);

                if node == &current {
                    label = label
                        .style(Style::Filled)
                        .fillcolor(X11Color::Blue)
                        .fontcolor(X11Color::White);
                }

                label
            });

            match current {
                Node::Jmp(jmp) => {
                    breakpoint!("Jump Threading: is jump!", self.graph, &|node: &Node| {
                        let mut label = default_label(node);

                        if node == &current {
                            label = label
                                .style(Style::Filled)
                                .fillcolor(X11Color::Pink)
                                .fontcolor(X11Color::White);
                        }

                        label
                    });

                    let block = current.block();

                    if block.out_nodes().len() == 1 {
                        // the block contains only a single out node,
                        // which we know is the Jmp Node, so the block
                        // can be removed
                        breakpoint!("Block can be removed!", self.graph, &|node: &Node| {
                            let mut label = default_label(node);

                            if node == &current || node == &Node::Block(block) {
                                label = label
                                    .style(Style::Filled)
                                    .fillcolor(X11Color::Orange)
                                    .fontcolor(X11Color::White);
                            }

                            label
                        });
                    }

                    worklist.push(Node::Block(block));
                }
                Node::Block(block) => {
                    log::debug!("cfg preds: {:?}", block.cfg_preds().collect::<Vec<_>>());
                    // add all nodes that jump to the current block
                    // to the work list
                    worklist.extend(block.cfg_preds());
                }
                _ => {
                    // we don't care about this node, walk over it,
                    // maybe the next block is a block with a single
                    // jump.
                    worklist.push(Node::Block(current.block()));
                }
                //Cond(cond) => {
                //}
                //Proj(_, Cond_Val(val, cond)) => {
                    //if self.lookup(cond.into()).value.is_bool_val(!val) {
                        //reachable = false;
                    //}
                //}
            }

            visited.insert(current);
        }

        Outcome::Unchanged
    }
}
