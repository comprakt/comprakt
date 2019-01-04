//! Replaces chains of blocks that only contain a jump with a single
//! jump.
//!
//! "Compilers have been implementing Jump Threading for a long time, and
//! little scientific attention has been devoted to it because it is a
//! seemingly basic optimization. However, what compiler authors mean by Jump
//! Threading differs wildly between compilers."
//!
//! We implement two very basic substitutions here. Removal of unnecessary
//! conditional jumps:
//!
//! ```
//!       Block A
//!         |
//!        Cond                   Block A
//!       /    \                     |
//!    Proj   Proj      -->         Jmp
//!    True   False                  |
//!       \    /                  Block B
//!       Block B
//! ```
//!
//!
//! Removal of unnecessary unconditional jumps:
//!
//! ```
//!        Block A
//!        |
//!        Jmp                    Block A
//!        |                         |
//!        Block B      -->         Jmp
//!        |                         |
//!        Jmp                    Block C
//!        |
//!        Block C
//! ```
//!
//! # References
//!
//! - Blog entry "Jump Threading" by Andreas Zwickau:
//!
//!   http://beza1e1.tuxen.de/articles/jump_threading.html
//!
//! - Master thesis "Generalized Jump Threading in LibFirm" by Joachim Priesner
//!
//!   https://pp.ipd.kit.edu/uploads/publikationen/priesner17masterarbeit.pdf
use super::Outcome;
use crate::{dot::*, optimization};
use libfirm_rs::{
    nodes::{Node, NodeTrait, ProjKind},
    Graph,
};
use std::collections::HashSet;

pub struct JumpThreading {
    graph: Graph,
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
        let mut worklist = vec![Node::Block(self.graph.end_block())];
        let mut visited = HashSet::new();
        let mut num_eliminated = 0;

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
                    // In theory, we have to recompute the outs after each
                    // rewiring. But given the access pattern in the code
                    // below, recomputing once per iteration is sufficient.
                    //
                    // Note that this is necessary for the correctness of
                    // - block.out_nodes()
                    // - jmp.out_target_block()
                    self.graph.assure_outs();

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
                    let block_node = Node::Block(block);

                    if block.out_nodes().len() == 1 {
                        // the block contains only a single node,
                        // which we know is the Jmp Node, so the block
                        // can be removed
                        breakpoint!("Block can be removed!", self.graph, &|node: &Node| {
                            let mut label = default_label(node);

                            if node == &current || node == &block_node {
                                label = label
                                    .style(Style::Filled)
                                    .fillcolor(X11Color::Orange)
                                    .fontcolor(X11Color::White);
                            }

                            label
                        });
                    }

                    if let Some(target_block) = jmp.out_target_block() {
                        let mut new_inputs = target_block
                            .in_nodes()
                            .filter(|pred| {
                                // remove the unecessary jump from the input edges of the target
                                // block
                                pred != &Node::Jmp(jmp)
                            })
                            .collect::<Vec<_>>();

                        // we can only deal with some predecessors, only change
                        // the graph when we know how to deal with every predecessor
                        if block.cfg_preds().all(|node| match node {
                            Node::Jmp(_) | Node::Proj(_, _) => true,
                            unsupported => {
                                log::debug!(
                                    "skipping jump threading opportunity \
                                     because of unsupported predecessor variant {:?}",
                                    unsupported
                                );
                                false
                            }
                        }) {
                            // rewire the block and add its children to
                            // the list of nodes that should be visited
                            for node in block.cfg_preds() {
                                match node {
                                    Node::Jmp(pred_jmp) => {
                                        log::debug!(
                                            "Rewiring {:?} to directly point to {:?}",
                                            pred_jmp,
                                            target_block
                                        );

                                        new_inputs.push(node);
                                        target_block.set_in_nodes(&new_inputs);

                                        breakpoint!(
                                            &format!(
                                                "Rewiring {:?} to directly point to {:?}",
                                                pred_jmp, target_block
                                            ),
                                            self.graph,
                                            &|rendered: &Node| {
                                                let mut label = default_label(rendered);

                                                if rendered == &node
                                                    || rendered == &Node::Block(target_block)
                                                {
                                                    label = label
                                                        .style(Style::Filled)
                                                        .fillcolor(X11Color::Brown)
                                                        .fontcolor(X11Color::White);
                                                }

                                                label
                                            }
                                        );
                                    }
                                    Node::Proj(proj, _kind) => {
                                        log::debug!(
                                            "Rewiring {:?} to directly point to {:?}",
                                            proj,
                                            target_block
                                        );

                                        new_inputs.push(node);
                                        target_block.set_in_nodes(&new_inputs);

                                        breakpoint!(
                                            &format!(
                                                "Rewiring {:?} to directly point to {:?}",
                                                proj, target_block
                                            ),
                                            self.graph,
                                            &|rendered: &Node| {
                                                let mut label = default_label(rendered);

                                                if rendered == &node
                                                    || rendered == &Node::Block(target_block)
                                                {
                                                    label = label
                                                        .style(Style::Filled)
                                                        .fillcolor(X11Color::Brown)
                                                        .fontcolor(X11Color::White);
                                                }

                                                label
                                            }
                                        );
                                    }
                                    _ => unreachable!(),
                                }

                                worklist.push(node);
                            }

                            // mark the block as bad, it will be removed
                            // with all nodes it dominates (which is only the
                            // currently observed, unnecessary jmp)
                            // TODO: Is this necessary?
                            self.graph.mark_as_bad(block_node);
                            self.graph.remove_bads();
                            num_eliminated += 1;
                        } else {
                            // queueing the Jmp node failed because of an unknown predecessor,
                            // treat it like all other nodes
                            worklist.push(Node::Block(current.block()));
                        }
                    } else {
                        log::debug!("ignoring reducable jump without a target block");
                    }
                }
                Node::Proj(_proj, ProjKind::Cond_Val(arm, cond)) => {
                    self.graph.assure_outs();
                    match (
                        cond.out_proj_target_block(arm),
                        cond.out_proj_target_block(!arm),
                    ) {
                        (
                            Some((self_proj, target_block)),
                            Some((other_proj, other_target_block)),
                        ) => {
                            if target_block == other_target_block {
                                // we now know, that the conditional jump is useless
                                // as both paths (true and false) target the same block,
                                // remove it!
                                let cond_block = cond.block();
                                let new_jmp = Node::Jmp(cond_block.new_jmp());

                                let mut new_inputs = target_block.in_nodes().collect::<Vec<_>>();
                                new_inputs.push(new_jmp);
                                target_block.set_in_nodes(&new_inputs);

                                breakpoint!(
                                    &format!("Adding {:?} around unnecessary {:?}", new_jmp, cond),
                                    self.graph,
                                    &|rendered: &Node| {
                                        let mut label = default_label(rendered);

                                        match rendered {
                                            Node::Cond(some_cond) if some_cond == &cond => {
                                                label = label
                                                    .style(Style::Filled)
                                                    .fillcolor(X11Color::Orange)
                                                    .fontcolor(X11Color::White);
                                            }
                                            Node::Proj(proj, _)
                                                if proj == &self_proj || proj == &other_proj =>
                                            {
                                                label = label
                                                    .style(Style::Filled)
                                                    .fillcolor(X11Color::Orange)
                                                    .fontcolor(X11Color::White);
                                            }
                                            jmp if jmp == &new_jmp => {
                                                label = label
                                                    .style(Style::Filled)
                                                    .fillcolor(X11Color::Red)
                                                    .fontcolor(X11Color::White);
                                            }
                                            _ => {}
                                        }

                                        label
                                    }
                                );

                                // remove the elements that are marked as bad from the work list,
                                // queue the inserted jump instead
                                worklist = worklist
                                    .into_iter()
                                    .filter(|node| {
                                        if node != &Node::Cond(cond) {
                                            return false;
                                        }

                                        match node {
                                            Node::Proj(proj, _)
                                                if proj == &self_proj || proj == &other_proj =>
                                            {
                                                false
                                            }
                                            _ => true,
                                        }
                                    })
                                    .collect();

                                // mark the cond as bad, it will be removed
                                // with all nodes it dominates (which is exactly the
                                // cond + both CondVal projections)
                                self.graph.mark_as_bad(cond);
                                self.graph.remove_bads();
                                num_eliminated += 1;

                                worklist.push(new_jmp);
                            }
                        }

                        _ => {
                            log::debug!("skipping incomplete conditional structure");
                        }
                    }
                }
                Node::Block(block) => {
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
            }

            visited.insert(current);
        }

        if num_eliminated > 0 {
            Outcome::Changed
        } else {
            Outcome::Unchanged
        }
    }
}
