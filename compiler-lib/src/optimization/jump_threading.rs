//! Replaces chains of blocks that only contain a jump with a single
//! jump. That's more like a control flow optimization than jump threading.
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
    nodes::{self, Node, NodeTrait, ProjKind},
    Graph,
};
use std::collections::{VecDeque, HashSet};

pub struct JumpThreading {
    graph: Graph,
    worklist: VecDeque<Node>,
    visited: HashSet<Node>,
    num_eliminated: u32
}

impl optimization::Local for JumpThreading {
    fn optimize_function(graph: Graph) -> Outcome {
        JumpThreading::new(graph).run()
    }
}

impl JumpThreading {
    fn new(graph: Graph) -> Self {
        graph.assure_outs();
        Self { graph ,
            worklist: VecDeque::new(),
            visited: HashSet::new(),
            num_eliminated: 0
        }
    }

    fn run(&mut self) -> Outcome {
        self.worklist.push_back(Node::Block(self.graph.end_block()));

        while let Some(current) = self.worklist.pop_front() {
            if self.visited.contains(&current) {
                breakpoint!(
                    &format!(
                        "Jump Threading: stopping recursion on second visit of {:?}",
                        current
                    ),
                    self.graph,
                    &|node: &Node| {
                        let mut label = default_label(node);

                        if node == &current {
                            label = label
                                .style(Style::Filled)
                                .fillcolor(X11Color::Yellow)
                                .fontcolor(X11Color::White);
                        }

                        label
                    }
                );
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

            self.visit_node(current);
            self.visited.insert(current);
        }

        if self.num_eliminated > 0 {
            Outcome::Changed
        } else {
            Outcome::Unchanged
        }
    }

    fn visit_node(&mut self, current: Node) {
        match current {
            Node::Block(block) => {
                // we are only interested in nodes that can jump from a basic block to another basic
                // block. More specifically: Projs of a Cond and Jmps.
                self.worklist.extend(block.cfg_preds());
            }
            Node::Proj(..) => {
                self.visit_proj(current);
            }
            _ => {
                // we are only interested in nodes that can jump from a basic block to another basic
                // block, skip all contents of the current block, go directly to the block node
                self.worklist.push_back(Node::Block(current.block()));
            }
        }
    }

    /// Try to replace an unnecessary conditional jump with an
    /// unconditional jump. A conditional jump is unnecessary if:
    ///
    /// (1) true projection and false projection jump to the same
    ///     target block [using input array element `i` and `j`].
    /// (2) For all phi nodes in the target block, the `i`-th and `j`-th
    ///     value have to be identical
    /// (3) The true and false projection have to be the successors
    ///     of the same cond node. This is guaranteed in our implementation
    ///     since we do not use switch nodes.
    ///
    ///
    /// This is a sketch of the structure we are trying to match:
    ///
    /// ```
    ///     Predecessor Block
    ///      |           |
    ///     Cond       Other Nodes are
    ///    /    \      possible/allowed
    /// Proj   Proj
    /// True   False 
    ///   |     |
    /// in[i]   in[j]
    ///   |     |
    /// Current Block --- Other Nodes are possible/allowed
    ///  |      |
    ///  Phi 0  Phi N # forall phi in Current Block: phi[i] == phi[j]
    /// ```
    ///
    /// To replace a conditional jump with an unconditional jump, we:
    ///
    /// (1) Reuse the `i`-th element of the input array and phi nodes
    ///     for the jump node. There is no need to adapt the phi nodes
    ///     since the value at `i` is still correct.
    /// (2) Remove index `j` from the input array and all phi nodes.
    ///     Since `j` is not guranteed to be the last input element,
    ///     this means we might have to "compact" the arrays by moving
    ///     indices `x > j` to `x - 1`.
    fn visit_proj(&mut self, current: Node) {
        if let Some(unnecessary_cond) = self.match_unnecessary_cond(current) {
        } else {
            // we are only interested in nodes that can jump from a basic block to another basic
            // block, skip all contents of the current block, go directly to the block node
            self.worklist.push_back(Node::Block(current.block()));
        }
    }
    
    fn match_unnecessary_cond(&mut self, current: Node) -> Option<UnnecessaryCond> {
        let (arm, cond) = if let Node::Proj(_proj, ProjKind::Cond_Val(arm, cond)) = current {
            (arm, cond)
        } else {
            log::debug!("skipping {:?} which is not a projection of a conditional", current);
             return None;
        };

        self.graph.assure_outs();

        let (self_proj, target_block) = if let Some(out) = cond.out_proj_target_block(arm) {
            out
        } else {
            log::debug!("skipping {:?} with missing target for '{}' proj", cond, arm);
             return None;
        };

        let (other_proj, other_target_block) = if let Some(out) = cond.out_proj_target_block(!arm) {
            out
        } else {
            log::debug!("skipping {:?} with missing target for '{}' proj", cond, !arm);
             return None;
        };

        if target_block != other_target_block {
            log::debug!("skipping {:?} with projections {:?} and {:?} since they have different target blocks ({:?}, resp. {:?})", cond, self_proj, other_proj, target_block, other_target_block);
             return None;
        }

        let index_self_proj = target_block.in_nodes().position(|node| node == current).unwrap();
        let index_other_proj = target_block.in_nodes().position(|node| match node {
                                                                    Node::Proj(proj_node, _) =>  proj_node == other_proj,
                                                                    _ => false
                                                                }).unwrap();

        for phi in target_block.phis() {
            let phi_preds = phi.phi_preds().collect::<Vec<_>>();
            if phi_preds[index_self_proj] != phi_preds[index_other_proj] {
                log::debug!("skipping {:?} since {:?} has different definitions for path through {:?} (definition at {:?}) and path through {:?} (definition at {:?})", cond, phi, self_proj, phi_preds[index_self_proj], other_proj, phi_preds[index_other_proj]);
                 return None;
            }
        }

        breakpoint!(&format!("Jump Threading: detected unnecessary cond {:?}", cond), self.graph, &|rendered: &Node| {
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
                Node::Block(block) if block == &target_block => {
                    label = label
                        .style(Style::Filled)
                        .fillcolor(X11Color::Pink)
                        .fontcolor(X11Color::White);
                }
                _ => {}
            }

            label
        });

        Some(UnnecessaryCond {
            proj_current: self_proj,
            proj_other: other_proj,
            cond,
            target_block
        })
    }
}

struct UnnecessaryCond {
    cond: nodes::Cond,
    proj_current: nodes::Proj,
    proj_other: nodes::Proj,
    target_block: nodes::Block,
}
