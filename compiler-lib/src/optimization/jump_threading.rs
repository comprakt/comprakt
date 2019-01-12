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
            Node::Jmp(jmp) => {
                self.visit_jmp(jmp);
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
    fn visit_proj(&mut self, current: Node) {
        if let Some(unnecessary_cond) = self.match_unnecessary_cond(current) {
            // To replace a conditional jump with an unconditional jump, we:
            //
            // (1) Reuse the `i`-th element of the input array and phi nodes
            //     for the jump node. There is no need to adapt the phi nodes
            //     since the value at `i` is still correct.
            // (2) Remove index `j` from the input array and all phi nodes.
            //     (Since `j` is not guranteed to be the last input element,
            //     this means we have to "compact" the arrays by moving
            //     indices `x > j` to `x - 1`, but thats done by the vector
            //     implementation for us.)
            let jmp = unnecessary_cond.cond.block().new_jmp();
            let mut target_block_preds = unnecessary_cond.target_block.in_nodes().collect::<Vec<_>>();
            target_block_preds[unnecessary_cond.proj_current.0] = Node::Jmp(jmp);

            // remove index of second projection, called step (2) above
            let idx = unnecessary_cond.proj_other.0;
            for phi in unnecessary_cond.target_block.phis() {
                let mut phi_preds = phi.phi_preds().collect::<Vec<_>>();
                phi_preds.remove(idx);
                phi.set_in_nodes(&phi_preds);
            }

            target_block_preds.remove(idx);
            unnecessary_cond.target_block.set_in_nodes(&target_block_preds);
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
            proj_current: (index_self_proj, self_proj),
            proj_other: (index_other_proj, other_proj),
            cond,
            target_block
        })
    }

    /// Try to merge the current block with a predecessor block.
    ///
    /// We consider two cases in which we try to merge blocks:
    ///
    /// (1) A predecessor with a single input, (and a current node with one or more inputs)
    ///
    ///     ```
    ///     in[1]                                     in[2]    in[n]
    ///        \                                       |        |
    ///        Predecessor Block ----> Jmp --in[1]--> Current Block
    ///        /             |                             |
    ///      Other nodes     |                            Other Nodes including Phis
    ///      No Mem edges/   given that there is          allowed
    ///      side effects    only one predecessor
    ///      allowed!        Phis can be trivially
    ///                      removed, but should
    ///                      not exist
    ///     ```
    ///
    ///     This case should be inlined with a heuristic since the backend will most likely
    ///     represent the Jmp using a fall through as shown below:
    ///
    ///     ```
    ///     Predecessor_Block_Label:
    ///         instructions of predecessor block
    ///         /* fall through */
    ///         /* jmp to Current_Block_Label optimized away */
    ///     Current_Block_Label:
    ///         instructions of current block
    ///     ```
    ///
    ///     If blocks are not merged, the inputs of current
    ///     block do not have to execute the nodes in the predecessor block since they can jump
    ///     directly to the current block instead of the merged block which will always execute
    ///     both.
    ///     
    ///     => Our heuristic: only merge if no nodes are in the predecessor block.
    ///
    /// (2) A predecessor with multiple inputs, and a current node with a single input.
    ///
    ///     ```
    ///     in[1]      in[2]
    ///        \        |
    ///        Predecessor Block ----> Jmp --in[1]--> Current Block
    ///        /                                        |
    ///      Other Nodes including                    No or trivial Phis,
    ///      phis, mem edges allowed                  Nodes, including mem edges
    ///     ```
    ///     [This is the case covered by line 522 calling `try_merge_blocks` in libfirm]
    /// 
    /// Both cases combined include the most important case of a chain of empty blocks as a special
    /// case.
    ///
    /// This optimization assumes that bads were removed previously. There is no detection of Bads,
    /// so a node with 2 Bad predecessors and a non Bad predecessor is not optimized even though it
    /// has only one real predecessor.
    fn visit_jmp(&mut self, current: nodes::Jmp) {
        if let Some(_) = self.try_remove_unnecessary_predecessor_block(current) {

        } else {
            // we are only interested in nodes that can jump from a basic block to another basic
            // block, skip all contents of the current block, go directly to the block node
            self.worklist.push_back(Node::Block(current.block()));
        }
    }

    /// detect an unnecessary predecessor that jumps into the current block and
    /// remove it.
    ///
    /// This is `case 1` in the description of [`visit_jmp`].
    ///
    /// ```
    /// in[1]                                     in[2]    in[n]
    ///    \                                       |        |
    ///    Predecessor Block ----> Jmp --in[1]--> Current Block
    ///    /             |                             |
    ///  Other nodes     |                            Other Nodes including Phis
    ///  No Mem edges/   given that there is          allowed
    ///  side effects    only one predecessor
    ///  allowed!        Phis can be trivially
    ///                  removed, but should
    ///                  not exist
    /// ```
    fn try_remove_unnecessary_predecessor_block(&mut self, jmp_inbetween: nodes::Jmp) -> Option<()> {
        // test if the block qualifies for this optimization
        let pred_block = jmp_inbetween.block();

        if JumpThreading::block_contains_nodes(&pred_block) ||
            pred_block.in_nodes().len() != 1
        {
            return None;
        }

        // get the only predecessor
        let pred_pred = pred_block.in_nodes().next().unwrap();

        self.graph.assure_outs();

        let target_block = if let Some(target_block) = jmp_inbetween.out_target_block() {
            target_block
        } else {
            log::debug!("cannot optimize jump since it does not have a target block");
            return None;
        };

        // we now know it qualifies, optimize the predecessor block
        // by rewiring the jmp.
        
        let mut target_block_preds = target_block.in_nodes().collect::<Vec<_>>();
        let jmp_index = target_block_preds.iter().position(|node| node == &Node::Jmp(jmp_inbetween)).unwrap();

        target_block_preds[jmp_index] = pred_pred;

        target_block.set_in_nodes(&target_block_preds);
        pred_block.set_in_nodes(&[]);

        // since the target block changed, reprocess it
        self.visited.remove(&Node::Block(target_block));
        for pred in target_block.in_nodes() {
            self.visited.remove(&pred);
        }
        self.worklist.push_back(Node::Block(target_block));

        Some(())
    }

    /// detect an unnecessary current block that can be merged into the predecessor.
    /// This is `case 2` in the description of [`visit_jmp`]:
    ///
    /// ```
    /// in[1]      in[2]
    ///    \        |
    ///    Predecessor Block ----> Jmp --in[1]--> Current Block
    ///    /                                        |
    ///  Other Nodes including                    No or trivial Phis,
    ///  phis, mem edges allowed                  Nodes, including mem edges
    /// ```
    /// [This is the case covered by line 522 calling `try_merge_blocks` in libfirm]
    ///
    fn match_unnecessary_current_block(&mut self, jmp_inbetween: nodes::Jmp) -> Option<()> {
        None
    }

    // TODO: this can be moved to the libfirm-rs API
    fn block_contains_nodes(block :&nodes::Block) -> bool {
        // TODO: you can be way smarter here:
        // - single predecessor blocks, with Phi nodes can optimize
        //   Phi nodes away
        // - bad nodes in a block should not be counted
        block.out_nodes().len() > 1
    }
}

struct UnnecessaryCond {
    cond: nodes::Cond,
    proj_current: (usize, nodes::Proj),
    proj_other: (usize, nodes::Proj),
    target_block: nodes::Block,
}
