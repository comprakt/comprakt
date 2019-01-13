//! Implements a control flow optimization that replaces some patterns of
//! unnecessary conditional and unconditional jumps.

// TODO: End and Start block are currently checked implicitly, add explicit
// checks

// TODO: The minimal graph built by this opt is a `Start -> Block -> Jmp ->
// End`, `Start -> Jmp -> End` is most likely allowed

// TODO: recursion is suboptimal
use super::Outcome;
use crate::{dot::*, optimization};
use libfirm_rs::{
    nodes::{self, Node, NodeTrait, ProjKind},
    Graph,
};
use std::collections::{HashSet, VecDeque};

pub struct ControlFlow {
    graph: Graph,
    /// a list of blocks that should be processed next. An item on this list
    /// should not be part of `done` unless there is a loop in the graph!
    worklist: VecDeque<nodes::Block>,
    /// marks a block as optimized in its current state, if the inputs of a
    /// block are changed, this flag has to be removed.
    done: HashSet<nodes::Block>,
    num_changed: u32,
}

impl optimization::Local for ControlFlow {
    fn optimize_function(graph: Graph) -> Outcome {
        ControlFlow::new(graph).run()
    }
}

impl ControlFlow {
    fn new(graph: Graph) -> Self {
        graph.assure_outs();
        Self {
            graph,
            worklist: VecDeque::new(),
            done: HashSet::new(),
            num_changed: 0,
        }
    }

    fn run(&mut self) -> Outcome {
        let end_block = self.graph.end_block();
        self.worklist.push_back(end_block);

        while let Some(current_block) = self.worklist.pop_front() {
            if self.done.contains(&current_block) {
                breakpoint!(
                    &format!(
                        "Jump Threading: stopping recursion on unscheduled revisit of {:?} (loop)",
                        current_block
                    ),
                    self.graph,
                    &|node: &Node| {
                        let mut label = default_label(node);

                        if node == &Node::Block(current_block) {
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

            breakpoint!(
                &format!("Jump Threading: optimizing {:?}", current_block),
                self.graph,
                &|node: &Node| {
                    let mut label = default_label(node);

                    if node == &Node::Block(current_block) {
                        label = label
                            .style(Style::Filled)
                            .fillcolor(X11Color::Blue)
                            .fontcolor(X11Color::White);
                    }

                    label
                }
            );

            // collecting is important since we edit the predecessors during iteration!!!
            for pred in current_block.cfg_preds().collect::<Vec<_>>() {
                breakpoint!(
                    &format!("Jump Threading: {:?} predecessor {:?}", current_block, pred),
                    self.graph,
                    &|node: &Node| {
                        let mut label = default_label(node);

                        if node == &Node::Block(current_block) || node == &pred {
                            label = label
                                .style(Style::Filled)
                                .fillcolor(X11Color::Pink)
                                .fontcolor(X11Color::White);
                        }

                        label
                    }
                );

                match pred {
                    Node::Proj(..) => {
                        if self.visit_proj(pred) {
                            // optimization was applied
                            // => num preds reduced by 1, new jmp inserted
                            // => new jmp optimization may be possible
                            // => reschedule block
                            self.mark_block_changed(&current_block);
                            // we removed another predecessor that is coming
                            // up in subsequent iterations (the other proj belonging
                            // to the cond just removed). We could be more efficient
                            // here and just skip this predecessor in a later iteration.
                            break;
                        } else {
                            self.worklist.push_back(pred.block());
                        }
                    }
                    Node::Jmp(jmp) => {
                        if self.visit_jmp(jmp) {
                            // optimization was applied
                            // => preds of two blocks were combined in a
                            //    single block
                            // => new Cond optimization may be possible
                            // => reschedule block
                            self.mark_block_changed(&current_block);
                        // we can continue our predecessor loop since
                        // we did only modify the now finished index.
                        // No `continue` necessary here.
                        } else {
                            self.worklist.push_back(pred.block());
                        }
                    }
                    _ => {
                        // Something we cannot handle, e.g. a Switch node. Just
                        // ignore it
                        self.worklist.push_back(pred.block());
                    }
                }
            }
        }

        //
        if self.num_changed > 0 {
            //self.graph.remove_unreachable_code();
            self.graph.remove_bads();
            Outcome::Changed
        } else {
            Outcome::Unchanged
        }
    }

    /// Mark a node as changed. This will put the node back on the worklist --
    /// even if it was visited before -- since new optimizations may be
    /// possible.
    fn mark_block_changed(&mut self, block: &nodes::Block) {
        // Control flow optimization in libfirm is implemented really funnily in libfirm
        // (with a goto and usage of the stack), this is because you have to
        // attack the problem from two sides:
        //
        // - the Cond control flow optimization requires the paths from the current node
        //   towards the end node to be already optimized.  (Since optimization might
        //   merge successors into a single block, therefore proofing that a Cond can be
        //   replaced by a Jmp.)
        // - the Jmp optimization requires the number of predecessors to be already
        //   optimized. (Since the replacement of a Cond with a Jmp reduces the number
        //   of predecessors by 1 and might therefore allow us to merge or remove
        //   blocks.)
        //
        // ---
        // NOTE: Our recursion scheme works for two reasons
        //
        // new control flow optimizations for a block can only be enabled by
        //
        // 1. the reduction of predecessors of a node
        // 2. the removal of nodes from a predecessor block
        //
        // since our control flow optimizations
        //
        // - do not remove nodes from predecessors, (2) is not possible
        // - (1) is possible since we call Cond. However, this also shows that if we
        //   walk from the end block, restarting the Jmp optimization in the target
        //   block of the Cond is sufficient.
        self.num_changed += 1;

        breakpoint!(
            &format!("Jump Threading: {:?} changed, rescheduling", block),
            self.graph,
            &|node: &Node| {
                let mut label = default_label(node);

                if node == &Node::Block(*block) {
                    label = label
                        .style(Style::Filled)
                        .fillcolor(X11Color::Red)
                        .fontcolor(X11Color::White);
                }

                label
            }
        );

        if self.done.contains(&block) {
            self.done.remove(&block);
        }

        if !self.worklist.contains(&block) {
            self.worklist.push_back(*block);
        }
    }

    /// Try to replace an unnecessary conditional jump with an
    /// unconditional jump. A conditional jump is unnecessary if:
    ///
    /// 1. true projection and false projection jump to the same
    ///    target block [using input array element `i` and `j`].
    /// 2. For all phi nodes in the target block, the `i`-th and `j`-th
    ///    value have to be identical
    /// 3. The true and false projection have to be the successors
    ///    of the same cond node. This is guaranteed in our implementation
    ///    since we do not use switch nodes.
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
    fn visit_proj(&mut self, current: Node) -> bool {
        if let Some(unnecessary_cond) = self.match_unnecessary_cond(current) {
            // To replace a conditional jump with an unconditional jump, we:
            //
            // 1. Reuse the `i`-th element of the input array and phi nodes
            //    for the jump node. There is no need to adapt the phi nodes
            //    since the value at `i` is still correct.
            // 2. Remove index `j` from the input array and all phi nodes.
            //    (Since `j` is not guranteed to be the last input element,
            //    this means we have to "compact" the arrays by moving
            //    indices `x > j` to `x - 1`, but thats done by the vector
            //    implementation for us.)
            let jmp = unnecessary_cond.cond.block().new_jmp();
            let mut target_block_preds =
                unnecessary_cond.target_block.in_nodes().collect::<Vec<_>>();
            target_block_preds[unnecessary_cond.proj_current.0] = Node::Jmp(jmp);

            // remove index of second projection, called step (2) above
            let idx = unnecessary_cond.proj_other.0;
            for phi in unnecessary_cond.target_block.phis() {
                let mut phi_preds = phi.phi_preds().collect::<Vec<_>>();
                phi_preds.remove(idx);
                phi.set_in_nodes(&phi_preds);
            }

            target_block_preds.remove(idx);
            unnecessary_cond
                .target_block
                .set_in_nodes(&target_block_preds);

            true
        } else {
            false
        }
    }

    fn match_unnecessary_cond(&mut self, current: Node) -> Option<UnnecessaryCond> {
        let (arm, cond) = if let Node::Proj(_proj, ProjKind::Cond_Val(arm, cond)) = current {
            (arm, cond)
        } else {
            log::debug!(
                "skipping {:?} which is not a projection of a conditional",
                current
            );
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
            log::debug!(
                "skipping {:?} with missing target for '{}' proj",
                cond,
                !arm
            );
            return None;
        };

        if target_block != other_target_block {
            log::debug!(
                "skipping {:?} with projections {:?} and {:?} since they have \
                 different target blocks ({:?}, resp. {:?})",
                cond,
                self_proj,
                other_proj,
                target_block,
                other_target_block
            );
            return None;
        }

        let index_self_proj = target_block
            .in_nodes()
            .position(|node| node == current)
            .unwrap();
        let index_other_proj = target_block
            .in_nodes()
            .position(|node| match node {
                Node::Proj(proj_node, _) => proj_node == other_proj,
                _ => false,
            })
            .unwrap();

        for phi in target_block.phis() {
            let phi_preds = phi.phi_preds().collect::<Vec<_>>();
            if phi_preds[index_self_proj] != phi_preds[index_other_proj] {
                log::debug!(
                    "skipping {:?} since {:?} has different definitions for path \
                     through {:?} (definition at {:?}) and path through {:?} \
                     (definition at {:?})",
                    cond,
                    phi,
                    self_proj,
                    phi_preds[index_self_proj],
                    other_proj,
                    phi_preds[index_other_proj]
                );
                return None;
            }
        }

        breakpoint!(
            &format!("Jump Threading: detected unnecessary cond {:?}", cond),
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
                    Node::Proj(proj, _) if proj == &self_proj || proj == &other_proj => {
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
            }
        );

        Some(UnnecessaryCond {
            proj_current: (index_self_proj, self_proj),
            proj_other: (index_other_proj, other_proj),
            cond,
            target_block,
        })
    }

    /// Try to merge the current block with a predecessor block.
    ///
    /// We consider three cases in which we try to reduce the number of jumps:
    ///
    /// # Case 1
    ///
    /// A predecessor with a single input, (and a current node with one or more
    /// inputs)
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
    ///
    /// This case should be inlined with a heuristic since the backend might
    /// represent the Jmp using a fall through as shown below:
    ///
    /// ```
    /// Predecessor_Block_Label:
    ///     instructions of predecessor block
    ///     /* fall through */
    ///     /* jmp to Current_Block_Label optimized away */
    /// Current_Block_Label:
    ///     instructions of current block
    /// ```
    ///
    /// If blocks are not merged, the inputs of current
    /// block do not have to execute the nodes in the predecessor block since
    /// they can jump directly to the current block instead of the merged
    /// block which will always execute both.
    ///
    /// => Our heuristic: only merge if no nodes are in the predecessor block.
    ///
    /// # Case 2
    ///
    /// A predecessor with multiple inputs and an empty body, and a current node
    /// with mutiple inputs
    ///
    /// In this case, the predecessor can be merged into the current node by
    /// just appending the input vectors. The phi nodes can just replicate
    /// the input of the connecting Jmp for all inputs that previously
    /// belonged to the predecessor.
    ///
    /// Since our heurisitc for case (1) is to only merge when the predecessor
    /// block is empty. Case (1) is fully contained within this case.
    ///
    /// # Case 3
    /// A predecessor with multiple inputs, and a current node with a single
    /// input.
    ///
    /// ```
    /// in[1]      in[2]
    ///    \        |
    ///    Predecessor Block ----> Jmp --in[1]--> Current Block
    ///    /                                        |
    ///  Other Nodes including                    No or trivial Phis,
    ///  phis, mem edges allowed                  Nodes, including mem edges
    /// ```
    /// [This is the case covered by line 522 calling `try_merge_blocks` in
    /// libfirm]
    ///
    /// ---
    ///
    /// This optimization assumes that bads were removed previously. There is no
    /// detection of Bads, so a node with Bad predecessors and a non Bad
    /// predecessor is not optimized even though it has only one real
    /// predecessor.
    fn visit_jmp(&mut self, current: nodes::Jmp) -> bool {
        self.try_remove_unnecessary_predecessor_block(current)
            || self.try_merge_unnecessary_current_into_predecessor(current)
    }

    /// detect an unnecessary predecessor that jumps into the current block and
    /// remove it.
    ///
    /// This is `case 2` in the description of [`visit_jmp`].
    ///
    /// ```
    /// in[1]      in[2]                          in[2]    in[n]
    ///    \        |                              |        |
    ///    Predecessor Block ----> Jmp --in[1]--> Current Block
    ///    /                                           |
    ///  No other nodes                               Other Nodes including Phis
    ///                                               allowed
    /// ```
    fn try_remove_unnecessary_predecessor_block(&mut self, jmp_inbetween: nodes::Jmp) -> bool {
        // test if the block qualifies for this optimization
        let pred_block = jmp_inbetween.block();

        if self.block_contains_nodes(&pred_block) {
            return false;
        }

        let pred_preds = pred_block.in_nodes().collect::<Vec<_>>();

        self.graph.assure_outs();

        let target_block = if let Some(target_block) = jmp_inbetween.out_target_block() {
            target_block
        } else {
            log::debug!("cannot optimize jump since it does not have a target block");
            return false;
        };

        breakpoint!(
            &format!("Jump Threading: Optimizable {:?}", jmp_inbetween),
            self.graph,
            &|node: &Node| {
                let mut label = default_label(node);

                if node == &Node::Jmp(jmp_inbetween) {
                    label = label
                        .style(Style::Filled)
                        .fillcolor(X11Color::Green)
                        .fontcolor(X11Color::White);
                }

                label
            }
        );

        // we now know it qualifies, optimize the predecessor block
        // by rewiring all predecessors of the predecessor directly
        // to the current block.
        let target_block_preds = target_block.in_nodes().collect::<Vec<_>>();
        let jmp_index = target_block_preds
            .iter()
            .position(|node| node == &Node::Jmp(jmp_inbetween))
            .unwrap();

        let mut new_target_block_preds = target_block_preds.clone();
        new_target_block_preds.remove(jmp_index);
        new_target_block_preds.extend(pred_preds.iter());

        // repeat the index of the jmp inbetween for each new input
        for phi in target_block.phis() {
            let mut phi_preds = phi.phi_preds().collect::<Vec<_>>();
            let phi_for_jmp = phi_preds.remove(jmp_index);
            phi_preds.extend(std::iter::repeat(phi_for_jmp).take(pred_preds.len()));
            phi.set_in_nodes(&phi_preds);
        }

        target_block.set_in_nodes(&new_target_block_preds);
        pred_block.set_in_nodes(&[]);

        true
    }

    /// detect an unnecessary current block that can be merged into the
    /// predecessor. This is `case 3` in the description of [`visit_jmp`]:
    ///
    /// ```
    /// in[1]      in[2]
    ///    \        |
    ///    Predecessor Block ----> Jmp --in[1]--> Current Block
    ///    /                                        |
    ///  Other Nodes including                    No or trivial Phis,
    ///  phis, mem edges allowed                  Nodes, including mem edges
    /// ```
    ///
    /// [This is the case covered by line 522 calling `try_merge_blocks` in
    /// libfirm]
    fn try_merge_unnecessary_current_into_predecessor(
        &mut self,
        jmp_inbetween: nodes::Jmp,
    ) -> bool {
        false
    }

    // TODO: this can be moved to the libfirm-rs API
    fn block_contains_nodes(&self, block: &nodes::Block) -> bool {
        self.graph.assure_outs();
        // TODO: you can be way smarter here:
        // - single predecessor blocks, with Phi nodes can optimize Phi nodes away
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
