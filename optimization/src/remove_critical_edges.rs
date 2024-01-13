//! Optimization that removes all critical edges from a firm graph.
//!
//! A critical edge is an edge which is neither
//!
//! - the only edge leaving its source block, nor
//! - the only edge entering its destination block.
//!
//! These edges must be split: a new block must be created in the middle of the
//! edge, in order to insert computations on the edge without affecting any
//! other edges.
use super::Outcome;
use crate::{dot::*, optimization};
use libfirm_rs::{
    nodes::{self, Node, NodeTrait, ProjKind},
    Graph,
};

pub struct RemoveCriticalEdges {
    graph: Graph,
    changed: Outcome,
}

impl optimization::Local for RemoveCriticalEdges {
    fn optimize_function(graph: Graph) -> Outcome {
        Self::new(graph).run()
    }
}

impl RemoveCriticalEdges {
    fn new(graph: Graph) -> Self {
        Self {
            graph,
            changed: Outcome::Unchanged,
        }
    }

    /// Ensure that all edges leading into `block` are not critical edges
    fn visit_block(&mut self, block: nodes::Block) {
        breakpoint!(
            &format!("Remove Critical Edge: Visiting {:?}", block),
            self.graph,
            &|node: &Node| {
                let mut label = default_label(node);

                if let Node::Block(rendered_block) = node {
                    label = label
                        .style(Style::Filled)
                        .fillcolor(if rendered_block == &block {
                            X11Color::Red
                        } else {
                            X11Color::Orange
                        })
                        .fontcolor(X11Color::White);
                }

                label
            }
        );

        if block.graph().end_block() == block || block.cfg_preds().len() <= 1 {
            return;
        }

        // we modify the in_nodes while iterating over them, but we
        // are only modifying the contents of indices already read,
        // so this is safe without collecting into a Vec first
        for (pred_index, pred) in block.in_nodes().enumerate() {
            // we already know that the edge defined by (pred, block[pred_index]) is
            // not the only edge entering the destination block. See check a few
            // lines above.
            //
            // This means we have a critical edge if the block of the `pred` has more than
            // one edge leaving the block. Since each basic block can have at
            // most one predicate that is used to jump to another block, we only
            // have to check if the `pred` is a jump node with multiple out edges.
            //
            // These nodes are marked by the flag "forking" in libfirm. All notes
            // with this flag are: Cond, Switch, IJmp.
            //
            // => if the pred is a Cond,Switch or IJmp, we might have a critical edge,
            // => insert an empty basic block
            match pred {
                Node::Proj(_proj, ProjKind::Cond_Val(_arm, _cond)) => {
                    self.new_block_on_edge(pred, pred_index, block)
                }
                node if Node::is_bad(node) => continue,
                Node::Jmp(..) => continue,
                _ => {
                    unreachable!();
                }
            }
        }
    }

    /// Adds an empty basic block on the given edge
    ///
    /// ```text
    ///   in_node                          in_node
    ///   (e.g. IJmp)                        |
    ///      |             ==========>     Block
    ///  in[input_idx]                       |
    ///      |                              Jmp
    ///      v                               |
    ///  target_block                   in[input_idx]
    ///                                      |
    ///                                      v
    ///                                 target_block
    /// ```
    fn new_block_on_edge(&mut self, in_node: Node, input_idx: usize, target_block: nodes::Block) {
        breakpoint!(
            &format!(
                "Remove Critical Edge: Possibly critical edge from {:?} to {:?}",
                in_node, target_block
            ),
            self.graph,
            &|node: &Node| {
                let mut label = default_label(node);

                if node == &in_node || node == &Node::Block(target_block) {
                    label = label
                        .style(Style::Filled)
                        .fillcolor(X11Color::Blue)
                        .fontcolor(X11Color::White);
                }

                label
            }
        );

        let new_block = self.graph.new_block(&[in_node]);
        let jmp = new_block.new_jmp();

        target_block.set_input_at(input_idx as i32, Node::Jmp(jmp));

        self.changed = Outcome::Changed;

        breakpoint!(
            &format!("Remove Critical Edge: Inserted Empty Block {:?}", new_block),
            self.graph,
            &|node: &Node| {
                let mut label = default_label(node);

                if node == &in_node || node == &Node::Block(target_block) {
                    label = label
                        .style(Style::Filled)
                        .fillcolor(X11Color::Blue)
                        .fontcolor(X11Color::White);
                } else if node == &Node::Block(new_block) || node == &Node::Jmp(jmp) {
                    label = label
                        .style(Style::Filled)
                        .fillcolor(X11Color::Pink)
                        .fontcolor(X11Color::White);
                }

                label
            }
        );
    }

    fn run(&mut self) -> Outcome {
        // modifying the graph during the walk works since:
        // - we walk from start to end block.
        // - During this process we only insert blocks into a predecessor edge that is
        //   currently visited.
        // => Which means that only already visited control flow edges are modified.
        self.graph.walk_blocks_postorder(|block| {
            self.visit_block(*block);
        });

        self.changed
    }
}
