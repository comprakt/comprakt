//! Optimization that removes all critical edges from a firm
//! graph.
use super::Outcome;
use crate::{dot::*, optimization};
use libfirm_rs::{
    nodes::{self, Node, NodeTrait, ProjKind},
    Graph,
};

pub struct RemoveCriticalEdges {
    graph: Graph,
}

impl optimization::Local for RemoveCriticalEdges {
    fn optimize_function(graph: Graph) -> Outcome {
        RemoveCriticalEdges::new(graph).run()
    }
}

impl RemoveCriticalEdges {
    fn new(graph: Graph) -> Self {
        Self { graph }
    }

    fn visit_block(&self, block: nodes::Block) -> bool {
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

        false
    }

    fn run(&mut self) -> Outcome {
        let mut changed = Outcome::Unchanged;

        // this is a valid recursion since we walk from start to end block.  During this
        // process we only insert blocks into a predecessor edge that was
        // already visited. (Which means that only already visited control flow
        // is modified.)
        self.graph.walk_blocks_postorder(|block| {
            if self.visit_block(*block) {
                changed = Outcome::Changed;
            }
        });

        changed
    }
}
