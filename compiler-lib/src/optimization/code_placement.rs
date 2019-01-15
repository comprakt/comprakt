///! Implements the "Code Placement" Optimization in Libfirm as an improved
///! variant of Loop Invariant Code Motion.
///!
///! TODO: Also perform some global common subexpression elimination by
///! taking this into consideration during optimal position computation.
///!
///!
///! Things to consider:
///! - Libfirm distingiusies between floating and pinned nodes.
///!   - Pinned nodes should not be moved.
///!     - Moving Code above a pinned node is not valid (I think?), e.g.
///!       ```text
///!       Cond[pinned] -> Something Floating
///!       ```
///!       => WRONG! moving above a Cond is allowed since we are in
///!          SSA form! <3
///!   - Some pinned nodes are:
///!     - Control Flow Nodes: Jmp, Cond, Switch, IJmp, Return...
///!     - Start Block Nodes: NoMem, Unknown
///!     - Memory Nodes: Free, Alloc
///!     - Structural Nodes: Block, End, Start, Phi, Anchor
///!     - Confirm, Bad, Dummy, Unknown, Deleted ...
///!     - Nodes that might throw an exception: Div, Mod, Load, Raise, Store,
///!       ASM, Builtin, Call...
///!       [special pin mode is "pinned = exception" instead of just
///!       "pinned = yes"]
///!     - There is also a special Pin Node
///! - Libfirm has "code_placement.c", which subsumes CSE.
///! - According to the Libfirm impl we should remove critical edges first.
///!   Why?
///! - It is dangerous that code could be move into a node not reachable from
///!   start => remove unreachable code first!
///! - Finding the correct block to place the common sub expression is tricky
///!   - the earliest possible place is the dominator of both old positions
///!     "closest to the start node"/furthest dominator
///!   - this is however not what you necessarily want to do, since this
///!     might unnecessarily cause the execution of the moved code. So
///!     moving to the earliest common dominator is the position "closest
///!     to the en node"/most control dependent
///!   - However, this is not the best location either, since this might
///!     be in a loop => the best location is the earliest common dominator
///!     with the least loop nesting.
///! - "Floating nodes form subgraphs that begin at nodes as Const, Load,
///!   Start, Call and that end at pinned nodes as Store, Call."
///! - "[...] we break cycles at pinned nodes which will not move anyway:
///!   This works because in firm each cycle contains a Phi or Block node
///!   (which are pinned)"
///! - Moving assignments out of a if or else branch works, since there
///!   is always just one assignment.
///! Moves nodes into blocks where we suspect they will be executed less often.
///!
///! # How Can We Detect Movable Nodes or Subtrees?
///!
///! # What Is the Correct Recursion Scheme?
///!
///! # Where Should Movable Nodes Be Placed?
use super::Outcome;
use crate::{
    dot::*,
    optimization::{self, Local},
};
use libfirm_rs::{
    bindings,
    nodes::{self, Node, NodeTrait, ProjKind},
    Graph,
};

use std::collections::{HashSet, VecDeque};
pub struct CodePlacement {
    graph: Graph,
}

impl optimization::Local for CodePlacement {
    fn optimize_function(graph: Graph) -> Outcome {
        CodePlacement::new(graph).run()
    }
}

impl CodePlacement {
    fn new(graph: Graph) -> Self {
        graph.assure_outs();
        Self { graph }
    }

    fn all_to_earliest_allowed(&mut self) {}

    fn run(&mut self) -> Outcome {
        EarliestPlacement::optimize_function(self.graph)
        //self.all_to_earliest_allowed();
        //self.find_cheaper_placement();
    }
}

/// You can call this optimization on its own, but that's probably
/// not a good idea given the reasoning above.
pub struct EarliestPlacement {
    graph: Graph,
    worklist: VecDeque<Node>,
    visited: HashSet<Node>,
    num_changed: usize,
}

impl optimization::Local for EarliestPlacement {
    fn optimize_function(graph: Graph) -> Outcome {
        EarliestPlacement::new(graph).run()
    }
}

impl EarliestPlacement {
    fn new(graph: Graph) -> Self {
        graph.assure_outs();
        graph.compute_doms();
        Self {
            graph,
            worklist: VecDeque::new(),
            visited: HashSet::new(),
            num_changed: 0,
        }
    }

    fn run(&mut self) -> Outcome {
        let end_block = self.graph.end_block();
        self.worklist.push_back(Node::Block(end_block));

        while let Some(current_node) = self.worklist.pop_front() {
            self.visit_node(current_node, "worklist");
        }

        if self.num_changed > 0 {
            Outcome::Changed
        } else {
            Outcome::Unchanged
        }
    }

    fn visit_node(&mut self, current_node: Node, debug_context: &str) -> Outcome {
        if self.visited.contains(&current_node) {
            breakpoint!(
                &format!(
                    "Earliest Placement: ignoring second visit of {:?}",
                    current_node
                ),
                self.graph,
                &|node: &Node| label_with_dom_info(self.graph, node, &current_node)
            );
            return Outcome::Unchanged;
        }
        self.visited.insert(current_node);

        breakpoint!(
            &format!("Earliest Placement: {} {:?}", debug_context, current_node),
            self.graph,
            &|node: &Node| label_with_dom_info(self.graph, node, &current_node)
        );

        self.move_to_earliest_valid_block(current_node)
    }

    fn move_to_earliest_valid_block(&mut self, current_node: Node) -> Outcome {
        if current_node.is_pinned() {
            // if we arrive at a pinned node, the movable graph chunk
            // is complete / a new movable graph chunk might begin at each
            // predecessor
            for pred in current_node.in_nodes() {
                self.worklist.push_back(pred);
                if !Node::is_block(pred) {
                    self.worklist.push_back(Node::Block(pred.block()));
                }
            }

            Outcome::Unchanged
        } else {
            // we are within a moveable graph chunk. Yay! To optimize the whole moveable
            // graph chunk, we move each node within the chunk on its own. Since
            // the earliest allowed position of a node depends on the position
            // of its predecessors, we have to move the predecessors of each node
            // first.

            // # Optimize Predecessors

            // node is pinned
            // => node is not a block (since blocks are pinned)
            // => node.block is safe to call
            self.visit_node(
                Node::Block(current_node.block()),
                &format!("block of {:?}", current_node),
            );

            for pred in current_node.in_nodes() {
                self.visit_node(pred, &format!("pred of {:?}", current_node));
            }

            // # Optimize Position of Current Node
            //
            // - Blocks are sorted in a dominance tree from start, where edges run from a
            //   immediate dominator to the dominated block. For example the following
            //   dominance tree belongs to this code snippet:
            //
            // ```text
            // class A {
            //     public static void main(String[] args) {
            //         int x = System.in.read() + 4;
            //         int i = 0;
            //         int a;
            //         while (i < 5) {
            //             a = x * 5;
            //             i = a + i;
            //         }
            //
            //         System.out.write(a);
            //     }
            // }
            // ```
            //
            // ```text
            //       Start Block with System.in.read              [ dom depth = 1 ]
            //                    |
            //          Block with Cmp Loop Header                [ dom depth = 2 ]
            //          /         |             \
            //     Block       Block after      End Block         [ dom depth = 3 ]
            //     loop body   loop with
            //                 System.out.write
            // ```
            // - Blocks with a smaller dominance tree depth are closer to the start block
            // - If we want to move the current node, all its predecessors have to be
            //   already available. This means they must have a dominance tree depth <= the
            //   current node.
            // - We already moved the predecessors to the minimal possible dominance tree
            //   depth by calling this optimization routine on predecessors first (as done
            //   above)
            // - This means the earliest allowed position for the current node is inside the
            //   block of the predecessor with the __maximal__ dominator tree depth. (Since
            //   the predecessor with the maximal dominator tree depth is computed/available
            //   last.)
            // - The predecessor with maximal dominator tree depth is __not__ unique.
            let mut earliest_allowed_block = None;
            let mut earliest_allowed_depth = 0;
            for pred in current_node.in_nodes() {
                let pred_block = pred.block();

                let pred_depth =
                    unsafe { bindings::get_Block_dom_depth(pred_block.internal_ir_node()) };
                if pred_depth > earliest_allowed_depth {
                    earliest_allowed_depth = pred_depth;
                    earliest_allowed_block = Some(pred_block);
                }
            }

            if let Some(block) = earliest_allowed_block {
                if block == current_node.block() {
                    return Outcome::Unchanged;
                }

                breakpoint!(
                    &format!(
                        "Earliest Placement: can move {:?} from {:?} to {:?}",
                        current_node,
                        current_node.block(),
                        block
                    ),
                    self.graph,
                    &|node: &Node| {
                        let mut label = label_with_dom_info(self.graph, node, &current_node);

                        if node == &Node::Block(block) {
                            label = label
                                .add_style(Style::Filled)
                                .fillcolor(X11Color::Green)
                                .fontcolor(X11Color::White);
                        }

                        label
                    }
                );
                current_node.set_block(block);
                self.num_changed += 1;
                Outcome::Changed
            } else {
                Outcome::Unchanged
            }
        }
    }
}

pub fn label_with_dom_info(_graph: Graph, node: &Node, highlight: &Node) -> Label {
    let mut label = dom_info_box(node);

    let highlight_block = if let Node::Block(block) = highlight {
        *block
    } else {
        highlight.block()
    };

    if let Node::Block(node_block) = node {
        // NOTE: block also dominates itself!
        if highlight_block.dominates(*node_block) {
            label = label
                .add_style(Style::Filled)
                .fillcolor(X11Color::Pink)
                .fontcolor(X11Color::White);
        }
    }

    if node == &Node::Block(highlight_block) {
        label = label
            .add_style(Style::Filled)
            .fillcolor(X11Color::Red)
            .fontcolor(X11Color::White);
    }

    if node == highlight {
        label = label
            .add_style(Style::Filled)
            .fillcolor(X11Color::Blue)
            .fontcolor(X11Color::White);
    }

    label
}

pub fn dom_info_box(node: &Node) -> Label {
    let mut label = if let Node::Block(block) = node {
        let dom_depth = unsafe { bindings::get_Block_dom_depth(node.internal_ir_node()) };
        Label::from_text(format!(
            r#"{{{body}|{{Dom Depth|{}}}}}"#,
            dom_depth = dom_depth,
            body = escape_record_content(&format!("{:?}", block)),
        ))
        .shape(Shape::Record)
        .styles(vec![Style::Rounded, Style::Filled])
    } else {
        default_label(node)
    };

    if node.is_pinned() {
        label.add_style(Style::Bold)
    } else {
        label.add_style(Style::Dashed)
    }
}

// TODO: deduplicate after 172 is merged
pub fn escape_record_content(text: &str) -> String {
    text.replace("|", "\\|")
        .replace("{", "\\{")
        .replace("}", "\\}")
        .replace("<", "\\<")
        .replace(">", "\\>")
}
