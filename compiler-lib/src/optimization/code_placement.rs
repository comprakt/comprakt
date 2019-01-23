///! # Code Motion / Code Placement
///!
///! This reimplements the "Code Placement" Optimization in Libfirm as
///! a more powerful optimization than "Loop Invariant Code Motion".
///! The algorithm has two steps, which are thoroughly explained below:
///!
///! 1. Find earliest placement allowed by operands of each node that
///!    can be savely moved into another block. [ optimize on in edges ]
///! 2. Push each movable nodes deeper into the graph trying to find an optimal
///!    placement between earliest and latest allowed placement that minimizes
///!    the number of times the computation is done. It may be done
///!    too often because of loops in deepest position, and may be
///!    unnecessarily execute without a usage afterwards in earliest
///!    position. [ optimize on out edges and loop nesting level ]
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
///!
///! # References
///! http://compilers.cs.uni-saarland.de/teaching/cc/2009/slides/l10_pre.pdf
///! http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.92.4197&rep=rep1&type=pdf
use super::Outcome;
use crate::{
    dot::*,
    optimization::{self, Local},
};
use libfirm_rs::{
    bindings,
    nodes::{self, Node, NodeTrait},
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
        Self { graph }
    }

    fn run(&mut self) -> Outcome {
        let earlier_placement = EarliestPlacement::optimize_function(self.graph);

        breakpoint!("Graph in Earliest Placement", self.graph, &|node: &Node| {
            dom_info_box(node)
        });

        let optimal_placement = CostMinimizingPlacement::optimize_function(self.graph);

        if earlier_placement == Outcome::Changed || optimal_placement == Outcome::Changed {
            Outcome::Changed
        } else {
            Outcome::Unchanged
        }
    }
}

/// Places nodes / computations as early as possible. This will move loop
/// invariant code out of loops, but it is obviously not a good strategy on its
/// own since a lot of computations will be partially dead (unused by some paths
/// passing through the basic block).
///
/// Use [`CodePlacement`] instead.
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
        // TODO: do we use outs in EarliestPlacement?
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
            //
            // => Use Depth First Search on predecessor/input edges

            // # Optimize Predecessors / Operands of the Current Node

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
            // - The predecessor with maximal dominator tree depth is unique since it is a
            //   tree.
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

/// Places nodes / computations at the most cost minimal position. Currently
/// considered factors are:
/// - control deepest position, by pushing to the dominance deepest (minimize
///   unnecessary computations)
/// - loop nesting level, effectively implementing loop invariant code motion
///
/// NOTE: this expects all computations the be placed as early as possible. Call
/// [`EarliestPlacement`] first or use [`CodePlacement`], which combines both
/// optimizations, directly.
pub struct CostMinimizingPlacement {
    graph: Graph,
    worklist: VecDeque<Node>,
    visited: HashSet<Node>,
    num_changed: usize,
}

impl optimization::Local for CostMinimizingPlacement {
    fn optimize_function(graph: Graph) -> Outcome {
        Self::new(graph).run()
    }
}

impl CostMinimizingPlacement {
    fn new(graph: Graph) -> Self {
        graph.assure_outs();
        graph.assure_loopinfo();
        Self {
            graph,
            worklist: VecDeque::new(),
            visited: HashSet::new(),
            num_changed: 0,
        }
    }

    fn run(&mut self) -> Outcome {
        // analog recursion to `EarliestPlacement`. The latest valid position
        // of the current node is dependent on the position of all its users/consumers.
        //
        // => Use Depth First Search on out edges
        let start_block = self.graph.start_block();
        self.worklist.push_back(Node::Block(start_block));

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
                    "MinCost Placement: ignoring second visit of {:?}",
                    current_node
                ),
                self.graph,
                &|node: &Node| label_with_dom_info(self.graph, node, &current_node)
            );
            return Outcome::Unchanged;
        }
        self.visited.insert(current_node);

        breakpoint!(
            &format!("MinCost Placement: {} {:?}", debug_context, current_node),
            self.graph,
            &|node: &Node| label_with_dom_info(self.graph, node, &current_node)
        );

        self.move_to_cost_minimizing_block(current_node)
    }

    fn move_to_cost_minimizing_block(&mut self, current_node: Node) -> Outcome {
        if current_node.is_pinned() {
            self.worklist.extend(current_node.out_nodes());
            Outcome::Unchanged
        } else {
            // # Optimize Consumers (Nodes that use the current Node as Operand)

            for pred in current_node.in_nodes() {
                self.visit_node(pred, &format!("consumer of {:?}", current_node));
            }

            // # Optimize Position of Current Node
            //
            // - Some nodes are not in theory safe to move, but optimizations in libfirm
            //   require them to be in the start block. Currently, the only floatable node
            //   that has to stay in the start block is Const. All other start-block-only
            //   nodes like Bad, Unknown, ... are pinned anyways.
            // - Projections have to stay in the same block as the node they are projecting
            //   (I think? But it would be weird otherwise), we move them separately
            if current_node.is_only_valid_in_start_block() || Node::is_proj(current_node) {
                return Outcome::Unchanged;
            }

            // "deepest common ancestor" is just the dom tree for the out edges
            //
            // What is happening here?
            //
            // TODO: explain
            //
            let earliest_allowed = current_node.block();
            let latest_by_consumers =
                self.latest_possible_placement_allowed_by_consumers(current_node);

            // TODO: we do not have to set this here since we move again below
            if let Some(block) = latest_by_consumers {
                if block != current_node.block() {
                    breakpoint!(
                        &format!(
                            "MinCost Placement: Non-trivial control dependence motion\
                             for {:?} to {:?} possible",
                            current_node, latest_by_consumers
                        ),
                        self.graph,
                        &|node: &Node| label_for_late_placement(
                            node,
                            current_node,
                            earliest_allowed,
                            latest_by_consumers
                        )
                    );

                    current_node.set_block(block);
                }
            }

            // TODO: why is this done when earliest and latest_possible_placement are
            // identical?
            let loop_invariant_placement = Self::minimize_loop_nesting(
                earliest_allowed,
                latest_by_consumers.unwrap_or_else(|| earliest_allowed),
            );

            if loop_invariant_placement != current_node.block() {
                breakpoint!(
                    &format!(
                        "MinCost Placement: Loop Invariant Code Motion to {:?} possible",
                        loop_invariant_placement
                    ),
                    self.graph,
                    &|node: &Node| {
                        let mut label = label_for_late_placement(
                            node,
                            current_node,
                            earliest_allowed,
                            latest_by_consumers,
                        );
                        if node == &Node::Block(loop_invariant_placement) {
                            label = label
                                .add_style(Style::Filled)
                                .fillcolor(X11Color::Green)
                                .fontcolor(X11Color::White);
                        }

                        label
                    }
                );

                current_node.set_block(loop_invariant_placement);
            }

            Outcome::Unchanged
        }
    }

    /// Returns the deepest common dominator of all consumers of `current_node`
    /// and `current_node`. If the node does not have consumers (it is only
    /// kept alive), the block of the `current_node` is returned.
    fn latest_possible_placement_allowed_by_consumers(
        &self,
        current_node: Node,
    ) -> Option<nodes::Block> {
        // `current_node.block()` is the correct initialization since we assume
        // `current_node` is placed at the earliest valid position.
        // => "shallowest common dominator"
        // WRONG! otherwise we always get current_node.block() as a result ;)
        let mut deepest_common_dominator = None; // current_node.block();

        for consumer in current_node.out_nodes() {
            match consumer {
                Node::End(..) => {
                    // following keep-alive edges should be avoided since end/keep-alives have
                    // special semantic regarding dominance. See dominance tree
                    // example above!
                    continue;
                }
                Node::Proj(..) => {
                    // this check is not necessary, for correctness
                    // but improves placement since projections are always in the same block as the
                    // node they project (e.g. the same block as the Cond node)
                    // so the real users can be found in the children of the
                    // proj (I think?) TODO: but this does not seem necessary
                    // since we visit the proj anyway? They will only be
                    // temporarily split, right?

                    // "act as if Cond followed by Projs is a single node"
                    if let Some(common_dominator) =
                        self.latest_possible_placement_allowed_by_consumers(consumer)
                    {
                        deepest_common_dominator = Self::update_deepest_common_dominator(
                            deepest_common_dominator,
                            common_dominator,
                        );
                    }
                }
                Node::Phi(phi) => {
                    // TODO: think about this again, does this make sense????
                    // act as if the Phi does not exist, since it is just proxying the
                    // actual user
                    // find the actual consumer correspondig to the current node
                    let actual_consumer_index = phi
                        .phi_preds()
                        .position(|pred| pred == current_node)
                        .unwrap();
                    let actual_consumer =
                        phi.block().cfg_preds().nth(actual_consumer_index).unwrap();

                    //self.latest_possible_placement(actual_consumer)
                    deepest_common_dominator = Self::update_deepest_common_dominator(
                        deepest_common_dominator,
                        actual_consumer.block(),
                    );
                }
                node => {
                    deepest_common_dominator = Self::update_deepest_common_dominator(
                        deepest_common_dominator,
                        node.block(),
                    );
                }
            }
        }

        deepest_common_dominator
    }

    fn update_deepest_common_dominator(
        dca: Option<nodes::Block>,
        block: nodes::Block,
    ) -> Option<nodes::Block> {
        match dca {
            None => Some(block),
            Some(common) if common == block => Some(common),
            Some(common) => Some(nodes::Block::deepest_common_dominator(block, common)),
        }
    }

    // TODO: rename to optimize_execution_frequency, and use built in frequency
    // analysis instead
    fn minimize_loop_nesting(
        earliest_allowed: nodes::Block,
        latest_allowed: nodes::Block,
    ) -> nodes::Block {
        // initialize best to latest allowed by the consumers of the node
        let mut block = latest_allowed;
        let mut best = block;
        let mut best_depth = best.loop_depth();

        // walk the segment of the dominance tree from the latest ("closest to end") to
        // earliest ("closest to start"), push the current node out of all loops
        while block != earliest_allowed {
            let idom = block.immediate_dominator().unwrap();
            let idom_depth = idom.loop_depth();
            if idom_depth < best_depth {
                best = idom;
                best_depth = idom_depth;
            }
            block = idom;
        }

        best
    }
}

pub fn label_with_dom_info(graph: Graph, node: &Node, highlight: &Node) -> Label {
    // TODO: no side effects in debug code
    graph.assure_loopinfo();

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
    let label = if let Node::Block(block) = node {
        let dom_depth = unsafe { bindings::get_Block_dom_depth(node.internal_ir_node()) };
        Label::from_text(format!(
            r#"{{{body}|{{Dom Depth|{dom_depth}}}|{{Loop Depth|{loop_depth}}}}}"#,
            dom_depth = dom_depth,
            loop_depth = block.loop_depth(),
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

pub fn label_for_late_placement(
    node: &Node,
    current_node: Node,
    earliest_allowed: nodes::Block,
    latest_allowed: Option<nodes::Block>,
) -> Label {
    let mut label = dom_info_box(node);

    if let Node::Block(rendered_block) = node {
        // is within the chain of possibilities
        if let Some(latest_block) = latest_allowed {
            if earliest_allowed.dominates(*rendered_block) && rendered_block.dominates(latest_block)
            {
                label = label
                    .add_style(Style::Filled)
                    .fillcolor(X11Color::Pink)
                    .fontcolor(X11Color::White);
            }
        }

        if rendered_block == &earliest_allowed || Some(*rendered_block) == latest_allowed {
            label = label
                .add_style(Style::Filled)
                .fillcolor(X11Color::Red)
                .fontcolor(X11Color::White);
        }
    }

    if node == &current_node {
        label = label
            .add_style(Style::Filled)
            .fillcolor(X11Color::Blue)
            .fontcolor(X11Color::White);
    }

    label
}

// TODO: deduplicate after 172 is merged
pub fn escape_record_content(text: &str) -> String {
    text.replace("|", "\\|")
        .replace("{", "\\{")
        .replace("}", "\\}")
        .replace("<", "\\<")
        .replace(">", "\\>")
}
