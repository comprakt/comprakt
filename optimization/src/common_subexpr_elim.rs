/// Performs common subexpression elimination. The implementation performs
/// local common subexpression elimination, but will result in a global
/// common subexpression elimination, when it is executed after the
/// `EarliestPlacement` optimization.
///
/// Keep in mind that global common subexpression elimination is a trade-off.
/// It's not always better to merge common subexpressions. This is especially
/// true if you merge two common subexpression A and B, where neither A
/// dominates B, nor B dominates A.
use super::Outcome;
use crate::{dot::*, optimization};
use libfirm_rs::{
    bindings,
    nodes::{Node, NodeTrait},
    Graph,
};

use std::{
    cmp::Ordering,
    collections::{HashSet, VecDeque},
    hash::{Hash, Hasher},
};

struct SubExprNode(Node);

impl Hash for SubExprNode {
    fn hash<H: Hasher>(&self, state: &mut H) {
        // this has to reflect the contents of PartialEq::eq for SubExprNode
        // The following invariant has to hold: k1 == k2 -> hash(k1) == hash(k2)
        debug_assert!(CommonSubExpr::node_qualifies_for_elim(self.0));
        self.0.block().hash(state);

        let op_eq = unsafe { bindings::get_irn_op(self.0.internal_ir_node()) };
        op_eq.hash(state);

        self.0.mode().hash(state);

        for pred in self.0.in_nodes() {
            pred.hash(state);
        }

        // hash attributes of each node
        if let Node::Const(c) = self.0 {
            c.tarval().kind().hash(state);
        }
    }
}

impl PartialEq for SubExprNode {
    fn eq(&self, other: &SubExprNode) -> bool {
        debug_assert!(CommonSubExpr::node_qualifies_for_elim(self.0));
        debug_assert!(CommonSubExpr::node_qualifies_for_elim(other.0));
        debug_assert!(!Node::is_block(self.0));
        debug_assert!(!Node::is_block(other.0));

        // test if it describes the same node variant,
        // e.g. is both a "Const Is" node?
        if (unsafe {
            bindings::get_irn_op(self.0.internal_ir_node())
                != bindings::get_irn_op(other.0.internal_ir_node())
        }) || self.0.mode() != other.0.mode()
        {
            return false;
        }

        if self.0 == other.0 {
            return true;
        }

        if self.0.block() != other.0.block() {
            return false;
        }

        // check if the predecessors are the same
        //
        // Speed up the common case of iterators with unequal
        // length. (Iterator::eq does not optimize this case)
        let in_nodes_self = self.0.in_nodes();
        let in_nodes_other = other.0.in_nodes();

        if in_nodes_self.len() != in_nodes_other.len() {
            return false;
        }

        if !Iterator::eq(in_nodes_self, in_nodes_other) {
            return false;
        }

        // Some nodes have attributes that have to be identical, e.g.
        // - Const nodes have their tarval as attribute
        // - ASM nodes have the assembly as their attributes
        //
        // Internally, the equivalence of node attributes can be checked using
        // `node->op->ops.attrs_equal(a,b)`, but this is not part of the public API.
        //
        // Since libfirm does not expose shit regarding this API, we employ a whitelist.
        // In case you want to compare this to the actual implementation:
        //
        // - Nodes default to no attributes, therefore they are by default 'always
        //   equal'
        // - A list of overrides for middle end nodes can be found in:
        //   83e6f63dba4f83f743b7f5d383af08454f95e90d/ir/ir/irop.c#L580
        // - Backend specific nodes for the amd64 target default to 'always unequal' by
        //   default
        // - Overrides for backend nodes are implemented in:
        //   fa4fea6c01a13e4cb7bfbffb018b9407f531f80b/ir/be/benode.c#L652

        // performing GCSE on const like nodes, e.g. Const and Address, that do not
        // result in instructions is still desired as we expect predecessors to
        // be indentical (pointer equality)
        // TODO: only match one, assert the other.0
        match (self.0, other.0) {
            (Node::Const(a), Node::Const(b)) => {
                // TODO: add asserts for unknown and bad tarval
                a.tarval() == b.tarval()
            }
            (Node::Or(_), Node::Or(_))
            | (Node::Add(_), Node::Add(_))
            | (Node::Sub(_), Node::Sub(_))
            | (Node::Mul(_), Node::Mul(_))
            | (Node::And(_), Node::And(_))
            | (Node::Not(_), Node::Not(_))
            | (Node::Minus(_), Node::Minus(_)) => true,
            unknown => {
                log::debug!("missing attribute equality for {:?}", unknown);
                false
            }
        }
    }
}

impl Eq for SubExprNode {}

pub struct CommonSubExpr {
    graph: Graph,
    // TODO: throw away data for a block when it is done
    similarity: HashSet<SubExprNode>,
    worklist: VecDeque<Node>,
    // we could use the same hashset for visited and eliminated, but
    // that makes debugging really hard!
    visited: HashSet<Node>,
    eliminated: HashSet<Node>,
    num_changed: usize,
}

impl optimization::Local for CommonSubExpr {
    fn optimize_function(graph: Graph) -> Outcome {
        CommonSubExpr::new(graph).run()
    }
}

impl CommonSubExpr {
    fn new(graph: Graph) -> Self {
        Self {
            graph,
            similarity: HashSet::new(),
            worklist: VecDeque::new(),
            visited: HashSet::new(),
            eliminated: HashSet::new(),
            num_changed: 0,
        }
    }

    fn run(&mut self) -> Outcome {
        let end_block = self.graph.end_block();
        self.worklist.push_back(Node::Block(end_block));

        self.graph.assure_outs();

        while let Some(current_node) = self.worklist.pop_front() {
            self.visit_node(current_node, "worklist");
        }

        if self.num_changed > 0 {
            self.graph.remove_unreachable_code();
            self.graph.remove_bads();
            Outcome::Changed
        } else {
            Outcome::Unchanged
        }
    }

    fn visit_node(&mut self, current_node: Node, debug_context: &str) -> Outcome {
        if self.visited.contains(&current_node) {
            breakpoint!(
                &format!(
                    "Earliest Placement: ignoring second visit of {:?} via '{}'",
                    current_node, debug_context
                ),
                self.graph,
                &|node: &Node| label_with_cse_info(node, &current_node)
            );
            return Outcome::Unchanged;
        }

        self.visited.insert(current_node);

        breakpoint!(
            &format!("CSE: visiting {:?} {}", current_node, debug_context),
            self.graph,
            &|node: &Node| label_with_cse_info(node, &current_node)
        );

        self.attempt_cse(current_node, debug_context)
    }

    fn attempt_cse(&mut self, current_node: Node, debug_context: &str) -> Outcome {
        if !CommonSubExpr::node_qualifies_for_elim(current_node) {
            // its only important to move depth first over the predecessors
            // of a chunk of nodes that all qualify for CSE. Since
            // the current node cannot be eliminated, we can push it onto
            // the worklist to keep the stack small.
            for pred in current_node.in_nodes() {
                self.worklist.push_back(pred);
                if !Node::is_block(pred) {
                    self.worklist.push_back(Node::Block(pred.block()));
                }
            }

            return Outcome::Unchanged;
        }

        if self.eliminated.contains(&current_node) {
            breakpoint!(
                &format!("CSE: ignoring visit of eliminated {:?}", current_node),
                self.graph,
                &|node: &Node| label_with_cse_info(node, &current_node)
            );

            return Outcome::Unchanged;
        }

        // we want to visit the predecessors of each node first. This allows us
        // to simplify the predecessor comparison in CSE to a simple pointer
        // comparison => move depth first over chunk that qualifies for elim.
        self.visit_node(
            Node::Block(current_node.block()),
            &format!("block of {:?}", current_node),
        );

        for pred in current_node.in_nodes() {
            self.visit_node(pred, &format!("pred of {:?}", current_node));
        }

        breakpoint!(
            &format!(
                "CSE: checking {:?} for congruence as '{}'",
                current_node, debug_context
            ),
            self.graph,
            &|node: &Node| {
                let mut label = label_with_cse_info(node, &current_node);

                if current_node == *node {
                    label = label
                        .add_style(Style::Filled)
                        .fillcolor(X11Color::Red)
                        .fontcolor(X11Color::White);
                }

                label
            }
        );

        // normalize node, this allows us to detect commutative expressions,
        // e.g. 'x * 5' and '5 * x'
        self.normalize_node(current_node);

        // at this point, predecessors were processed by CSE, the predecessor
        // equality can now be solved using simple pointer equality, process
        // the current node
        if let Some(SubExprNode(reprasentative)) = self.similarity.get(&SubExprNode(current_node)) {
            // we found a reprasentative node that is congruent to the current node,
            // use the reprasentative instead
            //log::error!(
            //"CSE: {:?} and {:?} are congruent",
            //current_node,
            //reprasentative
            //);

            breakpoint!(
                &format!(
                    "CSE: {:?} and {:?} are congruent",
                    current_node, reprasentative
                ),
                self.graph,
                &|node: &Node| {
                    let mut label = label_with_cse_info(node, &current_node);

                    if reprasentative == node {
                        label = label
                            .add_style(Style::Filled)
                            .fillcolor(X11Color::Green)
                            .fontcolor(X11Color::White);
                    }

                    label
                }
            );

            self.num_changed += 1;
            self.eliminated.insert(current_node);

            // remove the current node from the graph
            for (consumer, idx) in current_node.out_nodes_ex() {
                consumer.set_input_at(idx, *reprasentative);
            }

            current_node.set_in_nodes(&[]);

            // In this method, we
            // - generate unreachable code (nodes removed by merging)
            // - invalide outs (merged node gets additional inputs, inputs are reordered)
            Outcome::Changed
        } else {
            // no congruent node yet, use the current node as representative
            self.similarity.insert(SubExprNode(current_node));
            Outcome::Unchanged
        }
    }

    fn normalize_node(&self, node: Node) {
        // put commutative ops in an arbitrary but stable order
        // Exaustive list of commutative nodes: add, and, eor, mul, mulh, or
        if node.is_commutative() {
            let operand_right =
                Node::wrap(unsafe { bindings::get_binop_right(node.internal_ir_node()) });
            let operand_left =
                Node::wrap(unsafe { bindings::get_binop_left(node.internal_ir_node()) });

            if CommonSubExpr::cmp_node(operand_left, operand_right) == Ordering::Greater {
                breakpoint!(
                    &format!(
                        "CSE: normalizing order of commutative operands {:?} and {:?} of {:?}",
                        operand_left, operand_right, node
                    ),
                    self.graph,
                    &|rendered: &Node| {
                        let mut label = dom_info_box(rendered);

                        if *rendered == node {
                            label = label
                                .add_style(Style::Filled)
                                .fillcolor(X11Color::Blue)
                                .fontcolor(X11Color::White);
                        }

                        if *rendered == operand_right || *rendered == operand_left {
                            label = label
                                .add_style(Style::Filled)
                                .fillcolor(X11Color::Green)
                                .fontcolor(X11Color::White);
                        }
                        label
                    }
                );

                unsafe {
                    bindings::set_binop_right(
                        node.internal_ir_node(),
                        operand_left.internal_ir_node(),
                    )
                };
                unsafe {
                    bindings::set_binop_left(
                        node.internal_ir_node(),
                        operand_right.internal_ir_node(),
                    )
                };
            }
        }
    }

    /// Fast check that is false if the node cannot be equal to anything
    /// => is guranteed to be not subexpression eliminatable
    fn node_qualifies_for_elim(node: Node) -> bool {
        match node {
            Node::Const(_)
            | Node::Or(_)
            | Node::Add(_)
            | Node::Sub(_)
            | Node::Mul(_)
            | Node::And(_)
            | Node::Not(_)
            | Node::Minus(_) => true,
            _ => false,
        }
    }

    fn cmp_node(a: Node, b: Node) -> Ordering {
        // Libfirm sorts by multiple critera:
        // df79debd25b9372f92c416c4d659d2b1cf17009d/ir/opt/iropt.c#L7812
        // df79debd25b9372f92c416c4d659d2b1cf17009d/ir/opt/iropt.c#L2169
        a.node_id().cmp(&b.node_id())
    }
}

pub fn label_with_cse_info(node: &Node, highlight: &Node) -> Label {
    let mut label = default_label(node);

    if node == highlight {
        label = label
            .add_style(Style::Filled)
            .fillcolor(X11Color::Blue)
            .fontcolor(X11Color::White);
    }

    if !CommonSubExpr::node_qualifies_for_elim(*node) {
        label.add_style(Style::Bold)
    } else {
        label.add_style(Style::Dashed)
    }
}
