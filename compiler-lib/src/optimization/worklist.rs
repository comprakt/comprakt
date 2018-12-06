//!
//!
//! # Priorities
//!  We try to converge faster by identifying inner loops through back edges.
//! We increase the priority by 1 each time we walk a back edge.
use libfirm_rs::{
    bindings,
    nodes::NodeTrait,
    nodes_gen::{Node, NodeFactory},
    other::Graph,
};
use std::{
    cmp::Ordering,
    collections::{binary_heap::BinaryHeap, hash_map::HashMap},
};

pub struct WorkList {
    values: HashMap<Node, ()>,
    list: Vec<Node>, //list: BinaryHeap<PrioritizedNode>
    graph: Graph,
}

//struct AnalysisType {
///// startzustand alles drin
//Must,
///// startzustand leer
//May,
//}

//struct AnalysisDirection {
//Backwards,
//Forwards
//}

#[derive(Eq, PartialEq)]
pub struct PrioritizedNode {
    priority: u32,
    node: Node,
}

impl Ord for PrioritizedNode {
    fn cmp(&self, other: &PrioritizedNode) -> Ordering {
        // sort by priority, then by memory address
        other.priority.cmp(&self.priority).then_with(|| {
            let self_ptr = self.node.internal_ir_node();
            let other_ptr = other.node.internal_ir_node();
            self_ptr.cmp(&other_ptr)
        })
    }
}

// `PartialOrd` needs to be implemented as well.
impl PartialOrd for PrioritizedNode {
    fn partial_cmp(&self, other: &PrioritizedNode) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl WorkList {
    pub fn new(graph: Graph) -> Self {
        let mut list = Vec::new();
        graph.walk_topological(|node| {
            list.push(*node);
        });

        let mut values = HashMap::new();

        for node in &list {
            values.insert(*node, ());
        }

        Self {
            list,
            values,
            graph,
        }
    }

    pub fn fixpoint_iterate_const_folding(&mut self) {
        unsafe {
            bindings::assure_irg_outs(self.graph.into());
        }

        println!(
            "Topological Sort: {:?}",
            self.list
                .iter()
                .map(|node| node.node_id())
                .collect::<Vec<_>>()
        );

        for node in &self.list {
            let outs = node
                .reverse_edges()
                .into_iter()
                .map(|edge| edge.node_id())
                .collect::<Vec<_>>();

            println!("Out Nodes for Node {}: {:?}", node.node_id(), outs);
        }
        //let mut changed = true;

        //while changed {
        //match curr {
        //Node::Phi(phi) => {
        //Node::Const(constant) => {
        //let outs = phi.outs();

        ////self.mark_changed(outs);
        //changed = true;
        //self.list.push();
        //}
    }

    //pub fn update() {
    //changed = true;
    //self.list.push();
    //}
}
