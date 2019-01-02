use super::{Outcome, OutcomeCollector};
use crate::{dot::*, optimization};
use libfirm_rs::{
    nodes::{try_as_value_node, Node, NodeTrait, ProjKind},
    Graph,
};
use priority_queue::PriorityQueue;
use std::collections::{hash_map::HashMap, HashSet};

pub struct LoadStoreInlining {
    graph: Graph,
    values: HashMap<(Node, Entity), Node>,
    visited: HashSet<Node>,
    queue: PriorityQueue<Node, usize>,
    //phi_nodes_to_optimize
}

impl optimization::Local for LoadStoreInlining {
    fn optimize_function(graph: Graph) -> Outcome {}
}

impl LoadStoreInlining {
    fn new(graph: Graph) -> Self {
        LoadStoreInlining {
            graph,
            values: HashMap::new(),
            queue: PriorityQueue::new(),
            visited: HashSet::new(),
        }
    }

    fn run(&mut self) {
        self.graph.assure_outs();
        let start = self.graph.start();
        self.queue.push(start);
        while self.queue.len() > 0 {
            let (node, idx): (Node, _) = self.queue.pop();
            self.visited.insert(node);

            match node {
                Node::Phi(phi) => {

                }
                Node::Load(load) => {
                    if Node::Member(m) = load.ptr() {


                        self.values.get(&(m.ptr(), m.entity());



                    }
                },
                Node::Store(store) => {
                    if Node::Member(m) = load.ptr() {

                        self.values.insert((m.ptr(), m.entity()), store.value());

                    }
                },

            }

            for out in node.out_nodes() {
                if node.mode().is_mem() || out.mode().is_mem() {
                    if !self.visited.contains(&node) {
                        self.queue.push(node);
                    }
                }
            }
        }
    }
}
