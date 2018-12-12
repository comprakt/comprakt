use crate::{debugging, firm::Program};
use libfirm_rs::{
    graph::{Graph, NodeData},
    nodes::NodeTrait,
    nodes_gen::{Node, ProjKind},
    tarval::{Tarval, TarvalKind},
};
use std::{collections::hash_map::HashMap, path::PathBuf};
// extern crate priority_queue;
use priority_queue::PriorityQueue;

#[derive(PartialEq, Eq, Clone, Copy)]
struct Priority {
    topo_order: u32,
    priority: u32, // is not needed right now
}

impl std::cmp::Ord for Priority {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.priority
            .cmp(&other.priority)
            .then_with(|| self.topo_order.cmp(&other.topo_order).reverse())
    }
}

impl PartialOrd for Priority {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

pub fn run(program: &Program<'_, '_>) {
    for class in program.classes.values() {
        for method in class.borrow().methods.values() {
            if let Some(graph) = method.borrow().graph {
                log::debug!("Graph for Method: {:?}", method.borrow().entity.name());
                let mut cf = ConstantFolding::new(graph.into());
                cf.run();
            }
        }
    }
}

#[derive(Debug, Clone, PartialEq, Copy)]
struct MyLattice {
    reachable: bool,
    value: Tarval,
}

struct ConstantFolding {
    values: HashMap<Node, MyLattice>,
    queue: PriorityQueue<Node, Priority>,
    node_topo_idx: HashMap<Node, u32>,
    graph: Graph,
    start_block: Node,
    // duplicates are not that important for the inner list as there are at most less than 10.
    deps: HashMap<Node, Vec<Node>>,
}

impl ConstantFolding {
    fn new(graph: Graph) -> Self {
        let mut queue = PriorityQueue::new();
        let mut topo_order = 0;
        let mut node_topo_idx = HashMap::new();
        let mut values = HashMap::new();

        graph.walk_topological(|node| {
            topo_order += 1;
            // blocks immediately, as they are not considered in walk_topological
            node_topo_idx.insert(*node, if node.is_block() { 0 } else { topo_order });

            values.insert(
                *node,
                MyLattice {
                    reachable: false,
                    value: Tarval::unknown(),
                },
            );
        });
        graph.assure_outs();

        let start_block = graph.start().block().into();

        queue.push(
            start_block,
            Priority {
                topo_order: 0,
                priority: 0,
            },
        );

        Self {
            queue,
            values,
            graph,
            node_topo_idx,
            start_block,
            deps: HashMap::new(),
        }
    }

    fn lookup(&self, node: Node) -> MyLattice {
        self.values.get(&node).unwrap().clone()
    }

    fn update(&mut self, node: Node, new: MyLattice) {
        self.values.insert(node, new).unwrap();
    }

    fn run(&mut self) {
        loop {
            let (cur_node, _priority) = match self.queue.pop() {
                Some(t) => t,
                None => break,
            };

            let cur_lattice = self.lookup(cur_node);
            let updated_lattice = self.update_node(cur_node, cur_lattice);
            if updated_lattice != cur_lattice {
                if let Some(deps) = self.deps.get(&cur_node) {
                    for out_node in deps.iter() {
                        self.queue.push(
                            *out_node,
                            Priority {
                                topo_order: *self.node_topo_idx.get(out_node).unwrap(),
                                priority: 0,
                            },
                        );
                    }
                }

                for out_node in cur_node.out_nodes() {
                    self.queue.push(
                        out_node,
                        Priority {
                            topo_order: *self.node_topo_idx.get(&out_node).unwrap(),
                            priority: 0,
                        },
                    );
                }

                self.update(cur_node, updated_lattice);
            }

            if self.graph.entity().name_string() == "CF.M.foo" {
                // only for debugging/development
                self.graph.dump_dot_data(
                    &PathBuf::from(format!(
                        "./dot-out/{}.dot",
                        self.graph.entity().name_string()
                    )),
                    |n| {
                        let mut str = match self.values.get(&n) {
                            Some(data) => format!("r: {:?}, val: {:?}", data.reachable, data.value),
                            None => " ".to_string(),
                        };

                        match n {
                            Node::Phi(phi) => {
                                let mut string = "".to_string();
                                let block = phi.block();
                                let block_preds: Vec<_> = block.in_nodes().collect();
                                for (idx, pred) in phi.in_nodes().enumerate() {
                                    string += &format!("\n{:?} => {:?}", block_preds[idx], pred);
                                }
                                str += &string;
                            }
                            _ => {}
                        }

                        let mut nd = NodeData::new(format!(
                            "{:?}; {}\n{}",
                            n,
                            self.node_topo_idx.get(&n).unwrap(),
                            str
                        ));
                        nd.filled(n == cur_node);
                        if let Some(_) = self.queue.get(&n) {
                            nd.bold(true)
                        }
                        nd
                    },
                );

                debugging::wait();
            }
        }
    }

    fn update_node(&mut self, cur_node: Node, cur_lattice: MyLattice) -> MyLattice {
        let mut reachable = cur_lattice.reachable || if cur_node.is_block() {
            cur_node.in_nodes().any(|pred| self.lookup(pred).reachable)
                || cur_node == self.start_block
        } else {
            self.lookup(cur_node.block().into()).reachable
        };

        let mut value = Tarval::bad();

        match cur_node {
            Node::Const(constant) => {
                value = constant.tarval();
            }
            Node::Cmp(cmp) => {
                let left_val = self.lookup(cmp.left()).value;
                let right_val = self.lookup(cmp.right()).value;
                value = left_val.lattice_cmp(cmp.relation(), right_val);
            }
            Node::Cond(cond) => {
                value = self.lookup(cond.selector()).value;
            }
            Node::Proj(_proj, ProjKind::Cond_False(cond)) => {
                if self.lookup(cond.into()).value.is_bool_val(true) {
                    reachable = false;
                }
            }
            Node::Proj(_proj, ProjKind::Cond_True(cond)) => {
                if self.lookup(cond.into()).value.is_bool_val(false) {
                    reachable = false;
                }
            }
            Node::Phi(phi) => {
                value = Tarval::unknown();
                let block = phi.block();
                let block_preds: Vec<_> = block.in_nodes().collect();

                for (idx, pred) in phi.in_nodes().enumerate() {
                    // only consider reachable blocks for phi inputs
                    if !self.lookup(block_preds[idx]).reachable {
                        // we must get informed when that block gets reachable
                        self.deps
                            .entry(block_preds[idx])
                            .and_modify(|e| e.push(cur_node))
                            .or_insert(vec![cur_node]);

                        log::debug!(
                            "{:?} is unreachable, thus {:?} can be ignored",
                            block_preds[idx],
                            pred
                        );
                        continue;
                    }
                    let pred_val = self.lookup(pred).value;
                    value = match (value.kind(), pred_val.kind()) {
                        (TarvalKind::Unknown, _) => pred_val,
                        (_, TarvalKind::Unknown) => value,
                        (TarvalKind::Bad, _) | (_, TarvalKind::Bad) => Tarval::bad(),
                        _ if value.lattice_eq(pred_val) => value,
                        _ => Tarval::bad(),
                    };
                    log::debug!("for {:?}; pred_val: {:?}, val: {:?}", pred, pred_val, value);
                }
            }
            _ => {}
        }

        MyLattice { reachable, value }
    }
}
