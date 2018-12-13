use crate::firm::Program;
use libfirm_rs::{
    graph::{Graph, NodeData},
    nodes::{try_as_bin_op, try_as_unary_op, NodeTrait},
    nodes_gen::{Node, ProjKind},
    tarval::{Tarval, TarvalKind},
};
use priority_queue::PriorityQueue;
use std::{collections::hash_map::HashMap, path::PathBuf};

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
                let graph: Graph = graph.into();
                log::debug!("Graph for Method: {:?}", method.borrow().entity.name());
                let mut cf = ConstantFolding::new(graph);
                cf.run();
                cf.apply();
                graph.dump_dot_data(
                    &PathBuf::from(format!("./dot-out/{}.dot", graph.entity().name_string())),
                    |n| NodeData::new(format!("{:?}", n)),
                );
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
            node_topo_idx.insert(
                *node,
                match node {
                    Node::Block(_) | Node::Jmp(_) => 0,
                    _ => topo_order,
                },
            );

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
        self.values[&node]
    }

    fn update(&mut self, node: Node, new: MyLattice) {
        self.values.insert(node, new).unwrap();
    }

    fn run(&mut self) {
        macro_rules! invalidate {
            ($node: expr) => {
                self.queue.push(
                    $node,
                    Priority {
                        topo_order: *self.node_topo_idx.get(&$node).unwrap(),
                        priority: 0,
                    },
                );
            };
        }

        while let Some((cur_node, _priority)) = self.queue.pop() {
            let cur_lattice = self.lookup(cur_node);
            let updated_lattice = self.update_node(cur_node, cur_lattice);
            if updated_lattice != cur_lattice {
                if let Some(deps) = self.deps.get(&cur_node) {
                    for out_node in deps.iter() {
                        invalidate!(*out_node);
                    }
                }

                for out_node in cur_node.out_nodes() {
                    invalidate!(out_node);
                }

                self.update(cur_node, updated_lattice);
            }
            /*
            if false && self.graph.entity().name_string() == "CF.M.foo" {
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
                                for (pred, block_pred) in phi.in_nodes().zip(phi.block().in_nodes())
                                {
                                    string += &format!("\n{:?} => {:?}", block_pred, pred);
                                }
                                str += &string;
                            }
                            _ => {}
                        }
                        let mut nd =
                            NodeData::new(format!("{:?}; {}\n{}", n, self.node_topo_idx[&n], str));
                        nd.filled(n == cur_node);
                        if self.queue.get(&n).is_some() {
                            nd.bold(true)
                        }
                        nd
                    },
                );
                debugging::wait();
            }
            */
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

        if let Ok(binop) = try_as_bin_op(&cur_node) {
            let left_val = self.lookup(binop.left()).value;
            let right_val = self.lookup(binop.right()).value;
            value = match (left_val.kind(), right_val.kind()) {
                (TarvalKind::Bad, _) | (_, TarvalKind::Bad) => Tarval::bad(),
                (TarvalKind::Unknown, _) | (_, TarvalKind::Unknown) => Tarval::unknown(),
                _ => binop.compute(left_val, right_val),
            };
            log::debug!("compute: {:?} op {:?} = {:?}", left_val, right_val, value);
        } else if let Ok(unary_op) = try_as_unary_op(&cur_node) {
            let operand_val = self.lookup(unary_op.operand()).value;
            value = match operand_val.kind() {
                TarvalKind::Bad => Tarval::bad(),
                TarvalKind::Unknown => Tarval::unknown(),
                _ => unary_op.compute(operand_val),
            };
            log::debug!("compute: op {:?} = {:?}", operand_val, value);
        } else {
            use self::{Node::*, ProjKind::*};
            match cur_node {
                Const(constant) => {
                    value = constant.tarval();
                }
                Cond(cond) => {
                    value = self.lookup(cond.selector()).value;
                }
                Proj(_, Cond_Val(val, cond)) => {
                    if self.lookup(cond.into()).value.is_bool_val(!val) {
                        reachable = false;
                    }
                }
                Phi(phi) => {
                    value = phi.in_nodes().zip(phi.block().in_nodes()).fold(
                        Tarval::unknown(),
                        |val, (pred, block)| {
                            // only consider reachable blocks for phi inputs
                            if !self.lookup(block).reachable {
                                // we must get informed when that block gets reachable
                                self.deps
                                    .entry(block)
                                    .and_modify(|e| e.push(cur_node))
                                    .or_insert_with(|| vec![cur_node]);

                                log::debug!(
                                    "{:?} is unreachable, thus {:?} can be ignored",
                                    block,
                                    pred
                                );
                                val
                            } else {
                                let pred_val = self.lookup(pred).value;
                                let new_val = val.join(pred_val);
                                log::debug!(
                                    "for {:?}; pred_val: {:?} -> val: {:?}",
                                    pred,
                                    pred_val,
                                    new_val
                                );
                                new_val
                            }
                        },
                    )
                }
                _ => {}
            }
        }

        MyLattice { reachable, value }
    }

    fn apply(&mut self) {
        let mut values = self.values.iter().collect::<Vec<_>>();
        values.sort_by_key(|(l, _)| l.node_id());

        for (node, lattice) in values {
            if !lattice.value.is_constant() {
                continue;
            }
            if node.is_const() {
                continue;
            }

            let const_node = Node::Const(self.graph.new_const(lattice.value));
            log::debug!("EXCHANGE NODE {:?} val={:?}", node, lattice.value);

            match node {
                Node::Cond(_) => {}
                /* IMPROVEMENT?
                This might be more elegant, but does not do the exact same:
                It fails if there are multiple projects to that pin!
                Node::Div(div) => {
                    div.out_proj_res().then(|res| Graph::exchange(res, const_node))
                    div.out_proj_m().then(|mem| Graph::exchange(mem, div.mem()))
                }
                */
                Node::Div(div) => {
                    for out_node in node.out_nodes() {
                        match out_node {
                            Node::Proj(res_proj, ProjKind::Div_Res(_)) => {
                                Graph::exchange(&res_proj, &const_node);
                            }
                            Node::Proj(m_proj, ProjKind::Div_M(_)) => {
                                Graph::exchange(&m_proj, &div.mem());
                            }
                            _ => {}
                        }
                    }
                }
                Node::Mod(modulo) => {
                    for out_node in node.out_nodes() {
                        match out_node {
                            Node::Proj(res_proj, ProjKind::Mod_Res(_)) => {
                                Graph::exchange(&res_proj, &const_node);
                            }
                            Node::Proj(m_proj, ProjKind::Mod_M(_)) => {
                                Graph::exchange(&m_proj, &modulo.mem());
                            }
                            _ => {}
                        }
                    }
                }
                _ => {
                    Graph::exchange(node, &const_node);
                }
            }
        }
    }
}
