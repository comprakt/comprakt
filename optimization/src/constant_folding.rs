use super::{lattices::*, Outcome};
use crate::{dot::*, optimization};
use firm_construction::program_generator::Spans;
use libfirm_rs::{
    nodes::{try_as_value_node, Block, Node, NodeTrait, ProjKind},
    Graph, TarvalKind,
};
use priority_queue::PriorityQueue;
use std::collections::{HashMap, HashSet};

// == Priority ==

#[derive(PartialEq, Eq, Clone, Copy)]
struct Priority {
    topo_order: u32,
    priority: u32, // highest priority first
}

impl std::cmp::Ord for Priority {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        (self.priority)
            .cmp(&other.priority)
            .then_with(|| self.topo_order.cmp(&other.topo_order).reverse())
    }
}

impl PartialOrd for Priority {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

pub struct ConstantFolding {
    // lattice per each node
    values: HashMap<Node, NodeLattice>,
    // worklist queue
    queue: PriorityQueue<Node, Priority>,
    // Tracks additional update dependencys between nodes.
    // This is needed for nodes which use `values` information
    // from non-predecessors.
    deps: HashMap<Node, HashSet<Node>>,
    // Map each node to a priority. Nodes with higher priority are updated first.
    node_topo_idx: HashMap<Node, Priority>,
    // The graph on which the optimization is applied to.
    graph: Graph,
    // the start block of the graph.
    start_block: Node,
    // counts how many updates were scheduled.
    node_update_count: usize,
    // this field is for debugging only.
    cur_node: Option<Node>,
}

impl optimization::Local for ConstantFolding {
    fn optimize_function(graph: Graph) -> Outcome {
        let mut constant_folding = Self::new(graph);
        constant_folding.run();
        constant_folding.apply()
    }
}

impl ConstantFolding {
    fn new(graph: Graph) -> Self {
        let mut queue = PriorityQueue::new();

        let mut node_topo_idx = HashMap::new();
        let mut values = HashMap::new();

        graph.assure_outs();

        let mut topo_order = 0;
        graph.walk_topological(|node| {
            topo_order += 1;
            // blocks and jumps are handled immediately,
            // so that they make things alive quicker.
            // they don't depend on anything but its predecessor.
            log::debug!("insert {:?}", node);
            node_topo_idx.insert(
                *node,
                Priority {
                    topo_order,
                    priority: match node {
                        Node::Block(_) | Node::Jmp(_) => 1,
                        _ => 0,
                    },
                },
            );

            values.insert(*node, NodeLattice::start());
        });

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
            cur_node: None,
            deps: HashMap::new(),
            node_update_count: 0,
        }
    }

    fn lookup(&self, node: Node) -> &NodeLattice {
        &self.values[&node]
    }

    fn lookup_val(&self, node: Node) -> Option<&NodeValue> {
        self.lookup(node).expect_value_or_no_info()
    }

    fn update(&mut self, node: Node, new: NodeLattice) {
        self.values.insert(node, new).unwrap();
    }

    fn run(&mut self) {
        log::info!(
            "Run constant folding on {}",
            self.graph.entity().name_string()
        );

        macro_rules! invalidate {
            ($node: expr) => {
                let prio = *self
                    .node_topo_idx
                    .get(&$node)
                    .expect(&format!("{:?} have an topological order", $node));
                self.queue.push($node, prio);
            };
        }

        let mut deps = Vec::new();

        while let Some((cur_node, _priority)) = self.queue.pop() {
            self.cur_node = Some(cur_node);
            self.node_update_count += 1;
            let cur_lattice = self.lookup(cur_node);
            let updated_lattice = self.update_node(cur_node, cur_lattice, &mut deps);
            if &updated_lattice != cur_lattice {
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

            if !deps.is_empty() {
                for dep in &deps {
                    self.deps
                        .entry(*dep)
                        .and_modify(|e| {
                            e.insert(cur_node);
                        })
                        .or_insert_with(|| vec![cur_node].into_iter().collect());
                }
                deps.clear();
            }
        }

        self.cur_node = None;
    }

    fn breakpoint(&self, cur_node: Node) {
        breakpoint!("Constant Folding: iteration", self.graph, &|node: &Node| {
            let mut label = default_label(node);
            if let Some(lattice) = self.values.get(&node) {
                label = label.append(format!("\n{:?}", lattice));
            }

            if node == &cur_node {
                label = label
                    .style(Style::Filled)
                    .fillcolor(X11Color::Blue)
                    .fontcolor(X11Color::White);
            }

            if let Node::Phi(phi) = node {
                for (arg, pred) in phi.in_nodes().zip(phi.block().in_nodes()) {
                    label = label.append(format!(
                        "\nfrom {} use {:?}{}",
                        pred.node_id(),
                        self.lookup(arg),
                        if self.lookup(pred).reachable() {
                            ""
                        } else {
                            " [dead]"
                        }
                    ));
                }
            }

            if self.queue.get(&node).is_some() {
                label = label.style(Style::Bold);
            }

            label = label.append(format!(
                "\ntopo: {} dom: {}",
                self.node_topo_idx[node].topo_order,
                node.block().dom_depth()
            ));

            label
        });
    }

    fn update_node(
        &self,
        cur_node: Node,
        cur_lattice: &'_ NodeLattice,
        deps: &mut Vec<Node>,
    ) -> NodeLattice {
        use self::{Node::*, ProjKind::*};
        self.breakpoint(cur_node);

        let reachable = cur_lattice.reachable()
            || if Node::is_block(cur_node) {
                cur_node
                    .in_nodes()
                    .any(|pred| self.lookup(pred).reachable())
                    || cur_node == self.start_block
            } else {
                self.lookup(cur_node.block().into()).reachable()
            };

        if !reachable {
            // we don't need to update non-reachable nodes
            return NodeLattice::NotReachableYet;
        }

        match cur_node {
            // == Conditionals ==
            Cond(cond) => self.lookup(cond.selector()).clone(),
            Proj(_, Cond_Val(is_true_branch, cond)) => match &self.lookup_val(cond.into()) {
                Some(val) if val.tarval().is_bool_val(is_true_branch) || val.tarval().is_bad() => {
                    NodeLattice::Invalid
                }
                _ => NodeLattice::NotReachableYet,
            },

            // == Phi ==
            Phi(phi) => {
                if phi.in_nodes().len() != 2 {
                    log::warn!("phi pred count: {}", phi.in_nodes().len());
                }
                phi.in_nodes().zip(phi.block().in_nodes()).fold(
                    NodeLattice::NotReachableYet,
                    |val, (pred, block)| {
                        // only consider reachable blocks for phi inputs
                        if !self.lookup(block).reachable() {
                            // we must get informed when that block gets reachable
                            deps.push(block);
                            log::debug!(
                                "{:?} is unreachable, thus {:?} can be ignored",
                                block,
                                pred
                            );
                            val
                        } else {
                            let pred_lat = &self.lookup(pred);
                            let new_lat = val.join(pred_lat, &mut JoinContext::None);
                            log::debug!(
                                "for {:?}; pred_val: {:?} -> val: {:?}",
                                pred,
                                pred_lat,
                                new_lat
                            );
                            new_lat
                        }
                    },
                )
            }

            // == Value nodes ==
            _ => {
                if let Ok(value_node) = try_as_value_node(cur_node) {
                    let mut tarval_args = vec![];
                    let mut non_constant = false;
                    let mut no_info = false;
                    for arg in value_node.value_nodes() {
                        match self.lookup_val(arg.as_node()) {
                            None => no_info = true,
                            Some(val) => {
                                let tv = val.tarval();
                                if tv.is_bad() {
                                    non_constant = true;
                                } else {
                                    tarval_args.push(tv)
                                }
                            }
                        }
                    }
                    if non_constant {
                        NodeValue::non_const_node(cur_node).into()
                    } else if no_info {
                        return NodeLattice::NotReachableYet;
                    } else {
                        let tarval = value_node.compute(tarval_args);
                        NodeLattice::from_tarval_node(tarval, cur_node)
                    }
                } else {
                    NodeLattice::Invalid
                }
            }
        }
    }

    fn apply(&mut self) -> Outcome {
        let mut values = self.values.iter().collect::<Vec<_>>();
        values.sort_by_key(|(l, _)| l.node_id());

        let mut to_be_marked_as_bad: Vec<Block> = Vec::new();
        let mut folded_constants = 0;
        let mut optimized_conds = 0;

        for (&node, lattice) in values {
            if Node::is_const(node) {
                continue;
            }

            let value = if let NodeLattice::Value(val) = lattice {
                val.tarval()
            } else {
                continue;
            };

            if try_as_value_node(node).is_ok() {
                let new_node = if value.is_constant() {
                    let const_node = self.graph.new_const(value);
                    const_node.as_node()
                } else {
                    continue;
                };

                folded_constants += 1;

                log::debug!(
                    "exchange value {:?}{} with {:?}{}",
                    node,
                    Spans::span_str(node),
                    new_node,
                    Spans::span_str(new_node)
                );

                let mem = match node {
                    Node::Div(div) => Some((div.mem(), div.out_proj_m())),
                    Node::Mod(m) => Some((m.mem(), m.out_proj_m())),
                    _ => None,
                };
                if let Some((prev_mem, next_mem)) = mem {
                    // only remove this node from memory flow.
                    // Its value is handled in its res-proj.
                    if let Some(next_mem) = next_mem {
                        Graph::exchange(next_mem, prev_mem);
                    }
                } else {
                    Graph::exchange(node, new_node);
                }
            } else if let (Node::Cond(cond), TarvalKind::Bool(val)) = (node, value.kind()) {
                // delete unnecessary branching

                let (always_taken_path, target_block, _target_block_idx) =
                    cond.out_proj_target_block(val).unwrap();
                let (dead_path, nontarget_block, _nontarget_block_idx) =
                    cond.out_proj_target_block(!val).unwrap();

                if nontarget_block.cfg_preds().len() <= 1 {
                    log::debug!(
                        "Schedule nontarget_block {:?} and its children to be marked as bad",
                        nontarget_block
                    );
                    to_be_marked_as_bad.push(nontarget_block);
                }

                let jmp = cond.block().new_jmp();

                optimized_conds += 1;
                log::debug!(
                    "Replace {:?} with {:?} to {:?}",
                    always_taken_path,
                    jmp,
                    target_block
                );

                self.graph.mark_as_bad(dead_path);
                self.graph.mark_as_bad(cond);

                Graph::exchange(always_taken_path, jmp);

                target_block.keep_alive();
            }
        }

        for block in &to_be_marked_as_bad {
            for child in block.out_nodes() {
                log::debug!("Mark block child {:?} as bad", child);
                self.graph.mark_as_bad(child);
            }
            self.graph.mark_as_bad(*block);
        }

        self.graph.remove_bads();
        self.graph.remove_unreachable_code();
        self.graph.remove_bads();

        log::info!(
            "Optimized {:>3} constants and {:>3} conds \
             with {:>4} node updates and {:>4} total nodes in graph {}",
            folded_constants,
            optimized_conds,
            self.node_update_count,
            self.node_topo_idx.len(),
            self.graph.entity().name_string(),
        );

        if folded_constants + optimized_conds > 0 {
            Outcome::Changed
        } else {
            Outcome::Unchanged
        }
    }
}
