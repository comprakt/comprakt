use super::{Outcome, OutcomeCollector};
use crate::{dot::*, optimization};
use libfirm_rs::{
    nodes::{try_as_value_node, Node, NodeTrait, ProjKind},
    Graph, Tarval, TarvalKind,
};
use priority_queue::PriorityQueue;
use std::collections::hash_map::HashMap;

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

// The lattice used for constant folding.
#[derive(Debug, Clone, PartialEq, Copy)]
struct CfLattice {
    reachable: bool,
    value: Tarval,
}

pub struct ConstantFolding {
    values: HashMap<Node, CfLattice>,
    queue: PriorityQueue<Node, Priority>,
    node_topo_idx: HashMap<Node, u32>,
    graph: Graph,
    start_block: Node,
    // duplicates are not that important for the inner list as there are at most less than 10.
    deps: HashMap<Node, Vec<Node>>,
}

impl optimization::Local for ConstantFolding {
    fn optimize_function(graph: Graph) -> Outcome {
        let mut constant_folding = ConstantFolding::new(graph);
        constant_folding.run();
        constant_folding.apply()
    }
}

impl ConstantFolding {
    fn new(graph: Graph) -> Self {
        let mut queue = PriorityQueue::new();
        let mut topo_order = 0;
        let mut node_topo_idx = HashMap::new();
        let mut values = HashMap::new();

        graph.walk_topological(|node| {
            topo_order += 1;
            // blocks and jumps are handled immediately,
            // as they are not considered in walk_topological (in case of blocks)
            // or don't depend much on anything before

            node_topo_idx.insert(
                *node,
                match node {
                    Node::Block(_) | Node::Jmp(_) => 0,
                    _ => topo_order,
                },
            );

            values.insert(
                *node,
                CfLattice {
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

    fn lookup(&self, node: Node) -> CfLattice {
        self.values[&node]
    }

    fn update(&mut self, node: Node, new: CfLattice) {
        self.values.insert(node, new).unwrap();
    }

    fn run(&mut self) {
        macro_rules! invalidate {
            ($node: expr) => {
                let topo_order = self.node_topo_idx.get(&$node);
                if let Some(topo_order) = topo_order {
                    self.queue.push(
                        $node,
                        Priority {
                            topo_order: *topo_order,
                            priority: 0,
                        },
                    );
                } else {
                    // walk_topological only considers nodes reachable from end.
                    // projs with no outs might not be reachable from end.
                    log::debug!("Don't queue {:?} as it is not reachable from end", $node);
                }
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
        }
    }

    fn update_node(&mut self, cur_node: Node, cur_lattice: CfLattice) -> CfLattice {
        breakpoint!("Constant Folding: iteration", self.graph, &|node: &Node| {
            let mut label = default_label(node);

            if let Some(tarval) = self.values.get(&node) {
                label = label.append(format!("\n{:?}", tarval));
            }

            if node == &cur_node {
                label = label
                    .style(Style::Filled)
                    .fillcolor(X11Color::Blue)
                    .fontcolor(X11Color::White);
            }

            if let Some(_priority) = self.queue.get(&node) {
                label = label.style(Style::Bold);
            }

            label
        });

        let mut reachable = cur_lattice.reachable
            || if Node::is_block(cur_node) {
                cur_node.in_nodes().any(|pred| self.lookup(pred).reachable)
                    || cur_node == self.start_block
            } else {
                self.lookup(cur_node.block().into()).reachable
            };

        let mut value = Tarval::bad();

        use self::{Node::*, ProjKind::*};
        match cur_node {
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
            _ => {
                if let Ok(value_node) = try_as_value_node(cur_node) {
                    let values: Vec<_> = value_node
                        .value_nodes()
                        .iter()
                        .map(|n| self.lookup(n.into()).value)
                        .collect();
                    value = if values.iter().any(|n| n.is_bad()) {
                        Tarval::bad()
                    } else if values.iter().any(|n| n.is_unknown()) {
                        Tarval::unknown()
                    } else {
                        value_node.compute(values)
                    };
                }
            }
        }

        CfLattice { reachable, value }
    }

    fn apply(&mut self) -> Outcome {
        let mut collector = OutcomeCollector::new();
        let mut values = self.values.iter().collect::<Vec<_>>();
        values.sort_by_key(|(l, _)| l.node_id());

        let mut dangling_blocks = Vec::new();

        for (node, lattice) in values {
            if !lattice.value.is_constant() {
                collector.push(Outcome::Unchanged);
                continue;
            }
            if Node::is_const(*node) {
                collector.push(Outcome::Unchanged);
                continue;
            }

            if let Ok(value_node) = try_as_value_node(*node) {
                let const_node = self.graph.new_const(lattice.value);
                log::debug!("EXCHANGE NODE {:?} val={:?}", node, lattice.value);
                Graph::exchange_value(value_node, const_node);
                collector.push(Outcome::Changed);
            } else if let (Node::Cond(cond), TarvalKind::Bool(val)) = (node, lattice.value.kind()) {
                let (always_taken_path, target_block) = cond.out_proj_target_block(val).unwrap();

                let (dead_path, nontarget_block) = cond.out_proj_target_block(!val).unwrap();

                if nontarget_block.cfg_preds().len() <= 1 {
                    // If the unused_proj is the sole predecessor of its successor,
                    // mark the successor, eliminate it.
                    // One would think this happens automatically, but it doesn't:
                    // The successor block (if(f?) it contains a Jmp), is kept alive
                    // somehow, causing be_lower_for_target to fail
                    log::debug!("Mark nontarget_block {:?} as dangling", nontarget_block);
                    dangling_blocks.push(nontarget_block);
                }

                let jmp = cond.block().new_jmp();
                log::debug!("Replace {:?} with {:?} to {:?}", node, jmp, target_block);
                Graph::exchange(always_taken_path, jmp);
                self.graph.mark_as_bad(dead_path);

                // We need this because if we have a while(true) loop, the code will be
                // unreachable (libfirm-edge wise) from the end block (because the end
                // block is never reached control-flow wise), but
                // libfirm needs to find the loop (and it starts
                // searching from the end
                // block)
                target_block.keep_alive();
                collector.push(Outcome::Changed);
            }
        }

        for block in &dangling_blocks {
            for (i, child) in block.out_nodes().enumerate() {
                log::debug!("Mark block child #{} {:?} as bad", i, child);
                self.graph.mark_as_bad(child);
            }
        }

        self.graph.remove_bads();

        collector.result()
    }
}
