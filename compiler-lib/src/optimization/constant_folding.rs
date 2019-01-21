use super::{lattices::*, Outcome, OutcomeCollector};
use crate::{dot::*, optimization};
use libfirm_rs::{
    nodes::{try_as_value_node, Block, IsNewResult, Node, NodeTrait, ProjKind},
    types::ClassTy,
    Entity, Graph, Tarval, TarvalKind,
};
use priority_queue::PriorityQueue;
use std::{
    cell::Cell,
    collections::{HashMap, HashSet},
    rc::Rc,
};

// == Priority ==

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

pub struct ConstantFolding {
    values: HashMap<Node, ConstantFoldingLattice>,
    queue: PriorityQueue<Node, Priority>,
    node_topo_idx: HashMap<Node, u32>,
    // this field is for debugging only.
    cur_node: Option<Node>,
    graph: Graph,
    start_block: Node,
    deps: HashMap<Node, HashSet<Node>>,
    obj_id: Cell<usize>,
    node_update_count: usize,
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
        log::debug!("new");
        let mut queue = PriorityQueue::new();
        let mut topo_order = 0;
        let mut node_topo_idx = HashMap::new();
        let mut values = HashMap::new();

        graph.assure_outs();

        // calling this function crashes libfirm
        // graph.compute_dominance_frontiers();

        graph.walk(|node| {
            topo_order += 1;
            // blocks and jumps are handled immediately,
            // so that they make things alive quicker.
            // they don't depend on anything but its predecessor.
            log::debug!("insert {:?}", node);
            node_topo_idx.insert(
                *node,
                match node {
                    Node::Block(_) | Node::Jmp(_) => 0,
                    _ => topo_order,
                },
            );

            values.insert(*node, ConstantFoldingLattice::start());
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
            obj_id: Cell::default(),
            node_update_count: 0,
        }
    }

    fn lookup(&self, node: Node) -> &ConstantFoldingLattice {
        &self.values[&node]
    }

    fn lookup_val(&self, node: Node) -> &Val {
        self.lookup(node).value()
    }

    fn update(&mut self, node: Node, new: ConstantFoldingLattice) {
        self.values.insert(node, new).unwrap();
    }

    fn run(&mut self) {
        macro_rules! invalidate {
            ($node: expr) => {
                let topo_order = *self
                    .node_topo_idx
                    .get(&$node)
                    .expect(&format!("{:?} to exist", $node));
                self.queue.push(
                    $node,
                    Priority {
                        topo_order,
                        priority: 0,
                    },
                );
            };
        }

        while let Some((cur_node, _priority)) = self.queue.pop() {
            self.cur_node = Some(cur_node.clone());
            self.node_update_count += 1;
            let cur_lattice = self.lookup(cur_node);
            let (updated_lattice, deps) = self.update_node(cur_node, cur_lattice);
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

            for dep in deps {
                self.deps
                    .entry(dep)
                    .and_modify(|e| {
                        e.insert(cur_node);
                    })
                    .or_insert_with(|| vec![cur_node].into_iter().collect());
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

            match node {
                Node::Phi(phi) => {
                    for (arg, pred) in phi.in_nodes().zip(phi.block().in_nodes()) {
                        label = label.append(format!(
                            "\nfrom {} use {:?}{}",
                            pred.node_id(),
                            self.lookup_val(arg),
                            if self.lookup(pred).reachable() {
                                ""
                            } else {
                                " [dead]"
                            }
                        ));
                    }
                }
                _ => {}
            }

            if let Some((_node, priority)) = self.queue.get(&node) {
                label = label.style(Style::Bold);
                label = label.append(format!("\n{}", priority.topo_order));
            }

            label
        });
    }

    fn get_new_id(&self) -> usize {
        let id = self.obj_id.get();
        self.obj_id.set(id + 1);
        id
    }

    fn update_node(
        &self,
        cur_node: Node,
        cur_lattice: &'_ ConstantFoldingLattice,
        // TODO maybe Option<Vec<Node>> might improve perf
    ) -> (ConstantFoldingLattice, Vec<Node>) {
        self.breakpoint(cur_node);

        let mut reachable = cur_lattice.reachable()
            || if Node::is_block(cur_node) {
                cur_node
                    .in_nodes()
                    .any(|pred| self.lookup(pred).reachable())
                    || cur_node == self.start_block
            } else {
                self.lookup(cur_node.block().into()).reachable()
            };

        let mut deps = vec![];

        use self::{Node::*, ProjKind::*};
        let value = match cur_node {
            // == Load-Store optimizations ==
            Proj(_, Start_M(_)) => Val::Heap(Rc::new(Heap::start())),

            Call(call) => {
                match (self.lookup_val(call.mem()), call.is_new()) {
                    (Val::Heap(heap), IsNewResult::Yes(class_ty)) => {
                        let mut heap = (**heap).clone();
                        let ptr = match cur_lattice.value() {
                            Val::Tuple(old_ptrs, _) => {
                                let old_ptr = if let Val::ObjPointers(old_ptrs) = &**old_ptrs {
                                    old_ptrs.as_single_ignoring_null().unwrap()
                                } else {
                                    panic!("unreachable");
                                };
                                let ptr = if heap.exists(old_ptr) {
                                    old_ptr.as_recent()
                                } else {
                                    old_ptr
                                };

                                heap.new_obj(ptr, class_ty);

                                ptr
                            }
                            Val::NoInfoYet => {
                                let ptr = Pointer::new(self.get_new_id());
                                heap.new_obj(ptr, class_ty);
                                ptr
                            }
                            val => panic!("unexpected value {:?}", val),
                        };
                        Val::tuple(Val::ObjPointers(ptr.into()), Val::Heap(Rc::new(heap)))
                    }
                    // reset heap if an unknown method is called that could modify arbitrary memory.
                    (Val::Heap(heap), IsNewResult::No) => {
                        let mut heap = (&**heap).clone();
                        heap.reset();
                        Val::tuple(Val::NonConstant, Val::Heap(Rc::new(heap)))
                    }
                    (Val::NoInfoYet, _) => Val::NoInfoYet,
                    val => panic!("unreachable {:?}", val),
                }
            }
            Proj(_, Call_TResult(node)) => self.lookup_val(node.into()).tuple_1().clone(),
            Proj(_, Call_TResult_Arg(_, _, node)) => self.lookup_val(node.into()).clone(),
            Proj(_, Call_M(node)) => self.lookup_val(node.into()).tuple_2().clone(),

            Store(store) => match (store.ptr(), self.lookup_val(store.mem())) {
                (Node::Member(member), Val::Heap(heap)) => {
                    let field = member.entity();
                    let ptr = member.ptr();
                    let ptrs = match &self.lookup_val(ptr) {
                        Val::ObjPointers(ptrs) => Some(ptrs),
                        _ => None,
                    };
                    let mut heap = (**heap).clone();
                    let val = self.lookup_val(store.value());
                    heap.update_field(ptr, ptrs, field, val);
                    Val::Heap(Rc::new(heap))
                }
                (_, heap) => heap.clone(),
            },
            Proj(_, Store_M(store)) => self.lookup_val(store.into()).clone(),

            Load(load) => {
                let heap = &self.lookup_val(load.mem());

                match (load.ptr(), heap) {
                    (Node::Member(member), Val::Heap(heap)) => {
                        let field = member.entity();
                        let ptr = member.ptr();
                        let ptrs = match &self.lookup_val(ptr) {
                            Val::ObjPointers(ptrs) => Some(ptrs),
                            _ => None,
                        };
                        let mut heap = (**heap).clone();
                        let val = heap.lookup_field(ptr, ptrs, field);
                        Val::tuple(val, Val::Heap(Rc::new(heap)))
                    }
                    (_, mem) => Val::tuple(Val::NonConstant, (*mem).clone()),
                }
            }
            Proj(_, Load_Res(node)) => self.lookup_val(node.into()).tuple_1().clone(),
            Proj(_, Load_M(node)) => self.lookup_val(node.into()).tuple_2().clone(),

            Proj(_, Div_M(node)) => {
                deps.push(node.mem());
                self.lookup_val(node.mem()).clone()
            }
            Proj(_, Mod_M(node)) => {
                deps.push(node.mem());
                self.lookup_val(node.mem()).clone()
            }

            // == Conditionals ==
            Cond(cond) => self.lookup_val(cond.selector()).clone(),
            Proj(_, Cond_Val(val, cond)) => {
                if let Val::Tarval(tarval) = &self.lookup_val(cond.into()) {
                    if tarval.is_bool_val(!val) {
                        reachable = false;
                    }
                }
                Val::NonConstant
            }

            // == Phi ==
            Phi(phi) => {
                phi.in_nodes().zip(phi.block().in_nodes()).fold(
                    Val::NoInfoYet,
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
                            let pred_val = &self.lookup_val(pred);
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

            // == Value nodes ==
            _ => {
                if let Ok(value_node) = try_as_value_node(cur_node) {
                    let mut tarval_args = vec![];
                    let mut non_constant = false;
                    let mut no_info = false;
                    let mut invalid = false;
                    for arg in value_node.value_nodes() {
                        match self.lookup_val(arg.into()) {
                            Val::NonConstant => non_constant = true,
                            Val::NoInfoYet => no_info = true,
                            Val::Tarval(val) => tarval_args.push(*val),
                            Val::ArrPointers(..)
                            | Val::ObjPointers(..)
                            | Val::Heap(..)
                            | Val::Tuple(..) => invalid = true,
                        }
                    }
                    if non_constant | invalid {
                        Val::NonConstant
                    } else if no_info {
                        Val::NoInfoYet
                    } else {
                        Val::from_tarval(value_node.compute(tarval_args))
                    }
                } else {
                    Val::NonConstant
                }
            }
        };

        (ConstantFoldingLattice::new(reachable, value), deps)
    }

    fn apply(&mut self) -> Outcome {
        let mut collector = OutcomeCollector::new();
        let mut values = self.values.iter().collect::<Vec<_>>();
        values.sort_by_key(|(l, _)| l.node_id());

        let mut to_be_marked_as_bad: Vec<Block> = Vec::new();

        for (node, lattice) in values {
            let value = match lattice.value() {
                Val::Tarval(tarval) => tarval,
                _ => {
                    collector.push(Outcome::Unchanged);
                    continue;
                }
            };

            if Node::is_const(*node) {
                collector.push(Outcome::Unchanged);
                continue;
            }

            if let Ok(value_node) = try_as_value_node(*node) {
                let const_node = self.graph.new_const(*value);
                log::debug!("exchange value {:?} with {:?}", node, value);
                Graph::exchange_value(value_node, const_node);
                collector.push(Outcome::Changed);
            } else if let (Node::Cond(cond), TarvalKind::Bool(val)) = (node, value.kind()) {
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

                log::debug!(
                    "Replace {:?} with {:?} to {:?}",
                    always_taken_path,
                    jmp,
                    target_block
                );

                self.graph.mark_as_bad(dead_path);
                self.graph.mark_as_bad(*cond);

                Graph::exchange(always_taken_path, jmp);

                target_block.keep_alive();
                collector.push(Outcome::Changed);
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

        collector.result()
    }
}
