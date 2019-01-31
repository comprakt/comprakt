use super::{lattices::*, Outcome, OutcomeCollector};
use crate::{dot::*, optimization};
use libfirm_rs::{
    nodes::{try_as_value_node, Block, IsNewResult, Node, NodeTrait, ProjKind},
    Graph, Tarval, TarvalKind,
};
use priority_queue::PriorityQueue;
use std::{
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
    node_update_count: usize,
}

impl optimization::Local for ConstantFolding {
    fn optimize_function(graph: Graph) -> Outcome {
        if !graph.entity().name_string().starts_with("Main.M.cur_") {
            return Outcome::Unchanged;
        }

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
        graph.compute_doms();

        breakpoint!("Dominator information", graph, &|node: &Node| {
            let block = match node {
                Node::Block(block) => *block,
                _ => node.block(),
            };
            default_label(node).append(format!("\ndom_depth: {}", block.dom_depth()))
        });

        graph.walk_blkwise_dom_top_down(|node| {
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
                    .expect(&format!("{:?} have an topological order", $node));
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
            self.cur_node = Some(cur_node);
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
            label = label.append(format!(" dom: {}", node.block().dom_depth()));

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
                        self.lookup_val(arg),
                        if self.lookup(pred).reachable() {
                            ""
                        } else {
                            " [dead]"
                        }
                    ));
                }
            }

            if let Some((_node, priority)) = self.queue.get(&node) {
                label = label.style(Style::Bold);
                label = label.append(format!("\n{}", priority.topo_order));
            }

            label
        });
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
                        let result_node = call.out_single_result();
                        if let Some(result_node) = result_node {
                            let mut heap = (**heap).clone();
                            let ptr = heap.new_obj(result_node, class_ty);
                            Val::tuple(Val::Pointer(ptr), Val::Heap(Rc::new(heap)))
                        } else {
                            Val::tuple(Val::Invalid, Val::Heap(heap.clone()))
                        }
                    }
                    // reset heap if an unknown method is called that could modify arbitrary memory.
                    (Val::Heap(heap), IsNewResult::No) => {
                        /*let mut heap = (&**heap).clone();
                        let mut used_ptrs = PointerSet::new_empty();
                        let mut used_ptr_nodes = HashSet::new();
                        for arg in call.args() {
                            match self.lookup_val(arg) {
                                Val::Pointer(ptrs) => {
                                    used_ptrs = used_ptrs.join(
                                        ptrs,
                                        JoinContext {
                                            dom_depth: call.block().dom_depth(),
                                        },
                                    );
                                    used_ptr_nodes.insert(arg);
                                }
                                _ => {}
                            }
                        }

                        log::debug!(
                            "{:?} uses {:?} and {:?} as args",
                            call,
                            used_ptrs,
                            used_ptr_nodes
                        );
                        heap.reset_heap_accessed_by(used_ptrs, used_ptr_nodes);

                        let val = if let Some(res) = call.out_single_result() {
                            heap.non_const_val(res.mode())
                        } else {
                            // result is not used
                            Val::Invalid
                        };*/

                        Val::tuple(
                            call.single_result_ty()
                                .map(|ty| heap.non_const_val(ty))
                                .unwrap_or(Val::Invalid),
                            Val::Heap(heap.clone()),
                        )
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
                    let ptr_val = member.ptr();
                    let ptr = match &self.lookup_val(ptr_val) {
                        Val::Pointer(ptr) => ptr,
                        _ => panic!("unreach"),
                    };
                    let mut heap = (**heap).clone();
                    let val = self.lookup_val(store.value());
                    heap.update_field(ptr_val, ptr, field, val);
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
                        let ptr_val = member.ptr();
                        let ptr = match &self.lookup_val(ptr_val) {
                            Val::Pointer(ptr) => ptr,
                            _ => panic!("unreach"),
                        };
                        let mut heap = (**heap).clone();
                        let val = heap.lookup_field(ptr_val, ptr, field);
                        Val::tuple(val, Val::Heap(Rc::new(heap)))
                    }
                    (_, Val::Heap(heap)) => Val::tuple(
                        panic!("todo"),
                        //heap.non_const_val(cur_node),
                        Val::Heap(Rc::clone(heap)),
                    ),
                    (_, _) => Val::tuple(Val::NoInfoYet, Val::NoInfoYet),
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
            Cmp(cmp) => {
                let left_val = self.lookup_val(cmp.left());
                let right_val = self.lookup_val(cmp.right());

                match (left_val, right_val) {
                    (Val::Tarval(t1), Val::Tarval(t2)) => {
                        Val::Tarval(t1.lattice_cmp(cmp.relation(), *t2))
                    }
                    (Val::NoInfoYet, _) => Val::NoInfoYet,
                    (_, Val::NoInfoYet) => Val::NoInfoYet,
                    (Val::Pointer(ptrs1), Val::Pointer(ptrs2)) => {
                        log::debug!("cmp {:?} with {:?}", ptrs1, ptrs2);
                        //Tarval::bool_val()
                        Val::Tarval(Tarval::bad())
                    }
                    val => panic!("unreachable {:?}", val),
                }
            }
            Cond(cond) => self.lookup_val(cond.selector()).clone(),
            Proj(_, Cond_Val(val, cond)) => {
                reachable = match &self.lookup_val(cond.into()) {
                    Val::Tarval(tarval) => !tarval.is_bool_val(!val),
                    _ => false,
                };
                Val::Invalid
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
                    for arg in value_node.value_nodes() {
                        let val = self.lookup_val(arg.as_node());
                        match val {
                            Val::NoInfoYet => no_info = true,
                            Val::Tarval(val) => {
                                if val.is_bad() {
                                    non_constant = true;
                                } else {
                                    tarval_args.push(*val)
                                }
                            }
                            Val::Pointer(..) | Val::Heap(..) | Val::Tuple(..) | Val::Invalid => {
                                panic!(
                                    "{:?} from {:?} is not a \
                                     valid argument to a value node except for cmp",
                                    val,
                                    arg.as_node(),
                                )
                            }
                        }
                    }
                    if non_constant {
                        Val::from_tarval_initially(Tarval::bad(), cur_node.mode())
                    } else if no_info {
                        Val::NoInfoYet
                    } else {
                        let tarval = value_node.compute(tarval_args);
                        Val::from_tarval_initially(tarval, cur_node.mode())
                    }
                } else {
                    Val::Invalid
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
                Val::Tarval(tarval) if !tarval.is_bad() => tarval,
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
