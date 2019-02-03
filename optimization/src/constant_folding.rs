use super::{lattices::*, Outcome, OutcomeCollector};
use crate::{dot::*, optimization};
use firm_construction::program_generator::Spans;
use libfirm_rs::{
    bindings,
    nodes::{try_as_value_node, Block, NewKind, Node, NodeTrait, ProjKind},
    Graph, Mode, Tarval, TarvalKind,
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
    values: HashMap<Node, ConstantFoldingLattice>,
    queue: PriorityQueue<Node, Priority>,
    node_topo_idx: HashMap<Node, Priority>,
    // this field is for debugging only.
    cur_node: Option<Node>,
    graph: Graph,
    start_block: Node,
    deps: HashMap<Node, HashSet<Node>>,
    node_update_count: usize,
}

impl optimization::Local for ConstantFolding {
    fn optimize_function(graph: Graph) -> Outcome {
        // Uncomment this code for debugging and prefix the method to debug with "cur_".

        /* if !graph.entity().name_string().contains("cur") {
            return Outcome::Unchanged;
        } */

        let mut constant_folding = ConstantFolding::new(graph);
        constant_folding.run();
        let result = constant_folding.apply();

        if cfg!(test) {
            check_asserts(graph);
        }

        result
    }
}

fn check_asserts(graph: Graph) {
    graph.walk(|node| {
        if let Some(call) = Node::as_call(*node) {
            let check_constant = if call
                .method_name()
                .map(|n| n.contains(&"assertNodesAreConst"))
                .unwrap_or(false)
            {
                Some(true)
            } else if call
                .method_name()
                .map(|n| n.contains(&"assertNodesAreNonConst"))
                .unwrap_or(false)
            {
                Some(false)
            } else {
                None
            };

            if let Some(assert_constant) = check_constant {
                for arg in call.args().skip(1) {
                    if Node::is_const(arg) && !assert_constant {
                        panic!(
                            "Node {:?}{} was asserted to be non-constant, but was constant",
                            arg,
                            Spans::span_str(arg),
                        )
                    } else if !Node::is_const(arg) && assert_constant {
                        panic!(
                            "Node {:?}{} was asserted to be constant, but was not",
                            arg,
                            Spans::span_str(arg),
                        )
                    }
                }
            }
        }
    });
}

impl ConstantFolding {
    fn new(graph: Graph) -> Self {
        let mut queue = PriorityQueue::new();

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
                let prio = *self
                    .node_topo_idx
                    .get(&$node)
                    .expect(&format!("{:?} have an topological order", $node));
                self.queue.push($node, prio);
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

                // todo: test only
                match updated_lattice.value() {
                    Val::Tarval(val) => assert_ne!(
                        cur_node.mode(),
                        Mode::P(),
                        "node: {:?}, val: {:?}",
                        cur_node,
                        val
                    ),
                    Val::Pointer(ptr) => assert_eq!(
                        cur_node.mode(),
                        Mode::P(),
                        "node: {:?}, {:?}",
                        cur_node,
                        ptr
                    ),
                    _ => {}
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

    fn node_to_idx(&self, idx_node: Node) -> Idx {
        match &self.lookup_val(idx_node) {
            Val::Tarval(val) if val.is_constant() => Idx::Const(val.get_long() as usize),
            _ => Idx::Node(idx_node),
        }
    }

    fn has_ptr_info(&self, ptr_node: Node) -> Option<&Pointer> {
        match &self.lookup_val(ptr_node) {
            Val::Pointer(ptr) => Some(ptr),
            Val::NoInfoYet => None,
            val => panic!("got unexpected {:?}", val),
        }
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
                let result_node = call.out_single_result();
                let mem_val = self.lookup_val(call.mem());
                let new_kind = call.new_kind();
                if new_kind.is_some() && result_node.is_none() {
                    Val::tuple(Val::Invalid, mem_val.clone())
                } else {
                    match (mem_val, new_kind) {
                        (Val::Heap(heap), Some(NewKind::Object(class_ty))) => {
                            let mut heap = (**heap).clone();
                            let ptr = heap.new_obj(result_node.unwrap(), class_ty);
                            Val::tuple(Val::Pointer(ptr), Val::Heap(Rc::new(heap)))
                        }
                        (Val::Heap(heap), Some(NewKind::Array { item_ty, .. })) => {
                            let mut heap = (**heap).clone();
                            let ptr = heap.new_arr(result_node.unwrap(), item_ty);
                            Val::tuple(Val::Pointer(ptr), Val::Heap(Rc::new(heap)))
                        }
                        // reset heap if an unknown method is called that could modify arbitrary
                        // memory.
                        (Val::Heap(heap), None) => {
                            let mut heap = (&**heap).clone();
                            let mut used_mem = MemoryArea::empty();
                            let mut used_nodes = HashSet::new();
                            for arg in call.args() {
                                if let Val::Pointer(ptr) = self.lookup_val(arg) {
                                    used_mem.join_mut(&ptr.target);
                                    used_nodes.insert(arg);
                                }
                            }

                            log::debug!(
                                "{:?} uses {:?} and {:?} as args",
                                call,
                                used_mem,
                                used_nodes
                            );
                            heap.reset_heap_accessed_by(used_mem, used_nodes);

                            Val::tuple(
                                call.single_result_ty()
                                    .map(|ty| heap.non_const_val(ty))
                                    .unwrap_or(Val::Invalid),
                                Val::Heap(Rc::new(heap.clone())),
                            )
                        }
                        (Val::NoInfoYet, _) => Val::NoInfoYet,
                        val => panic!("unreachable {:?}", val),
                    }
                }
            }
            Proj(_, Call_TResult(node)) => {
                // we have to wrap the result in a tuple, as modeT nodes cannot have a pointer
                // as value
                Val::tuple(self.lookup_val(node.into()).tuple_1().clone(), Val::Invalid)
            }
            Proj(_, Call_TResult_Arg(_, _, node)) => self.lookup_val(node.into()).tuple_1().clone(),
            Proj(_, Call_M(node)) => self.lookup_val(node.into()).tuple_2().clone(),

            Store(store) => match (store.ptr(), self.lookup_val(store.mem())) {
                (Node::Member(member), Val::Heap(heap)) => {
                    let ptr_node = member.ptr();
                    deps.push(ptr_node);
                    if let Some(ptr) = self.has_ptr_info(ptr_node) {
                        let mut heap = (**heap).clone();
                        let val = self.lookup_val(store.value());
                        heap.update_field(ptr_node, ptr, member.entity(), val);
                        Val::Heap(Rc::new(heap))
                    } else {
                        Val::NoInfoYet
                    }
                }
                (Node::Sel(sel), Val::Heap(heap)) => {
                    let ptr_node = sel.ptr();
                    deps.push(ptr_node);
                    if let Some(ptr) = self.has_ptr_info(ptr_node) {
                        let mut heap = (**heap).clone();
                        let val = self.lookup_val(store.value());
                        let idx = self.node_to_idx(sel.index());
                        heap.update_cell(ptr_node, ptr, idx, val, sel.element_ty());
                        Val::Heap(Rc::new(heap))
                    } else {
                        Val::NoInfoYet
                    }
                }
                (_, heap) => heap.clone(),
            },
            Proj(_, Store_M(store)) => self.lookup_val(store.into()).clone(),

            Load(load) => {
                let heap = &self.lookup_val(load.mem());
                match (load.ptr(), heap) {
                    (Node::Member(member), Val::Heap(heap)) => {
                        let ptr_node = member.ptr();
                        deps.push(ptr_node);
                        if let Some(ptr) = self.has_ptr_info(ptr_node) {
                            let mut heap = (**heap).clone();
                            let val = heap.lookup_field(ptr_node, ptr, member.entity());
                            Val::tuple(val, Val::Heap(Rc::new(heap)))
                        } else {
                            Val::NoInfoYet
                        }
                    }
                    (Node::Sel(sel), Val::Heap(heap)) => {
                        let ptr_node = sel.ptr();
                        deps.push(ptr_node);
                        if let Some(ptr) = self.has_ptr_info(ptr_node) {
                            let mut heap = (**heap).clone();
                            let idx = self.node_to_idx(sel.index());
                            let val = heap.lookup_cell(ptr_node, ptr, idx, sel.element_ty());
                            Val::tuple(val, Val::Heap(Rc::new(heap)))
                        } else {
                            Val::NoInfoYet
                        }
                    }
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
                    (Val::Pointer(ptr1), Val::Pointer(ptr2)) => {
                        let res = match (cmp.relation(), ptr1.eq(ptr2)) {
                            (bindings::ir_relation::Equal, Some(res)) => {
                                Val::Tarval(Tarval::bool_val(res))
                            }
                            (bindings::ir_relation::LessGreater, Some(res)) => {
                                Val::Tarval(Tarval::bool_val(!res))
                            }
                            _ => Val::Tarval(Tarval::bad()),
                        };
                        log::debug!("cmp {:?} with {:?} = {:?}", ptr1, ptr2, res);
                        res
                    }
                    val => panic!("unreachable {:?}", val),
                }
            }
            Cond(cond) => self.lookup_val(cond.selector()).clone(),
            Proj(_, Cond_Val(val, cond)) => {
                reachable = match &self.lookup_val(cond.into()) {
                    Val::Tarval(tarval) => !tarval.is_bool_val(!val) && reachable,
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

        for (&node, lattice) in values {
            let value = match lattice.value() {
                Val::Tarval(tarval) if !tarval.is_bad() => tarval,
                _ => {
                    collector.push(Outcome::Unchanged);
                    continue;
                }
            };

            if Node::is_const(node) {
                collector.push(Outcome::Unchanged);
                continue;
            }

            if try_as_value_node(node).is_ok() {
                let const_node = self.graph.new_const(*value);
                log::debug!("node original: {:?}", Node::wrap(node.internal_ir_node()));
                log::debug!("exchange value {:?} with {:?}", node, value);

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
                    Graph::exchange(node, const_node);
                }

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
                self.graph.mark_as_bad(cond);

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
