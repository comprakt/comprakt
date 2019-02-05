use super::{lattices::*, Outcome};
use crate::{dot::*, optimization};
use firm_construction::program_generator::Spans;
use libfirm_rs::{
    bindings,
    nodes::{try_as_value_node, Block, NewKind, Node, NodeTrait, ProjKind},
    types::Ty,
    Entity, Graph, Mode, Tarval, TarvalKind,
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
        if let Ok(filter) = std::env::var("FILTER_CONSTANT_FOLDING_METHOD") {
            // for debugging
            if graph.entity().name_string().contains(&filter) {
                return Outcome::Unchanged;
            }
        }

        let mut constant_folding = ConstantFolding::new(graph);
        constant_folding.run();
        let result = constant_folding.apply();

        if cfg!(test) {
            check_asserts(graph);
        }

        result
    }
}

#[derive(Clone, Copy, Debug)]
enum AssertNodesAre {
    Const(bool),
    Eq(bool),
}

fn check_asserts(graph: Graph) {
    log::debug!("Checking {}", graph.entity().name_string());
    breakpoint!("Graph", graph, &|node: &Node| default_label(node)
        .append(Spans::span_str(*node)));

    graph.walk(|node| {
        let _res = (|| -> Result<(), std::option::NoneError> {
            let call = Node::as_call(*node)?;
            let method_name = call.method_name()?;

            let assert_type = if method_name.contains(&"assertNodesAreConst") {
                Some(AssertNodesAre::Const(true))
            } else if method_name.contains(&"assertNodesAreNonConst") {
                Some(AssertNodesAre::Const(false))
            } else if method_name.contains(&"assertNodesAreEq") {
                Some(AssertNodesAre::Eq(true))
            } else if method_name.contains(&"assertNodesAreNotEq") {
                Some(AssertNodesAre::Eq(false))
            } else {
                None
            };

            match assert_type? {
                AssertNodesAre::Const(assert_const) => {
                    for arg in call.args().skip(1) {
                        if Node::is_const(arg) && !assert_const {
                            panic!(
                                "Node {:?}{} was asserted to be non-constant, but was constant",
                                arg,
                                Spans::span_str(arg),
                            )
                        } else if !Node::is_const(arg) && assert_const {
                            panic!(
                                "Node {:?}{} was asserted to be constant, but was not",
                                arg,
                                Spans::span_str(arg),
                            )
                        }
                    }
                }
                AssertNodesAre::Eq(assert_eq) => {
                    let set: HashSet<_> = call.args().skip(1).collect();

                    if assert_eq && set.len() > 1 {
                        let mut iter = set.iter();
                        let (first, second) = (iter.next().unwrap(), iter.next().unwrap());
                        panic!(
                            "Node {:?}{} was asserted to equal {:?}{}, but it wasn't",
                            first,
                            Spans::span_str(*first),
                            second,
                            Spans::span_str(*second),
                        )
                    }
                    if !assert_eq && set.len() != call.args().len() - 1 {
                        panic!(
                            "Some nodes were equal in {:?}{}",
                            call,
                            Spans::span_str(call),
                        )
                    }
                }
            }

            Ok(())
        })();
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
                    Val::NodeValue(NodeValue::Tarval(val), _) => assert_ne!(
                        cur_node.mode(),
                        Mode::P(),
                        "node: {:?}, val: {:?}",
                        cur_node,
                        val
                    ),
                    Val::NodeValue(NodeValue::Pointer(ptr), _) => assert_eq!(
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

    #[allow(clippy::cyclomatic_complexity)]
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
                let mem_val = self.lookup_val(call.mem());
                match (mem_val, call.new_kind(), call.out_single_result()) {
                    (Val::NoInfoYet, _, _) => Val::NoInfoYet,
                    (mem_val, Some(_new_kind), None) => Val::tuple(Val::Invalid, mem_val.clone()),

                    (Val::Heap(heap), Some(new_kind), result_node) => {
                        let mut heap = (**heap).clone();

                        let result = if let Some(result_node) = result_node {
                            let ptr = match new_kind {
                                NewKind::Object(class_ty) => heap.new_obj(result_node, class_ty),
                                NewKind::Array { item_ty, .. } => {
                                    heap.new_arr(result_node, item_ty)
                                }
                            };
                            Val::NodeValue(ptr.into(), Some(result_node))
                        } else {
                            Val::Invalid
                        };

                        Val::tuple(result, Val::Heap(Rc::new(heap)))
                    }

                    // reset heap if an unknown method is called
                    // which could modify arbitrary memory.
                    (Val::Heap(heap), None, _result_node) => {
                        let mut used_mem = MemoryArea::empty();
                        let mut used_nodes = HashSet::new();
                        for arg in call.args() {
                            if let Val::NodeValue(NodeValue::Pointer(ptr), _) = self.lookup_val(arg)
                            {
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
                        let heap = heap.reset_heap_accessed_by(used_mem, used_nodes);

                        Val::tuple(
                            call.single_result_ty()
                                .map(|ty| heap.non_const_val(ty))
                                .unwrap_or(Val::Invalid),
                            Val::Heap(Rc::new(heap)),
                        )
                    }
                    val => panic!("unreachable {:?}", val),
                }
            }
            Proj(_, Call_TResult(node)) => {
                // we have to wrap the result in a tuple, as modeT nodes cannot have a pointer
                // as value
                Val::tuple(self.lookup_val(node.into()).tuple_1().clone(), Val::Invalid)
            }
            Proj(_, Call_TResult_Arg(_, _, node)) => self.lookup_val(node.into()).tuple_1().clone(),
            Proj(_, Call_M(node)) => self.lookup_val(node.into()).tuple_2().clone(),

            cur_node @ Store(_) | cur_node @ Load(_) => {
                enum TK {
                    ArrItem(Idx, Ty),
                    ObjField(Entity),
                }

                let (raw_ptr_node, mem) = match cur_node {
                    Store(store) => (store.ptr(), store.mem()),
                    Load(load) => (load.ptr(), load.mem()),
                    _ => panic!("unreach"),
                };

                let (ptr_node, target_kind) = match raw_ptr_node {
                    Member(member) => (member.ptr(), TK::ObjField(member.entity())),
                    Sel(sel) => (
                        sel.ptr(),
                        TK::ArrItem(
                            match &self.lookup_val(sel.index()) {
                                Val::NodeValue(NodeValue::Tarval(val), _) if val.is_constant() => {
                                    Idx::Const(val.get_long() as usize)
                                }
                                Val::NodeValue(_, Some(source_idx_node)) => {
                                    Idx::Node(*source_idx_node)
                                }
                                _ => Idx::Node(sel.index()),
                            },
                            sel.element_ty(),
                        ),
                    ),
                    _ => panic!("unreach"),
                };

                deps.push(ptr_node);

                match (self.lookup_val(mem), self.lookup_val(ptr_node)) {
                    (Val::NoInfoYet, _) => Val::NoInfoYet,
                    (_, Val::NoInfoYet) => Val::NoInfoYet,
                    (Val::Heap(heap), Val::NodeValue(NodeValue::Pointer(ptr), source_ptr_node)) => {
                        let o = source_ptr_node.unwrap_or(ptr_node);
                        let mut heap = (**heap).clone();

                        match cur_node {
                            Store(store) => {
                                let val = self.lookup_val(store.value());
                                match target_kind {
                                    TK::ArrItem(idx, ty) => heap.update_cell(o, ptr, idx, val, ty),
                                    TK::ObjField(entity) => heap.update_field(o, ptr, entity, val),
                                }
                                Val::Heap(Rc::new(heap))
                            }
                            Load(load) => {
                                let val = match target_kind {
                                    TK::ArrItem(idx, ty) => heap.lookup_cell(o, ptr, idx, ty),
                                    TK::ObjField(entity) => heap.lookup_field(o, ptr, entity),
                                };
                                let val = match (val, load.out_proj_res()) {
                                    (Val::NodeValue(val, None), Some(res)) => {
                                        let v = Val::NodeValue(val, Some(res.into()));
                                        match target_kind {
                                            TK::ArrItem(idx, ty) => {
                                                heap.enhance_cell(o, ptr, idx, &v, ty)
                                            }
                                            TK::ObjField(entity) => {
                                                heap.enhance_field(o, ptr, entity, &v)
                                            }
                                        }
                                        v
                                    }
                                    (Val::NodeValue(val, node), _) => Val::NodeValue(val, node),
                                    (Val::NoInfoYet, _) => Val::NoInfoYet,
                                    _ => panic!("unreach"),
                                };

                                Val::tuple(val, Val::Heap(Rc::new(heap)))
                            }
                            _ => panic!("unreach"),
                        }
                    }
                    (heap, val) => panic!("unreach {:?} {:?}", heap, val),
                }
            }

            Proj(_, Store_M(store)) => self.lookup_val(store.into()).clone(),

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
                #[derive(Clone, Copy)]
                enum SimpleRelation {
                    Equal,
                    NotEqual,
                }
                fn as_simple_relation(
                    relation: bindings::ir_relation::Type,
                ) -> Option<SimpleRelation> {
                    match relation {
                        bindings::ir_relation::Equal => Some(SimpleRelation::Equal),
                        bindings::ir_relation::LessGreater => Some(SimpleRelation::NotEqual),
                        _ => None,
                    }
                }

                enum CmpResult {
                    Bool(bool),
                    NoInfoYet,
                    Bad,
                    Tarval(Tarval),
                }

                let left_val = self.lookup_val(cmp.left()).expect_node_value_or_no_info();
                let right_val = self.lookup_val(cmp.right()).expect_node_value_or_no_info();

                let result = match (left_val, right_val) {
                    (None, _) => CmpResult::NoInfoYet,
                    (_, None) => CmpResult::NoInfoYet,
                    (Some((val1, node1)), Some((val2, node2))) => {
                        match (val1, val2, as_simple_relation(cmp.relation())) {
                            (_, _, Some(simple_rel)) if node1.is_some() && node1 == node2 => {
                                match simple_rel {
                                    SimpleRelation::Equal => CmpResult::Bool(true),
                                    SimpleRelation::NotEqual => CmpResult::Bool(false),
                                }
                            }
                            (NodeValue::Pointer(ptr1), NodeValue::Pointer(ptr2), rel) => {
                                match (rel, ptr1.eq(ptr2)) {
                                    (Some(SimpleRelation::Equal), Some(res)) => {
                                        CmpResult::Bool(res)
                                    }
                                    (Some(SimpleRelation::NotEqual), Some(res)) => {
                                        CmpResult::Bool(!res)
                                    }
                                    _ => CmpResult::Bad,
                                }
                            }
                            (NodeValue::Tarval(t1), NodeValue::Tarval(t2), _) => {
                                CmpResult::Tarval(t1.lattice_cmp(cmp.relation(), *t2))
                            }
                            (val1, val2, _) => panic!("unreachable {:?}, {:?}", val1, val2),
                        }
                    }
                };

                let tarval = match result {
                    CmpResult::Tarval(val) => val,
                    CmpResult::Bool(val) => Tarval::bool_val(val),
                    CmpResult::Bad => Tarval::bad(),
                    CmpResult::NoInfoYet => Tarval::unknown(),
                };
                Val::from_tarval_initially(tarval, Mode::b(), None)
            }
            Cond(cond) => self.lookup_val(cond.selector()).clone(),
            Proj(_, Cond_Val(val, cond)) => {
                reachable = match &self.lookup_val(cond.into()) {
                    Val::NodeValue(NodeValue::Tarval(tarval), _) => {
                        reachable && !tarval.is_bool_val(!val)
                    }
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
                            match new_val {
                                Val::NodeValue(val, node) => {
                                    Val::NodeValue(val, Some(node.unwrap_or_else(|| phi.into())))
                                }
                                val => val,
                            }
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
                            Val::NodeValue(NodeValue::Tarval(val), _) => {
                                if val.is_bad() {
                                    non_constant = true;
                                } else {
                                    tarval_args.push(*val)
                                }
                            }
                            Val::NodeValue(NodeValue::Pointer(..), _)
                            | Val::Heap(..)
                            | Val::Tuple(..)
                            | Val::Invalid => panic!(
                                "{:?} from {:?} is not a \
                                 valid argument to a value node except for cmp",
                                val,
                                arg.as_node(),
                            ),
                        }
                    }
                    if non_constant {
                        Val::from_tarval_initially(Tarval::bad(), cur_node.mode(), Some(cur_node))
                    } else if no_info {
                        Val::NoInfoYet
                    } else {
                        let tarval = value_node.compute(tarval_args);
                        Val::from_tarval_initially(tarval, cur_node.mode(), Some(cur_node))
                    }
                } else {
                    Val::Invalid
                }
            }
        };

        (ConstantFoldingLattice::new(reachable, value), deps)
    }

    fn apply(&mut self) -> Outcome {
        let mut values = self.values.iter().collect::<Vec<_>>();
        values.sort_by_key(|(l, _)| l.node_id());

        let mut to_be_marked_as_bad: Vec<Block> = Vec::new();
        let mut outcome = Outcome::Unchanged;

        for (&node, lattice) in values {
            if Node::is_const(node) {
                continue;
            }

            let (value, source_node) = match lattice.value() {
                Val::NodeValue(NodeValue::Tarval(tarval), source_node) => (*tarval, *source_node),
                Val::NodeValue(NodeValue::Pointer(ptr), source_node) => (
                    if ptr.is_null() {
                        Tarval::zero(Mode::P())
                    } else {
                        Tarval::bad()
                    },
                    *source_node,
                ),
                _ => continue,
            };

            if try_as_value_node(node).is_ok() {
                let new_node = if value.is_constant() {
                    let const_node = self.graph.new_const(value);
                    Spans::copy_span(const_node, node);
                    const_node.into()
                } else if let Some(source_node) = source_node {
                    if source_node.block().dominates(node.block()) {
                        source_node
                    } else {
                        continue;
                    }
                } else {
                    continue;
                };

                if new_node == node {
                    continue;
                }

                log::debug!("exchange value {:?} with {:?}", node, new_node);

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

            outcome = Outcome::Changed;
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

        outcome
    }
}
