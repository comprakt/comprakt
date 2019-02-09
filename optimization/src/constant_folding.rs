use super::{lattices::*, Outcome};
use crate::{dot::*, optimization};
use firm_construction::program_generator::Spans;
use libfirm_rs::{
    bindings,
    nodes::{try_as_value_node, Block, NewKind, Node, NodeDebug, NodeTrait, ProjKind, Store},
    types::{Ty, TyTrait},
    Entity, Graph, Mode, Tarval, TarvalKind,
};
use priority_queue::PriorityQueue;
use std::{
    collections::{HashMap, HashSet},
    fmt::Write,
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
    // lattice per each node
    values: HashMap<Node, ConstantFoldingLattice>,
    // global lattice, tracks which stores cannot be removed
    required_stores: HashSet<Store>,
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
        if cfg!(debug_assertions) {
            if let Ok(filter) = std::env::var("FILTER_CONSTANT_FOLDING_METHOD") {
                // for debugging
                if !graph.entity().name_string().contains(&filter) {
                    return Outcome::Unchanged;
                }
            }
        }

        let mut constant_folding = ConstantFolding::new(graph);
        constant_folding.run();
        let result = constant_folding.apply();

        if cfg!(debug_assertions) {
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
    /*breakpoint!("Graph", graph, &|node: &Node| default_label(node)
    .append(Spans::span_str(*node)));*/

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
            required_stores: HashSet::new(),
        }
    }

    // used for debugging
    #[allow(clippy::single_match)]
    fn debug_data(&self) -> String {
        if let Some(node) = self.cur_node {
            if let Some(span) = Spans::lookup_span(node) {
                let mut result = String::new();
                write!(
                    &mut result,
                    "highlight-line:{},{},{},{}",
                    span.start_position().line_number(),
                    span.start_position().column() + 1,
                    span.end_position().line_number(),
                    span.end_position().column() + 1,
                )
                .unwrap();

                let mem = match node {
                    Node::Load(load) => load.mem(),
                    Node::Store(store) => store.mem(),
                    Node::Call(call) => call.mem(),

                    Node::Proj(proj, ProjKind::Store_M(_))
                    | Node::Proj(proj, ProjKind::Load_M(_))
                    | Node::Proj(proj, ProjKind::Call_M(_)) => proj.pred(),
                    _ => node,
                };

                let val = self.lookup_lat(mem);
                let val = match val {
                    NodeLattice::Tuple(_a, b) => &b,
                    val => val,
                };

                let mut text = HashMap::new();
                match val {
                    NodeLattice::Heap(heap) => {
                        for (node, info) in &heap.array_infos {
                            if let InfoIdx::Node(node) = node {
                                text.insert(*node, format!("{:?}", info));
                            }
                        }
                        for (node, info) in &heap.object_infos {
                            if let InfoIdx::Node(node) = node {
                                text.insert(*node, format!("{:?}", info));
                            }
                        }
                    }
                    _ => {}
                }

                for (n, val) in &self.values {
                    match val.value() {
                        NodeLattice::Value(val) if !Node::is_const(*n) => {
                            if let Some(span) = Spans::lookup_span(*n) {
                                write!(
                                    &mut result,
                                    "\n{}:{}: {:?}{}",
                                    span.start_position().line_number(),
                                    (*n).debug_fmt().short(true),
                                    val,
                                    if let Some(t) = text.get(n) {
                                        " |  ".to_owned() + t
                                    } else {
                                        "".to_owned()
                                    }
                                )
                                .unwrap();
                            }
                        }
                        _ => {}
                    }
                }

                return result;
            }
        }
        "None".to_owned()
    }

    fn lookup(&self, node: Node) -> &ConstantFoldingLattice {
        &self.values[&node]
    }

    fn lookup_lat(&self, node: Node) -> &NodeLattice {
        self.lookup(node).value()
    }

    fn lookup_val(&self, node: Node) -> Option<&NodeValue> {
        self.lookup(node).value().expect_value_or_no_info()
    }

    fn update(&mut self, node: Node, new: ConstantFoldingLattice) {
        self.values.insert(node, new).unwrap();
    }

    fn run(&mut self) {
        log::info!(
            "Run constant folding on {}",
            self.graph.entity().name_string()
        );

        self.debug_data();
        macro_rules! invalidate {
            ($node: expr) => {
                let prio = *self
                    .node_topo_idx
                    .get(&$node)
                    .expect(&format!("{:?} have an topological order", $node));
                self.queue.push($node, prio);
            };
        }

        let mut required_stores = HashSet::new();

        let mut deps = Vec::new();

        while let Some((cur_node, _priority)) = self.queue.pop() {
            self.cur_node = Some(cur_node);
            self.node_update_count += 1;
            let cur_lattice = self.lookup(cur_node);
            let updated_lattice =
                self.update_node(cur_node, cur_lattice, &mut deps, &mut required_stores);
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

        self.required_stores = required_stores;
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
                        self.lookup_lat(arg),
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
        deps: &mut Vec<Node>,
        required_stores: &mut HashSet<Store>,
    ) -> ConstantFoldingLattice {
        self.breakpoint(cur_node);

        // TODO implement for returns!
        let mut mark_stores_as_required = |stores: &HashSet<self::Store>| {
            log::debug!(
                "Mark stores as required: {:?}",
                stores
                    .iter()
                    .map(|s| format!("{:?}{}", s, Spans::span_str(*s)))
                    .collect::<Vec<_>>()
                    .join(","),
            );
            required_stores.extend(stores);
        };

        let mut reachable = cur_lattice.reachable()
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
            return cur_lattice.clone();
        }

        use self::{Node::*, ProjKind::*};
        let value = match cur_node {
            // == Load-Store optimizations ==
            Proj(_, Start_M(_)) => NodeLattice::Heap(Rc::new(Heap::start())),

            Call(call) => {
                let mem_val = self.lookup_lat(call.mem());
                match (mem_val, call.new_kind(), call.out_single_result()) {
                    (NodeLattice::NoInfoYet, _, _) => NodeLattice::NoInfoYet,
                    (mem_val, Some(_new_kind), None) => {
                        NodeLattice::tuple(NodeLattice::Invalid, mem_val.clone())
                    }

                    (NodeLattice::Heap(heap), Some(new_kind), result_node) => {
                        let mut heap = (**heap).clone();

                        let result = if let Some(result_node) = result_node {
                            let ptr = match new_kind {
                                NewKind::Object(class_ty) => heap.new_obj(result_node, class_ty),
                                NewKind::Array { item_ty, .. } => {
                                    heap.new_arr(result_node, item_ty)
                                }
                            };
                            NodeValue::new(ptr.into(), Some(result_node)).into()
                        } else {
                            NodeLattice::Invalid
                        };

                        NodeLattice::tuple(result, NodeLattice::Heap(Rc::new(heap)))
                    }

                    // reset heap if an unknown method is called
                    // which could modify arbitrary memory.
                    (NodeLattice::Heap(heap), None, _result_node) => {
                        let mut used_mem = MemoryArea::empty();
                        let mut used_nodes = HashSet::new();
                        for arg in call.args() {
                            if let Some(val) = self.lookup_val(arg) {
                                if let Some(ptr) = val.as_pointer() {
                                    used_mem.join_mut(&ptr.target);
                                    used_nodes.insert(arg);
                                }
                            }
                        }

                        log::debug!(
                            "{:?} uses {:?} and {:?} as args",
                            call,
                            used_mem,
                            used_nodes
                        );
                        let accessible_mem = heap.mem_reachable_from(used_mem);

                        let stores = &heap.last_stores_into(&accessible_mem);
                        mark_stores_as_required(&stores);

                        let heap = heap.reset_mem(&accessible_mem);

                        NodeLattice::tuple(
                            call.single_result_ty()
                                .map(|ty| {
                                    NodeValue::non_const_val(ty.mode(), accessible_mem).into()
                                })
                                .unwrap_or(NodeLattice::Invalid),
                            NodeLattice::Heap(Rc::new(heap)),
                        )
                    }
                    val => panic!("unreachable {:?}", val),
                }
            }
            Proj(_, Call_TResult(node)) => {
                // we have to wrap the result in a tuple, as modeT nodes cannot have a pointer
                // as value
                NodeLattice::tuple(
                    self.lookup_lat(node.into()).tuple_1().clone(),
                    NodeLattice::Invalid,
                )
            }
            Proj(_, Call_TResult_Arg(_, _, node)) => self.lookup_lat(node.into()).tuple_1().clone(),
            Proj(_, Call_M(node)) => self.lookup_lat(node.into()).tuple_2().clone(),

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
                    Sel(sel) => {
                        // as sel.index() is not a direct predecessor, we need to put it on deps
                        deps.push(sel.index());
                        (
                            sel.ptr(),
                            TK::ArrItem(
                                match &self.lookup_val(sel.index()) {
                                    Some(val) if val.is_tarval() => {
                                        let idx_val = val.tarval();
                                        let idx_source = val.source_or_some(sel.index());
                                        if idx_val.is_constant() {
                                            Idx::Const(idx_val.get_long() as usize, idx_source)
                                        } else {
                                            Idx::Dynamic(idx_source)
                                        }
                                    }
                                    Some(_) => panic!("unreach"),
                                    None => return cur_lattice.clone(),
                                },
                                sel.element_ty(),
                            ),
                        )
                    }
                    _ => panic!("unreach"),
                };

                // as ptr_node is not a direct predecessor, we need to put it on deps
                deps.push(ptr_node);

                match (self.lookup_lat(mem), self.lookup_val(ptr_node)) {
                    (NodeLattice::NoInfoYet, _) => NodeLattice::NoInfoYet,
                    (_, None) => NodeLattice::NoInfoYet,
                    (NodeLattice::Heap(heap), Some(ptr_val)) if ptr_val.is_pointer() => {
                        let o = ptr_val.source_or_some(ptr_node);
                        let ptr = ptr_val.as_pointer().unwrap();
                        if ptr.is_null_or_empty() {
                            // we would crash on such a `ptr` anyways, so wait for more info.
                            return cur_lattice.clone();
                        }
                        let mut heap = (**heap).clone();

                        match cur_node {
                            Store(store) => {
                                let val = self.lookup_val(store.value());
                                if val.is_none() {
                                    return cur_lattice.clone();
                                }
                                let val =
                                    ValWithStoreInfo::single_store(val.unwrap().clone(), store);

                                match target_kind {
                                    TK::ArrItem(idx, ty) => heap.update_cell(o, ptr, idx, &val, ty),
                                    TK::ObjField(entity) => heap.update_field(o, ptr, entity, &val),
                                }

                                NodeLattice::Heap(Rc::new(heap))
                            }
                            Load(load) => {
                                let val = match target_kind {
                                    TK::ArrItem(idx, ty) => heap.lookup_cell(o, ptr, idx, ty),
                                    TK::ObjField(entity) => heap.lookup_field(o, ptr, entity),
                                };
                                let ValWithStoreInfo { val, stores } = if let Some(val) = val {
                                    val
                                } else {
                                    return cur_lattice.clone();
                                };

                                if val.source.is_none() && val.tarval().is_bad() {
                                    mark_stores_as_required(&stores);
                                }

                                let val = match load.out_proj_res() {
                                    Some(res) if val.source.is_none() => {
                                        let val = val.into_updated_source(res.into());
                                        let val = ValWithStoreInfo { val, stores };
                                        match target_kind {
                                            TK::ArrItem(idx, ty) => {
                                                heap.enhance_cell(o, ptr, idx, &val, ty)
                                            }
                                            TK::ObjField(entity) => {
                                                heap.enhance_field(o, ptr, entity, &val)
                                            }
                                        }
                                        val.val
                                    }
                                    _ => val,
                                };
                                // TODO implement into for heap
                                NodeLattice::tuple(val.into(), NodeLattice::Heap(Rc::new(heap)))
                            }
                            _ => panic!("unreach"),
                        }
                    }
                    (heap, val) => panic!("unreach {:?} {:?}", heap, val),
                }
            }

            Proj(_, Store_M(store)) => self.lookup_lat(store.into()).clone(),

            Proj(_, Load_Res(node)) => self.lookup_lat(node.into()).tuple_1().clone(),
            Proj(_, Load_M(node)) => self.lookup_lat(node.into()).tuple_2().clone(),

            Proj(_, Div_M(node)) => {
                deps.push(node.mem());
                self.lookup_lat(node.mem()).clone()
            }
            Proj(_, Mod_M(node)) => {
                deps.push(node.mem());
                self.lookup_lat(node.mem()).clone()
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

                let left_val = self.lookup_val(cmp.left());
                let right_val = self.lookup_val(cmp.right());

                let result = match (left_val, right_val) {
                    (None, _) => CmpResult::NoInfoYet,
                    (_, None) => CmpResult::NoInfoYet,
                    (Some(val1), Some(val2)) => {
                        match (&val1.value, &val2.value, as_simple_relation(cmp.relation())) {
                            (_, _, Some(simple_rel))
                                if val1.source.is_some() && val1.source == val2.source =>
                            {
                                // we have `node == node` or `node != node`
                                match simple_rel {
                                    SimpleRelation::Equal => CmpResult::Bool(true),
                                    SimpleRelation::NotEqual => CmpResult::Bool(false),
                                }
                            }
                            (AbstractValue::Pointer(ptr1), AbstractValue::Pointer(ptr2), rel) => {
                                match (rel, ptr1.eq(ptr2)) {
                                    (Some(SimpleRelation::Equal), Some(res)) => {
                                        // e.g. p1 == p2
                                        // with p1 -> {obj1,null} and p2 -> {@obj2}
                                        CmpResult::Bool(res)
                                    }
                                    (Some(SimpleRelation::NotEqual), Some(res)) => {
                                        // e.g. p1 != p2
                                        // with p1 -> {obj1,obj3} and p2 -> {obj2,null}
                                        CmpResult::Bool(!res)
                                    }
                                    _ => CmpResult::Bad,
                                }
                            }
                            (AbstractValue::Tarval(t1), AbstractValue::Tarval(t2), _) => {
                                if t1.is_bad() || t2.is_bad() {
                                    CmpResult::Tarval(Tarval::bad())
                                } else {
                                    CmpResult::Tarval(t1.lattice_cmp(cmp.relation(), *t2))
                                }
                            }
                            (v1, v2, _) => panic!(
                                "Cannot compare values with invalid types: {:?}, {:?}",
                                v1, v2,
                            ),
                        }
                    }
                };

                let tarval = match result {
                    CmpResult::Tarval(val) => val,
                    CmpResult::Bool(val) => Tarval::bool_val(val),
                    CmpResult::Bad => Tarval::bad(),
                    CmpResult::NoInfoYet => Tarval::unknown(),
                };
                NodeLattice::from_tarval(tarval, Mode::b())
            }
            Cond(cond) => self.lookup_lat(cond.selector()).clone(),
            Proj(_, Cond_Val(is_true_branch, cond)) => {
                reachable = reachable
                    && if let Some(val) = &self.lookup_val(cond.into()) {
                        val.tarval().is_bool_val(is_true_branch) || val.tarval().is_bad()
                    } else {
                        false // no info for cond yet
                    };
                NodeLattice::Invalid
            }

            // == Phi ==
            Phi(phi) => {
                if phi.in_nodes().len() != 2 {
                    log::warn!("phi pred count: {}", phi.in_nodes().len());
                }
                phi.in_nodes().zip(phi.block().in_nodes()).fold(
                    NodeLattice::NoInfoYet,
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
                            let pred_lat = &self.lookup_lat(pred);
                            let new_lat = val.join(pred_lat);
                            log::debug!(
                                "for {:?}; pred_val: {:?} -> val: {:?}",
                                pred,
                                pred_lat,
                                new_lat
                            );
                            match new_lat {
                                NodeLattice::Value(val) => {
                                    val.into_updated_source(phi.as_node()).into()
                                }
                                lat => lat,
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
                            None => no_info = true,
                            Some(val) if val.is_tarval() => match &val.value {
                                AbstractValue::Pointer(..) => panic!("unreach"),
                                AbstractValue::Tarval(val) => {
                                    if val.is_bad() {
                                        non_constant = true;
                                    } else {
                                        tarval_args.push(*val)
                                    }
                                }
                            },
                            // todo optimize code
                            Some(_) => panic!(
                                "Cannot use {:?} from {:?} - pointer are not valid here",
                                val,
                                arg.as_node(),
                            ),
                        }
                    }
                    if non_constant {
                        NodeValue::non_const_node(cur_node).into()
                    } else if no_info {
                        NodeLattice::NoInfoYet
                    } else {
                        let tarval = value_node.compute(tarval_args);
                        NodeLattice::from_tarval_node(tarval, cur_node)
                    }
                } else {
                    NodeLattice::Invalid
                }
            }
        };

        ConstantFoldingLattice::new(reachable, value)
    }

    #[allow(clippy::cyclomatic_complexity)]
    fn apply(&mut self) -> Outcome {
        let mut values = self.values.iter().collect::<Vec<_>>();
        values.sort_by_key(|(l, _)| l.node_id());

        let mut to_be_marked_as_bad: Vec<Block> = Vec::new();
        let mut folded_constants = 0;
        let mut optimized_loads = 0;
        let mut optimized_conds = 0;

        for (&node, lattice) in values {
            if Node::is_const(node) {
                continue;
            }

            let (value, source_node) = if let NodeLattice::Value(val) = lattice.value() {
                (val.tarval(), val.source)
            } else {
                continue;
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

                if Node::is_add(new_node) {
                    log::warn!("Skip add {:?}", new_node);
                    continue;
                }

                if let Node::Proj(_, ProjKind::Load_Res(_)) = node {
                    optimized_loads += 1;
                } else {
                    folded_constants += 1;
                }

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

                if let Node::Proj(_, ProjKind::Load_Res(load)) = node {
                    let prev_mem = load.mem();
                    if let Some(next_mem) = load.out_proj_m() {
                        log::debug!("Remove load from memory flow",);
                        Graph::exchange(next_mem, prev_mem);
                    }
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
            "Optimized {:>3} constants, {:>3} loads and {:>3} conds \
             with {:>4} node updates and {:>4} total nodes in graph {}",
            folded_constants,
            optimized_loads,
            optimized_conds,
            self.node_update_count,
            self.node_topo_idx.len(),
            self.graph.entity().name_string(),
        );

        if folded_constants + optimized_loads + optimized_conds > 0 {
            Outcome::Changed
        } else {
            Outcome::Unchanged
        }
    }
}
