use super::nodes_gen::*;
use crate::{
    bindings,
    types::{ClassTy, Ty},
    Entity, Graph, Mode,
};
use std::{
    fmt,
    hash::{Hash, Hasher},
};

lazy_static! {
    static ref NODE_FACTORY: NodeFactory = NodeFactory::new();
}

impl Node {
    pub fn wrap(ir_node: *mut bindings::ir_node) -> Node {
        //NodeFactory::new().create(ir_node)
        NODE_FACTORY.create(ir_node)
    }
}

unsafe impl Send for Node {}

macro_rules! linked_list_iterator {
    ($iter_name: ident, $item: ident, $head_fn: ident, $next_fn: ident) => {
        pub struct $iter_name {
            cur: Option<$item>,
        }

        impl $iter_name {
            fn new(node: *mut bindings::ir_node) -> Self {
                Self {
                    cur: $iter_name::raw_to_option(unsafe { bindings::$head_fn(node) }),
                }
            }

            fn raw_to_option(raw: *mut bindings::ir_node) -> Option<$item> {
                if raw.is_null() {
                    None
                } else {
                    Some($item::new(raw))
                }
            }
        }

        impl Iterator for $iter_name {
            type Item = $item;

            fn next(&mut self) -> Option<$item> {
                let out = self.cur;

                if let Some(node) = self.cur {
                    self.cur = $iter_name::raw_to_option(unsafe {
                        bindings::$next_fn(node.internal_ir_node())
                    });
                }
                out
            }
        }
    };
}

macro_rules! simple_node_iterator {
    ($iter_name: ident, $len_fn: ident, $get_fn: ident, $idx_type: ty) => {
        generate_iterator!(
            $iter_name,
            *mut bindings::ir_node,
            $len_fn,
            node,
            idx,
            $idx_type,
            {
                let out = unsafe { bindings::$get_fn(node, idx) };
                Node::wrap(out)
            },
            Node,
        );
    };
}

/// A trait to abstract from Node enum and various *-Node structs.
/// Inspired by https://github.com/libfirm/jFirm/blob/master/src/firm/nodes/Node.java.
pub trait NodeTrait {
    fn internal_ir_node(&self) -> *mut bindings::ir_node;

    // TODO move to graph
    fn keep_alive(&self) {
        unsafe { bindings::keep_alive(self.internal_ir_node()) }
    }

    fn mode(&self) -> Mode {
        Mode::from_libfirm(unsafe { bindings::get_irn_mode(self.internal_ir_node()) })
    }

    fn block(&self) -> Block {
        let block_ir_node = unsafe { bindings::get_nodes_block(self.internal_ir_node()) };
        Block::new(block_ir_node)
    }

    fn set_block(&self, block: Block) {
        unsafe { bindings::set_nodes_block(self.internal_ir_node(), block.internal_ir_node()) }
    }

    fn out_nodes(&self) -> OutNodeIterator {
        OutNodeIterator::new(self.internal_ir_node())
    }

    fn out_nodes_ex(&self) -> OutNodeExIterator {
        OutNodeExIterator::new(self.internal_ir_node())
    }

    fn all_out_projs(&self) -> Vec<Proj> {
        let mut result = Vec::new();
        self.collect_all_out_projs(&mut result);
        result
    }

    fn collect_all_out_projs(&self, projs: &mut Vec<Proj>) {
        for n in self.out_nodes() {
            if let Node::Proj(proj, _) = n {
                projs.push(proj);
                proj.collect_all_out_projs(projs);
            }
        }
    }

    fn in_nodes(&self) -> InNodeIterator {
        InNodeIterator::new(self.internal_ir_node())
    }

    fn set_input_at(&self, idx: i32, pred: Node) {
        unsafe {
            bindings::set_irn_n(self.internal_ir_node(), idx, pred.internal_ir_node());
        }
    }

    fn set_in_nodes(&self, nodes: &[Node]) {
        let nodes: Vec<*mut bindings::ir_node> =
            nodes.iter().map(|v| v.internal_ir_node()).collect();
        unsafe {
            bindings::set_irn_in(self.internal_ir_node(), nodes.len() as i32, nodes.as_ptr());
        }
    }

    fn node_id(&self) -> i64 {
        unsafe { bindings::get_irn_node_nr(self.internal_ir_node()) }
    }

    fn is_pinned(&self) -> bool {
        unsafe { bindings::get_irn_pinned(self.internal_ir_node()) > 0 }
    }

    fn is_only_valid_in_start_block(&self) -> bool {
        unsafe { bindings::is_irn_start_block_placed(self.internal_ir_node()) > 0 }
    }

    fn graph(&self) -> Graph {
        Graph {
            irg: unsafe { bindings::get_irn_irg(self.internal_ir_node()) },
        }
    }
}

simple_node_iterator!(InNodeIterator, get_irn_arity, get_irn_n, i32);

// TODO: should we use dynamic reverse edges instead of reverse
simple_node_iterator!(OutNodeIterator, get_irn_n_outs, get_irn_out, u32);

generate_iterator!(
    OutNodeExIterator,
    *mut bindings::ir_node,
    get_irn_n_outs,
    node,
    idx,
    u32,
    {
        let mut in_pos: i32 = 0;
        let out = unsafe { bindings::get_irn_out_ex(node, idx, &mut in_pos) };
        (Node::wrap(out), in_pos)
    },
    (Node, i32),
);

impl Hash for Node {
    fn hash<H: Hasher>(&self, state: &mut H) {
        // k1 == k2 => hash(k1) == hash(k2)
        // has to hold, update PartialEq implementation if this code
        // is updated.
        self.internal_ir_node().hash(state);
    }
}

impl PartialEq for Node {
    fn eq(&self, other: &Self) -> bool {
        // k1 == k2 => hash(k1) == hash(k2)
        // has to hold, update Hash implementation if this code
        // is updated.
        self.internal_ir_node() == other.internal_ir_node()
    }
}

impl Eq for Node {}

impl From<Node> for *mut bindings::ir_node {
    fn from(n: Node) -> *mut bindings::ir_node {
        n.internal_ir_node()
    }
}

// == Node extensions ==

impl End {
    pub fn keep_alives(self) -> EndKeepAliveIterator {
        EndKeepAliveIterator::new(self.internal_ir_node())
    }
}

simple_node_iterator!(
    EndKeepAliveIterator,
    get_End_n_keepalives,
    get_End_keepalive,
    i32
);

impl Return {
    pub fn return_res(self) -> ReturnResIterator {
        ReturnResIterator::new(self.internal_ir_node())
    }
}

simple_node_iterator!(ReturnResIterator, get_Return_n_ress, get_Return_res, i32);
linked_list_iterator!(
    PhisOfBlockLinkedListIterator,
    Phi,
    get_Block_phis,
    get_Phi_next
);

impl Block {
    pub fn deepest_common_dominator(a: Block, b: Block) -> Block {
        let cdom = unsafe {
            bindings::ir_deepest_common_dominator(a.internal_ir_node(), b.internal_ir_node())
        };

        Block::new(cdom)
    }

    pub fn dominates(&self, other: Block) -> bool {
        // TODO: check if dominators are computed
        unsafe { bindings::block_dominates(self.internal_ir_node(), other.internal_ir_node()) > 0 }
    }

    pub fn immediate_dominator(&self) -> Option<Block> {
        // TODO: check if dominators are computed
        let idom = unsafe { bindings::get_Block_idom(self.internal_ir_node()) };
        if idom.is_null() {
            None
        } else {
            Some(Block::new(idom))
        }
    }

    pub fn loop_depth(&self) -> u32 {
        // TODO: check if loop info is computed
        let loop_ref = unsafe { bindings::get_irn_loop(self.internal_ir_node()) };
        unsafe { bindings::get_loop_depth(loop_ref) }
    }

    pub fn immediate_post_dominator(&self) -> Option<Block> {
        // TODO: check if post dominators are computed
        let ipostdom = unsafe { bindings::get_Block_ipostdom(self.internal_ir_node()) };
        if ipostdom.is_null() {
            None
        } else {
            Some(Block::new(ipostdom))
        }
    }

    pub fn cfg_preds(self) -> CfgPredsIterator {
        CfgPredsIterator::new(self.internal_ir_node())
    }

    /// Access the phis of a block using `get_Block_phis` and `get_Phi_next`.
    /// Note that this list is not updated automatically. Fill the list
    /// manually or use `phis` instead.
    pub fn linked_list_of_phis(self) -> PhisOfBlockLinkedListIterator {
        PhisOfBlockLinkedListIterator::new(self.internal_ir_node())
    }

    pub fn phis(self) -> Vec<Phi> {
        let mut result = vec![];
        for node in self.out_nodes() {
            if let Node::Phi(phi) = node {
                result.push(phi);
            }
        }
        result
    }

    pub fn phi_or_node(self, nodes: &[Node]) -> Node {
        assert!(!nodes.is_empty());
        assert!(nodes.iter().all(|n| n.mode() == nodes[0].mode()));

        if nodes.len() == 1 {
            nodes[0]
        } else {
            let mode = nodes[0].mode();
            self.new_phi(nodes, mode).into()
        }
    }

    pub fn set_cfg_pred(self, idx: i32, pred: impl NodeTrait) {
        unsafe {
            bindings::set_Block_cfgpred(self.internal_ir_node(), idx, pred.internal_ir_node());
        }
    }

    pub fn mature(self) {
        unsafe {
            bindings::mature_immBlock(self.internal_ir_node());
        }
    }

    pub fn value(self, slot_idx: usize, mode: Mode) -> Node {
        Node::wrap(unsafe {
            bindings::get_b_value(
                self.internal_ir_node(),
                slot_idx as i32,
                mode.libfirm_mode(),
            )
        })
    }

    pub fn set_value(self, slot_idx: usize, val: impl NodeTrait) {
        unsafe {
            bindings::set_b_value(
                self.internal_ir_node(),
                slot_idx as i32,
                val.internal_ir_node(),
            )
        }
    }

    pub fn cur_store(self) -> Node {
        Node::wrap(unsafe { bindings::get_b_store(self.internal_ir_node()) })
    }

    pub fn set_store(self, s: impl NodeTrait) {
        unsafe { bindings::set_b_store(self.internal_ir_node(), s.internal_ir_node()) }
    }

    pub fn imm_add_pred(self, pred: impl NodeTrait) {
        unsafe {
            bindings::add_immBlock_pred(self.internal_ir_node(), pred.internal_ir_node());
        }
    }
}

simple_node_iterator!(
    CfgPredsIterator,
    get_Block_n_cfgpreds,
    get_Block_cfgpred,
    i32
);

impl Phi {
    pub fn phi_preds(self) -> PhiPredsIterator {
        PhiPredsIterator::new(self.internal_ir_node())
    }
}

simple_node_iterator!(PhiPredsIterator, get_Phi_n_preds, get_Phi_pred, i32);

impl Proj {
    pub fn new_proj(self, num: u32, mode: Mode) -> Proj {
        Proj::new(unsafe {
            bindings::new_r_Proj(self.internal_ir_node(), mode.libfirm_mode(), num)
        })
    }

    pub fn pred_or_none(self) -> Option<Node> {
        if self.in_nodes().len() == 0 {
            None
        } else {
            let unwrapped = unsafe { bindings::get_Proj_pred(self.internal_ir_node()) };
            Some(Node::wrap(unwrapped))
        }
    }
}

impl Jmp {
    pub fn out_target_block(self) -> Option<Block> {
        self.out_nodes().next().and_then(Node::as_block)
    }
}

impl Cond {
    pub fn out_proj_val(self, val: bool) -> Option<Proj> {
        if val {
            self.out_proj_true()
        } else {
            self.out_proj_false()
        }
    }

    pub fn out_proj_target_block(self, val: bool) -> Option<(Proj, Block, i32)> {
        self.out_proj_val(val).and_then(|proj| {
            proj.out_nodes_ex().next().map(|(target_block, idx)| {
                if let Node::Block(target_block) = target_block {
                    (proj, target_block, idx)
                } else {
                    unreachable!("Target of a Proj must be a Block")
                }
            })
        })
    }
}

impl Address {
    pub fn entity(self) -> Entity {
        unsafe { bindings::get_Address_entity(self.internal_ir_node()).into() }
    }

    pub fn set_entity(self, ir_entity: Entity) {
        unsafe {
            bindings::set_Address_entity(self.internal_ir_node(), ir_entity.into());
        }
    }
}

#[derive(Debug)]
pub enum IsNewResult {
    Yes(ClassTy),
    No,
}

impl Call {
    pub fn args(self) -> CallArgsIterator {
        CallArgsIterator::new(self.internal_ir_node())
    }

    pub fn is_new(self) -> IsNewResult {
        if let Some(addr) = Node::as_address(self.ptr()) {
            if addr.entity().name_string() == "mjrt_new" {
                let first_arg = self.args().idx(0);
                if let Some(Node::Size(size_node)) = first_arg {
                    let class_ty = ClassTy::from(size_node.ty()).unwrap();
                    return IsNewResult::Yes(class_ty);
                }
            }
        }
        IsNewResult::No
    }
}

simple_node_iterator!(CallArgsIterator, get_Call_n_params, get_Call_param, i32);

impl Size {
    pub fn ty(self) -> Ty {
        unsafe { Ty::from_ir_type(bindings::get_Size_type(self.internal_ir_node())) }
    }
}

// = Debug fmt =

pub trait NodeDebug {
    fn fmt(&self, f: &mut fmt::Formatter, options: NodeDebugOpts) -> fmt::Result;

    fn debug_fmt(self) -> NodeDebugFmt<Self>
    where
        Self: Sized + Copy,
    {
        NodeDebugFmt(self, NodeDebugOpts::default())
    }
}

#[derive(Debug, Clone, Copy, Default)]
pub struct NodeDebugOpts {
    short: bool,
}

impl NodeDebugOpts {
    pub fn short(self) -> bool {
        self.short
    }

    pub fn with_short(mut self, val: bool) -> Self {
        self.short = val;
        self
    }
}

#[derive(Copy, Clone)]
pub struct NodeDebugFmt<T: NodeDebug + Sized + Copy>(T, NodeDebugOpts);
impl<T: NodeDebug + Copy> NodeDebugFmt<T> {
    pub fn short(self, val: bool) -> Self {
        NodeDebugFmt(self.0, self.1.with_short(val))
    }
    pub fn with(self, opts: NodeDebugOpts) -> Self {
        NodeDebugFmt(self.0, opts)
    }
}

impl<T: NodeDebug + Copy> fmt::Display for NodeDebugFmt<T> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        NodeDebug::fmt(&self.0, f, self.1)
    }
}

impl<T: NodeDebug + Copy> fmt::Debug for NodeDebugFmt<T> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "NodeDebugFmt {:?}", self.1)
    }
}

// = Debug fmt impls =

impl NodeDebug for Proj {
    fn fmt(&self, f: &mut fmt::Formatter, _opts: NodeDebugOpts) -> fmt::Result {
        write!(f, "Proj {}: {:?}", self.node_id(), self.kind())
    }
}

impl NodeDebug for Const {
    fn fmt(&self, f: &mut fmt::Formatter, _opts: NodeDebugOpts) -> fmt::Result {
        write!(f, "Const {} ({:?})", self.node_id(), self.tarval())
    }
}

impl NodeDebug for Call {
    fn fmt(&self, f: &mut fmt::Formatter, opts: NodeDebugOpts) -> fmt::Result {
        if opts.short {
            write!(f, "Call {}", self.node_id())
        } else {
            write!(
                f,
                "Call to {} {}",
                self.ptr().debug_fmt().short(true),
                self.node_id()
            )
        }
    }
}

impl NodeDebug for Address {
    fn fmt(&self, f: &mut fmt::Formatter, opts: NodeDebugOpts) -> fmt::Result {
        if opts.short {
            write!(f, "@{}", self.entity().name_string(),)
        } else {
            write!(
                f,
                "Address of {:?} {}",
                self.entity().name_string(),
                self.node_id(),
            )
        }
    }
}

impl NodeDebug for Member {
    fn fmt(&self, f: &mut fmt::Formatter, opts: NodeDebugOpts) -> fmt::Result {
        if opts.short {
            write!(f, "@{}", self.entity().name_string(),)
        } else {
            write!(
                f,
                "Member ({:?}) {}",
                self.entity().name_string(),
                self.node_id(),
            )
        }
    }
}

impl NodeDebug for Load {
    fn fmt(&self, f: &mut fmt::Formatter, opts: NodeDebugOpts) -> fmt::Result {
        if opts.short {
            write!(f, "Load {}", self.node_id())
        } else {
            write!(
                f,
                "Load {} {}",
                self.ptr().debug_fmt().short(true),
                self.node_id()
            )
        }
    }
}

impl NodeDebug for Store {
    fn fmt(&self, f: &mut fmt::Formatter, opts: NodeDebugOpts) -> fmt::Result {
        if opts.short {
            write!(f, "Store {}", self.node_id())
        } else {
            write!(
                f,
                "Store {} {}",
                self.ptr().debug_fmt().short(true),
                self.node_id()
            )
        }
    }
}
