pub use super::{nodes_gen::*, value_nodes::*};
use crate::entity::Entity;
use libfirm_rs_bindings as bindings;
use std::{
    fmt,
    hash::{Hash, Hasher},
};

macro_rules! simple_node_iterator {
    ($iter_name: ident, $len_fn: ident, $get_fn: ident, $id_type: ty) => {
        pub struct $iter_name {
            node: *mut bindings::ir_node,
            cur: $id_type,
            len: $id_type,
        }

        impl $iter_name {
            fn new(node: *mut bindings::ir_node) -> Self {
                Self {
                    node,
                    len: unsafe { bindings::$len_fn(node) },
                    cur: 0,
                }
            }

            pub fn idx(&self, index: $id_type) -> Option<Node> {
                if (0..self.len).contains(&index) {
                    let out = unsafe { bindings::$get_fn(self.node, index) };
                    Some(NodeFactory::node(out))
                } else {
                    None
                }
            }
        }

        impl Iterator for $iter_name {
            type Item = Node;

            fn next(&mut self) -> Option<Node> {
                if self.cur == self.len {
                    None
                } else {
                    let out = unsafe { bindings::$get_fn(self.node, self.cur) };
                    self.cur += 1;
                    Some(NodeFactory::node(out))
                }
            }
        }

        impl ExactSizeIterator for $iter_name {
            fn len(&self) -> usize {
                self.len as usize
            }
        }
    };
}

/// A trait to abstract from Node enum and various *-Node structs.
pub trait NodeTrait {
    fn internal_ir_node(&self) -> *mut bindings::ir_node;

    fn keep_alive(&self) {
        unsafe { bindings::keep_alive(self.internal_ir_node()) }
    }

    fn mode(&self) -> bindings::mode::Type {
        unsafe { bindings::get_irn_mode(self.internal_ir_node()) }
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

    fn in_nodes(&self) -> InNodeIterator {
        InNodeIterator::new(self.internal_ir_node())
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

    // TODO implement methods from
    // https://github.com/libfirm/jFirm/blob/master/src/firm/nodes/Node.java
}

simple_node_iterator!(InNodeIterator, get_irn_arity, get_irn_n, i32);

// TODO: should we use dynamic reverse edges instead of reverse
simple_node_iterator!(OutNodeIterator, get_irn_n_outs, get_irn_out, u32);

#[allow(clippy::derive_hash_xor_eq)]
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

impl From<Box<dyn ValueNode>> for Node {
    fn from(n: Box<dyn ValueNode>) -> Node {
        NodeFactory::node(n.internal_ir_node())
    }
}

impl From<&Box<dyn ValueNode>> for Node {
    fn from(n: &Box<dyn ValueNode>) -> Node {
        NodeFactory::node(n.internal_ir_node())
    }
}

impl From<&dyn ValueNode> for Node {
    fn from(n: &dyn ValueNode) -> Node {
        NodeFactory::node(n.internal_ir_node())
    }
}

impl<T> From<&T> for Node
where
    T: ValueNode,
{
    fn from(n: &T) -> Node {
        NodeFactory::node(n.internal_ir_node())
    }
}

// == Node extensions ==

impl Return {
    pub fn return_res(self) -> ReturnResIterator {
        ReturnResIterator::new(self.internal_ir_node())
    }
}

simple_node_iterator!(
    ReturnResIterator,
    get_Return_n_ress,
    get_Return_res,
    i32
);

impl Block {
    pub fn cfg_preds(self) -> CfgPredsIterator {
        CfgPredsIterator::new(self.internal_ir_node())
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
    pub fn proj(self, num: u32, mode: bindings::mode::Type) -> Proj {
        Proj::new(unsafe { bindings::new_r_Proj(self.internal_ir_node(), mode, num) })
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

    pub fn out_proj_target_block(self, val: bool) -> Option<(Proj, Block)> {
        self.out_proj_val(val).and_then(|proj| {
            proj.out_nodes().next().map(|target_block| {
                if let Node::Block(target_block) = target_block {
                    (proj, target_block)
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

impl Call {
    pub fn args(self) -> CallArgsIterator {
        CallArgsIterator::new(self.internal_ir_node())
    }
}

simple_node_iterator!(CallArgsIterator, get_Call_n_params, get_Call_param, i32);


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

/*
impl NodeDebug for nodes_gen::Const {
    fn fmt(&self, f: &mut fmt::Formatter, _opts: NodeDebugOpts) -> fmt::Result {
        let x = self.ptr().debug_fmt().short(true);
        write!(f, "Const ", x, self.node_id())
    }
}
*/

impl NodeDebug for Call {
    fn fmt(&self, f: &mut fmt::Formatter, _opts: NodeDebugOpts) -> fmt::Result {
        let x = self.ptr().debug_fmt().short(true);
        write!(f, "Call to {} {}", x, self.node_id())
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
