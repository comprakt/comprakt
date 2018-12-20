pub use crate::nodes_gen::*;
use crate::{
    entity::Entity,
    graph::{self, Graph},
    nodes_gen,
    value_nodes::ValueNode,
};
use libfirm_rs_bindings as bindings;
use std::{
    collections::HashSet,
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

impl Phi {
    /// `Node` is the result of the phi node when entering this phi's block via
    /// `Block`
    pub fn preds(self) -> impl Iterator<Item = (Block, Node)> {
        // From libfirm docs:
        // A phi node has 1 input for each predecessor of its block. If a
        // block is entered from its nth predecessor all phi nodes produce
        // their nth input as result.
        let block = self.block();
        PhiPredsIterator::new(self.internal_ir_node())
            .enumerate()
            .map(move |(i, pred)| (block.cfg_pred(i as i32).block(), pred))
    }
}

simple_node_iterator!(PhiPredsIterator, get_Phi_n_preds, get_Phi_pred, i32);

impl Proj {
    pub fn proj(self, num: u32, mode: bindings::mode::Type) -> Proj {
        Proj::new(unsafe { bindings::new_r_Proj(self.internal_ir_node(), mode, num) })
    }

    pub fn kind(self) -> ProjKind {
        NodeFactory::proj_kind(self)
    }
}

/// A trait to abstract from Node enum and various *-Node structs.
pub trait NodeTrait {
    fn internal_ir_node(&self) -> *mut bindings::ir_node;

    fn mode(&self) -> bindings::mode::Type {
        unsafe { bindings::get_irn_mode(self.internal_ir_node()) }
    }

    fn block(&self) -> Block {
        assert!(!self.is_block());
        let block_ir_node = unsafe { bindings::get_nodes_block(self.internal_ir_node()) };
        match NodeFactory::node(block_ir_node) {
            Node::Block(block) => block,
            _ => panic!("Expected block."),
        }
    }

    fn graph(&self) -> Graph {
        unsafe { bindings::get_irn_irg(self.internal_ir_node()) }.into()
    }

    /// These er the "reverse" libfirm edges, i.e. those going downwards in the
    /// graph, i.e in the directon of the data/mem/control flow.
    fn out_nodes(&self) -> OutNodeIterator {
        let internal = self.internal_ir_node();
        graph::Graph::from(unsafe { bindings::get_irn_irg(internal) }).assure_outs();
        OutNodeIterator::new(internal)
    }

    /// These are the "normal" libfirm edges, i.e. those going upwards in the
    /// graph, i.e. in the directorn of the data/mem/flow *dependency*
    fn in_nodes(&self) -> InNodeIterator {
        InNodeIterator::new(self.internal_ir_node())
    }

    fn node_id(&self) -> i64 {
        unsafe { bindings::get_irn_node_nr(self.internal_ir_node()) }
    }

    // TODO autogenerate
    fn is_block(&self) -> bool {
        unsafe { bindings::is_Block(self.internal_ir_node()) != 0 }
    }

    // TODO autogenerate
    fn is_jmp(&self) -> bool {
        unsafe { bindings::is_Jmp(self.internal_ir_node()) != 0 }
    }

    fn is_const(&self) -> bool {
        unsafe { bindings::is_Const(self.internal_ir_node()) != 0 }
    }

    // TODO implement methods from
    // https://github.com/libfirm/jFirm/blob/master/src/firm/nodes/Node.java

    /// libifrm irg_walk wrapper
    ///
    /// Walks over the ir graph, starting at the this node and going to all
    /// predecessors, i.e., dependencies (operands) of this node.
    /// Note that this traversal crosses block boundaries, since blocks are
    /// also just predecessors in the Graph.
    fn walk<F>(&self, mut walker: F)
    where
        F: FnMut(graph::VisitTime, Node),
        Self: Sized,
    {
        // We need the type ascription here, because otherwise rust infers `&mut F`,
        // but in `closure_handler` we transmute to `&mut &mut dyn FnMut(_)` (because
        // `closure_handler` doesn't know the concrete `F`.
        let mut fat_pointer: &mut dyn FnMut(graph::VisitTime, Node) = &mut walker;
        let thin_pointer = &mut fat_pointer;

        unsafe {
            use std::ffi::c_void;
            bindings::irg_walk(
                self.internal_ir_node(),
                Some(pre_closure_handler),
                Some(post_closure_handler),
                thin_pointer as *mut &mut _ as *mut c_void,
            );
        }
    }

    /// Perform a DFS over all nodes within `block` starting at `self`,
    /// plus the nodes that are just outside of the block.
    /// The primary use case for this API is in codegen.
    fn walk_dfs_in_block<Callback>(&self, block: Block, callback: &mut Callback)
    where
        Callback: FnMut(Node),
        Self: Sized,
    {
        fn recurse<Callback>(
            visited: &mut HashSet<Node>,
            cur_node: Node,
            block: Block,
            callback: &mut Callback,
        ) where
            Callback: FnMut(Node),
        {
            if cur_node.block() == block {
                for operand in cur_node.in_nodes() {
                    // cannot filter before the loop because recurse adds to visited
                    if visited.contains(&operand) {
                        continue;
                    }
                    visited.insert(operand);
                    recurse(visited, operand, block, callback);
                }
            }
            callback(cur_node);
        }

        let mut visited = HashSet::new();

        let this = NodeFactory::node(self.internal_ir_node());
        recurse(&mut visited, this, block, callback);
    }
}

pub use self::graph::VisitTime;
use std::{ffi::c_void, mem};

unsafe extern "C" fn pre_closure_handler(node: *mut bindings::ir_node, closure: *mut c_void) {
    #[allow(clippy::transmute_ptr_to_ref)]
    let closure: &mut &mut FnMut(VisitTime, Node) = mem::transmute(closure);
    closure(VisitTime::BeforePredecessors, NodeFactory::node(node));
}

unsafe extern "C" fn post_closure_handler(node: *mut bindings::ir_node, closure: *mut c_void) {
    #[allow(clippy::transmute_ptr_to_ref)]
    let closure: &mut &mut FnMut(VisitTime, Node) = mem::transmute(closure);
    closure(VisitTime::AfterPredecessors, NodeFactory::node(node));
}

simple_node_iterator!(InNodeIterator, get_irn_arity, get_irn_n, i32);

// TODO: should we use dynamic reverse edges instead of reverse
simple_node_iterator!(OutNodeIterator, get_irn_n_outs, get_irn_out, u32);

impl Hash for Node {
    fn hash<H: Hasher>(&self, state: &mut H) {
        // k1 == k2 => hash(k1) == hash(k2)
        // has to hold, update PartialEq implementation if this code
        // is updated.
        self.internal_ir_node().hash(state);
    }
}

// TODO Autogenerate for all node kinds
impl Hash for Block {
    fn hash<H: Hasher>(&self, state: &mut H) {
        NodeFactory::node(self.internal_ir_node()).hash(state)
    }
}

impl PartialEq for Block {
    fn eq(&self, other: &Self) -> bool {
        NodeFactory::node(self.internal_ir_node()).eq(&NodeFactory::node(other.internal_ir_node()))
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

impl fmt::Debug for nodes_gen::Call {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "Call to {:?} {}", self.ptr(), self.node_id())
    }
}

impl nodes_gen::Cond {
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

impl nodes_gen::Address {
    pub fn entity(self) -> Entity {
        unsafe { bindings::get_Address_entity(self.internal_ir_node()).into() }
    }

    pub fn set_entity(self, ir_entity: Entity) {
        unsafe {
            bindings::set_Address_entity(self.internal_ir_node(), ir_entity.into());
        }
    }
}

impl nodes_gen::Block {
    pub fn num_cfgpreds(self) -> i32 {
        unsafe { bindings::get_Block_n_cfgpreds(self.internal_ir_node()) }
    }

    /// This block's CFG predecessor blocks.
    /// NOTE: see cfg_pred_nodes for the difference between block and node CFG
    /// preds.
    pub fn cfg_preds(&self) -> impl Iterator<Item = nodes_gen::Block> {
        self.cfg_pred_nodes().map(|n| n.block())
    }

    /// This block's CFG predecessor nodes.
    /// NOTE: CFG predecessor node != predecessor block:
    /// The predecessor node is a Jmp, Cond or Return whereas the
    /// predecessor block is the predeceddor node's block.
    pub fn cfg_pred_nodes(&self) -> impl Iterator<Item = nodes_gen::Node> {
        BlockPredIterator::new(self.internal_ir_node())
    }

    pub fn keep_alive(self) {
        unsafe { bindings::keep_alive(self.internal_ir_node()) }
    }

    /// This is the control flow Node that enters this block, such as Jmp,
    /// Proj(X) or Return.
    pub fn cfg_pred(self, idx: i32) -> Node {
        NodeFactory::node(unsafe { bindings::get_Block_cfgpred(self.internal_ir_node(), idx) })
    }
}

simple_node_iterator!(
    BlockPredIterator,
    get_Block_n_cfgpreds,
    get_Block_cfgpred,
    i32
);

impl fmt::Debug for nodes_gen::Address {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "Address of {:?} {}",
            self.entity().name_string(),
            self.node_id()
        )
    }
}
