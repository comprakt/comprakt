use crate::nodes_gen::{self, Block, Call, Node, NodeFactory, Phi, Proj};
use libfirm_rs_bindings as bindings;
use std::{
    ffi::{CStr, CString},
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

impl Block {
    pub fn keep_alive(self) {
        unsafe { bindings::keep_alive(self.internal_ir_node()) }
    }
}

impl Phi {
    pub fn phi_preds(self) -> PhiPredsIterator {
        PhiPredsIterator::new(self.internal_ir_node())
    }
}

simple_node_iterator!(PhiPredsIterator, get_Phi_n_preds, get_Phi_pred, i32);

impl Proj {
    pub fn proj(&self, num: u32, mode: bindings::mode::Type) -> Proj {
        Proj::new(unsafe { bindings::new_r_Proj(self.internal_ir_node(), mode, num) })
    }
}

/// A trait to abstract from Node enum and various *-Node structs.
pub trait NodeTrait {
    fn internal_ir_node(&self) -> *mut bindings::ir_node;

    fn block(&self) -> Block {
        let block_ir_node = unsafe { bindings::get_nodes_block(self.internal_ir_node()) };
        match NodeFactory::node(block_ir_node) {
            Node::Block(block) => block,
            _ => panic!("Expected block."),
        }
    }

    fn out_nodes(&self) -> OutNodeIterator {
        OutNodeIterator::new(self.internal_ir_node())
    }

    fn node_id(&self) -> i64 {
        unsafe { bindings::get_irn_node_nr(self.internal_ir_node()) }
    }

    // TODO implement methods from
    // https://github.com/libfirm/jFirm/blob/master/src/firm/nodes/Node.java
}

// TODO: should we use dynamic reverse edges instead of reverse
simple_node_iterator!(OutNodeIterator, get_irn_n_outs, get_irn_out, u32);

impl Hash for Node {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.internal_ir_node().hash(state);
    }
}

// FIXME generate this
impl Into<*mut bindings::ir_node> for Node {
    fn into(self) -> *mut bindings::ir_node {
        self.internal_ir_node()
    }
}

impl fmt::Debug for nodes_gen::Call {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "Call to {:?} {}", self.ptr(), self.node_id())
    }
}

impl nodes_gen::Address {
    pub fn entity(self) -> *mut bindings::ir_entity {
        unsafe { bindings::get_Address_entity(self.internal_ir_node()) }
    }

    pub fn set_entity(self, ir_entity: *mut bindings::ir_entity) {
        unsafe {
            bindings::set_Address_entity(self.internal_ir_node(), ir_entity);
        }
    }
}

impl fmt::Debug for nodes_gen::Address {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let entity = self.entity();
        let entity_name = unsafe { CStr::from_ptr(bindings::get_entity_name(entity)) };
        write!(f, "Address of {:?} {}", entity_name, self.node_id())
    }
}

/*
// FIXME generate this
impl Into<*mut bindings::ir_node> for crate::nodes_gen::Phi {
    fn into(self) -> *mut bindings::ir_node {
        self.internal_ir_node()
    }
}

// FIXME generate this
impl Into<*const bindings::ir_node> for crate::nodes_gen::Phi {
    fn into(self) -> *const bindings::ir_node {
        self.internal_ir_node() as *const _
    }
}*/

// TODO: derive Eq here, current is incorrect

// TODO maybe Into<*const ir_node> for NodeTrait?
