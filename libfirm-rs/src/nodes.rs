use crate::nodes_gen::{Block, Node, NodeFactory};
use libfirm_rs_bindings as bindings;
use std::hash::{Hash, Hasher};

impl Block {

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

    // TODO: should we use dynamic reverse edges instead of reverse
    fn reverse_edges(&self) -> Vec<Node> {
        let id = self.internal_ir_node();
        let num_outs = unsafe { bindings::get_irn_n_outs(id) };

        let mut outs = Vec::new();

        for out_idx in 0..num_outs {
            let out = unsafe { bindings::get_irn_out(id, out_idx) };
            outs.push(NodeFactory::node(out));
        }

        outs
    }

    fn reverse_edge_iterator(&self) -> ReverseEdgeIterator {
        ReverseEdgeIterator::new(self.internal_ir_node())
    }

    fn node_id(&self) -> i64 {
        unsafe { bindings::get_irn_node_nr(self.internal_ir_node()) }
    }

    // TODO implement methods from
    // https://github.com/libfirm/jFirm/blob/master/src/firm/nodes/Node.java
}

impl Hash for Node {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.internal_ir_node().hash(state);
    }
}

// TODO: derive Eq here, current is incorrect

pub struct ReverseEdgeIterator {
    node: *mut bindings::ir_node,
    cur: u32,
    len: u32,
}

impl ReverseEdgeIterator {
    fn new(node: *mut bindings::ir_node) -> Self {
        Self {
            node,
            len: unsafe { bindings::get_irn_n_outs(node) },
            cur: 0,
        }
    }
}

impl Iterator for ReverseEdgeIterator {
    type Item = Node;

    fn next(&mut self) -> Option<Node> {
        if self.cur == self.len {
            None
        } else {
            let out = unsafe { bindings::get_irn_out(self.node, self.cur) };
            self.cur += 1;
            Some(NodeFactory::node(out))
        }
    }
}

// TODO maybe Into<*const ir_node> for NodeTrait?
