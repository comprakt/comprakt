use crate::nodes_gen::{Block, Node, NodeFactory};
use libfirm_rs_bindings as bindings;

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

    // TODO implement methods from
    // https://github.com/libfirm/jFirm/blob/master/src/firm/nodes/Node.java
}

// TODO maybe Into<*const ir_node> for NodeTrait?
