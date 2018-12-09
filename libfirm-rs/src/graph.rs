use super::{
    nodes::NodeTrait,
    nodes_gen::{Block, End, Node, NodeFactory, Start, Proj},
};
use libfirm_rs_bindings as bindings;
use std::{
    ffi::{c_void, CString},
    mem,
};

impl From<crate::Graph> for Graph {
    fn from(graph: crate::Graph) -> Graph {
        Graph { irg: graph.irg }
    }
}

#[derive(Clone, Copy)]
pub struct Graph {
    pub(super) irg: *mut bindings::ir_graph,
}

impl Into<*mut bindings::ir_graph> for Graph {
    fn into(self) -> *mut bindings::ir_graph {
        self.irg
    }
}

impl Into<*const bindings::ir_graph> for Graph {
    fn into(self) -> *const bindings::ir_graph {
        self.irg as *const _
    }
}

impl Graph {
    pub fn entity(self) -> *mut bindings::ir_entity {
        unsafe { bindings::get_irg_entity(self.irg) }
    }

    pub fn start_block(self) -> Block {
        Block::new(unsafe { bindings::get_irg_start_block(self.irg) })
    }

    pub fn end_block(self) -> Block {
        Block::new(unsafe { bindings::get_irg_end_block(self.irg) })
    }

    pub fn start(self) -> Start {
        Start::new(unsafe { bindings::get_irg_start(self.irg) })
    }

    pub fn end(self) -> End {
        End::new(unsafe { bindings::get_irg_end(self.irg) })
    }

    pub fn args_node(self) -> Proj {
        Proj::new(unsafe { bindings::get_irg_args(self.irg) })
    }

    pub fn dump(self, suffix: &str) {
        let suffix = CString::new(suffix).unwrap();
        unsafe { bindings::dump_ir_graph(self.irg, suffix.as_ptr()) }
    }

    pub fn assure_outs(self) {
        unsafe { bindings::assure_irg_outs(self.irg) }
    }

    /// Walks over all reachable nodes in the graph, ensuring that nodes inside
    /// a basic block are visited in topological order.
    ///
    /// Nodes in different blocks might get visited in an interleaved order.
    ///
    /// ## Parameters
    ///  - `walker`	walker function
    ///
    /// Does not use the link field.
    pub fn walk_topological<F>(&self, mut walker: F)
    where
        F: FnMut(&Node),
    {
        // We need the type ascription here, because otherwise rust infers `&mut F`,
        // but in `closure_handler` we transmute to `&mut &mut dyn FnMut(_)` (because
        // `closure_handler` doesn't know the concrete `F`.
        let mut fat_pointer: &mut dyn FnMut(&Node) = &mut walker;
        let thin_pointer = &mut fat_pointer;

        unsafe {
            bindings::irg_walk_topological(
                self.irg,
                Some(closure_handler),
                thin_pointer as *mut &mut _ as *mut c_void,
            );
        }
    }

    pub fn exchange(prev: Node, new: Node) {
        unsafe {
            bindings::exchange(prev.internal_ir_node(), new.internal_ir_node());
        }
    }
}

unsafe extern "C" fn closure_handler(node: *mut bindings::ir_node, closure: *mut c_void) {
    let closure: &mut &mut FnMut(&Node) = mem::transmute(closure);
    closure(&NodeFactory::node(node))
}
