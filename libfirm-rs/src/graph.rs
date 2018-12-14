use super::{
    entity::Entity,
    nodes::NodeTrait,
    nodes_gen::{Block, End, Node, NodeFactory, Proj, Start},
};
use libfirm_rs_bindings as bindings;
use std::{
    ffi::{c_void, CString},
    fs::File,
    io::{BufWriter, Write},
    mem,
    path::PathBuf,
};

impl From<crate::Graph> for Graph {
    fn from(graph: crate::Graph) -> Graph {
        Graph { irg: graph.irg }
    }
}

#[derive(Clone, Copy, From)]
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
    pub fn entity(self) -> Entity {
        unsafe { Entity::new(bindings::get_irg_entity(self.irg)) }
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

    pub fn remove_bads(self) {
        unsafe { bindings::remove_bads(self.irg) }
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
    pub fn walk_topological<F>(self, mut walker: F)
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

    /// Walks over reachable Block nodes in the graph, starting at the
    /// end_block.
    ///
    /// For each block, the walker function is called twice, once before and
    /// once after all predecessors of the block are visited. This is indicated
    /// by the `VisitTime` parameter to the closure.
    ///
    /// ## Parameters
    ///  - `walker` walker function
    ///
    /// Has its own visited flag, so that it can be interleaved
    /// with the other walker. Does not use the link
    /// field.
    pub fn walk_blocks<F>(&self, mut walker: F)
    where
        F: FnMut(VisitTime, &Block),
    {
        // We need the type ascription here, because otherwise rust infers `&mut F`,
        // but in `closure_handler` we transmute to `&mut &mut dyn FnMut(_)` (because
        // `closure_handler` doesn't know the concrete `F`.
        let mut fat_pointer: &mut dyn FnMut(VisitTime, &Block) = &mut walker;
        let thin_pointer = &mut fat_pointer;

        unsafe {
            bindings::irg_block_walk_graph(
                self.irg,
                Some(pre_closure_handler),
                Some(post_closure_handler),
                thin_pointer as *mut &mut _ as *mut c_void,
            );
        }
    }

    pub fn exchange(prev: &impl NodeTrait, new: &impl NodeTrait) {
        unsafe {
            bindings::exchange(prev.internal_ir_node(), new.internal_ir_node());
        }
    }

    /// Replace the given node with a "bad" node, thus marking it and all the
    /// nodes dominated by it as unreachable. The whole subtree can then be
    /// removed using `Graph::remove_bads`.
    pub fn mark_as_bad(self, node: &impl NodeTrait) {
        Graph::exchange(node, &self.new_bad(unsafe { bindings::mode::b }))
    }

    pub fn dump_dot_data<T>(self, filename: &PathBuf, data: T)
    where
        T: Fn(Node) -> NodeData,
    {
        let write_file = File::create(filename).unwrap();
        let mut writer = BufWriter::new(&write_file);

        let mut list = Vec::new();
        self.walk_topological(|node| {
            list.push(*node);
        });

        writeln!(writer, "digraph G {{").unwrap();
        for node in list.iter() {
            let node_data = data(*node);
            writeln!(
                writer,
                "{:?} [label=\"{}\", style={}, shape=box{}];",
                node.node_id(),
                node_data.text.replace("\"", "'").replace("\n", "\\n"),
                if node_data.filled { "filled" } else { "none" },
                if node_data.bold { ", penwidth=3" } else { "" },
            )
            .unwrap();

            if !node.is_block() {
                writeln!(
                    writer,
                    "{:?} -> {:?} [color=blue];",
                    node.block().node_id(),
                    node.node_id()
                )
                .unwrap();
            }
            for ref_node in node.in_nodes() {
                writeln!(writer, "{:?} -> {:?};", ref_node.node_id(), node.node_id()).unwrap();
            }
        }
        writeln!(writer, "}}").unwrap();

        writer.flush().unwrap();
    }
}

pub struct NodeData {
    text: String,
    filled: bool,
    bold: bool,
}

impl NodeData {
    pub fn new(text: String) -> NodeData {
        NodeData {
            text,
            filled: false,
            bold: false,
        }
    }

    pub fn filled(&mut self, val: bool) {
        self.filled = val;
    }

    pub fn bold(&mut self, val: bool) {
        self.bold = val;
    }
}

unsafe extern "C" fn closure_handler(node: *mut bindings::ir_node, closure: *mut c_void) {
    #[allow(clippy::transmute_ptr_to_ref)]
    let closure: &mut &mut FnMut(&Node) = mem::transmute(closure);
    closure(&NodeFactory::node(node))
}

unsafe extern "C" fn pre_closure_handler(node: *mut bindings::ir_node, closure: *mut c_void) {
    // TODO: is this allow correct, Joshua?
    #[allow(clippy::transmute_ptr_to_ref)]
    let closure: &mut &mut FnMut(VisitTime, &Block) = mem::transmute(closure);
    match NodeFactory::node(node) {
        Node::Block(block) => closure(VisitTime::BeforePredecessors, &block),
        _ => unreachable!("irg_block_walk_graph only walks over blocks"),
    }
}

unsafe extern "C" fn post_closure_handler(node: *mut bindings::ir_node, closure: *mut c_void) {
    // TODO: is this allow correct, Joshua?
    #[allow(clippy::transmute_ptr_to_ref)]
    let closure: &mut &mut FnMut(VisitTime, &Block) = mem::transmute(closure);
    match NodeFactory::node(node) {
        Node::Block(block) => closure(VisitTime::AfterPredecessors, &block),
        _ => unreachable!("irg_block_walk_graph only walks over blocks"),
    }
}

#[derive(Debug)]
pub enum VisitTime {
    BeforePredecessors,
    AfterPredecessors,
}
