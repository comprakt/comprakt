use super::{
    entity::Entity,
    mode::Mode,
    nodes::{Block, End, NoMem, Node, NodeFactory, NodeTrait, Proj, ProjKind, Start},
    value_nodes::ValueNode,
};
use libfirm_rs_bindings as bindings;
use std::{
    ffi::{c_void, CString},
    mem, ptr,
};

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
    pub fn function_with_entity(entity: Entity, num_slots: usize) -> Graph {
        unsafe {
            let irg = bindings::new_ir_graph(entity.ir_entity(), num_slots as i32);
            Graph { irg }
        }
    }

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

    pub fn set_start_block(self, block: Block) {
        unsafe {
            bindings::set_irg_start_block(self.irg, block.internal_ir_node());
        }
    }

    pub fn end(self) -> End {
        End::new(unsafe { bindings::get_irg_end(self.irg) })
    }

    pub fn args(self) -> Proj {
        Proj::new(unsafe { bindings::get_irg_args(self.irg) })
    }

    pub fn no_mem(self) -> NoMem {
        NoMem::new(unsafe { bindings::get_irg_no_mem(self.irg) })
    }

    pub fn frame(self) -> Node {
        NodeFactory::node(unsafe { bindings::get_irg_frame(self.irg) })
    }

    pub fn dump(self, suffix: &str) {
        let suffix = CString::new(suffix).unwrap();
        // use ir_dump_flags to change dump
        // use self::bindings::ir_dump_flags_t::*;
        // unsafe { bindings::ir_set_dump_flags(IdxLabel | NumberLabel | KeepaliveEdges
        // | BlocksAsSubgraphs | Iredges | AllAnchors | LdNames); }
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

    pub fn nodes(self) -> Vec<Node> {
        let mut result = Vec::new();
        self.walk_topological(|n| {
            result.push(*n);
        });
        result
    }

    pub fn exchange(prev: &impl NodeTrait, new: &impl NodeTrait) {
        unsafe {
            bindings::exchange(prev.internal_ir_node(), new.internal_ir_node());
        }
    }

    /*pub fn partition_block_by_node(node: &impl NodeTrait) {
        unsafe {
            bindings::part_block(node.internal_ir_node());
        }
    }*/

    // FIXME why does not work this with `&impl ValueNode`?
    pub fn exchange_value(prev: &dyn ValueNode, new: &dyn ValueNode) {
        let prev: Node = prev.into();
        let new: Node = new.into();
        use self::Node::*;
        match prev {
            /* IMPROVEMENT?
            This might be more elegant, but does not do the exact same:
            It fails if there are multiple projects to that pin!
            Node::Div(div) => {
                div.out_proj_res().then(|res| Graph::exchange(res, const_node))
                div.out_proj_m().then(|mem| Graph::exchange(mem, div.mem()))
            }
            */
            Div(node) => {
                for out_node in node.out_nodes() {
                    match out_node {
                        Proj(res_proj, ProjKind::Div_Res(_)) => {
                            Graph::exchange(&res_proj, &new);
                        }
                        Proj(m_proj, ProjKind::Div_M(_)) => {
                            Graph::exchange(&m_proj, &node.mem());
                        }
                        _ => {}
                    }
                }
            }
            Mod(node) => {
                for out_node in node.out_nodes() {
                    match out_node {
                        Proj(res_proj, ProjKind::Mod_Res(_)) => {
                            Graph::exchange(&res_proj, &new);
                        }
                        Proj(m_proj, ProjKind::Mod_M(_)) => {
                            Graph::exchange(&m_proj, &node.mem());
                        }
                        _ => {}
                    }
                }
            }
            node => {
                Graph::exchange(&node, &new);
            }
        }
    }

    /// Replace the given node with a "bad" node, thus marking it and all the
    /// nodes dominated by it as unreachable. The whole subtree can then be
    /// removed using `Graph::remove_bads`.
    pub fn mark_as_bad(self, node: &impl NodeTrait) {
        Graph::exchange(node, &self.new_bad(Mode::b()))
    }

    pub fn copy_node<F>(self, node: Node, mut copy_fn: F) -> Node
    where
        F: FnMut(Node) -> Node,
    {
        unsafe {
            let ptr = node.internal_ir_node();
            let op = bindings::get_irn_op(ptr);
            let mode = bindings::get_irn_mode(ptr);
            let arity = bindings::get_irn_arity(ptr);

            let block = if Node::is_block(node) {
                ptr::null_mut()
            } else {
                copy_fn(node.block().into()).internal_ir_node()
            };

            let ins: Vec<_> = node
                .in_nodes()
                .map(copy_fn)
                .map(|n| n.internal_ir_node())
                .collect();

            let new_node_ptr = bindings::new_ir_node(
                ptr::null_mut(),
                self.irg,
                block,
                op,
                mode,
                arity,
                ins.as_ptr(),
            );
            bindings::copy_node_attr(self.irg, ptr, new_node_ptr);

            NodeFactory::node(new_node_ptr)
        }
    }

    // == Construction ==
    /*
    pub fn value(self, slot_idx: usize, mode: Mode) -> Node {
        NodeFactory::node(unsafe {
            bindings::get_r_value(self.irg, slot_idx as i32, mode.libfirm_mode())
        })
    }

    pub fn set_value(self, slot_idx: usize, val: Node) {
        unsafe { bindings::set_r_value(self.irg, slot_idx as i32, val.internal_ir_node()) }
    }

    pub fn cur_store(self) -> Node {
        NodeFactory::node(unsafe { bindings::get_r_store(self.irg) })
    }

    pub fn set_store(self, s: Node) {
        unsafe { bindings::set_r_store(self.irg, s.into()) }
    }

    pub fn cur_block(self) -> Block {
        Block::new(unsafe { bindings::get_r_cur_block(self.irg) }.into())
    }

    pub fn set_cur_block(self, block: Block) {
        unsafe { bindings::set_r_cur_block(self.irg, block.into()) }
    }
    */

    pub fn new_imm_block(self, preds: &[Node]) -> Block {
        let block = Block::new(unsafe { bindings::new_r_immBlock(self.irg) });
        for pred in preds {
            block.imm_add_pred(*pred);
        }
        block
    }

    pub fn slots(self) -> i32 {
        unsafe { bindings::get_irg_n_locs(self.irg) }
    }

    pub fn finalize_construction(self) {
        unsafe {
            bindings::irg_finalize_cons(self.irg);
        }
    }
}

unsafe extern "C" fn closure_handler(node: *mut bindings::ir_node, closure: *mut c_void) {
    #[allow(clippy::transmute_ptr_to_ref)]
    let closure: &mut &mut FnMut(&Node) = mem::transmute(closure);
    closure(&NodeFactory::node(node))
}
