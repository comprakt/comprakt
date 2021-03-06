use super::{
    entity::Entity,
    mode::Mode,
    nodes::{Block, End, EndKeepAliveIterator, NoMem, Node, NodeTrait, Proj, Start},
};
use libfirm_rs_bindings as bindings;
use std::{
    ffi::{c_void, CString},
    mem, ptr,
};

#[derive(Debug, Clone, Copy, From)]
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
    pub fn function_with_entity(entity: Entity, num_slots: usize) -> Self {
        unsafe {
            let irg = bindings::new_ir_graph(entity.ir_entity(), num_slots as i32);
            Self { irg }
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

    pub fn end_keep_alives(self) -> EndKeepAliveIterator {
        self.end().keep_alives()
    }

    pub fn args(self) -> Proj {
        Proj::new(unsafe { bindings::get_irg_args(self.irg) })
    }

    pub fn no_mem(self) -> NoMem {
        NoMem::new(unsafe { bindings::get_irg_no_mem(self.irg) })
    }

    pub fn frame(self) -> Node {
        Node::wrap(unsafe { bindings::get_irg_frame(self.irg) })
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

    pub fn recompute_outs(self) {
        unsafe { bindings::compute_irg_outs(self.irg) }
    }

    pub fn assure_loopinfo(self) {
        unsafe { bindings::assure_loopinfo(self.irg) }
    }

    pub fn compute_doms(self) {
        unsafe { bindings::compute_doms(self.irg) }
    }

    /// Compute all post dominator information in the graph
    ///
    /// NOTE: Also constructs out information
    /// NOTE: calling this function multiple times without changing
    ///       the graph will not recompute information and is cheap.
    pub fn compute_postdoms(self) {
        unsafe { bindings::compute_postdoms(self.irg) }
    }

    pub fn remove_bads(self) {
        unsafe { bindings::remove_bads(self.irg) }
    }

    pub fn remove_critical_cf_edges(self) {
        unsafe { bindings::remove_critical_cf_edges(self.irg) }
    }

    pub fn remove_unreachable_code(self) {
        unsafe { bindings::remove_unreachable_code(self.irg) }
    }

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

    pub fn walk_blkwise_dom_top_down<F>(self, mut walker: F)
    where
        F: FnMut(&Node),
    {
        // We need the type ascription here, because otherwise rust infers `&mut F`,
        // but in `closure_handler` we transmute to `&mut &mut dyn FnMut(_)` (because
        // `closure_handler` doesn't know the concrete `F`.
        let mut fat_pointer: &mut dyn FnMut(&Node) = &mut walker;
        let thin_pointer = &mut fat_pointer;

        unsafe {
            bindings::irg_walk_blkwise_dom_top_down(
                self.irg,
                None,
                Some(closure_handler),
                thin_pointer as *mut &mut _ as *mut c_void,
            );
        }
    }

    pub fn walk<F>(self, mut walker: F)
    where
        F: FnMut(&Node),
    {
        // We need the type ascription here, because otherwise rust infers `&mut F`,
        // but in `closure_handler` we transmute to `&mut &mut dyn FnMut(_)` (because
        // `closure_handler` doesn't know the concrete `F`.
        let mut fat_pointer: &mut dyn FnMut(&Node) = &mut walker;
        let thin_pointer = &mut fat_pointer;

        unsafe {
            bindings::irg_walk_graph(
                self.irg,
                None,
                Some(closure_handler),
                thin_pointer as *mut &mut _ as *mut c_void,
            );
        }
    }

    pub fn walk_blocks_postorder<F>(self, mut walker: F)
    where
        F: FnMut(&Block),
    {
        let mut fat_pointer: &mut dyn FnMut(&Block) = &mut walker;
        let thin_pointer = &mut fat_pointer;

        unsafe {
            bindings::irg_block_walk_graph(
                self.irg,
                None,
                Some(closure_handler_walk_blocks),
                thin_pointer as *mut &mut _ as *mut c_void,
            );
        }
    }

    pub fn walk_dom_tree_postorder<F>(self, mut walker: F)
    where
        F: FnMut(&Block),
    {
        let mut fat_pointer: &mut dyn FnMut(&Block) = &mut walker;
        let thin_pointer = &mut fat_pointer;

        unsafe {
            bindings::dom_tree_walk_irg(
                self.irg,
                None,
                Some(closure_handler_walk_blocks),
                thin_pointer as *mut &mut _ as *mut c_void,
            );
        }
    }

    pub fn walk_postdom_tree_postorder<F>(self, mut walker: F)
    where
        F: FnMut(&Block),
    {
        let mut fat_pointer: &mut dyn FnMut(&Block) = &mut walker;
        let thin_pointer = &mut fat_pointer;

        unsafe {
            bindings::postdom_tree_walk_irg(
                self.irg,
                None,
                Some(closure_handler_walk_blocks),
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
    pub fn walk_blocks<F>(self, mut walker: F)
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

    pub fn nodes(self) -> Vec<Node> {
        let mut result = Vec::new();
        self.walk(|n| {
            result.push(*n);
        });
        result
    }

    pub fn exchange(prev: impl NodeTrait, new: impl NodeTrait) {
        unsafe {
            bindings::exchange(prev.internal_ir_node(), new.internal_ir_node());
        }
    }

    /// Replace the given node with a "bad" node, thus marking it and all the
    /// nodes dominated by it as unreachable. The whole subtree can then be
    /// removed using `Graph::remove_bads`.
    pub fn mark_as_bad(self, node: impl NodeTrait) {
        Self::exchange(node, self.new_bad(Mode::b()))
    }

    #[allow(clippy::similar_names)]
    pub fn copy_node_without_ins(self, node: Node, target: Option<Block>) -> Node {
        unsafe {
            let ptr = node.internal_ir_node();
            let op = bindings::get_irn_op(ptr);
            let mode = bindings::get_irn_mode(ptr);

            let block = if Node::is_block(node) {
                ptr::null_mut()
            } else {
                target
                    .expect("must have some value for non-block nodes")
                    .internal_ir_node()
            };

            let empty: Vec<*mut bindings::ir_node> = vec![];
            let new_node_ptr = bindings::new_ir_node(
                ptr::null_mut(),
                self.irg,
                block,
                op,
                mode,
                empty.len() as i32,
                empty.as_ptr(),
            );
            bindings::copy_node_attr(self.irg, ptr, new_node_ptr);

            Node::wrap(new_node_ptr)
        }
    }

    // == Construction ==

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
    closure(&Node::wrap(node))
}

unsafe extern "C" fn closure_handler_walk_blocks(
    block: *mut bindings::ir_node,
    closure: *mut c_void,
) {
    #[allow(clippy::transmute_ptr_to_ref)]
    let closure: &mut &mut FnMut(&Block) = mem::transmute(closure);
    closure(&Block::new(block))
}

unsafe extern "C" fn pre_closure_handler(node: *mut bindings::ir_node, closure: *mut c_void) {
    // TODO: is this allow correct, Joshua?
    #[allow(clippy::transmute_ptr_to_ref)]
    let closure: &mut &mut FnMut(VisitTime, &Block) = mem::transmute(closure);
    match Node::wrap(node) {
        Node::Block(block) => closure(VisitTime::BeforePredecessors, &block),
        _ => unreachable!("irg_block_walk_graph only walks over blocks"),
    }
}

unsafe extern "C" fn post_closure_handler(node: *mut bindings::ir_node, closure: *mut c_void) {
    // TODO: is this allow correct, Joshua?
    #[allow(clippy::transmute_ptr_to_ref)]
    let closure: &mut &mut FnMut(VisitTime, &Block) = mem::transmute(closure);
    match Node::wrap(node) {
        Node::Block(block) => closure(VisitTime::AfterPredecessors, &block),
        _ => unreachable!("irg_block_walk_graph only walks over blocks"),
    }
}

#[derive(Debug, PartialEq, Eq)]
pub enum VisitTime {
    BeforePredecessors,
    AfterPredecessors,
}
