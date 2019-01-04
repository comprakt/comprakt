use super::firm_program::FirmProgram;
use libfirm_rs::{
    nodes::{Node, NodeTrait},
    types::{Ty, TyTrait},
    Graph, Tarval,
};

pub fn lower_ir_nodes(program: &FirmProgram<'_, '_>) {
    for method in program.methods.values() {
        if let Some(graph) = method.borrow().graph {
            graph.walk_topological(|node| match node {
                Node::Sel(sel) => {
                    let ptr = sel.ptr();
                    let elem_ty = if let Ty::Array(arr) = sel.ty() {
                        arr.element_type()
                    } else {
                        unreachable!("Sel has always ArrayTy");
                    };
                    let elem_size = elem_ty.size();

                    let new_node: Node = if elem_size == 0 {
                        ptr
                    } else {
                        let idx = sel.index();
                        let mode = sel.mode();
                        let offset_mode = mode.reference_offset_mode();
                        let idx_conv = node.block().new_conv(idx, offset_mode);

                        let scaled_idx: Node = if elem_size == 1 {
                            idx_conv.into()
                        } else {
                            let elem_size =
                                graph.new_const(Tarval::val(elem_size.into(), offset_mode));
                            node.block().new_mul(idx_conv, elem_size).into()
                        };
                        node.block().new_add(ptr, scaled_idx).into()
                    };

                    Graph::exchange(*node, new_node);
                }
                Node::Member(member) => {
                    let ptr = member.ptr();
                    let entity = member.entity();
                    let offset = entity.offset();

                    let new_node = if offset == 0 {
                        ptr
                    } else {
                        let mode = member.mode();
                        let offset_mode = mode.reference_offset_mode();
                        let offset = graph.new_const(Tarval::val(offset.into(), offset_mode));
                        node.block().new_add(ptr, offset).into()
                    };

                    Graph::exchange(*node, new_node);
                }
                _ => (), // Only Member and Sel nodes need to be transformed
            });
        }
    }
}
