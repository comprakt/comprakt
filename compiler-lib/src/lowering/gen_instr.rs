use super::{
    lir::{Allocator, *},
    lir_allocator::Ptr,
};
use itertools::Itertools;
use libfirm_rs::{
    nodes::{Node, NodeTrait, ProjKind},
    types::{Ty, TyTrait},
    Entity,
};
use std::{collections::HashMap, convert::TryInto};
use strum_macros::*;

#[derive(Debug, Clone, EnumDiscriminants)]
// TODO naming
/// This enum describes how a FIRM node has been converted to instructions, and
/// where the potential result is stored.
enum Computed {
    /// Instructions that generate the value are in CFGpred, and
    /// `lir.rs::construct_flows` already set up the necessary copy-out/ copy-in
    /// code.
    InCFGPred(Ptr<MultiSlot>),
    /// Instructions for this value have been emitted in this block, but the
    /// instructions do not produce a result (apart from mem flow).
    Void,
    /// Instructions for this value have been emitted in this block and the
    /// result was written to MultiSlot.
    Value(Ptr<MultiSlot>),
}

pub struct GenInstrBlock {
    code: Code<CopyPropagation, CopyPropagation, Instruction, Leave>,
    computed: HashMap<Node, Computed>,
}

impl GenInstrBlock {
    pub(super) fn fill_instrs(graph: &BlockGraph, mut block: Ptr<BasicBlock>, alloc: &Allocator) {
        let mut b = GenInstrBlock {
            code: Code::default(),
            computed: HashMap::new(),
        };
        if let Some(start_node) = block.start_node() {
            if let Some(proj_args) = start_node.out_proj_t_args() {
                for arg in proj_args.out_nodes() {
                    if let Node::Proj(_, ProjKind::Start_TArgs_Arg(idx, ..)) = arg {
                        b.code.body.push(Instruction::LoadParam { idx });
                    } else {
                        unreachable!(
                            "the proj node Start_TArgs has only \
                             Start_TArgs_Arg projs as out nodes"
                        );
                    }
                }
            }
        }
        b.gen(graph, block, alloc);
        let GenInstrBlock { code, .. } = b;
        block.code = code;
    }

    fn comment(&mut self, args: std::fmt::Arguments<'_>) {
        self.code
            .body
            .push(Instruction::Comment(format!("{}", args)));
    }

    fn gen(&mut self, graph: &BlockGraph, block: Ptr<BasicBlock>, alloc: &Allocator) {
        for (num, multislot) in block.regs.iter().enumerate() {
            self.comment(format_args!("Slot {}:", num));

            // LIR has mem nodes (mem Phi's in particular) in value slots because it enables
            // code generation (`values_to_compute`, see below) to just use post-order DFS
            // starting from each out-flowed value. Mem nodes need to be included in
            // values_to_compute because of memory-only side effects (e.g. a function body
            // that only calls other functions will only have out-flowing mems.
            //
            // However, we don't want to burden consumers of the LIR with that
            // implementation ~~detail~~ hack, so let's filter out any copy
            // propagation that uses mem nodes. Note that this leaves the mem
            // nodes' value slots intact, but the LIR consumer will usually not
            // enumerate over those, so that's fine for now -,-.
            let not_a_mem_flow = &|src: Ptr<MultiSlot>, dst: Ptr<ValueSlot>| -> bool {
                let src_is_mem = src.firm().mode().is_mem();
                let dst_is_mem = dst.firm.mode().is_mem();
                assert!((src_is_mem && dst_is_mem) || (!src_is_mem && !dst_is_mem));
                !src_is_mem && !dst_is_mem
            };

            let slot_to_copy_propagation_src = &|src: Ptr<MultiSlot>| -> CopyPropagationSrc {
                match src.firm() {
                    Node::Proj(_, ProjKind::Start_TArgs_Arg(idx, ..)) => {
                        CopyPropagationSrc::Param { idx }
                    }
                    Node::Const(val) => CopyPropagationSrc::Imm(val.tarval()),
                    _ => CopyPropagationSrc::Slot(src),
                }
            };

            // Fill copy_in
            for edge in block.preds.iter() {
                edge.register_transitions
                    .iter()
                    .enumerate()
                    .filter(|(_, (_, dst))| dst.num == num)
                    .filter(|(idx, _)| {
                        let must_copy_in_source = edge.must_copy_in_source(*idx);
                        assert!(
                            // "must_copy_in_source => !must_copy_in_target"
                            !must_copy_in_source || !edge.must_copy_in_target(*idx),
                            "possible lost-copy detected"
                        );

                        // We copy everything that doesn't have to be copied in `source` in `target`
                        // (this includes everything that *has* to be copied in `target`), as
                        // demonstrated by the above assertion
                        !must_copy_in_source
                    })
                    .filter(|(_, (src, dst))| not_a_mem_flow(*src, *dst))
                    .for_each(|(_, (src, dst))| {
                        let (src, dst) = (*src, *dst);
                        let src = slot_to_copy_propagation_src(src);
                        self.code.copy_in.push(CopyPropagation { src, dst });
                    })
            }

            // Debug output ( @josh still necessary?)
            match &(**multislot) {
                MultiSlot::Single(slot) => self.comment(format_args!("\t=  {:?}", slot.firm)),
                MultiSlot::Multi { ref slots, .. } => {
                    for slot in slots {
                        self.comment(format_args!("\t=  {:?}", slot.firm));
                    }
                }
            }

            // Fill copy_out
            for edge in block.succs.iter() {
                edge.register_transitions
                    .iter()
                    .enumerate()
                    .filter(|(_, (src, _))| src.num() == num)
                    .filter(|(idx, _)| edge.must_copy_in_source(*idx))
                    .filter(|(_, (src, dst))| not_a_mem_flow(*src, *dst))
                    .for_each(|(_, (src, dst))| {
                        let (src, dst) = (*src, *dst);
                        let src = slot_to_copy_propagation_src(src);
                        self.code.copy_out.push(CopyPropagation { src, dst });
                    })
            }
        }

        // LIR conversion
        // We compute the values required by our successors.
        let values_to_compute = {
            let mut v: Vec<Node> = Vec::new();

            v.extend(
                block
                    .succs
                    .iter()
                    .flat_map(move |out_edge| {
                        out_edge
                            .register_transitions
                            .iter()
                            .map(|(src_slot, _)| src_slot)
                            .collect::<Vec<_>>()
                    })
                    .map(|src_slot| src_slot.firm())
                    .dedup(),
            );

            // Apart from the values that _must_ be flown out, all leave
            // instructions must be computed to account for side-effects calls, etc.
            // => return_nodes, jmp_nodes, cond_nodes
            let leave_nodes = block
                .firm
                .out_nodes()
                .filter(|n| Node::is_return(*n) || Node::is_jmp(*n) || Node::is_cond(*n))
                .collect::<Vec<_>>();
            log::debug!("block {:?} leave nodes: {:?}", block.firm, leave_nodes);
            v.extend(leave_nodes);

            v
        };

        let already_computed = block
            .preds
            .iter()
            .flat_map(|in_edge| {
                in_edge
                    .register_transitions
                    .iter()
                    .map(|(_, dst_slot)| dst_slot.multislot())
                    .collect::<Vec<_>>()
            })
            // if the same value flows in over multiple preds, ignore the dupes
            .unique_by(|dst_slot| dst_slot.firm());

        for dst_slot in already_computed {
            log::debug!(
                "InCFGPred for slot.num={:?} {:?}",
                dst_slot.num(),
                dst_slot.firm()
            );
            self.mark_computed(dst_slot.firm(), Computed::InCFGPred(dst_slot));
        }

        log::debug!("values to compute = {:?}", values_to_compute);
        for out_value in values_to_compute {
            log::debug!("gen code for out_value={:?}", out_value);
            self.comment(format_args!("\tgen code for out_value={:?}", out_value));
            self.gen_value(graph, block, alloc, out_value);
        }
    }

    fn is_computed(&self, node: Node) -> bool {
        self.computed.contains_key(&node)
    }

    fn must_computed(&self, node: Node) -> &Computed {
        self.computed
            .get(&node)
            .unwrap_or_else(|| panic!("must have computed value for node {:?}", node))
    }

    fn mark_computed(&mut self, node: Node, computed: Computed) {
        let did_overwrite = self.computed.insert(node, computed);
        debug_assert!(
            did_overwrite.is_none(),
            "duplicate computed for {:?}: {:?}",
            node,
            did_overwrite
        );
    }

    fn gen_value(
        &mut self,
        graph: &BlockGraph,
        block: Ptr<BasicBlock>,
        alloc: &Allocator,
        out_value: Node,
    ) {
        out_value.walk_dfs_in_block_stop_at_phi_node(block.firm, &mut |n| {
            self.gen_value_walk_callback(graph, block, alloc, n)
        });
    }

    fn gen_dst_slot(
        &mut self,
        block: Ptr<BasicBlock>,
        node: Node,
        alloc: &Allocator,
    ) -> Ptr<MultiSlot> {
        let dst_slot = block.new_private_slot(node, alloc);
        self.mark_computed(node, Computed::Value(dst_slot));
        dst_slot
    }

    fn gen_operand_jit(&self, node: Node) -> Operand {
        match node {
            Node::Const(c) => Operand::Imm(c.tarval()),
            Node::Size(s) => Operand::Imm(libfirm_rs::Tarval::mj_int(i64::from(s.ty().size()))),
            Node::Proj(_, ProjKind::Start_TArgs_Arg(idx, ..)) => Operand::Param { idx },
            n => match self.must_computed(n) {
                Computed::InCFGPred(ms) | Computed::Value(ms) => Operand::Slot(*ms),
                Computed::Void => panic!("expecting computed operand for {:?}, got Void", n),
            },
        }
    }

    fn gen_value_walk_callback(
        &mut self,
        graph: &BlockGraph,
        block: Ptr<BasicBlock>,
        alloc: &Allocator,
        node: Node,
    ) {
        log::debug!("visit node={:?}", node);
        if self.is_computed(node) {
            log::debug!("\tnode is already computed");
            return;
        }

        use self::{BinopKind::*, UnopKind::*};
        macro_rules! op_operand {
            ($name:ident, $op:expr) => {{
                let node = $op.$name();
                self.gen_operand_jit(node)
            }};
        }
        macro_rules! gen_binop {
            ($kind:ident, $op:expr, $block:expr, $node:expr) => {{
                let (src1, src2, dst) = gen_binop!(@INTERNAL, $op, $block, $node);
                self.code.body.push(Instruction::Binop {
                    kind: $kind,
                    src1,
                    src2,
                    dst,
                });
            }};
            (@DIV, $op:expr, $block:expr, $node:expr) => {{
                let (src1, src2, dst) = gen_binop!(@INTERNAL, $op, $block, $node);
                self.code.body.push(Instruction::Div {
                    src1,
                    src2,
                    dst,
                });
            }};
            (@MOD, $op:expr, $block:expr, $node:expr) => {{
                let (src1, src2, dst) = gen_binop!(@INTERNAL, $op, $block, $node);
                self.code.body.push(Instruction::Mod {
                    src1,
                    src2,
                    dst,
                });
            }};
            (@INTERNAL, $op:expr, $block:expr, $node:expr) => {{
                let src1 = op_operand!(left, $op);
                let src2 = op_operand!(right, $op);
                let dst = self.gen_dst_slot($block, $node, alloc);
                (src1, src2, dst)
            }};
        }
        macro_rules! gen_unop {
            ($kind:ident, $op:expr, $block:expr, $node:expr) => {{
                let src = op_operand!(op, $op);
                let dst = self.gen_dst_slot($block, $node, alloc);
                self.code.body.push(Instruction::Unop {
                    kind: $kind,
                    src,
                    dst,
                });
            }};
        }
        match node {
            // Start node is always ready
            Node::Start(_) => {
                self.mark_computed(node, Computed::Void);
            }

            // The following group of nodes doesn't need code gen as
            // we know their result at compile time.
            // They are only used as operands and constructed in gen_operand_jit
            Node::Const(_)
            | Node::Proj(_, ProjKind::Start_TArgs_Arg(..))
            | Node::Address(_)
            | Node::Member(_)
            | Node::Sel(_)
            | Node::Size(_) => (),

            Node::Conv(conv) => {
                let pred = conv.op();
                let src = self.gen_operand_jit(pred);
                let dst = self.gen_dst_slot(block, node, alloc);
                self.code.body.push(Instruction::Conv { src, dst });
            }

            Node::Add(add) => gen_binop!(Add, add, block, node),
            Node::Sub(sub) => gen_binop!(Sub, sub, block, node),
            Node::Mul(mul) => gen_binop!(Mul, mul, block, node),
            Node::Div(div) => gen_binop!(@DIV, div, block, node),
            Node::Mod(mod_) => gen_binop!(@MOD, mod_, block, node),
            Node::And(and) => gen_binop!(And, and, block, node),
            Node::Or(or) => gen_binop!(Or, or, block, node),
            Node::Eor(or) => gen_binop!(Xor, or, block, node),
            Node::Not(not) => gen_unop!(Not, not, block, node),
            Node::Minus(neg) => gen_unop!(Neg, neg, block, node),
            Node::Return(ret) => {
                let value: Option<Operand> = if ret.return_res().len() != 0 {
                    assert_eq!(ret.return_res().len(), 1);
                    log::debug!("{:?}", ret);
                    Some(self.gen_operand_jit(ret.return_res().idx(0).unwrap()))
                } else {
                    None
                };
                let end_block = graph.end_block;
                self.code.leave.push(Leave::Return { value, end_block });
            }
            Node::Jmp(jmp) => {
                let firm_target_block = jmp.out_target_block().unwrap();
                let target = block.graph.get_block(firm_target_block);
                self.code.leave.push(Leave::Jmp { target });
            }

            // Cmp and Cond are handled together
            Node::Cmp(cmp) => {
                let succs = cmp.out_nodes().collect::<Vec<_>>();
                assert_eq!(1, succs.len());
                match succs[0] {
                    Node::Cond(_) => (),
                    x => panic!(
                        "{:?} expected to be followed by Cond node, but got {:?}",
                        cmp, x
                    ),
                }
            }
            Node::Cond(cond) => {
                let preds = cond.in_nodes().collect::<Vec<_>>();
                assert_eq!(1, preds.len());
                let cmp = match preds[0] {
                    Node::Cmp(cmp) => cmp,
                    x => panic!(
                        "{:?} expected to be preceded by Cmp node, but got {:?}",
                        cond, x
                    ),
                };
                let op: CondOp = cmp.relation().try_into().unwrap();
                let lhs = self.gen_operand_jit(cmp.left());
                let rhs = self.gen_operand_jit(cmp.right());
                macro_rules! cond_target {
                    ($branch:expr) => {{
                        let (_, block, _) = cond.out_proj_target_block($branch).unwrap();
                        graph.get_block(block)
                    }};
                }
                let true_target = cond_target!(true);
                let false_target = cond_target!(false);
                self.code.leave.push(Leave::CondJmp {
                    op,
                    lhs,
                    rhs,
                    true_target,
                    false_target,
                });
            }

            // Call nodes are special, in that they do not create a value directly:
            // To get the return value of a call, the following projection chain is found in the
            // graph: Call => Call_TResult => Call_TResult_Arg
            // Like other nodes, a Call node may produce at most one value slot.
            // And that value slot's FIRM value _must_ be the return value (an int, bool, ptr)
            // returned by the call, because this is the value we need to flow over the edges.
            // That return value is the Call_TResult_Arg.
            // However, Call_TResult and Call must have entries in self.computed because
            // other Projs, e.g. Call_M expect there to be a value for Call.
            Node::Call(call) => {
                log::debug!("call={:?}", call);
                let func: Entity = match call.ptr() {
                    Node::Address(addr) => addr.entity(),
                    x => panic!("call must go to an Address node, got {:?}", x),
                };
                assert!(func.ty().is_method());
                log::debug!("called func={:?}", func.ld_name());

                // Allocate value slot for this call node as described in match arm comment
                let dst = {
                    // Find this call node's result tuple projection.
                    // This code expects there to be at most one.
                    let result_tuple = {
                        let result_tuple_projs = call
                            .out_nodes()
                            .filter_map(|n| match n {
                                Node::Proj(rt, ProjKind::Call_TResult(_)) => Some(rt),
                                _ => None,
                            })
                            .collect::<Vec<_>>();
                        assert!(result_tuple_projs.len() <= 1);
                        result_tuple_projs.into_iter().next()
                    };
                    log::debug!("call: result tuple proj: {:?}", result_tuple);

                    let result_tuple_0 = result_tuple.map(|rt| {
                        let tuple_elems = rt.all_out_projs();
                        // Not sure if we are allowed to expect the following,
                        // but we require it because tuple_elems[0] will be
                        // the FIRM node for the ValueSlot produced by this call.
                        // And there cannot be multiple ValueSlots produced by a call.
                        // So this assumption better hold!
                        assert_eq!(tuple_elems.len(), 1);
                        tuple_elems[0]
                    });

                    if let Some(elem0) = result_tuple_0 {
                        // (gen_dst_slot has implicit mark_computed)
                        let slot = self.gen_dst_slot(block, Node::from(elem0), alloc);
                        // but other nodes will require Call to be computed
                        self.mark_computed(Node::from(call), Computed::Value(slot));
                        // and for completeness, do the same for the resul_tuple
                        self.mark_computed(
                            Node::from(result_tuple.unwrap()),
                            Computed::Value(slot),
                        );
                        Some(slot)
                    } else {
                        self.mark_computed(Node::from(call), Computed::Void);
                        if let Some(rt) = result_tuple {
                            // can this even happen? would be a dangling tuple
                            self.mark_computed(Node::from(rt), Computed::Void);
                        }
                        // result_tuple_0 is None, so nothing to mark as computed
                        None
                    }
                };
                log::debug!("\tdst slot {:?}", dst);

                let args = call
                    .args()
                    .map(|node| {
                        log::debug!("\tparam node {:?}", node);
                        self.gen_operand_jit(node)
                    })
                    .collect();

                let func_name = func.ld_name().to_str().unwrap().to_owned();
                self.code.body.push(Instruction::Call {
                    func: func_name,
                    args,
                    dst,
                });
            }
            // Call_TResult and Call_TResult_Arg are handled in Node::Call match arm
            Node::Proj(_, ProjKind::Call_TResult(_))
            | Node::Proj(_, ProjKind::Call_TResult_Arg(_, _, _)) => {}

            Node::Unknown(_) => {
                // TODO ??? this happens in the runtime function wrapper's
                // arguments
                //
                // write somehting to computed for now
                //
                let dst_slot = block.new_private_slot(node, alloc);
                self.mark_computed(node, Computed::Value(dst_slot));
            }

            // Load itself does not create a computed value, only its result projection does.
            Node::Load(load) => {
                let (src, size) = self.gen_address_computation(load.ptr());
                self.mark_computed(node, Computed::Void);
                if let Some(res_proj) = load.out_proj_res() {
                    let dst = self.gen_dst_slot(block, Node::from(res_proj), alloc);
                    self.code
                        .body
                        .push(Instruction::LoadMem(LoadMem { src, dst, size }));
                }
            }
            // handled in Node::Load match arm
            Node::Proj(_, ProjKind::Load_Res(_)) => {}

            Node::Store(store) => {
                let src = self.gen_operand_jit(store.value());
                let (dst, size) = self.gen_address_computation(store.ptr());
                self.code
                    .body
                    .push(Instruction::StoreMem(StoreMem { src, dst, size }));
                let dst_slot = block.new_private_slot(node, alloc);
                self.mark_computed(node, Computed::Value(dst_slot));
            }

            // The default case for proj is that its predecessor must have been visited before and
            // must have been marked as computed (i.e. produced a value).
            Node::Proj(proj, kind) => {
                log::debug!("proj={:?} kind={:?}", proj, kind);
                // copy the value produced by predecessor
                let pred = proj.pred();
                let pred_slot = self.must_computed(pred).clone();
                self.mark_computed(node, pred_slot);
            }

            x => panic!("unimplemented: {:?}", x),
        }
    }

    fn gen_address_computation(&self, node: Node) -> (AddressComputation<Operand>, u32) {
        match node {
            Node::Member(member) => {
                let base = self.gen_operand_jit(member.ptr());
                let index = IndexComputation::Zero;
                let offset = member.entity().offset() as isize;
                let member_size = member.entity().ty().size();

                (
                    AddressComputation {
                        offset,
                        base,
                        index,
                    },
                    member_size,
                )
            }
            Node::Sel(sel) => {
                let base = self.gen_operand_jit(sel.ptr());
                let elem_ty = if let Ty::Array(arr) = sel.ty() {
                    arr.element_type()
                } else {
                    unreachable!("Sel has always ArrayTy");
                };
                let elem_size = elem_ty.size();

                let index = if elem_size == 0 {
                    IndexComputation::Zero
                } else {
                    let idx = self.gen_operand_jit(sel.index());
                    IndexComputation::Displacement(
                        idx,
                        match elem_size {
                            1 => Stride::One,
                            2 => Stride::Two,
                            4 => Stride::Four,
                            8 => Stride::Eight,
                            _ => unreachable!("Unexpected element size: {}", elem_size),
                        },
                    )
                };

                (
                    AddressComputation {
                        offset: 0,
                        base,
                        index,
                    },
                    elem_size,
                )
            }
            _ => unreachable!("Load/Store nodes only have Sel and Member nodes as input"),
        }
    }
}
