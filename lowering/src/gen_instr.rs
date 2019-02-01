use super::{lir::*, lir_allocator::Ptr};
use libfirm_rs::{
    nodes::{Node, NodeTrait, ProjKind},
    types::{Ty, TyTrait},
    Entity,
};
use std::{collections::HashMap, convert::TryInto};
use strum_macros::*;

#[derive(Debug, Clone, EnumDiscriminants)]
// TODO naming
/// This enum describes how a FIRM node has been converted to instructions,
/// whether that instruction produced a result and in which var that result is
/// stored.
enum Computed {
    /// Instructions for this FIRM node have been emitted in this block, but the
    /// instructions do not produce a result.
    Void,
    /// Instructions for this value have been emitted in this block and the
    /// result was written to Var.
    Value(Var),
}

pub struct GenInstrBlock {
    body: Vec<Instruction>,
    leave: Vec<Leave>,
    computed: HashMap<Node, Computed>,
}

impl GenInstrBlock {
    pub(super) fn fill_instrs(graph: Ptr<BlockGraph>, mut block: Ptr<BasicBlock>) {
        let mut b = GenInstrBlock {
            body: vec![],
            leave: vec![],
            computed: HashMap::new(),
        };
        b.gen(graph, block);
        let GenInstrBlock { body, leave, .. } = b;
        // do not overwrite block.code because it already contains instrs from
        // construction / LoadParam
        block.code.body.extend(body);
        block.code.leave.extend(leave);
    }

    fn gen(&mut self, graph: Ptr<BlockGraph>, block: Ptr<BasicBlock>) {
        for ValueReq { firm, .. } in &block.value_requirements {
            log::debug!("REQ {:?}", firm);
            let computed = if graph.can_be_var(*firm) {
                Computed::Value(graph.var(*firm))
            } else {
                Computed::Void
            };
            self.mark_computed(*firm, computed);
        }
        for node in &block.values_to_compute {
            log::debug!("TO COMPUTE: {:?}", node);
        }
        for node in &block.values_to_compute {
            log::debug!("=> COMPUTE: {:?}", node);
            debug_assert!(node.block() == block.firm);
            self.gen_value(graph, block, *node);
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

    fn gen_value(&mut self, graph: Ptr<BlockGraph>, block: Ptr<BasicBlock>, out_value: Node) {
        out_value.walk_dfs_in_block_stop_at_phi_node(block.firm, &mut |n| {
            self.gen_value_walk_callback(graph, block, n)
        });
    }

    fn gen_dst_var(&mut self, block: Ptr<BasicBlock>, node: Node) -> Var {
        let var = block.graph.var(node);
        self.mark_computed(node, Computed::Value(var));
        var
    }

    fn gen_operand_jit(&self, node: Node) -> Operand {
        match node {
            Node::Const(c) => Operand::Imm(c.tarval()),
            Node::Size(s) => Operand::Imm(libfirm_rs::Tarval::mj_int(i64::from(s.ty().size()))),
            n => match self.must_computed(n) {
                Computed::Value(ms) => Operand::Var(*ms),
                Computed::Void => panic!("expecting computed operand for {:?}, got Void", n),
            },
        }
    }

    /// `gen_value_walk_callback` is the callback for a DFS post-order traversal
    /// over the nodes of the given `block` which
    ///     1. stops at Phi nodes
    ///     2. does not cross block boundaries (green dots in ycomp)
    /// The DFS invokes `gen_value_walk_callback` for each `node: Node` in the
    /// traversal. For each node, this function determines the node's entry
    /// in `self.computed`:
    ///
    /// ### `Node::{Alloc,Call,Load,Div,Mod}`
    /// Do not produce a value themselves: That is because these
    /// operations have mem and result projections in libFIRM. Thus, the
    /// value-result for these node kinds is their result out projection.
    /// (Above list was determined by a search for `pn_(.*)_res` over the
    /// libfirm code base)
    ///
    /// ### `Node::{Const,Address,Member,Sel,Size}`
    /// Are compile-time constants. While we allocate vars for them, match
    /// arms for other node types night use their tarval as an immediate.
    /// Note that const nodes that are preds of Phi nodes are converted
    /// to variable definitions in ssa_deconstruction.
    ///
    /// ### Projections of `ProjKind::Start_TArgs_Arg`
    /// Are almost like constants, but are also special in that for each of
    /// them, a `LoadParm` instruction is emitted at the beginning of the start
    /// block, which is an implicit requirement of the later stages of the
    /// backend.
    ///
    /// ### Other Projections
    /// Projections that are not result projections of the above nodes copy
    /// their predecessors `self.computed` value.
    ///
    /// ### All other nodes
    /// All other nodes produce a value value.
    fn gen_value_walk_callback(
        &mut self,
        graph: Ptr<BlockGraph>,
        block: Ptr<BasicBlock>,
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
                self.body.push(Instruction::Binop {
                    kind: $kind,
                    src1,
                    src2,
                    dst,
                });
            }};
            (@INTERNAL, $op:expr, $block:expr, $node:expr) => {{
                let src1 = op_operand!(left, $op);
                let src2 = op_operand!(right, $op);
                let dst = self.gen_dst_var($block, $node);
                (src1, src2, dst)
            }};

            // DIV is special, see function-level comment
            (@DIV, $op:expr, $block:expr, $div_node:expr) => {{
                self.mark_computed(Node::from($div_node), Computed::Void);
                if let Some(res_proj) = $div_node.out_proj_res() {
                    let (src1, src2, dst) =
                        gen_binop!(@INTERNAL, $op, $block, Node::from(res_proj));
                    self.body.push(Instruction::Div {
                        src1,
                        src2,
                        dst,
                    });
                }
            }};
            // MOD is special, see function-level comment
            (@MOD, $op:expr, $block:expr, $mod_node:expr) => {{
                self.mark_computed(Node::from($mod_node), Computed::Void);
                if let Some(res_proj) = $mod_node.out_proj_res() {
                    let (src1, src2, dst) =
                        gen_binop!(@INTERNAL, $op, $block, Node::from(res_proj));
                    self.body.push(Instruction::Mod {
                        src1,
                        src2,
                        dst,
                    });
                }
            }};

        }
        macro_rules! gen_unop {
            ($kind:ident, $op:expr, $block:expr, $node:expr) => {{
                let src = op_operand!(op, $op);
                let dst = self.gen_dst_var($block, $node);
                self.body.push(Instruction::Unop {
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
            Node::Const(_) | Node::Address(_) | Node::Member(_) | Node::Sel(_) | Node::Size(_) => {}

            Node::Proj(_, ProjKind::Start_TArgs_Arg(..)) => {
                self.mark_computed(node, Computed::Value(graph.existing_var(node)))
            }

            Node::Phi(phi) => {
                if phi.mode().is_data() {
                    self.gen_dst_var(block, node);
                } else {
                    self.mark_computed(node, Computed::Void);
                }
            }

            Node::Conv(conv) => {
                let pred = conv.op();
                let src = self.gen_operand_jit(pred);
                let dst = self.gen_dst_var(block, node);
                self.body.push(Instruction::Conv { src, dst });
            }

            Node::Add(add) => gen_binop!(Add, add, block, node),
            Node::Sub(sub) => gen_binop!(Sub, sub, block, node),
            Node::Mul(mul) => gen_binop!(Mul, mul, block, node),
            // Div is special, see function-level comment
            Node::Div(div) => gen_binop!(@DIV, div, block, div),
            // Mod is special, see function-level comment
            Node::Mod(mod_) => gen_binop!(@MOD, mod_, block, mod_),
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
                self.leave.push(Leave::Return { value, end_block });
            }
            Node::Jmp(jmp) => {
                let firm_target_block = jmp.out_target_block().unwrap();
                let target = block.graph.get_block(firm_target_block);
                self.leave.push(Leave::Jmp { target });
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
                self.leave.push(Leave::CondJmp {
                    op,
                    lhs,
                    rhs,
                    true_target,
                    false_target,
                });
                self.mark_computed(node, Computed::Void);
            }

            // Only Call's result projection (if there is any) produces a value.
            // (See function-level comment)
            //
            // To get the return value of a call, the following projection chain is found in the
            // graph: Call => Call_TResult => Call_TResult_Arg
            Node::Call(call) => {
                log::debug!("call={:?}", call);
                let func: Entity = match call.ptr() {
                    Node::Address(addr) => addr.entity(),
                    x => panic!("call must go to an Address node, got {:?}", x),
                };
                assert!(func.ty().is_method());
                log::debug!("called func={:?}", func.ld_name());

                // Allocate var for this call node as described in match arm comment
                let dst = {
                    // Find this call node's result tuple projection.
                    // This code expects there to be at most one.
                    let result_tuple = call.out_proj_t_result();
                    log::debug!("call: result tuple proj: {:?}", result_tuple);

                    let result_tuple_0 = result_tuple.map(|rt| {
                        let tuple_elems = rt.all_out_projs();
                        // Not sure if we are allowed to expect the following,
                        // but we require it because tuple_elems[0] will be
                        // the FIRM node for the Var produced by this call.
                        // And there cannot be multiple Vars produced by a call.
                        // So this assumption better hold!
                        assert_eq!(tuple_elems.len(), 1);
                        tuple_elems[0]
                    });

                    if let Some(elem0) = result_tuple_0 {
                        // (gen_dst_var has implicit mark_computed)
                        let var = self.gen_dst_var(block, Node::from(elem0));
                        // but other nodes will require Call to be computed
                        self.mark_computed(Node::from(call), Computed::Value(var));
                        // and for completeness, do the same for the resul_tuple
                        self.mark_computed(
                            Node::from(result_tuple.unwrap()),
                            Computed::Value(var),
                        );
                        Some(var)
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

                let args = call
                    .args()
                    .map(|node| {
                        log::debug!("\tparam node {:?}", node);
                        self.gen_operand_jit(node)
                    })
                    .collect();

                let func_name = func.ld_name().to_str().unwrap().to_owned();
                self.body.push(Instruction::Call {
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
                let var = self.gen_dst_var(block, node);
                self.mark_computed(node, Computed::Value(var));
            }

            // Load itself does not create a computed value, only its result projection does.
            Node::Load(load) => {
                let (src, size) = self.gen_address_computation(load.ptr());
                self.mark_computed(node, Computed::Void);
                if let Some(res_proj) = load.out_proj_res() {
                    let dst = self.gen_dst_var(block, Node::from(res_proj));
                    self.body
                        .push(Instruction::LoadMem(LoadMem { src, dst, size }));
                }
            }
            // handled in Node::Load match arm
            Node::Proj(_, ProjKind::Load_Res(_)) => {}

            Node::Store(store) => {
                let src = self.gen_operand_jit(store.value());
                let (dst, size) = self.gen_address_computation(store.ptr());
                self.body
                    .push(Instruction::StoreMem(StoreMem { src, dst, size }));
                self.mark_computed(node, Computed::Void);
            }

            // The default case for proj is that its predecessor must have been visited before and
            // must have been marked as computed (i.e. produced a value).
            Node::Proj(proj, kind) => {
                log::debug!("proj={:?} kind={:?}", proj, kind);
                // copy the value produced by predecessor
                let pred = proj.pred();
                let pred_computed = self.must_computed(pred).clone();
                self.mark_computed(node, pred_computed);
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
