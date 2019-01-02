use super::{lir::*, molki};
use crate::utils::cell::{MutRc, MutWeak};
use itertools::Itertools;
use libfirm_rs::{
    nodes::{Node, NodeTrait},
    types::Ty,
    Entity,
};
use std::collections::HashMap;
use strum_macros::*;

#[derive(Debug, Clone, EnumDiscriminants)]
enum Computed {
    Void,
    Value(MutRc<ValueSlot>),
}
impl Computed {
    fn must_value(&self) -> std::cell::Ref<'_, ValueSlot> {
        match self {
            Computed::Value(vs) => vs.borrow(),
            x => panic!(
                "computed must be a value, got {:?}",
                ComputedDiscriminants::from(x)
            ),
        }
    }
}

pub struct GenInstrBlock {
    instrs: Vec<Instruction>,
    computed: HashMap<Node, Computed>,
}

impl GenInstrBlock {
    pub fn fill_instrs(block: MutRc<BasicBlock>) {
        let mut b = GenInstrBlock {
            instrs: Vec::new(),
            computed: HashMap::new(),
        };
        b.gen(MutRc::clone(&block));
        let GenInstrBlock { instrs, .. } = b;
        block.borrow_mut().code = instrs;
    }

    fn comment(&mut self, args: std::fmt::Arguments<'_>) {
        self.instrs.push(molki::Instr::Comment(format!("{}", args)));
    }

    fn gen(&mut self, block: MutRc<BasicBlock>) {
        // LIR metadata dump + fill computed values
        for (num, multislot) in block.borrow().regs.iter().enumerate() {
            self.comment(format_args!("Slot {}:", num));
            for slot in multislot {
                self.comment(format_args!("\t=  {:?}", slot.borrow().firm));
            }

            for edge in block.borrow().preds.iter() {
                edge.upgrade()
                    .unwrap()
                    .borrow()
                    .register_transitions
                    .iter()
                    .filter(|(_, dst)| dst.borrow().num == num)
                    .for_each(|(src, _)| {
                        self.comment(format_args!(
                            "\t<- {:?}({}): {:?}",
                            upborrow!(src.borrow().allocated_in).firm,
                            src.borrow().num,
                            src.borrow().firm
                        ));
                    })
            }
        }

        // LIR conversion
        // We compute the values required by our successors.
        let values_to_compute = {
            let mut v: Vec<Node> = Vec::new();

            v.extend(
                block
                    .borrow()
                    .succs
                    .iter()
                    .flat_map(move |out_edge| {
                        out_edge
                            .borrow()
                            .register_transitions
                            .iter()
                            .map(|(src_slot, _)| src_slot.clone())
                            .collect::<Vec<_>>()
                    })
                    .map(|src_slot| src_slot.borrow().firm)
                    .dedup(),
            );

            v.extend(
                block
                    .borrow()
                    .returns
                    .as_option()
                    .iter()
                    .map(|r| Node::Return(*r)),
            ); // Node::from missing
            v
        };

        for out_value in values_to_compute {
            self.comment(format_args!("\tgen code for out_value={:?}", out_value));
            self.gen_value(MutRc::clone(&block), out_value);
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

    fn must_computed_slot(&self, node: Node) -> std::cell::Ref<'_, ValueSlot> {
        self.must_computed(node).must_value()
    }

    fn mark_computed(&mut self, node: Node, computed: Computed) {
        let did_overwrite = self.computed.insert(node, computed);
        debug_assert!(did_overwrite.is_none(), "duplicate computed for {:?}", node);
    }

    fn gen_value(&mut self, block: MutRc<BasicBlock>, out_value: Node) {
        // force copy because we borrow_mut block when allocating private slots
        // inside the closure
        let block_firm = block.borrow().firm;
        out_value.walk_dfs_in_block(block_firm, &mut |n| {
            self.gen_value_walk_callback(MutRc::clone(&block), n)
        });
    }

    //FIXME: Just for faster dev, delete this later!!!
    #[allow(clippy::cyclomatic_complexity)]
    fn gen_value_walk_callback(&mut self, block: MutRc<BasicBlock>, node: Node) {
        log::debug!("visit node={:?}", node);
        if self.is_computed(node) {
            log::debug!("\tnode is already computed");
            return;
        }

        use self::molki::{BinopKind::*, DivKind::*, Instr, Operand, Reg};
        use libfirm_rs::Mode;
        macro_rules! binop_operand {
            ($side:ident, $op:expr) => {{
                let $side = $op.$side();
                if self.is_computed($side) {
                    let slot = self.must_computed_slot($side);
                    Reg::N(slot.num).into_operand()
                } else {
                    let tv = match $side {
                        Node::Const(c) => c.tarval(),
                        x => panic!(
                            "node must have been computed for {:?} or be const, error in DFS?",
                            x
                        ),
                    };
                    Operand::Imm(tv)
                }
            }}
        }
        macro_rules! gen_binop_with_dst {
            ($kind:ident, $op:expr, $block:expr, $node:expr) => {{
                let (src1, src2, dst) = gen_binop_with_dst!(@INTERNAL, $op, $block, $node);
                self.instrs.push(Instr::Binop {
                    kind: $kind,
                    src1,
                    src2,
                    dst: Some(dst),
                });
            }};
            (DIV, $kind:ident, $op:expr, $block:expr, $node:expr) => {{
                let (src1, src2, dst) = gen_binop_with_dst!(@INTERNAL, $op, $block, $node);
                self.instrs.push(Instr::Divop {
                    kind: $kind,
                    src1,
                    src2,
                    dst1: dst,
                    // We don't need the second register.
                    // Just write the mod result in a arbitrary register. This will hopefully be
                    // taken care of by the register allocator
                    dst2: Reg::N(usize::max_value()),
                });
            }};
            (MOD, $kind:ident, $op:expr, $block:expr, $node:expr) => {{
                let (src1, src2, dst) = gen_binop_with_dst!(@INTERNAL, $op, $block, $node);
                self.instrs.push(Instr::Divop {
                    kind: $kind,
                    src1,
                    src2,
                    // We don't need the first register.
                    // Just write the mod result in a arbitrary register. This will hopefully be
                    // taken care of by the register allocator
                    dst1: Reg::N(usize::max_value()),
                    dst2: dst,
                });
            }};
            (@INTERNAL, $op:expr, $block:expr, $node:expr) => {{
                let src1 = binop_operand!(left, $op);
                let src2 = binop_operand!(right, $op);

                let dst_slot = $block.new_private_slot($node); // internal borrow_mut
                self.mark_computed($node, Computed::Value(MutRc::clone(&dst_slot)));
                let dst = Reg::N(dst_slot.borrow().num);
                (src1, src2, dst)
            }};
        }
        match node {
            Node::Add(add) => gen_binop_with_dst!(Add, add, block, node),
            Node::Sub(sub) => gen_binop_with_dst!(Sub, sub, block, node),
            Node::Mul(mul) => gen_binop_with_dst!(Mul, mul, block, node),
            Node::Div(div) => gen_binop_with_dst!(DIV, IDiv, div, block, node),
            Node::Mod(mod_) => gen_binop_with_dst!(MOD, IDiv, mod_, block, node),
            Node::And(and) => gen_binop_with_dst!(And, and, block, node),
            Node::Or(or) => gen_binop_with_dst!(Or, or, block, node),
            Node::Not(_) | Node::Minus(_) => {
                // FIXME: No matching commands in molki
            }
            Node::Return(ret) => {
                if ret.return_res().len() != 0 {
                    assert_eq!(ret.return_res().len(), 1);
                    log::debug!("{:?}", ret);
                    let src = {
                        let retval_slot = self.must_computed_slot(ret.return_res().idx(0).unwrap());
                        log::debug!("{:?}", ret);
                        Reg::N(retval_slot.num).into_operand()
                    };
                    let dst = Reg::R0;
                    self.instrs.push(Instr::Movq { src, dst });
                } else {
                    self.comment(format_args!("ret"));
                }
            }
            Node::Proj(proj, _kind) => {
                let pred = proj.pred();
                if !self.is_computed(pred) {
                    debug_assert!(
                        Node::is_address(pred)
                            || Node::is_start(pred)
                            || pred.mode() == Mode::M()
                            || pred.mode() == Mode::X(),
                        "predecessor must produce value"
                    );
                } else {
                    // the normal case
                    // copy the value produced by predecessor
                    let pred_slot = self.must_computed(pred).clone();
                    self.mark_computed(node, pred_slot);
                }
            }
            Node::Call(call) => {
                log::debug!("call={:?}", call);
                let func: Entity = match call.ptr() {
                    Node::Address(addr) => addr.entity(),
                    x => panic!("call must go to an Address node, got {:?}", x),
                };
                assert!(func.ty().is_method());
                log::debug!("called func={:?}", func.ld_name());
                // extract the result (a tuple)
                // and extract that result tuple's 0th component if the function
                // returns

                let args = call
                    .args()
                    .map(|node| {
                        log::debug!("\tparam node {:?}", node);
                        match node {
                            Node::Const(c) => Operand::Imm(c.tarval()),
                            x => {
                                // TODO following is practically dup of Add code above
                                let value_slot = self.must_computed_slot(x);
                                let reg_num = value_slot.num;
                                Reg::N(reg_num).into_operand()
                            }
                        }
                    })
                    .collect();

                // TODO use type_system data here? need some cross reference...
                let n_res = if let Ty::Method(ty) = func.ty() {
                    ty.res_count()
                } else {
                    panic!("The type of a function is a method type");
                };
                let dst = if n_res == 0 {
                    self.mark_computed(node, Computed::Void);
                    None
                } else if n_res == 1 {
                    let dst_slot = block.new_private_slot(node); // internal borrow_mut
                    self.mark_computed(node, Computed::Value(MutRc::clone(&dst_slot)));
                    let num = dst_slot.borrow().num; // force copy, borrow checker...
                    Some(Reg::N(num))
                } else {
                    panic!("functions must return 0 or 1 result {:?}", n_res);
                };

                // the mapping below shouldn't exist
                // Molki call instrs should not simply take a string,
                // but an enum of 'user-defined-funtion' or 'runtime call'
                // as we already have in the type system
                // we can probably pass trhoguh that enum from the type
                // system up until the final codegen
                let func_name = match func.ld_name().to_str().unwrap() {
                    "mjrt_system_out_println" => "__stdlib_println",
                    "mjrt_system_out_write" => "__stdlib_write",
                    "mjrt_system_out_flush" => "__stdlib_flush",
                    "mjrt_system_in_read" => "__stdlib_read",
                    "mjrt_new" => "__stdlib_malloc",
                    // TODO exhaustive?
                    x => x,
                }
                .to_owned();
                self.instrs.push(Instr::Call {
                    func: func_name,
                    args,
                    dst,
                });
            }
            Node::Unknown(_) => {
                // TODO ??? this happens in the runtime function wrapper's
                // arguments
                //
                // write somehting to computed for now
                //
                let dst_slot = block.new_private_slot(node); // interanl borrow_mut
                self.mark_computed(node, Computed::Value(MutRc::clone(&dst_slot)));
            }
            Node::Address(_) => (), // ignored, only used by call node
            x => self.comment(format_args!("\t\t\tunimplemented: {:?}", x)),
        }
    }
}
