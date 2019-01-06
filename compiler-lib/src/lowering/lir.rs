//! Low level intermediate representation

use super::gen_instr::GenInstrBlock;
use crate::{
    firm,
    lowering::molki,
    type_checking::type_system::CheckedType,
    utils::cell::{MutRc, MutWeak},
};
use libfirm_rs::{
    nodes::{self, Node, NodeTrait},
    Mode, Tarval, VisitTime,
};
use std::{
    collections::{HashMap, HashSet, VecDeque},
    marker::PhantomData,
};

#[derive(Debug)]
pub struct LIR {
    pub functions: Vec<Function>,
}

impl From<&firm::FirmProgram<'_, '_>> for LIR {
    fn from(prog: &firm::FirmProgram<'_, '_>) -> Self {
        let mut functions = Vec::new();

        for method in prog.methods.values() {
            functions.push((&*method.borrow()).into());
        }

        LIR { functions }
    }
}

#[derive(Debug)]
pub struct Function {
    /// The mangled name of the function.
    pub name: String,
    pub nargs: usize,
    pub returns: bool,
    pub graph: MutRc<BlockGraph>,
}

impl From<&firm::FirmMethod<'_, '_>> for Function {
    fn from(method: &firm::FirmMethod<'_, '_>) -> Self {
        let graph: libfirm_rs::Graph = method
            .graph
            .unwrap_or_else(|| panic!("Cannot lower function without a graph {}", method.def.name));

        let returns = method.def.return_ty != CheckedType::Void;
        log::debug!("Generating block graph for {}", method.def.name);
        Function {
            name: method.entity.ld_name().to_str().unwrap().to_owned(),
            nargs: method.def.params.len(),
            returns,
            graph: graph.into(),
        }
    }
}

#[derive(Debug)]
/// A graph of basic blocks. Each block is a list of instructions and a set of
/// pseudo-registers called `ValueSlots`. This is a more localized
/// represantation of SSA, as the value
/// slots (or variable names) are
/// namespaced per block (and can only be
/// refered to by adjacent blocks) and
/// the sources of the values are
/// annotated on each edge, instead of
/// being phi-nodes pointing to some far
/// away firm-node.
pub struct BlockGraph {
    pub firm: libfirm_rs::Graph,
    blocks: HashMap<libfirm_rs::nodes::Block, MutRc<BasicBlock>>,
    pub head: MutRc<BasicBlock>,
}

#[derive(Debug, Clone, Copy)]
pub enum BasicBlockReturns {
    No,
    Void(nodes::Return),
    Value(nodes::Return),
}

impl BasicBlockReturns {
    pub fn as_option(self) -> Option<nodes::Return> {
        match self {
            BasicBlockReturns::No => None,
            BasicBlockReturns::Void(r) | BasicBlockReturns::Value(r) => Some(r),
        }
    }
}

#[derive(Debug, Default)]
pub struct Code {
    pub(super) copy_in: Vec<CopyPropagation>,
    pub(super) body: Vec<Instruction>,
    pub(super) copy_out: Vec<CopyPropagation>,
    pub(super) leave: Vec<Leave>,
}

/// This is a vertex in the basic-block graph
#[derive(Debug)]
pub struct BasicBlock {
    /// The Pseudo-registers used by the Block
    pub regs: Vec<Vec<MutRc<ValueSlot>>>,
    /// The instructions (using arbitrarily many registers) of the block
    pub code: Code,
    /// Control flow-transfers *to* this block.
    /// Usually at most 2
    pub preds: Vec<MutWeak<ControlFlowTransfer>>,
    /// Control flow-transfers *out of* this block
    /// Usually at most 2
    pub succs: Vec<MutRc<ControlFlowTransfer>>,

    /// The firm structure of this block
    pub firm: libfirm_rs::nodes::Block,

    /// Whether the block contains a return node, and if so, whether it returns
    /// a value or just terminates control flow plus the firm node.
    ///
    /// Blocks with `returns == BasicBlockReturns::Value` have the following
    /// properties:
    ///
    ///  * `succs.len() == 1`
    ///
    ///  * `let succ_edge = succs[0]`
    ///
    ///    * `succ_edge.target = <the end block>`
    ///
    ///    * `succ_edge.register_transitions.len() == 1`
    ///
    ///    * `succ_edge.register_transitions.[0].0 = <a value slot in this
    /// block>
    ///
    ///    * `succ_edge.register_transitions.[0].0.firm = <this block's return
    /// node's result value node>
    ///
    ///    * `succ_edge.register_transitions.[0].1 = .0`
    ///
    /// Above design enables codegen to simply iterate over
    /// the FIRM in_nodes for each value in succs.
    /// SSA copy-propagation code can check `returns` to omit
    /// copy-down of `succ_edge`.
    pub returns: BasicBlockReturns,

    pub graph: MutWeak<BlockGraph>,
}

#[derive(Debug, Clone)]
pub enum Instruction {
    Binop {
        kind: BinopKind,
        src1: Operand,
        src2: Operand,
        dst: MutRc<ValueSlot>,
    },
    Divop {
        kind: DivKind,
        src1: Operand,
        src2: Operand,
        /// The division result value slot. The remainder is discarded.
        dst: MutRc<ValueSlot>,
    },
    Mod {
        kind: DivKind,
        src1: Operand,
        src2: Operand,
        /// The remainder result value slot. The division result is discarded.
        dst: MutRc<ValueSlot>,
    },
    Basic {
        kind: BasicKind,
        op: Option<Operand>,
    },
    Movq {
        src: Operand,
        dst: Operand,
    },
    /// If dst is None, result is in register r0, which cannot be accessed
    /// using molki register names.
    Call {
        func: String,
        args: Vec<Operand>,
        dst: Option<MutRc<ValueSlot>>,
    },
    /// Loads parameter `#{idx}` into value slot `dst`.
    LoadParam {
        idx: usize,
        dst: Option<MutRc<ValueSlot>>,
    },
    Comment(String),
}

/// Instructions that are at the end of a basic block.
#[derive(Debug, Clone)]
pub enum Leave {
    CondJmp {
        lhs: Operand,
        lhs_target: MutRc<BasicBlock>,
        rhs: Operand,
        rhs_target: MutRc<BasicBlock>,
    },
    Jmp {
        target: MutRc<BasicBlock>,
    },
    Return {
        /// TODO Must only be Operand::Slot or Operand::Imm ?
        value: Option<Operand>,
    },
}

/// The representation of a single element in
/// ControlFlowTransfer.register_transitions. The consumer of the LIR
/// (a register allocator / target arch code generator) emits the
/// concrete instructions to flow values from one basic block to the other.
/// It will commonly have to choose between using registers or spill code.
#[derive(Debug, Clone)]
pub struct CopyPropagation {
    pub(super) src: MutRc<ValueSlot>,
    pub (super) dst: MutRc<ValueSlot>,
}

#[derive(Debug, Clone)]
pub enum Operand {
    Slot(MutRc<ValueSlot>),
    /// NOTE: Tarcval contains a raw pointer, thus Imm(t) is only valid for the
    /// lifetime of that pointer (the FIRM graph).
    Imm(Tarval),
    Addr {
        base: MutRc<ValueSlot>,
        offset: isize,
    },
    /// only readable!
    Param {
        idx: u32,
    },
}

#[derive(Debug, Display, Clone)]
pub enum BinopKind {
    Add,
    Sub,
    // We only multiply signed integers, so we can always use `imul`
    Mul,
    And,
    Or,
}

#[derive(Debug, Display)]
pub enum UnopKind {
    Neg,
    Not,
}

#[derive(Debug, Display, Clone)]
pub enum DivKind {
    /// unsigned
    Div,
    /// signed
    IDiv,
}

#[derive(Debug, Display, Clone)]
pub enum BasicKind {
    Not,
    Neg,
}

#[derive(Debug, Display, Clone)]
pub enum Cond {
    True,
    LessEqual,
}

/// An abstract pseudo-register
#[derive(Debug)]
pub struct ValueSlot {
    /// The slot number. Uniqe only per Block, not globally
    pub(super) num: usize,
    /// The firm node that corresponds to this value
    pub(super) firm: Node,

    /// The block in which this slot is allocated
    pub(super) allocated_in: MutWeak<BasicBlock>,
    /// The block in which the value of this slot originates
    pub(super) originates_in: MutWeak<BasicBlock>,
    /// The block in which this value is used
    pub(super) terminates_in: MutWeak<BasicBlock>,
}

/// This is currently unused (because the categorisations are wrong), but we
/// might need something similar later (or at least the comments).
#[derive(Debug, Clone, Copy, PartialEq, Eq, Display)]
pub enum _ValueSlotKind {
    /// The value in this slot originates in the local block, but is used in a
    /// later block
    ///
    /// Original values must appear in the transitions of all `succs`.
    #[display(fmt = "original")]
    Original,
    /// The value in this slot is used in the instructions of the local block,
    /// but originiates in a previous block
    ///
    /// Terminal values must appear in the transitions of all `preds`.
    #[display(fmt = "terminal")]
    Terminal,
    /// The slot is unused in the local block, but contains a value that must be
    /// kept alive for the duration of this block. This is created for values
    /// that are calculated in an earlier block, and used in a later block (but
    /// not in this block).
    ///
    /// Pass-through values must appear in the transitions both all `preds` and
    /// `succs`.
    #[display(fmt = "pass-through")]
    PassThrough,
    /// The value of the slot both originates and is used (terminates) in the
    /// local block.
    ///
    /// Private values must appear neither in the transitions of any `preds`,
    /// nor `succs`.
    #[display(fmt = "private")]
    Private,
}

/// Transfer control-flow from one block to another. This is an edge in the
/// basic-block graph
#[derive(Debug)]
pub struct ControlFlowTransfer {
    /// How do value slots used in the preceeding block map to value slots in
    /// the next block? SSA-information is encoded in this.
    ///
    /// ## How to use this in a register allocator
    /// A register-allocator can use this information to figure out which
    /// registers to use in the in the adjacent blocks (so the transitions have
    /// minimal "mismatches"). If there is copy-code needed to match the
    /// the registers for a given value slot, the
    /// following cases need to be considered:
    ///
    ///  - The `source` block has multiple `succ` edges for the slot: copy-code
    /// needs to be placed in each `target` block
    ///
    ///  - The `target` block has multiple `pred` edges for the slot: copy-code
    /// needs to be placed in each `source` block
    ///
    ///  - Both of the above (can happen in loops): An additional block needs
    /// to be introduced right on this edge containing the copy-code (This is
    /// exactly the *copy problem* from the lecture)
    ///
    /// The *swap problem* is handled by there being no semantic order between
    /// the register transitions.
    pub(super) register_transitions: Vec<(MutRc<ValueSlot>, MutRc<ValueSlot>)>,

    source: MutWeak<BasicBlock>,
    pub target: MutRc<BasicBlock>,
}

impl From<libfirm_rs::Graph> for MutRc<BlockGraph> {
    fn from(firm_graph: libfirm_rs::Graph) -> Self {
        firm_graph.assure_outs();
        let graph = BlockGraph::build_skeleton(firm_graph);
        graph.borrow_mut().construct_flows();
        graph.borrow_mut().gen_instrs();
        graph
    }
}

impl BlockGraph {
    fn build_skeleton(firm_graph: libfirm_rs::Graph) -> MutRc<Self> {
        let mut blocks = HashMap::new();

        // This is basically a `for each edge "firm_target -> firm_source"`
        firm_graph.walk_blocks(|visit, firm_target| match visit {
            VisitTime::BeforePredecessors => {
                let target = BasicBlock::skeleton_block(&mut blocks, *firm_target);

                for firm_source in firm_target.cfg_preds() {
                    let source = BasicBlock::skeleton_block(&mut blocks, firm_source.block());

                    let edge = MutRc::new(ControlFlowTransfer {
                        register_transitions: Vec::new(),
                        source: MutRc::downgrade(&source),
                        target: MutRc::clone(&target),
                    });

                    source.borrow_mut().succs.push(MutRc::clone(&edge));
                    target.borrow_mut().preds.push(MutRc::downgrade(&edge));
                }
            }

            VisitTime::AfterPredecessors => (),
        });

        let head = MutRc::clone(
            blocks
                .get(&firm_graph.start_block())
                .expect("All blocks (including start block) should have been generated"),
        );

        let block_graph = MutRc::new(BlockGraph {
            firm: firm_graph,
            blocks,
            head,
        });

        // patch up the weak-ref in each block's graph member
        for (_, block) in &block_graph.borrow_mut().blocks {
            block.borrow_mut().graph = block_graph.downgrade();
        }

        block_graph
    }

    fn construct_flows(&mut self) {
        self.firm.walk_blocks(|visit, firm_block| match visit {
            VisitTime::BeforePredecessors => {
                log::debug!("VISIT {:?}", firm_block);
                let local_block = self.get_block(*firm_block);

                // Foreign values are the green points in yComp (inter-block edges)
                for node_in_block in firm_block.out_nodes() {
                    match node_in_block {
                        Node::Phi(phi) => {
                            let multislot = local_block.new_terminating_multislot();
                            phi.preds()
                                // Mem edges are uninteresting across blocks
                                .filter(|(_, value)| value.mode() != Mode::M())
                                // The next two 'maps' need to be seperated in two closures
                                // because we wan't to selectively `move` `value` and
                                // `multislot` into the closure, but take `self` by reference
                                .map(|(cfg_pred, value)| {
                                    (
                                        cfg_pred,
                                        value,
                                        MutRc::downgrade(&self.get_block(value.block())),
                                    )
                                })
                                .map(move |(cfg_pred, value, original_block)| {
                                    (
                                        cfg_pred,
                                        multislot.add_possible_value(value, original_block),
                                    )
                                })
                                .for_each(|(cfg_pred, value_slot)| {
                                    local_block
                                        .borrow()
                                        .find_incoming_edge_from(cfg_pred)
                                        .unwrap()
                                        .add_incoming_value_flow(value_slot);
                                });
                        }

                        // The end node is only for keep alive edges, which we don't care about
                        Node::End(_) => (),

                        _ => node_in_block
                            .in_nodes()
                            // Mem edges are uninteresting across blocks
                            .filter(|value| value.mode() != Mode::M())
                            // If this is a value produced by our block, there is no need to
                            // transfer it from somewhere else
                            .filter(|value| {
                                log::debug!("value: {:?}", value);
                                value.block() != *firm_block
                            })
                            // Foreign values that are not phi, flow in from each cfg pred
                            // => values x cfg_preds
                            .for_each(|value| {
                                // Do this here, because we don't want to move `self` or
                                // `local_block` into closure
                                let original_block = self.get_block(value.block());
                                let local_block = MutRc::clone(&local_block);
                                local_block.new_terminating_slot(value, original_block.downgrade());
                            }),
                    }
                }
            }

            VisitTime::AfterPredecessors => (),
        });

        // Special case for return nodes, see BasicBlock.return comment
        let end_block = self.get_block(self.firm.end_block());
        let multislot = end_block.new_terminating_multislot();
        for return_node in self.firm.end_block().cfg_preds() {
            log::debug!("return_node = {:?}", return_node);
            log::debug!(
                "return_node.edges = {:?}",
                end_block
                    .borrow()
                    .preds
                    .iter()
                    .map(|x| format!("{:?}", upborrow!(upborrow!(x).source).firm))
                    .collect::<Vec<_>>()
            );
            let block_with_return_node = self.get_block(return_node.block());
            let return_node = match return_node {
                Node::Return(r) => r,
                _ => panic!("unexpected return node"),
            };
            if return_node.return_res().len() == 0 {
                block_with_return_node.borrow_mut().returns = BasicBlockReturns::Void(return_node);
            } else {
                block_with_return_node.borrow_mut().returns = BasicBlockReturns::Value(return_node);
                debug_assert_eq!(
                    1,
                    return_node.return_res().len(),
                    "MiniJava only supports a single return value"
                );
                let vs = multislot.add_possible_value(
                    return_node.return_res().idx(0).unwrap(),
                    block_with_return_node.downgrade(),
                );
                end_block
                    .borrow()
                    .find_incoming_edge_from(return_node.block())
                    .unwrap()
                    .add_incoming_value_flow(vs);
            }
        }
    }

    fn gen_instrs(&mut self) {
        for lir_block in self.iter_blocks() {
            log::debug!("GENINSTR block {:?}", lir_block.borrow().firm);
            GenInstrBlock::fill_instrs(lir_block);
        }
    }

    fn get_block(&self, firm_block: libfirm_rs::nodes::Block) -> MutRc<BasicBlock> {
        self.blocks
            .get(&firm_block)
            .expect("BlockGraph is incomplete")
            .clone()
    }

    /// Iterate over all basic blocks in `self` in a breadth-first manner (in
    /// control flow direction, starting at the start block)
    pub fn iter_blocks<'g>(&'g self) -> impl Iterator<Item = MutRc<BasicBlock>> + 'g {
        let mut visit_list = VecDeque::new();
        visit_list.push_front(MutRc::clone(&self.head));

        BasicBlockIter {
            _graph: PhantomData,
            visited: HashSet::new(),
            visit_list,
        }
    }

    /// Iterate over all control flow transfers in `self` in a breadth-first
    /// manner
    pub fn iter_control_flows<'g>(
        &'g self,
    ) -> impl Iterator<Item = MutRc<ControlFlowTransfer>> + 'g {
        self.iter_blocks().flat_map(|block| {
            block
                .borrow()
                .succs
                .iter()
                .map(MutRc::clone)
                .collect::<Vec<_>>()
        })
    }
}

struct BasicBlockIter<'g> {
    _graph: PhantomData<&'g !>,
    visited: HashSet<libfirm_rs::nodes::Block>,
    visit_list: VecDeque<MutRc<BasicBlock>>,
}

impl<'g> Iterator for BasicBlockIter<'g> {
    type Item = MutRc<BasicBlock>;

    fn next(&mut self) -> Option<Self::Item> {
        self.visit_list.pop_front().map(|block| {
            for edge in &block.borrow().succs {
                let succ = MutRc::clone(&edge.borrow().target);
                if !self.visited.contains(&succ.borrow().firm) {
                    self.visited.insert(succ.borrow().firm);
                    self.visit_list.push_back(succ);
                }
            }

            block
        })
    }
}

impl MutRc<ControlFlowTransfer> {
    /// Only call this from target
    fn add_incoming_value_flow(&self, target_slot: MutRc<ValueSlot>) {
        if let Some((source, target)) = self
            .borrow()
            .register_transitions
            .iter()
            .find(|(_, existing_slot)| target_slot.borrow().firm == existing_slot.borrow().firm)
        {
            assert_eq!(target.borrow().num, target_slot.borrow().num);
            log::debug!(
                "\tPIGGY: from='{:?}' to='{:?}' value='{:?}'",
                upborrow!(source.borrow().allocated_in).firm,
                upborrow!(target.borrow().allocated_in).firm,
                target.borrow().firm,
            );

            return;
        }

        let source_slot = self
            .borrow()
            .source
            .upgrade()
            .unwrap()
            .new_forwarding_slot(&target_slot.borrow());

        assert_eq!(target_slot.borrow().firm, source_slot.borrow().firm);
        log::debug!(
            "\tTRANS: from='{:?}' to='{:?}' value='{:?}'",
            upborrow!(self.borrow().source).firm,
            self.borrow().target.borrow().firm,
            target_slot.borrow().firm,
        );

        self.borrow_mut()
            .register_transitions
            .push((source_slot, target_slot));
    }

    /// Do there exist multiple incoming flows for the target slot of `flow_idx`
    /// in the target block?
    pub fn must_copy_in_source(&self, flow_idx: usize) -> bool {
        let target_slot_num = self.borrow().register_transitions[flow_idx].1.borrow().num;

        self.borrow()
            .target
            .borrow()
            .preds
            .iter()
            .filter(|pred| !MutWeak::ptr_eq(pred, &self.downgrade()))
            .any(|pred| {
                upborrow!(pred)
                    .register_transitions
                    .iter()
                    .any(|(_, other_target_slot)| other_target_slot.borrow().num == target_slot_num)
            })
    }

    /// Do there exist multiple outgoing flows for the source slot of `flow_idx`
    /// in the source block?
    pub fn must_copy_in_target(&self, flow_idx: usize) -> bool {
        let source_slot_num = self.borrow().register_transitions[flow_idx].0.borrow().num;

        upborrow!(self.borrow().source)
            .succs
            .iter()
            .filter(|succ| !MutRc::ptr_eq(succ, self))
            .any(|succ| {
                succ.borrow()
                    .register_transitions
                    .iter()
                    .any(|(other_source_slot, _)| other_source_slot.borrow().num == source_slot_num)
            })
    }
}

impl MutRc<BasicBlock> {
    fn new_multislot(&self, terminates_in: MutWeak<BasicBlock>) -> MultiSlotBuilder {
        MultiSlotBuilder::new(MutRc::clone(self), terminates_in)
    }

    fn new_terminating_multislot(&self) -> MultiSlotBuilder {
        self.new_multislot(MutRc::downgrade(self))
    }

    /// TODO This function makes the assupmtion that is not used for `value`s
    /// that are the inputs to phi nodes (instead `new_multislot`,
    /// `add_possible_value` and `add_incoming_value_flow` are used seperately
    /// in that case). BE AWARE OF THIS when refactoring
    fn new_slot(
        &self,
        value: libfirm_rs::nodes::Node,
        originates_in: MutWeak<BasicBlock>,
        terminates_in: MutWeak<BasicBlock>,
    ) -> MutRc<ValueSlot> {
        let this = self.borrow();
        let possibly_existing_slot = this
            .regs
            .iter()
            .filter_map(|multislot| multislot.iter().find(|slot| slot.borrow().firm == value))
            .next();

        if let Some(slot) = possibly_existing_slot {
            log::debug!(
                "\tREUSE: slot={} in='{:?}' value='{:?}'",
                slot.borrow().num,
                this.firm,
                slot.borrow().firm
            );

            MutRc::clone(slot)
        } else {
            drop(this);
            let slot = self
                .new_multislot(terminates_in)
                .add_possible_value(value, originates_in);

            // If the value is foreign, we need to "get it" from each blocks above us.
            //
            // NOTE:
            // In FIRM,const and address nodes are all in the start block, no
            // matter where they are used, however we don't want or need to
            // transfer them down to the usage from the start block, so we can
            // treat a const node as "originating here".
            // HOWEVER, we cannot make above assumption if this node is used as input to a
            // Phi node in this block, because the value needs to originate in the
            // corresponding cfg_pred. However, when creating slots for the inputs of phi
            // nodes, this function (`MutRc<BasicBlock>::new_slot`), in not used. So the
            // assumption holds, but BE AWARE OF THIS when refactoring.
            let originates_here = Node::is_const(slot.borrow().firm)
                || Node::is_address(slot.borrow().firm)
                || upborrow!(slot.borrow().allocated_in).firm
                    == upborrow!(slot.borrow().originates_in).firm;
            if !originates_here {
                for incoming_edge in &self.borrow().preds {
                    incoming_edge
                        .upgrade()
                        .unwrap()
                        .add_incoming_value_flow(MutRc::clone(&slot));
                }
            }

            slot
        }
    }

    fn new_forwarding_slot(&self, target_slot: &ValueSlot) -> MutRc<ValueSlot> {
        self.new_slot(
            target_slot.firm,
            MutWeak::clone(&target_slot.originates_in),
            MutWeak::clone(&target_slot.terminates_in),
        )
    }

    fn new_terminating_slot(
        &self,
        value: libfirm_rs::nodes::Node,
        origin: MutWeak<BasicBlock>,
    ) -> MutRc<ValueSlot> {
        self.new_slot(value, origin, MutRc::downgrade(self))
    }

    pub(super) fn new_private_slot(&self, value: libfirm_rs::nodes::Node) -> MutRc<ValueSlot> {
        assert_eq!(value.block(), self.borrow().firm);
        self.new_slot(value, MutRc::downgrade(self), MutRc::downgrade(self))
    }
}

impl BasicBlock {
    fn find_incoming_edge_from(
        &self,
        cfg_pred: libfirm_rs::nodes::Block,
    ) -> Option<MutRc<ControlFlowTransfer>> {
        self.preds
            .iter()
            .map(|edge| MutWeak::upgrade(edge).unwrap())
            .find(|edge| upborrow!(edge.borrow().source).firm == cfg_pred)
    }

    fn skeleton_block(
        known_blocks: &mut HashMap<libfirm_rs::nodes::Block, MutRc<BasicBlock>>,
        firm: libfirm_rs::nodes::Block,
    ) -> MutRc<Self> {
        known_blocks
            .entry(firm)
            .or_insert_with(|| {
                MutRc::new(BasicBlock {
                    regs: Vec::new(),
                    code: Code::default(),
                    preds: Vec::new(),
                    succs: Vec::new(),
                    firm,
                    // if No is not true, overridden in suring construct_flows
                    returns: BasicBlockReturns::No,
                    graph: MutWeak::new(), // will be patched up by caller
                })
            })
            .clone()
    }
}

#[derive(Debug)]
pub struct MultiSlotBuilder {
    num: usize,
    allocated_in: MutWeak<BasicBlock>,
    terminates_in: MutWeak<BasicBlock>,
}

impl MultiSlotBuilder {
    fn new(allocated_in: MutRc<BasicBlock>, terminates_in: MutWeak<BasicBlock>) -> Self {
        let num = allocated_in.borrow().regs.len();
        allocated_in.borrow_mut().regs.push(Vec::new());
        MultiSlotBuilder {
            num,
            allocated_in: MutRc::downgrade(&allocated_in),
            terminates_in,
        }
    }

    fn add_possible_value(
        &self,
        value: libfirm_rs::nodes::Node,
        originates_in: MutWeak<BasicBlock>,
    ) -> MutRc<ValueSlot> {
        assert!(upborrow!(self.allocated_in).regs.get(self.num).is_some());

        {
            let allocated_in = MutWeak::upgrade(&self.allocated_in).unwrap();
            let allocated_in = allocated_in.borrow();
            let is_duplicate = allocated_in.regs[self.num]
                .iter()
                .any(|slot| slot.borrow().firm == value);

            assert!(!is_duplicate);
        }

        let slot = ValueSlot {
            num: self.num,
            firm: value,
            allocated_in: MutWeak::clone(&self.allocated_in),
            originates_in,
            terminates_in: MutWeak::clone(&self.terminates_in),
        };

        log::debug!(
            "\tALLOC: slot={} in='{:?}' value='{:?}'",
            slot.num,
            upborrow!(self.allocated_in).firm,
            slot.firm
        );

        let slot = MutRc::new(slot);
        upborrow!(mut self.allocated_in).regs[self.num].push(MutRc::clone(&slot));

        slot
    }
}
