//! Low level intermediate representation

use super::{
    gen_instr::GenInstrBlock,
    lir_allocator::{self, Ptr},
};
use crate::{firm, type_checking::type_system::CheckedType};

use libfirm_rs::{
    nodes::{self, Node, NodeTrait},
    Mode, Tarval, VisitTime,
};
use std::{
    collections::{HashMap, HashSet, VecDeque},
    fmt::Display,
    marker::PhantomData,
};

#[derive(Debug, Default)]
pub(super) struct Allocator {
    graphs: lir_allocator::Allocator<BlockGraph>,
    blocks: lir_allocator::Allocator<BasicBlock>,
    slots: lir_allocator::Allocator<ValueSlot>,
    multi_slots: lir_allocator::Allocator<MultiSlot>,
    transfers: lir_allocator::Allocator<ControlFlowTransfer>,
}

macro_rules! allocation_method {
    ($name:ident, $type:ty, $member:ident) => {
        pub(super) fn $name(&self, elem: $type) -> Ptr<$type> {
            self.$member.alloc(elem)
        }
    }
}

impl Allocator {
    allocation_method!(graph, BlockGraph, graphs);
    allocation_method!(block, BasicBlock, blocks);
    allocation_method!(slot, ValueSlot, slots);
    allocation_method!(multi_slot, MultiSlot, multi_slots);
    allocation_method!(transfer, ControlFlowTransfer, transfers);
}

#[derive(Debug)]
pub struct LIR {
    pub(super) allocator: Allocator,
    pub functions: Vec<Function>,
}

impl From<&firm::FirmProgram<'_, '_>> for LIR {
    fn from(prog: &firm::FirmProgram<'_, '_>) -> Self {
        let mut functions = Vec::new();

        let allocator = Allocator::default();

        for method in prog.methods.values() {
            let function = Function::from(&*method.borrow(), &allocator);
            functions.push(function);
        }

        LIR {
            allocator,
            functions,
        }
    }
}

#[derive(Debug)]
pub struct Function {
    /// The mangled name of the function.
    pub name: String,
    pub nargs: usize,
    pub returns: bool,
    pub graph: Ptr<BlockGraph>,
}

impl Function {
    fn from(method: &firm::FirmMethod<'_, '_>, alloc: &Allocator) -> Self {
        let graph: libfirm_rs::Graph = method
            .graph
            .unwrap_or_else(|| panic!("Cannot lower function without a graph {}", method.def.name));

        let returns = method.def.return_ty != CheckedType::Void;
        log::debug!("Generating block graph for {}", method.def.name);
        Function {
            name: method.entity.ld_name().to_str().unwrap().to_owned(),
            nargs: method.def.params.len() + if method.def.is_static { 0 } else { 1 },
            returns,
            graph: BlockGraph::from(graph, alloc),
        }
    }
}

#[derive(Debug)]
/// A graph of basic blocks. Each block is a list of instructions and a set of
/// pseudo-registers called `ValueSlots`. This is a more localized
/// represantation of SSA, as the value slots (or variable names) are namespaced
/// per block (and can only be refered to by adjacent blocks) and the sources of
/// the values are annotated on each edge, instead of being phi-nodes pointing
/// to some far away firm-node.
pub struct BlockGraph {
    pub firm: libfirm_rs::Graph,
    pub blocks: HashMap<libfirm_rs::nodes::Block, Ptr<BasicBlock>>,
    pub head: Ptr<BasicBlock>,
    pub end_block: Ptr<BasicBlock>,
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
    pub copy_in: Vec<CopyPropagation>,
    pub body: Vec<Instruction>,
    pub copy_out: Vec<CopyPropagation>,
    pub leave: Vec<Leave>,
}

/// This is a vertex in the basic-block graph
#[derive(Debug)]
pub struct BasicBlock {
    /// Unique number for the BasicBlock
    pub num: i64,
    /// The Pseudo-registers used by the Block
    pub regs: Vec<Ptr<MultiSlot>>,
    /// The instructions (using arbitrarily many registers) of the block
    pub code: Code,
    /// Control flow-transfers *to* this block.
    /// Usually at most 2
    pub preds: Vec<Ptr<ControlFlowTransfer>>,
    /// Control flow-transfers *out of* this block
    /// Usually at most 2
    pub succs: Vec<Ptr<ControlFlowTransfer>>,

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

    pub graph: Ptr<BlockGraph>,
}

#[derive(Debug)]
pub enum MultiSlot {
    Single(Ptr<ValueSlot>),
    Multi {
        phi: nodes::Phi,
        slots: Vec<Ptr<ValueSlot>>,
    },
}

impl MultiSlot {
    pub fn num(&self) -> usize {
        use self::MultiSlot::*;
        match self {
            Single(slot) => slot.num,
            Multi { slots, .. } => slots[0].num,
        }
    }

    pub fn allocated_in(&self) -> Ptr<BasicBlock> {
        use self::MultiSlot::*;
        match self {
            Single(slot) => slot.allocated_in,
            Multi { slots, .. } => slots[0].allocated_in,
        }
    }

    pub fn firm(&self) -> nodes::Node {
        use self::MultiSlot::*;
        match self {
            Single(slot) => slot.firm,
            Multi { phi, .. } => Node::Phi(*phi),
        }
    }
}

#[derive(Debug, Clone)]
pub enum Instruction {
    Binop {
        kind: BinopKind,
        src1: Operand,
        src2: Operand,
        dst: Ptr<MultiSlot>,
    },
    Div {
        src1: Operand,
        src2: Operand,
        /// The division result value slot. The remainder is discarded.
        dst: Ptr<MultiSlot>,
    },
    Mod {
        src1: Operand,
        src2: Operand,
        /// The remainder result value slot. The division result is discarded.
        dst: Ptr<MultiSlot>,
    },
    Unop {
        kind: UnopKind,
        op: Operand,
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
        dst: Option<Ptr<MultiSlot>>,
    },
    /// Loads parameter `#{idx}` into value slot `dst`.
    LoadParam {
        idx: usize,
        dst: Ptr<MultiSlot>,
    },
    StoreMem {
        src: Operand,
        dst: AddressComputation<Operand>,
    },
    LoadMem {
        src: AddressComputation<Operand>,
        dst: Ptr<MultiSlot>,
    },
    Comment(String),
}

/// This implements address computation, see
/// http://www.c-jump.com/CIS77/CPU/x86/lecture.html#X77_0110_scaled_indexed
#[derive(Debug, Clone)]
pub struct AddressComputation<Op: Display> {
    // offset(base, index, stride) = offset + base + index * stride
    pub offset: isize,
    pub base: Op,
    pub index: IndexComputation<Op>,
}

impl<Op: Display> std::fmt::Display for AddressComputation<Op> {
    fn fmt(&self, fmt: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let index = match &self.index {
            IndexComputation::Zero => String::new(),
            IndexComputation::Displacement(op, stride) => format!(", {}, {}", op, stride),
        };
        write!(fmt, "{}({}{})", self.offset, self.base, index)
    }
}

/// Index of AddressComputation variant of the Instruction enum.
#[derive(Debug, Clone)]
pub enum IndexComputation<Op: Display> {
    Displacement(Op, Stride),
    Zero,
}

#[derive(Debug, Display, Clone)]
pub enum Stride {
    #[display(fmt = "1")]
    One,
    #[display(fmt = "2")]
    Two,
    #[display(fmt = "4")]
    Four,
    #[display(fmt = "8")]
    Eight,
}

/// Instructions that are at the end of a basic block.
#[derive(Debug, Clone)]
pub enum Leave {
    CondJmp {
        lhs: Operand,
        lhs_target: Ptr<BasicBlock>,
        rhs: Operand,
        rhs_target: Ptr<BasicBlock>,
    },
    Jmp {
        target: Ptr<BasicBlock>,
    },
    Return {
        /// TODO Must only be Operand::Slot or Operand::Imm ?
        value: Option<Operand>,
        /// The end block of the BlockGraph that this Return returns from.
        /// Depending on how the target arch code generator implements function
        /// returns, this pointer might be very convenient.
        end_block: Ptr<BasicBlock>,
    },
}

/// The representation of a single element in
/// ControlFlowTransfer.register_transitions. The consumer of the LIR
/// (a register allocator / target arch code generator) emits the
/// concrete instructions to flow values from one basic block to the other.
/// It will commonly have to choose between using registers or spill code.
#[derive(Debug, Clone)]
pub struct CopyPropagation {
    pub(super) src: Ptr<MultiSlot>,
    pub(super) dst: Ptr<ValueSlot>,
}

#[derive(Debug, Display, Clone, Copy)]
pub enum Operand {
    #[display(fmt = "Slot")]
    Slot(Ptr<MultiSlot>),
    /// NOTE: Tarcval contains a raw pointer, thus Imm(t) is only valid for the
    /// lifetime of that pointer (the FIRM graph).
    #[display(fmt = "Imm")]
    Imm(Tarval),
    /// only readable!
    #[display(fmt = "Param: {}", idx)]
    Param { idx: u32 },
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

#[derive(Debug, Display, Clone)]
pub enum UnopKind {
    Neg,
    Not,
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
    pub num: usize,
    /// The firm node that corresponds to this value
    pub(super) firm: Node,

    /// The block in which this slot is allocated
    pub(super) allocated_in: Ptr<BasicBlock>,
    /// The block in which the value of this slot originates
    pub(super) originates_in: Ptr<BasicBlock>,
    /// The block in which this value is used
    pub(super) terminates_in: Ptr<BasicBlock>,
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
    pub register_transitions: Vec<(Ptr<MultiSlot>, Ptr<ValueSlot>)>,

    source: Ptr<BasicBlock>,
    pub target: Ptr<BasicBlock>,
}

impl BlockGraph {
    fn from(firm_graph: libfirm_rs::Graph, alloc: &Allocator) -> Ptr<Self> {
        firm_graph.assure_outs();
        let mut graph = BlockGraph::build_skeleton(firm_graph, alloc);
        graph.construct_flows(alloc);
        graph.gen_instrs(alloc);
        graph
    }
}

impl BlockGraph {
    fn build_skeleton(firm_graph: libfirm_rs::Graph, alloc: &Allocator) -> Ptr<Self> {
        let mut blocks = HashMap::new();

        // This is basically a `for each edge "firm_target -> firm_source"`
        firm_graph.walk_blocks(|visit, firm_target| match visit {
            VisitTime::BeforePredecessors => {
                let mut target = BasicBlock::skeleton_block(&mut blocks, *firm_target, alloc);

                for firm_source in firm_target.cfg_preds() {
                    let mut source =
                        BasicBlock::skeleton_block(&mut blocks, firm_source.block(), alloc);

                    let edge = alloc.transfer(ControlFlowTransfer {
                        register_transitions: Vec::new(),
                        source,
                        target,
                    });

                    source.succs.push(edge);
                    target.preds.push(edge);
                }
            }

            VisitTime::AfterPredecessors => (),
        });

        let head = *blocks
            .get(&firm_graph.start_block())
            .expect("All blocks (including start block) should have been generated");

        let end_block = *blocks
            .get(&firm_graph.end_block())
            .expect("All blocks (including end block) should have been generated");

        let graph = alloc.graph(BlockGraph {
            firm: firm_graph,
            blocks,
            head,
            end_block,
        });

        // patch up the weak-ref in each block's graph member
        for mut block in graph.iter_blocks() {
            block.graph = graph;
        }

        graph
    }

    /// Iterate over all basic blocks in `self` in a breadth-first manner (in
    /// control flow direction, starting at the start block)
    pub fn iter_blocks<'g>(&'g self) -> impl Iterator<Item = Ptr<BasicBlock>> + 'g {
        let mut visit_list = VecDeque::new();
        visit_list.push_front(self.head);

        BasicBlockIter {
            _graph: PhantomData,
            visited: HashSet::new(),
            visit_list,
        }
    }

    /// Iterate over all control flow transfers in `self` in a breadth-first
    /// manner
    pub fn iter_control_flows<'g>(&'g self) -> impl Iterator<Item = Ptr<ControlFlowTransfer>> + 'g {
        self.iter_blocks()
            .flat_map(|block| block.succs.iter().map(|x| *x).collect::<Vec<_>>())
    }

    fn gen_instrs(&mut self, alloc: &Allocator) {
        for lir_block in self.iter_blocks() {
            log::debug!("GENINSTR block {:?}", lir_block.firm);
            GenInstrBlock::fill_instrs(self, lir_block, alloc);
        }
    }

    pub fn get_block(&self, firm_block: libfirm_rs::nodes::Block) -> Ptr<BasicBlock> {
        self.blocks
            .get(&firm_block)
            .expect("BlockGraph is incomplete")
            .clone()
    }
}

impl BlockGraph {
    fn construct_flows(&mut self, alloc: &Allocator) {
        self.firm.walk_blocks(|visit, firm_block| match visit {
            VisitTime::BeforePredecessors => {
                log::debug!("VISIT {:?}", firm_block);
                let local_block = self.get_block(*firm_block);

                // Foreign values are the green points in yComp (inter-block edges)
                for node_in_block in firm_block.out_nodes() {
                    match node_in_block {
                        // The end node is only for keep alive edges, which we don't care about
                        Node::End(_) => (),

                        Node::Phi(_) => {
                            local_block.new_terminating_slot(node_in_block, alloc);
                        }

                        _ => node_in_block
                            .in_nodes()
                            // Mem edges are uninteresting across blocks
                            .filter(|value| value.mode() != Mode::M())
                            // If this is a value produced by our block, there is no need to
                            // transfer it from somewhere else
                            .filter(|value| value.block() != *firm_block)
                            // Foreign values that are not phi, flow in from each cfg pred
                            // => values x cfg_preds
                            .for_each(|value| {
                                // Do this here, because we don't want to move `local_block`
                                // into closure
                                local_block.new_terminating_slot(value, alloc);
                            }),
                    }
                }
            }

            VisitTime::AfterPredecessors => (),
        });

        /* TODO Reenable: Like phi nodes, but without phi
        // Special case for return nodes, see BasicBlock.return comment
        let end_block = self.borrow().get_block(self.borrow().firm.end_block());
        let multislot = end_block.new_terminating_multislot();
        for return_node in self.borrow().firm.end_block().cfg_preds() {
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
            let block_with_return_node = self.borrow().get_block(return_node.block());
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
        */
    }
}

struct BasicBlockIter<'g> {
    _graph: PhantomData<&'g !>,
    visited: HashSet<libfirm_rs::nodes::Block>,
    visit_list: VecDeque<Ptr<BasicBlock>>,
}

impl<'g> Iterator for BasicBlockIter<'g> {
    type Item = Ptr<BasicBlock>;

    fn next(&mut self) -> Option<Self::Item> {
        self.visit_list.pop_front().map(|block| {
            for edge in &block.succs {
                let succ = edge.target;
                if !self.visited.contains(&succ.firm) {
                    self.visited.insert(succ.firm);
                    self.visit_list.push_back(succ);
                }
            }

            block
        })
    }
}

impl Ptr<ControlFlowTransfer> {
    /// Only call this from target
    fn add_incoming_value_flow(mut self, target_slot: Ptr<ValueSlot>, alloc: &Allocator) {
        if let Some((source, target)) = self
            .register_transitions
            .iter()
            .find(|(_, existing_slot)| target_slot.firm == existing_slot.firm)
        {
            log::debug!(
                "\t\t? {:?}({:?}) := {:?}",
                target_slot.allocated_in.firm,
                target.num,
                target.firm,
            );
            for multislot in target.allocated_in.regs.iter() {
                log::debug!(
                    "\t\t! {:?}({:?}) := {:?}",
                    target.allocated_in.firm,
                    multislot.num(),
                    multislot.firm(),
                );
                if let MultiSlot::Multi { slots, .. } = &**multislot {
                    for slot in slots.iter() {
                        log::debug!("\t\t\t {:?} := {:?} @ {:?}", slot.num, slot.firm, slot);
                    }
                }
            }
            log::debug!(
                "\t\t> {:?}({:?}) := {:?} @ {:?}",
                target_slot.allocated_in.firm,
                target_slot.num,
                target_slot.firm,
                target_slot,
            );

            log::debug!(
                "\tPIGGY: from='{:?}' to='{:?}' value='{:?}'",
                source.allocated_in().firm,
                target.allocated_in.firm,
                target.firm,
            );
            assert_eq!(target.num, target_slot.num);

            return;
        }

        let source_slot = self.source.new_forwarding_slot(&target_slot, alloc);

        log::debug!(
            "\tTRANS: from='{:?}' to='{:?}' value='{:?}'",
            self.source.firm,
            self.target.firm,
            target_slot.firm,
        );
        match &*source_slot {
            MultiSlot::Single(slot) => assert_eq!(slot.firm, target_slot.firm),
            MultiSlot::Multi { phi, .. } if Node::Phi(*phi) == target_slot.firm => (),
            MultiSlot::Multi { slots, .. } => assert!(
                slots.iter().any(|slot| slot.firm == target_slot.firm),
                "{:?} does not contain slot with firm == {:?}",
                slots.iter().map(|slot| slot.firm).collect(): Vec<_>,
                target_slot.firm
            ),
        }

        self.register_transitions.push((source_slot, target_slot));
    }

    /// Do there exist multiple incoming flows for the target slot of `flow_idx`
    /// in the target block?
    pub fn must_copy_in_source(self, flow_idx: usize) -> bool {
        let target_slot_num = self.register_transitions[flow_idx].1.num;

        self.target
            .preds
            .iter()
            .filter(|pred| !Ptr::ptr_eq(**pred, self))
            .any(|pred| {
                self.register_transitions
                    .iter()
                    .any(|(_, other_target_slot)| other_target_slot.num == target_slot_num)
            })
    }

    /// Do there exist multiple outgoing flows for the source slot of `flow_idx`
    /// in the source block?
    pub fn must_copy_in_target(&self, flow_idx: usize) -> bool {
        let source_slot_num = self.register_transitions[flow_idx].0.num();

        self.source
            .succs
            .iter()
            .filter(|succ| !Ptr::ptr_eq(**succ, *self))
            .any(|succ| {
                succ.register_transitions
                    .iter()
                    .any(|(other_source_slot, _)| other_source_slot.num() == source_slot_num)
            })
    }
}

impl Ptr<BasicBlock> {
    fn new_multislot(self, terminates_in: Ptr<BasicBlock>) -> MultiSlotBuilder {
        MultiSlotBuilder::new(None, self, terminates_in)
    }

    #[allow(dead_code)]
    fn new_terminating_multislot_from_phi(
        self,
        phi: nodes::Phi,
        alloc: &Allocator,
    ) -> Ptr<MultiSlot> {
        self.new_multislot_from_phi(phi, self, alloc)
    }

    fn new_multislot_from_phi(
        self,
        phi: nodes::Phi,
        terminates_in: Ptr<BasicBlock>,
        alloc: &Allocator,
    ) -> Ptr<MultiSlot> {
        assert_eq!(phi.block(), self.firm);
        let mut slotbuilder = MultiSlotBuilder::new(Some(phi), self, terminates_in);

        phi.preds()
            // Mem edges are uninteresting across blocks
            .filter(|(_, value)| value.mode() != Mode::M())
            // The next two 'maps' need to be seperated in two closures
            // because we wan't to selectively `move` `value` and
            // `multislot` into the closure, but take `self` by reference
            .map(|(cfg_pred, value)| (cfg_pred, value, self.graph.get_block(value.block())))
            .map(|(cfg_pred, value, original_block)| {
                (
                    cfg_pred,
                    slotbuilder.add_possible_value(value, original_block, alloc),
                )
            })
            .for_each(|(cfg_pred, value_slot)| {
                self.find_incoming_edge_from(cfg_pred)
                    .unwrap()
                    .add_incoming_value_flow(value_slot, alloc);
            });

        slotbuilder.get_multislot(alloc)
    }

    /// TODO This function makes the assupmtion that is not used for `value`s
    /// that are the inputs to phi nodes (instead `new_multislot`,
    /// `add_possible_value` and `add_incoming_value_flow` are used seperately
    /// in that case). BE AWARE OF THIS when refactoring
    fn new_slot(
        &self,
        value: libfirm_rs::nodes::Node,
        terminates_in: Ptr<BasicBlock>,
        alloc: &Allocator,
    ) -> Ptr<MultiSlot> {
        let this = self;
        let possibly_existing_multislot =
            this.regs
                .iter()
                .find(|multislot| match (&(***multislot), value) {
                    (MultiSlot::Multi { phi: slot_phi, .. }, Node::Phi(value_phi)) => {
                        *slot_phi == value_phi
                    }
                    (MultiSlot::Multi { slots, .. }, _) => {
                        slots.iter().any(|slot| slot.firm == value)
                    }
                    (MultiSlot::Single(slot), _) => slot.firm == value,
                });

        if let Some(multislot) = possibly_existing_multislot {
            log::debug!(
                "\tREUSE: slot={} in='{:?}' value='{:?}'",
                multislot.num(),
                this.firm,
                value
            );

            *multislot
        } else {
            drop(this);
            let originates_in = self.graph.get_block(value.block());

            match value {
                Node::Phi(phi) if value.block() == self.firm => {
                    self.new_multislot_from_phi(phi, terminates_in, alloc)
                }
                _ => {
                    let mut slotbuilder = self.new_multislot(terminates_in);
                    let slot = slotbuilder.add_possible_value(value, originates_in, alloc);

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
                    let originates_here = Node::is_const(slot.firm)
                        || Node::is_address(slot.firm)
                        || slot.allocated_in.firm == slot.originates_in.firm;
                    if !originates_here {
                        for incoming_edge in &self.preds {
                            incoming_edge.add_incoming_value_flow(slot, alloc);
                        }
                    }

                    slotbuilder.get_multislot(alloc)
                }
            }
        }
    }

    fn new_forwarding_slot(self, target_slot: &ValueSlot, alloc: &Allocator) -> Ptr<MultiSlot> {
        self.new_slot(target_slot.firm, target_slot.terminates_in, alloc)
    }

    fn new_terminating_slot(
        self,
        value: libfirm_rs::nodes::Node,
        alloc: &Allocator,
    ) -> Ptr<MultiSlot> {
        self.new_slot(value, self, alloc)
    }

    pub(super) fn new_private_slot(
        self,
        value: libfirm_rs::nodes::Node,
        alloc: &Allocator,
    ) -> Ptr<MultiSlot> {
        assert_eq!(value.block(), self.firm);
        self.new_slot(value, self, alloc)
    }
}

impl BasicBlock {
    fn find_incoming_edge_from(
        &self,
        cfg_pred: libfirm_rs::nodes::Block,
    ) -> Option<Ptr<ControlFlowTransfer>> {
        self.preds
            .iter()
            .find(|edge| edge.source.firm == cfg_pred)
            .map(|x| *x)
    }

    fn skeleton_block(
        known_blocks: &mut HashMap<libfirm_rs::nodes::Block, Ptr<BasicBlock>>,
        firm: libfirm_rs::nodes::Block,
        alloc: &Allocator,
    ) -> Ptr<Self> {
        known_blocks
            .entry(firm)
            .or_insert_with(|| {
                alloc.block(BasicBlock {
                    num: firm.node_id(),
                    regs: Vec::new(),
                    code: Code::default(),
                    preds: Vec::new(),
                    succs: Vec::new(),
                    firm,
                    // if No is not true, overridden in suring construct_flows
                    returns: BasicBlockReturns::No,
                    graph: Ptr::null(), // will be patched up by caller
                })
            })
            .clone()
    }
}

#[derive(Debug)]
pub struct MultiSlotBuilder {
    num: usize,
    slots: Vec<Ptr<ValueSlot>>,
    phi: Option<nodes::Phi>,
    allocated_in: Ptr<BasicBlock>,
    terminates_in: Ptr<BasicBlock>,
}

impl MultiSlotBuilder {
    fn new(
        phi: Option<nodes::Phi>,
        allocated_in: Ptr<BasicBlock>,
        terminates_in: Ptr<BasicBlock>,
    ) -> Self {
        let num = allocated_in.regs.len();
        MultiSlotBuilder {
            num,
            slots: Vec::new(),
            phi,
            allocated_in,
            terminates_in,
        }
    }

    fn add_possible_value(
        &mut self,
        value: libfirm_rs::nodes::Node,
        originates_in: Ptr<BasicBlock>,
        alloc: &Allocator,
    ) -> Ptr<ValueSlot> {
        assert!(self.allocated_in.regs.len() >= self.num);
        assert!(self.allocated_in.regs.len() <= self.num + 1);

        {
            let is_duplicate = self.slots.iter().any(|slot| slot.firm == value);

            assert!(!is_duplicate);
        }

        let slot = ValueSlot {
            num: self.num,
            firm: value,
            allocated_in: self.allocated_in,
            originates_in,
            terminates_in: self.terminates_in,
        };

        log::debug!(
            "\tALLOC: slot={} in='{:?}' value='{:?}'",
            slot.num,
            self.allocated_in.firm,
            slot.firm
        );

        let slot = alloc.slot(slot);
        self.slots.push(slot);

        self.commit(alloc);

        slot
    }

    #[allow(clippy::let_and_return)]
    fn get_multislot(mut self, alloc: &Allocator) -> Ptr<MultiSlot> {
        let num = self.num;
        self.commit(alloc);

        let x = self.allocated_in.regs[num];
        x
    }

    fn commit(&mut self, alloc: &Allocator) {
        let slot = alloc.multi_slot(if let Some(phi) = self.phi {
            MultiSlot::Multi {
                phi,
                slots: self.slots.clone(),
            }
        } else {
            assert_eq!(self.slots.len(), 1);
            MultiSlot::Single(self.slots[0].clone())
        });

        let allocated_in = self.allocated_in;
        let mut allocated_in = allocated_in;
        if allocated_in.regs.len() == self.num {
            allocated_in.regs.push(slot);
        } else if allocated_in.regs.len() == self.num + 1 {
            allocated_in.regs[self.num] = slot;
        } else {
            unreachable!()
        }
    }
}
