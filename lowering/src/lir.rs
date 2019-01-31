//! Low level intermediate representation

use super::{
    gen_instr::GenInstrBlock,
    lir_allocator::{self, Ptr},
};
use crate::{derive_ptr_debug, firm, type_checking::type_system::CheckedType};

use crate::optimization::{Local, RemoveCriticalEdges};
use libfirm_rs::{
    bindings,
    nodes::{self, Node, NodeTrait, ProjKind},
    Tarval, VisitTime,
};
use std::{
    collections::{HashMap, HashSet, VecDeque},
    convert::TryFrom,
    fmt::{self, Display},
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

derive_ptr_debug!(BlockGraph);

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
derive_ptr_debug!(Ptr<BlockGraph>);

#[derive(Debug, Clone)]
pub struct Code<CopyInInstr, CopyOutInstr, BodyInstr, LeaveInstr> {
    pub copy_in: Vec<CopyInInstr>,
    pub body: Vec<BodyInstr>,
    pub copy_out: Vec<CopyOutInstr>,
    pub leave: Vec<LeaveInstr>,
}

impl<A, B, C, D> Default for Code<A, B, C, D> {
    fn default() -> Self {
        Code {
            copy_in: vec![],
            body: vec![],
            copy_out: vec![],
            leave: vec![],
        }
    }
}

/// A unifying enum over the different operations contained in Code.
pub enum CodeInstruction<CopyInInstr, CopyOutInstr, BodyInstr, LeaveInstr> {
    CopyIn(CopyInInstr),
    Body(BodyInstr),
    CopyOut(CopyOutInstr),
    Leave(LeaveInstr),
}

impl<A, B, C, D> Code<A, B, C, D> {
    /// Iterate over the members of code in the following order:
    ///
    /// 1. `self.copy_in`
    /// 2. `self.body`
    /// 3. `self.copy_out`
    /// 4. `self.leave`
    pub fn iter_unified(&self) -> impl Iterator<Item = CodeInstruction<&A, &B, &C, &D>> {
        let copy_in = box self.copy_in.iter().map(CodeInstruction::CopyIn);
        let body = box self.body.iter().map(CodeInstruction::Body);
        let copy_out = box self.copy_out.iter().map(CodeInstruction::CopyOut);
        let leave = box self.leave.iter().map(CodeInstruction::Leave);
        copy_in.chain(body).chain(copy_out).chain(leave)
    }

    /// Like iter_unified, but iterate with mutable references.
    pub fn iter_unified_mut(
        &mut self,
    ) -> impl Iterator<Item = CodeInstruction<&mut A, &mut B, &mut C, &mut D>> {
        let copy_in = box self.copy_in.iter_mut().map(CodeInstruction::CopyIn);
        let body = box self.body.iter_mut().map(CodeInstruction::Body);
        let copy_out = box self.copy_out.iter_mut().map(CodeInstruction::CopyOut);
        let leave = box self.leave.iter_mut().map(CodeInstruction::Leave);
        copy_in.chain(body).chain(copy_out).chain(leave)
    }
}

/// This is a vertex in the basic-block graph
pub struct BasicBlock {
    /// Unique number for the BasicBlock
    pub num: i64,
    /// The Pseudo-registers used by the Block
    pub regs: Vec<Ptr<MultiSlot>>,
    /// The instructions (using arbitrarily many registers) of the block
    pub code: Code<CopyPropagation, CopyPropagation, Instruction, Leave>,
    /// Control flow-transfers *to* this block.
    /// Usually at most 2
    pub preds: Vec<Ptr<ControlFlowTransfer>>,
    /// Control flow-transfers *out of* this block
    /// Usually at most 2
    pub succs: Vec<Ptr<ControlFlowTransfer>>,

    /// The firm structure of this block
    pub firm: libfirm_rs::nodes::Block,

    pub graph: Ptr<BlockGraph>,
}

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

    pub fn is_single(&self) -> bool {
        match self {
            MultiSlot::Single(_) => true,
            MultiSlot::Multi { .. } => false,
        }
    }
}

impl fmt::Debug for Ptr<MultiSlot> {
    fn fmt(&self, out: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(out, "*{:?}", **self)
    }
}

impl fmt::Debug for MultiSlot {
    fn fmt(&self, out: &mut fmt::Formatter<'_>) -> fmt::Result {
        match &*self {
            MultiSlot::Single(vs) => write!(out, "{:?}", **vs), // prepends VS itself
            MultiSlot::Multi { .. } => write!(out, "MS:{}:{}", self.allocated_in().num, self.num()),
        }
    }
}

#[derive(Clone)]
pub enum Instruction {
    LoadParam {
        idx: u32,
    },
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
        src: Operand,
        dst: Ptr<MultiSlot>,
    },
    /// Value conversion pseudo-instruction:
    /// FIRM knows value conversions, but LIR doesn't.
    /// Keeping this seperate from mov though, for easier recognition in
    /// backend.
    Conv {
        src: Operand,
        dst: Ptr<MultiSlot>,
    },
    /// If dst is None, result is in register r0, which cannot be accessed
    /// using molki register names.
    Call {
        func: String,
        args: Vec<Operand>,
        dst: Option<Ptr<MultiSlot>>,
    },
    StoreMem(StoreMem),
    LoadMem(LoadMem),
    Comment(String),
}

#[derive(Clone)]
pub struct LoadMem {
    pub src: AddressComputation<Operand>,
    pub dst: Ptr<MultiSlot>,
    pub size: u32,
}

#[derive(Clone)]
pub struct StoreMem {
    pub src: Operand,
    pub dst: AddressComputation<Operand>,
    pub size: u32,
}

impl fmt::Debug for Instruction {
    fn fmt(&self, fmt: &mut fmt::Formatter<'_>) -> fmt::Result {
        use self::{Instruction::*, LoadMem, StoreMem};

        match self {
            LoadParam { idx } => write!(fmt, "load_param {}", idx),
            Binop {
                kind,
                src1,
                src2,
                dst,
            } => write!(fmt, "{:?} {:?} {:?} => {:?}", kind, src1, src2, dst),
            Div { src1, src2, dst } => write!(fmt, "div {:?} {:?} => {:?}", src1, src2, dst),
            Mod { src1, src2, dst } => write!(fmt, "mod {:?} {:?} => {:?}", src1, src2, dst),
            Unop { kind, src, dst } => write!(fmt, "{:?} {:?} => {:?}", kind, src, dst),
            Conv { src, dst } => write!(fmt, "conv {:?} => {:?}", src, dst),
            Call { func, args, dst } => {
                let args = args
                    .iter()
                    .map(|o| format!("{:?}", o))
                    .collect::<Vec<_>>()
                    .join(", ");
                write!(fmt, "call {:?} [ {:?} ] => {:?}", func, args, dst)
            }
            StoreMem(StoreMem { src, dst, .. }) => write!(fmt, "storemem {:?} => {:?}", src, dst),
            LoadMem(LoadMem { src, dst, .. }) => write!(fmt, "loadmem {:?} => {:?}", src, dst),
            Comment(comment) => {
                for line in comment.lines() {
                    write!(fmt, "// {}", line)?;
                }
                Ok(())
            }
        }
    }
}

/// This implements address computation, see
/// http://www.c-jump.com/CIS77/CPU/x86/lecture.html#X77_0110_scaled_indexed
#[derive(Debug, Copy, Clone)]
pub struct AddressComputation<Op: Display + Copy> {
    // offset(base, index, stride) = offset + base + index * stride
    pub offset: isize,
    pub base: Op,
    pub index: IndexComputation<Op>,
}

impl<Op: Display + Copy> AddressComputation<Op> {
    pub fn operands(&self) -> Vec<Op> {
        let mut operands = vec![self.base];
        if let IndexComputation::Displacement(op, _) = self.index {
            operands.push(op);
        }
        operands
    }
}

impl<Op: Display + Copy> std::fmt::Display for AddressComputation<Op> {
    fn fmt(&self, fmt: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let index = match &self.index {
            IndexComputation::Zero => String::new(),
            IndexComputation::Displacement(op, stride) => format!(", {}, {}", op, stride),
        };
        write!(fmt, "{}({}{})", self.offset, self.base, index)
    }
}

/// Index of AddressComputation variant of the Instruction enum.
#[derive(Debug, Copy, Clone)]
pub enum IndexComputation<Op: Display + Copy> {
    Displacement(Op, Stride),
    Zero,
}

impl<Op: Display + Copy> IndexComputation<Op> {
    pub fn is_zero(&self) -> bool {
        match self {
            IndexComputation::Zero => true,
            _ => false,
        }
    }
    pub fn displacement_op(&self) -> Option<&Op> {
        match self {
            IndexComputation::Displacement(op, _) => Some(op),
            _ => None,
        }
    }
}

#[derive(Debug, Display, Copy, Clone, Hash, Eq, PartialEq)]
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

#[derive(Debug, Clone, Copy)]
pub enum JmpKind {
    Unconditional,
    Conditional(CondOp),
}

impl std::fmt::Display for JmpKind {
    fn fmt(&self, fmt: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use self::{CondOp::*, JmpKind::*};
        match self {
            Unconditional => write!(fmt, "jmp"),
            Conditional(Equals) => write!(fmt, "je"),
            Conditional(NotEquals) => write!(fmt, "jne"),
            Conditional(LessThan) => write!(fmt, "jl"),
            Conditional(LessEquals) => write!(fmt, "jle"),
            Conditional(GreaterThan) => write!(fmt, "jg"),
            Conditional(GreaterEquals) => write!(fmt, "jge"),
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub enum CondOp {
    Equals,
    NotEquals,
    LessThan,
    GreaterThan,
    LessEquals,
    GreaterEquals,
    /* Zero
     * Nonzero
     * Negative
     * Nonnegative */
}

impl CondOp {
    pub(super) fn swap(self) -> Self {
        match self {
            CondOp::LessThan => CondOp::GreaterThan,
            CondOp::GreaterThan => CondOp::LessThan,
            CondOp::LessEquals => CondOp::GreaterEquals,
            CondOp::GreaterEquals => CondOp::LessEquals,
            op => op,
        }
    }
}

impl TryFrom<bindings::ir_relation::Type> for CondOp {
    type Error = bindings::ir_relation::Type;
    fn try_from(r: bindings::ir_relation::Type) -> Result<Self, Self::Error> {
        let ok = match r {
            bindings::ir_relation::Equal => CondOp::Equals,
            bindings::ir_relation::LessGreater => CondOp::NotEquals,
            bindings::ir_relation::Less => CondOp::LessThan,
            bindings::ir_relation::Greater => CondOp::GreaterThan,
            bindings::ir_relation::LessEqual => CondOp::LessEquals,
            bindings::ir_relation::GreaterEqual => CondOp::GreaterEquals,
            x => return Err(x),
        };
        Ok(ok)
    }
}

/// Instructions that are at the end of a basic block.
#[derive(Clone)]
pub enum Leave {
    CondJmp {
        op: CondOp,
        lhs: Operand,
        rhs: Operand,
        true_target: Ptr<BasicBlock>,
        false_target: Ptr<BasicBlock>,
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

impl fmt::Debug for Leave {
    fn fmt(&self, fmt: &mut fmt::Formatter<'_>) -> fmt::Result {
        use self::Leave::*;
        match self {
            CondJmp {
                op,
                lhs,
                rhs,
                true_target,
                false_target,
            } => write!(
                fmt,
                "cndjmp:t{}:f{} {:?} {:?} {:?}",
                true_target.num, false_target.num, op, lhs, rhs
            ),
            Jmp { target } => write!(fmt, "jmp {}", target.num),
            Return { value, .. } => write!(fmt, "ret {:?}", value),
        }
    }
}

/// The representation of a single element in
/// ControlFlowTransfer.register_transitions. The consumer of the LIR
/// (a register allocator / target arch code generator) emits the
/// concrete instructions to flow values from one basic block to the other.
/// It will commonly have to choose between using registers or spill code.
#[derive(Clone)]
pub struct CopyPropagation {
    // FIXME: This can be operand
    pub(super) src: CopyPropagationSrc,
    pub(super) dst: Ptr<ValueSlot>,
}

#[derive(Debug, Copy, Clone)]
pub enum CopyPropagationSrc {
    Slot(Ptr<MultiSlot>),
    Param { idx: u32 },
    Imm(Tarval),
}

impl fmt::Debug for CopyPropagation {
    fn fmt(&self, fmt: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(fmt, "copyprop {:?} => {:?}", self.src, *self.dst)
    }
}

#[derive(Clone, Copy)]
pub enum Operand {
    Slot(Ptr<MultiSlot>),

    /// NOTE: Tarval contains a raw pointer, thus Imm(t) is only valid for the
    /// lifetime of that pointer (the FIRM graph).
    Imm(Tarval),
    /// only readable!
    Param {
        idx: u32,
    },
}

impl fmt::Debug for Operand {
    fn fmt(&self, fmt: &mut fmt::Formatter<'_>) -> fmt::Result {
        use self::Operand::*;
        match self {
            Imm(tv) => write!(fmt, "Imm{{{:?}}}", tv),
            Slot(ms) => write!(fmt, "Slot#{:?}", ms),
            Param { idx } => write!(fmt, "Param#{}", idx),
        }
    }
}

impl fmt::Display for Operand {
    fn fmt(&self, fmt: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Debug::fmt(self, fmt)
    }
}

impl From<CopyPropagationSrc> for Operand {
    fn from(op: CopyPropagationSrc) -> Self {
        match op {
            CopyPropagationSrc::Slot(s) => Operand::Slot(s),
            CopyPropagationSrc::Param { idx } => Operand::Param { idx },
            CopyPropagationSrc::Imm(val) => Operand::Imm(val),
        }
    }
}

#[derive(Debug, Display, Clone)]
pub enum BinopKind {
    Add,
    /// `Binop { kind: Sub, src1, src2, dst }` <=> dst = src1 - src2
    Sub,
    // We only multiply signed integers, so we can always use `imul`
    Mul,
    And,
    Or,
    Xor,
}

#[derive(Debug, Display, Clone)]
pub enum UnopKind {
    Neg,
    Not,
}

/// An abstract pseudo-register
///
/// How to get a `MultiSlot` from a `ValueSlot`:
///
/// `value_slot` -> `allocated_in` -> `regs` (this is a Vec of `MultiSlot`s,
/// where our `value_slot` is at `value_slot.num`) => `multi_slot` of our
/// `value_slot` \\(*.*)/
///
/// ```rust,ignore
/// let multi_slot = value_slot.allocated_in.regs[value_slot.num];
/// ```
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

impl ValueSlot {
    pub fn multislot(&self) -> Ptr<MultiSlot> {
        self.allocated_in.regs[self.num]
    }
}

impl fmt::Debug for ValueSlot {
    fn fmt(&self, fmt: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(fmt, "VS:{}:{}", self.allocated_in.num, self.num)
    }
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

    pub source: Ptr<BasicBlock>,
    pub target: Ptr<BasicBlock>,
}

lazy_static::lazy_static! {
    static ref COMPRAKT_LOWERING_LIR_DUMP_YCOMP_PRE_LIR_DUMPED: std::sync::Once =
        std::sync::Once::new();
}

impl BlockGraph {
    fn from(firm_graph: libfirm_rs::Graph, alloc: &Allocator) -> Ptr<Self> {
        RemoveCriticalEdges::optimize_function(firm_graph);

        if std::env::var("COMPRAKT_LOWERING_LIR_DUMP_YCOMP_PRE_LIR").is_ok() {
            COMPRAKT_LOWERING_LIR_DUMP_YCOMP_PRE_LIR_DUMPED.call_once(|| {
                let suffix = std::ffi::CString::new("pre-lir").unwrap();
                unsafe { libfirm_rs::bindings::dump_all_ir_graphs(suffix.as_ptr()) };
            });
        }

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

    /// Gives a list of all `BasicBlock`s in the graph in postorder.
    pub fn postorder_blocks(&self) -> Vec<Ptr<BasicBlock>> {
        let mut postorder_visitor = BasicBlockPostorderVisitor::default();
        postorder_visitor.visit(self.head);
        postorder_visitor.res
    }

    /// Iterate over all control flow transfers in `self` in a breadth-first
    /// manner
    pub fn iter_control_flows<'g>(&'g self) -> impl Iterator<Item = Ptr<ControlFlowTransfer>> + 'g {
        self.iter_blocks().flat_map(|block| block.succs.clone())
    }

    fn gen_instrs(&mut self, alloc: &Allocator) {
        for lir_block in self.iter_blocks() {
            log::debug!("GENINSTR block {:?}", lir_block.firm);
            GenInstrBlock::fill_instrs(self, lir_block, alloc);
        }
    }

    pub fn get_block(&self, firm_block: libfirm_rs::nodes::Block) -> Ptr<BasicBlock> {
        *self
            .blocks
            .get(&firm_block)
            .expect("BlockGraph is incomplete")
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
                    log::debug!("VISIT {:?}: NODE {:?}", firm_block, node_in_block);
                    match node_in_block {
                        // The end node is only for keep alive edges, which we don't care about
                        Node::End(_) => (),

                        Node::Phi(_) => {
                            assert!(node_in_block.block() == *firm_block);
                            log::debug!(
"CONSTRUCT_FLOWS: PHI IN BLOCK: local_block.new_terminating_slot {:?}", node_in_block);
                            local_block.new_terminating_slot(node_in_block, alloc);
                        }

                        _ => node_in_block
                            .in_nodes()
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
            for succ in block.succ_blocks() {
                if !self.visited.contains(&succ.firm) {
                    self.visited.insert(succ.firm);
                    self.visit_list.push_back(succ);
                }
            }

            block
        })
    }
}

#[derive(Default)]
struct BasicBlockPostorderVisitor {
    visited: HashSet<libfirm_rs::nodes::Block>,
    res: Vec<Ptr<BasicBlock>>,
}

impl BasicBlockPostorderVisitor {
    fn visit(&mut self, block: Ptr<BasicBlock>) {
        block.succ_blocks().for_each(|block| {
            if self.visited.insert(block.firm) {
                self.visit(block);
            }
        });
        self.res.push(block);
    }
}

impl Ptr<ControlFlowTransfer> {
    /// Only call this from target
    fn add_incoming_value_flow(mut self, target_slot: Ptr<ValueSlot>, alloc: &Allocator) {
        if let Some((source, target)) =
            self.register_transitions.iter().find(|(_, existing_slot)| {
                target_slot.firm == existing_slot.firm && existing_slot.multislot().is_single()
            })
        {
            log::debug!("\t\tedge is {:?}->{:?}", self.source.firm, self.target.firm);
            log::debug!(
                "\t\tslot pair is source={:?} target={:?}",
                source,
                target.multislot()
            );
            log::debug!(
                "\t\tslot pair is source={:?} target={:?}",
                source.firm(),
                target.firm
            );
            log::debug!("\t\ttarget_slot.firm = {:?}", target_slot.firm);
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
                        log::debug!("\t\t\t {:?} := {:?} @ {:?}", slot.num, slot.firm, **slot);
                    }
                }
            }
            log::debug!(
                "\t\t> {:?}({:?}) := {:?} @ {:?}",
                target_slot.allocated_in.firm,
                target_slot.num,
                target_slot.firm,
                *target_slot,
            );

            log::debug!(
                "\tPIGGY: from='{:?}' to='{:?}' value='{:?}'",
                source.allocated_in().firm,
                target.allocated_in.firm,
                target.firm,
            );
            //assert_eq!(target.num, target_slot.num);

            if Node::is_phi(source.firm()) && Node::is_phi(target.firm) {
                // fallthrough
            } else {
                return;
            }
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
                pred.register_transitions
                    .iter()
                    .any(|(_, other_target_slot)| other_target_slot.num == target_slot_num)
            })
    }

    /// Do there exist multiple outgoing flows for the source slot of `flow_idx`
    /// in the source block?
    pub fn must_copy_in_target(self, flow_idx: usize) -> bool {
        let source_slot_num = self.register_transitions[flow_idx].0.num();

        self.source
            .succs
            .iter()
            .filter(|succ| !Ptr::ptr_eq(**succ, self))
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
            // The next two 'maps' need to be seperated in two closures
            // because we wan't to selectively `move` `value` and
            // `multislot` into the closure, but take `self` by reference
            .map(|(cfg_pred, value)| (cfg_pred, value, self.graph.get_block(value.block())))
            .map(|(cfg_pred, value, original_block)| {
                (
                    cfg_pred,
                    // emits ALLOC
                    slotbuilder.add_possible_value(value, original_block, alloc),
                )
            })
            .for_each(|(cfg_pred, value_slot)| {
                self.find_incoming_edge_from(cfg_pred)
                    .unwrap()
                    // emits \t\t notices
                    .add_incoming_value_flow(value_slot, alloc);
            });

        slotbuilder.get_multislot(alloc)
    }

    /// TODO This function makes the assupmtion that is not used for `value`s
    /// that are the inputs to phi nodes (instead `new_multislot`,
    /// `add_possible_value` and `add_incoming_value_flow` are used seperately
    /// in that case). BE AWARE OF THIS when refactoring
    fn new_slot(
        self,
        value: libfirm_rs::nodes::Node,
        terminates_in: Ptr<BasicBlock>,
        alloc: &Allocator,
    ) -> Ptr<MultiSlot> {
        let possibly_existing_multislot =
            self.regs
                .iter()
                .find(|multislot| match (&(***multislot), value) {
                    (MultiSlot::Multi { phi: slot_phi, .. }, Node::Phi(value_phi)) => {
                        *slot_phi == value_phi
                    }
                    (MultiSlot::Multi { .. }, _) => {
                        // This is a passthrough flow, i.e.,  the rhs value flows through this block
                        // unmodified for later use.
                        false
                    }
                    (MultiSlot::Single(slot), _) => slot.firm == value,
                });

        if let Some(multislot) = possibly_existing_multislot {
            log::debug!(
                "\tREUSE: slot={} in='{:?}' value='{:?}'",
                multislot.num(),
                self.firm,
                value
            );

            *multislot
        } else {
            let originates_in = self.graph.get_block(value.block());

            match value {
                Node::Phi(phi) if value.block() == self.firm => {
                    log::debug!("\tNEW_SLOT::MULTISLOT_FROM_PHI: {:?}", value);
                    self.new_multislot_from_phi(phi, terminates_in, alloc)
                }
                _ => {
                    log::debug!("\tNEW_SLOT::NORMAL: {:?}", value);
                    let mut slotbuilder = self.new_multislot(terminates_in);
                    let slot = slotbuilder.add_possible_value(value, originates_in, alloc);

                    // If the value is foreign, we need to "get it" from each blocks above us.
                    //
                    // NOTE:
                    // Some FIRM values (e.g. const) are always located in the FIRM start block.
                    // We do ont want to flow those values all the way through the graph because
                    // it would likely increase register pressure / incur overhead during LIR
                    // construction.
                    // In gen_instr.rs terms, those values are "JIT operands", which is a term
                    // that should be abandoned but stands for value that we know at compile time.
                    //
                    // For the above reasons, we'll treat those values as "originating here".
                    //
                    // The match arms below must be kept in sync with gen_instr.rs:
                    // Search for LIR_JIT_COMPUTED_OPERAND_SEARCH_MARKER
                    //
                    // IMPORTANT: we cannot make above assumption if this node is used as input to a
                    // Phi node in this block, because the value needs to originate in the
                    // corresponding cfg_pred. However, when creating slots for the inputs of phi
                    // nodes, this function (`MutRc<BasicBlock>::new_slot`), in not used. So the
                    // assumption holds, but BE AWARE OF THIS when refactoring.
                    let node_is_arg_proj =
                        if let Node::Proj(_, ProjKind::Start_TArgs_Arg(..)) = slot.firm {
                            true
                        } else {
                            false
                        };
                    let originates_here = Node::is_const(slot.firm)
                        || Node::is_address(slot.firm)
                        || Node::is_size(slot.firm)
                        || Node::is_member(slot.firm)
                        || Node::is_sel(slot.firm)
                        || node_is_arg_proj
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
            .cloned()
    }

    fn skeleton_block(
        known_blocks: &mut HashMap<libfirm_rs::nodes::Block, Ptr<BasicBlock>>,
        firm: libfirm_rs::nodes::Block,
        alloc: &Allocator,
    ) -> Ptr<Self> {
        *known_blocks.entry(firm).or_insert_with(|| {
            alloc.block(BasicBlock {
                num: firm.node_id(),
                regs: Vec::new(),
                code: Code::default(),
                preds: Vec::new(),
                succs: Vec::new(),
                firm,
                graph: Ptr::null(), // will be patched up by caller
            })
        })
    }

    pub(super) fn pred_blocks(&self) -> impl Iterator<Item = Ptr<BasicBlock>> + '_ {
        self.preds.iter().map(|pred| pred.source)
    }

    pub(super) fn succ_blocks(&self) -> impl Iterator<Item = Ptr<BasicBlock>> + '_ {
        self.succs.iter().map(|succ| succ.target)
    }

    pub fn start_node(&self) -> Option<libfirm_rs::nodes::Start> {
        if self.is_start() {
            for out_node in self.firm.out_nodes() {
                if let Node::Start(start) = out_node {
                    return Some(start);
                }
            }
        }
        None
    }

    fn is_start(&self) -> bool {
        let firm_block = self.firm;
        let firm_start_block = self.graph.head.firm;
        firm_block == firm_start_block
    }
}

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

        // let is_duplicate = self.slots.iter().any(|slot| slot.firm == value);
        // assert!(!is_duplicate);

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
            MultiSlot::Single(self.slots[0])
        });

        let allocated_in = self.allocated_in;
        let mut allocated_in = allocated_in;
        if allocated_in.regs.len() == self.num {
            allocated_in.regs.push(slot);
        } else if allocated_in.regs.len() > self.num {
            allocated_in.regs[self.num] = slot;
        }
    }
}

#[inline]
pub(super) fn gen_label(block_num: i64) -> String {
    format!(".L{}", block_num)
}

#[cfg(test)]
mod tests {

    use super::*;
    #[test]
    fn code_iterator_works() {
        let mut code = Code::default();

        code.copy_in.push(1);
        code.copy_in.push(2);
        code.body.push(3);
        code.body.push(4);
        code.copy_out.push(5);
        code.copy_out.push(6);
        code.leave.push(7);
        code.leave.push(8);

        let res = code
            .iter_unified()
            .map(|instr| match instr {
                CodeInstruction::CopyIn(v) => v,
                CodeInstruction::Body(v) => v,
                CodeInstruction::CopyOut(v) => v,
                CodeInstruction::Leave(v) => v,
            })
            .cloned()
            .collect::<Vec<_>>();
        assert_eq!(res.len(), 8);

        let (_, is_sorted) = res.iter().cloned().fold((0, true), |(prev, sorted), next| {
            (next, sorted && prev <= next)
        });
        assert!(is_sorted);
    }
}
