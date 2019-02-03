//! Low level intermediate representation

use super::{
    gen_instr::GenInstrBlock,
    lir_allocator::{self, Ptr},
};
use crate::{derive_ptr_debug, firm, type_checking::type_system::CheckedType};
use itertools::Itertools;

use crate::optimization::{Local, RemoveCriticalEdges};
use libfirm_rs::{
    bindings,
    nodes::{Node, NodeTrait},
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

#[derive(PartialEq, Eq, Clone, Copy)]
pub struct Var {
    firm: Node,
    num: usize,
}

impl Var {
    pub fn firm(self) -> Node {
        self.firm
    }
    pub fn num(self) -> usize {
        self.num
    }
}

impl fmt::Debug for Var {
    fn fmt(&self, fmt: &mut fmt::Formatter<'_>) -> fmt::Result {
        // write!(fmt, "Var#{}#{{{:?}}}", self.num, self.firm)
        write!(fmt, "Var#{}#{{{:?}}}", self.num, self.firm.node_id())
    }
}

impl fmt::Display for Var {
    fn fmt(&self, fmt: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Debug::fmt(self, fmt)
    }
}

/// A graph of basic blocks.
pub struct BlockGraph {
    pub firm: libfirm_rs::Graph,
    pub blocks: HashMap<libfirm_rs::nodes::Block, Ptr<BasicBlock>>,

    pub vars: HashMap<Node, Var>,
    pub var_counter: usize,

    pub start_block: Ptr<BasicBlock>,
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

impl<A: fmt::Debug, B: fmt::Debug, C: fmt::Debug, D: fmt::Debug> fmt::Debug
    for CodeInstruction<A, B, C, D>
{
    fn fmt(&self, fmt: &mut fmt::Formatter<'_>) -> fmt::Result {
        use self::CodeInstruction::*;
        let debug: &fmt::Debug = match self {
            CopyIn(x) => x,
            Body(x) => x,
            CopyOut(x) => x,
            Leave(x) => x,
        };
        debug.fmt(fmt)
    }
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

/// The firm nodes required by a block before its body starts executing.
///
/// We distinguish two situations:
///
/// ## Simple In-Flow from a preceding block
///
/// A value V produced in one block A and used in a subsequent block B is
/// a ValueReq of block B.
/// `firm` will be V, and `from` will contain a single entry (A,V).
///
/// ## Phi Node
/// Block A  produces V1, Block B produces V2, and Block C has a Phi
/// node of C = Phi(V1,V2).
/// `firm` will be C, and `from` will contain two entries `(A,V1),(B,V2)`.
///
/// Above invariants are checked by `must_consistency_check`.
pub(super) struct ValueReq {
    pub(super) firm: Node,
    pub(super) from: Vec<(Ptr<BasicBlock>, Node)>,
}

impl ValueReq {
    fn must_consistency_check(&self) {
        assert!(!self.from.is_empty());
        if self.from.len() == 1 {
            assert_eq!(self.firm, self.from[0].1);
        } else {
            assert!(Node::is_phi(self.firm));
            let mut occurrences: HashMap<Node, HashSet<libfirm_rs::nodes::Block>> = HashMap::new();
            for (block, phi_in) in self.firm.must_phi().preds() {
                let source_blocks = occurrences.entry(phi_in).or_insert_with(HashSet::new);
                if !source_blocks.insert(block) {
                    panic!("phi with duplicate pred {:?}", block)
                }
            }
            for (orig_block, val) in &self.from {
                assert!(
                    occurrences.contains_key(val),
                    "from contains an entry {:?} from block {:?}\
                     that is not a predecessor of {:?}",
                    val,
                    orig_block.num,
                    self.firm
                );
                let source_blocks = occurrences.get_mut(val).unwrap();
                if !source_blocks.remove(&orig_block.firm) {
                    panic!(
                        "self.from contains a source block that\
                         is not a source of the phi node: {:?}",
                        orig_block.num
                    );
                }
            }
            for (_, origs) in occurrences {
                assert!(
                    origs.is_empty(),
                    "ValueReq and Phi node {:?} are not in sync: {:?}",
                    self.firm,
                    origs
                );
            }
        }
    }
}

/// This is a vertex in the basic-block graph
pub struct BasicBlock {
    /// Unique number for the BasicBlock
    pub num: i64,

    pub(super) value_requirements: Vec<ValueReq>,
    pub(super) values_to_compute: Vec<Node>,

    /// The instructions (using arbitrarily many registers) of the block
    pub code: Code<CopyPropagation, CopyPropagation, Instruction, Leave>,
    /// Control flow-transfers *to* this block.
    /// Usually at most 2
    pub preds: Vec<Ptr<BasicBlock>>,
    /// Control flow-transfers *out of* this block
    /// Usually at most 2
    pub succs: Vec<Ptr<BasicBlock>>,

    /// The firm structure of this block
    pub firm: libfirm_rs::nodes::Block,

    pub graph: Ptr<BlockGraph>,
}

#[derive(Clone)]
pub enum Instruction {
    LoadParam {
        idx: u32,
        size: u32,
        dst: Var,
    },
    Binop {
        kind: BinopKind,
        src1: Operand,
        src2: Operand,
        dst: Var,
    },
    Div {
        src1: Operand,
        src2: Operand,
        /// The variable which ontains the division result after the operation.
        /// The remainder is discarded.
        dst: Var,
    },
    Mod {
        src1: Operand,
        src2: Operand,
        /// The variable which contains the remainder result after the
        /// operation. The division result is discarded.
        dst: Var,
    },
    Unop {
        kind: UnopKind,
        src: Operand,
        dst: Var,
    },
    /// Value conversion pseudo-instruction:
    /// FIRM knows value conversions, but LIR doesn't.
    /// Keeping this seperate from mov though, for easier recognition in
    /// backend.
    Conv {
        src: Operand,
        dst: Var,
    },
    /// If dst is None, result is in register r0, which cannot be accessed
    /// using molki register names.
    Call {
        func: String,
        args: Vec<Operand>,
        dst: Option<Var>,
    },
    StoreMem(StoreMem),
    LoadMem(LoadMem),
    Comment(String),
}

#[derive(Clone)]
pub struct LoadMem {
    pub src: AddressComputation<Operand>,
    pub dst: Var,
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
            LoadParam { idx, dst, .. } => write!(fmt, "load_param {} => {:?}", idx, dst),
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
    pub(super) dst: Var,
}

#[derive(Debug, Copy, Clone)]
pub enum CopyPropagationSrc {
    Var(Var),
    Imm(Tarval),
}

impl fmt::Debug for CopyPropagation {
    fn fmt(&self, fmt: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(fmt, "copyprop {:?} => {:?}", self.src, self.dst)
    }
}

#[derive(Clone, Copy)]
pub enum Operand {
    Var(Var),

    /// NOTE: Tarval contains a raw pointer, thus Imm(t) is only valid for the
    /// lifetime of that pointer (the FIRM graph).
    Imm(Tarval),
}

impl fmt::Debug for Operand {
    fn fmt(&self, fmt: &mut fmt::Formatter<'_>) -> fmt::Result {
        use self::Operand::*;
        match self {
            Imm(tv) => write!(fmt, "Imm{{{:?}}}", tv),
            Var(ms) => write!(fmt, "Var#{:?}", ms),
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
            CopyPropagationSrc::Var(s) => Operand::Var(s),
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
        let graph = BlockGraph::build_skeleton(firm_graph, alloc);
        graph.ssa_with_requirements();
        graph.fill_instrs();
        graph.ssa_deconstruction();
        graph.cleanup();
        graph
    }
}

impl BlockGraph {
    /// Iterate over all basic blocks in `self` in a breadth-first manner (in
    /// control flow direction, starting at the start block)
    pub fn iter_blocks<'g>(&'g self) -> impl Iterator<Item = Ptr<BasicBlock>> + 'g {
        let mut visit_list = VecDeque::new();
        visit_list.push_front(self.start_block);

        BasicBlockIter {
            _graph: PhantomData,
            visited: HashSet::new(),
            visit_list,
        }
    }

    /// Gives a list of all `BasicBlock`s in the graph in postorder.
    pub fn postorder_blocks(&self) -> Vec<Ptr<BasicBlock>> {
        let mut postorder_visitor = BasicBlockPostorderVisitor::default();
        postorder_visitor.visit(self.start_block);
        postorder_visitor.res
    }

    pub fn get_block(&self, firm_block: libfirm_rs::nodes::Block) -> Ptr<BasicBlock> {
        *self
            .blocks
            .get(&firm_block)
            .expect("BlockGraph is incomplete")
    }
}

/// BlockGraph construction
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

                    source.succs.push(target);
                    target.preds.push(source);
                }
            }

            VisitTime::AfterPredecessors => (),
        });

        let start_block = *blocks
            .get(&firm_graph.start_block())
            .expect("All blocks (including start block) should have been generated");

        let end_block = *blocks
            .get(&firm_graph.end_block())
            .expect("All blocks (including end block) should have been generated");

        let graph = alloc.graph(BlockGraph {
            var_counter: 0,
            vars: HashMap::new(),
            firm: firm_graph,
            blocks,
            start_block,
            end_block,
        });

        // patch up the weak-ref in each block's graph member
        for mut block in graph.iter_blocks() {
            block.graph = graph;
        }

        graph
    }
}

impl Ptr<BlockGraph> {
    fn ssa_with_requirements(self) {
        self.firm.walk_blocks(|visit, firm_block| match visit {
            VisitTime::BeforePredecessors => {
                let mut block = self.get_block(*firm_block);
                use itertools::Itertools;
                let reqs = firm_block
                    .all_nodes_in_block()
                    // all nodes outside of this block that this block requires
                    .flat_map(|node_in_block| {
                        node_in_block
                            .in_nodes()
                            .filter(|value| !Node::is_block(*value))
                            .filter(|value| Node::is_phi(*value) || value.block() != *firm_block)
                            // Nodes that are constant (tarvals known at compile time)
                            // are converted to `SrcOperand::Imm` by gen_instr.rs
                            .filter(|value| self.convert_to_tarval(*value).is_none())
                    })
                    // above doesn't give us control flow, thus also depend on the X nodes
                    // via cfg_preds
                    .chain(firm_block.cfg_preds())
                    // now create ValueReq out of those blocks
                    .unique_by(|node| node.node_id())
                    .map(|node| {
                        let from = if let Some(phi) = node.opt_phi() {
                            phi.preds()
                                .map(|(pred_block, pred)| (self.get_block(pred_block), pred))
                                .collect()
                        } else {
                            vec![(self.get_block(node.block()), node)]
                        };
                        ValueReq { firm: node, from }
                    })
                    .collect::<Vec<_>>();

                for req in &reqs {
                    let mut req_origin = self.get_block(req.firm.block());
                    req_origin.values_to_compute.push(req.firm);
                }

                block.value_requirements = reqs;
            }
            VisitTime::AfterPredecessors => (),
        });
    }

    /// SSA deconstruction by copy propagation
    fn ssa_deconstruction(self) {
        for block in self.iter_blocks() {
            debug_assert!({
                block
                    .value_requirements
                    .iter()
                    .for_each(|req| req.must_consistency_check());
                true
            });

            for ValueReq { firm, from } in &block.value_requirements {
                assert!(!from.is_empty());

                if !firm.mode().is_data() {
                    // Mem nodes are in value_requirements, but not relevant at this point
                    continue;
                }

                for (mut orig_block, in_value) in from.iter() {
                    if let Some(in_var) = self.vars.get(in_value) {
                        orig_block.code.copy_out.push(CopyPropagation {
                            src: CopyPropagationSrc::Var(*in_var),
                            dst: self.var(*firm),
                        });
                    } else {
                        // must be one of the constants that satisfy can_be_var
                        let tv = self.must_convertible_to_tarval(*in_value);
                        orig_block.code.copy_out.push(CopyPropagation {
                            src: CopyPropagationSrc::Imm(tv),
                            dst: self.var(*firm),
                        })
                    }
                }
            }
        }
    }

    fn cleanup(self) {
        for mut block in self.iter_blocks() {
            // sanitize copy_outs
            // tarval doesn't implement hash, so we'll sort then dedup
            use self::lir_cleanup::HomogenizedOperand;
            let new_copy_out = block
                .code
                .copy_out
                .iter()
                .map(|CopyPropagation { src, dst }| (src.into(), HomogenizedOperand::Var(*dst)))
                .sorted()
                .dedup()
                .filter(|(src, dst)| src != dst)
                .map(
                    |(src, dst): (HomogenizedOperand, HomogenizedOperand)| CopyPropagation {
                        src: src.into(),
                        dst: dst.var().unwrap(),
                    },
                )
                .collect();
            block.code.copy_out = new_copy_out;
        }
    }

    pub(super) fn can_be_var(self, firm: Node) -> bool {
        firm.mode().is_data() && !Node::is_const(firm)
    }

    // TODO move this to libfirm_rs
    pub(super) fn must_convertible_to_tarval(self, firm: Node) -> Tarval {
        if let Some(val) = self.convert_to_tarval(firm) {
            return val;
        }
        panic!(
            "{:?} is not convertible to tarval, but caller context expects it to be",
            firm
        );
    }

    pub(super) fn convert_to_tarval(self, firm: Node) -> Option<Tarval> {
        match firm {
            Node::Const(c) => Some(c.tarval()),
            _ => None,
        }
    }

    /// Lazyly generate a variable for a given FIRM node.
    pub(super) fn var(mut self, firm: Node) -> Var {
        let self_copy = self; // FIXME
        *self
            .vars
            .entry(firm)
            .or_insert_with(|| self_copy.always_new_var(firm))
    }

    pub(super) fn always_new_var(mut self, firm: Node) -> Var {
        assert!(
            self.can_be_var(firm),
            "firm node cannot be a var: {:?} {:?}",
            firm.mode(),
            firm
        );
        let num = self.var_counter;
        self.var_counter += 1;
        Var { firm, num }
    }
}

impl BlockGraph {
    pub(super) fn existing_var(&self, firm: Node) -> Var {
        if let Some(var) = self.vars.get(&firm) {
            return *var;
        }
        panic!("caller expects existing variable for FIRM node {:?}", firm)
    }
}

impl Ptr<BlockGraph> {
    fn fill_instrs(mut self) {
        // make all arguments live in the start block
        let argument_tuple_elements = self
            .firm
            .start()
            .out_proj_t_args()
            .into_iter()
            .flat_map(|n| n.out_nodes())
            .map(|tuple_element| match tuple_element {
                Node::Proj(proj, libfirm_rs::nodes::ProjKind::Start_TArgs_Arg(idx, ..)) => {
                    (proj, idx)
                }
                _ => unreachable!(
                    "the proj node Start_TArgs has only \
                     Start_TArgs_Arg projs as out nodes"
                ),
            })
            .collect::<Vec<_>>();
        for (tuple_element, idx) in argument_tuple_elements {
            let dst = self.var(Node::from(tuple_element));
            self.start_block.code.body.push(Instruction::LoadParam {
                idx,
                size: tuple_element.mode().size_bytes(),
                dst,
            });
        }

        for block in self.iter_blocks() {
            log::debug!("FILL_INSTRS for block {:?}", block.firm);
            GenInstrBlock::fill_instrs(self, block)
        }
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
            for succ in block.succs.iter().cloned() {
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
        block.succs.iter().cloned().for_each(|block| {
            if self.visited.insert(block.firm) {
                self.visit(block);
            }
        });
        self.res.push(block);
    }
}

impl BasicBlock {
    fn skeleton_block(
        known_blocks: &mut HashMap<libfirm_rs::nodes::Block, Ptr<BasicBlock>>,
        firm: libfirm_rs::nodes::Block,
        alloc: &Allocator,
    ) -> Ptr<Self> {
        *known_blocks.entry(firm).or_insert_with(|| {
            alloc.block(BasicBlock {
                value_requirements: vec![],
                values_to_compute: vec![],
                num: firm.node_id(),
                code: Code::default(),
                preds: Vec::new(),
                succs: Vec::new(),
                firm,
                graph: Ptr::null(), // will be patched up by caller
            })
        })
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

/// Supporting structures for LIR cleanup phase
mod lir_cleanup {

    use super::{CopyPropagationSrc, Tarval, Var};

    /// Uniform representation of CopyPropagation src and dst operand.
    #[derive(PartialEq, Eq)]
    pub enum HomogenizedOperand {
        Var(Var),
        Tarval(Tarval),
    }

    impl HomogenizedOperand {
        pub fn var(self) -> Option<Var> {
            match self {
                HomogenizedOperand::Var(var) => Some(var),
                _ => None,
            }
        }
    }
    impl From<&CopyPropagationSrc> for HomogenizedOperand {
        fn from(src: &CopyPropagationSrc) -> HomogenizedOperand {
            match src {
                CopyPropagationSrc::Imm(tv) => HomogenizedOperand::Tarval(*tv),
                CopyPropagationSrc::Var(var) => HomogenizedOperand::Var(*var),
            }
        }
    }

    impl Into<CopyPropagationSrc> for HomogenizedOperand {
        fn into(self) -> CopyPropagationSrc {
            match self {
                HomogenizedOperand::Tarval(tv) => CopyPropagationSrc::Imm(tv),
                HomogenizedOperand::Var(var) => CopyPropagationSrc::Var(var),
            }
        }
    }

    impl PartialOrd for HomogenizedOperand {
        fn partial_cmp(&self, other: &HomogenizedOperand) -> Option<std::cmp::Ordering> {
            Some(self.cmp(other))
        }
    }

    impl Ord for HomogenizedOperand {
        fn cmp(&self, other: &Self) -> std::cmp::Ordering {
            use self::HomogenizedOperand::{Tarval, Var};
            match (self, other) {
                (Var(a), Var(b)) => a.num.cmp(&b.num),
                (Tarval(a), Tarval(b)) => {
                    assert!(a.is_long());
                    assert!(b.is_long());
                    a.get_long().cmp(&b.get_long())
                }
                (Var(_), Tarval(_)) => std::cmp::Ordering::Greater,
                (Tarval(_), Var(_)) => std::cmp::Ordering::Less,
            }
        }
    }

}
