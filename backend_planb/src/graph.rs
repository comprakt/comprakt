use crate::{alloc::Ptr, Alloc};
use libfirm_rs;
use std::{collections::HashMap, fmt};

pub mod construction;

pub struct Program<'alloc> {
    alloc: &'alloc Alloc,
    functions: Vec<Function>,
}

pub struct Function {
    graph: Ptr<BlockGraph>,
    ld_name: String,
    nargs: usize,
    returns: bool,
    ar: ActivationRecord,
}

#[derive(Default)]
pub struct ActivationRecord {
    num_slots: usize,
}

/// A graph of basic blocks. Each block is a list of instructions and a set of
/// pseudo-registers called `ValueSlots`. This is a more localized
/// represantation of SSA, as the value slots (or variable names) are namespaced
/// per block (and can only be refered to by adjacent blocks) and the sources of
/// the values are annotated on each edge, instead of being phi-nodes pointing
/// to some far away firm-node.
pub struct BlockGraph {
    pub firm: libfirm_rs::Graph,
    pub blocks: HashMap<libfirm_rs::nodes::Block, Ptr<Block>>,
    pub start_block: Ptr<Block>,
    pub end_block: Ptr<Block>,
}

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
pub struct Block {
    /// The instructions (using arbitrarily many registers) of the block
    pub code: Code<Mov, Mov, Instr, Instr>,
    /// Control flow-transfers *to* this block.
    /// Usually at most 2
    pub preds: Vec<Ptr<Block>>,
    /// Control flow-transfers *out of* this block
    /// Usually at most 2
    pub succs: Vec<Ptr<Block>>,

    /// The firm structure of this block
    pub firm: libfirm_rs::nodes::Block,
}

pub enum Operand {
    /// unlowered-only
    Var(Var),

    Imm(isize),
    Reg(ArchReg),
    /// In the AR
    Ar(usize),
    /// Somewhere in memory that is not the stack
    Mem(Mem<ArchReg>),
}

impl Operand {
    fn size(&self) -> Option<usize> {
        match self {
            Operand::Var(v) => Some(v.size),
            Operand::Reg(r) => Some(r.size),
            Operand::Mem(m) => Some(m.size),
            Operand::Ar(_) => Some(8), // AR slot is always 8 bytes
            Operand::Imm(_) => None,
        }
    }
}

impl fmt::Display for Operand {
    fn fmt(&self, fmt: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Operand::Var(v) => v.fmt(fmt),
            Operand::Reg(r) => r.fmt(fmt),
            Operand::Mem(m) => m.fmt(fmt),
            Operand::Ar(m) => write!(fmt, "-{}(%rbp)", (m + 1) * 8),
            Operand::Imm(i) => i.fmt(fmt),
        }
    }
}
pub struct ArchReg {
    pub size: usize,
    pub name: ArchRegName,
}

impl fmt::Display for ArchReg {
    fn fmt(&self, fmt: &mut fmt::Formatter<'_>) -> fmt::Result {
        use self::ArchRegName::*;
        let (prefix, suffix) = match (self.name, self.size) {
            (name, 1) => match name {
                A | B | C | D => ("", "l"),
                Si | Di => ("", "l"),
                R8 | R9 | R10 | R11 | R12 | R13 | R14 | R15 => ("", ""),
            },
            (name, 4) => match name {
                A | B | C | D => ("e", "x"),
                Si | Di => ("e", ""),
                R8 | R9 | R10 | R11 | R12 | R13 | R14 | R15 => ("", "d"),
            },
            (name, 8) => match name {
                A | B | C | D => ("r", "x"),
                Si | Di => ("r", ""),
                R8 | R9 | R10 | R11 | R12 | R13 | R14 | R15 => ("", ""),
            },
            _ => unreachable!(),
        };
        write!(fmt, "{}{}{}", prefix, self.name, suffix)
    }
}

#[derive(Display, Debug, Hash, PartialEq, Eq, Copy, Clone)]
pub enum ArchRegName {
    #[display(fmt = "a")]
    A,
    #[display(fmt = "b")]
    B,
    #[display(fmt = "c")]
    C,
    #[display(fmt = "d")]
    D,

    #[display(fmt = "si")]
    Si,
    #[display(fmt = "di")]
    Di,

    #[display(fmt = "r8")]
    R8,
    #[display(fmt = "r9")]
    R9,
    #[display(fmt = "r10")]
    R10,
    #[display(fmt = "r11")]
    R11,
    #[display(fmt = "r12")]
    R12,
    #[display(fmt = "r13")]
    R13,
    #[display(fmt = "r14")]
    R14,
    #[display(fmt = "r15")]
    R15,
}

pub struct Mem<Op> {
    pub base: Op,
    pub index: Option<Op>,
    pub stride: usize,
    pub offset: i32,

    // the size of the memory location
    pub size: usize,
}

impl<Op: fmt::Display> fmt::Display for Mem<Op> {
    fn fmt(&self, fmt: &mut fmt::Formatter<'_>) -> fmt::Result {
        unimplemented!()
    }
}

pub struct Var {
    id: usize,
    size: usize,
}

impl fmt::Display for Var {
    fn fmt(&self, fmt: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(fmt, "Var_{}", self.id)
    }
}

#[rustfmt::skip]
pub enum Instr {
    Mov(Mov),
    Add { src: Operand, dst: Operand },
    Sub {
        subtrahend: Operand,
        acc: Operand,
    },
    Mul { src: Operand, dst: Operand },
    And { src: Operand, dst: Operand },
    Or { src: Operand, dst: Operand },
    Xor { src: Operand, dst: Operand },
    Div { divisor: Operand },
    Not { op: Operand },
    Neg { op: Operand },
    Push { src: Operand },
    Pop { dst: Operand },
    Cmp {
        subtrahend: Operand,
        minuend: Operand,
    },
    Jmp { kind: JmpKind, label: String },
    Call { label: String },
    Leave,
    Ret,
    Label { label: String },
    Cqto,
    Comment { comment: String },
}

pub struct Mov {
    src: Operand,
    dst: Operand,
}

#[derive(Debug, Clone, Copy)]
pub enum JmpKind {
    Unconditional,
    Conditional(CondOp),
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

impl std::fmt::Display for JmpKind {
    fn fmt(&self, fmt: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use self::JmpKind::*;
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

impl Instr {
    fn dst_operand_size(&self) -> Option<usize> {
        unimplemented!()
    }
    fn src_operand_size(&self) -> Option<usize> {
        unimplemented!()
    }
}

impl fmt::Display for Instr {
    #[rustfmt::skip]
    fn fmt(&self, fmt: &mut fmt::Formatter<'_>) -> fmt::Result {
        use self::Instr::*;
        macro_rules! w {
            ($($args:expr),+) => {
                write!(fmt, $($args),+)
            }
        }
        unimplemented!()

        // match (self.src_operand_size(), self.dst_operand_size(), self) {
        //     (x, y, Mov { src, dst }) => unimplemented!(),

        //     // normal binops
        //     (_, _, Add { src, dst }) => w!("add {}, {}", src, dst),
        //     (_, _, Sub { subtrahend, acc }) => w!("sub {}, {}", subtrahend, acc),
        //     (_, _, Mul { src, dst }) => w!("mul {}, {}", src, dst),
        //     (_, _, And { src, dst }) => w!("and {}, {}", src, dst),
        //     (_, _, Or { src, dst }) => w!("or {}, {}", src, dst),
        //     (_, _, Xor { src, dst }) => w!("xor {}, {}", src, dst),

        //     // unops
        //     (_, _, Div { divisor }) => w!("idiv {}", divisor),
        //     (_, _, Not { op }) => w!("not {}", op),
        //     (_, _, Neg { op }) => w!("neg {}", op),
        //     (_, _, Push { src }) => w!("push {}", src),
        //     (_, _, Pop { dst }) => w!("pop {}", dst),

        //     (_,_,Cmp {subtrahend,minuend}) => w!("cmp {}, {}", subtrahend, minuend),

        //     (_, _, Jmp { kind, label }) => w!("{} {}", kind, label),

        //     (_, _, Call { label }) => w!("call {}", label),
        //     (_, _, Leave) => w!("leave"),
        //     (_, _, Ret) => w!("ret"),
        //     (_, _, Label { label }) => w!("{}:", label),
        //     (_, _, Cqto) => w!("cqto"),
        //     (_, _, Comment { comment }) => w!("/* {} */", comment),
        // }
    }
}
