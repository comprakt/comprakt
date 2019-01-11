use crate::lowering::{lir, lir_allocator::Ptr};

mod function;
mod linear_scan;
mod live_variable_analysis;
mod register;

pub use self::function::Function;
use self::register::Amd64Reg;

#[allow(unused)]
pub(self) enum Instruction {
    Movq { src: MoveOperand, dst: MoveOperand },
    Pushq { src: Operand },
    Popq { dst: Operand },
    Addq { src: Operand, dst: Operand },
    Subq { src: Operand, dst: Operand },
    Cmpq { lhs: Operand, rhs: Operand },
    Jmp { target: String, cond: lir::JmpKind },
    Ret,
}

#[derive(Debug, Copy, Clone)]
pub(self) enum Operand {
    LirOperand(lir::Operand),
    Reg(Amd64Reg),
}

impl std::fmt::Display for Operand {
    fn fmt(&self, fmt: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Operand::LirOperand(op) => write!(fmt, "{}", op),
            Operand::Reg(reg) => write!(fmt, "{}", reg),
        }
    }
}

pub(self) enum MoveOperand {
    Operand(Operand),
    Addr(lir::AddressComputation<Operand>),
}

#[derive(Copy, Clone)]
pub enum CallingConv {
    X86_64,
    Stack,
}

impl CallingConv {
    /// This function gives the number of maximum available registers depending
    /// on the calling convention. For `CallingConv::Stack` it is always 14
    /// (since %rbp and %rsp are reserved). For `CallingConv::X86_64`
    /// the number of reserved argument registers is subtracted.
    pub fn max_regs_available(self, nargs: usize) -> usize {
        match self {
            CallingConv::Stack => 14, // We can't use %rbp and %rsp
            CallingConv::X86_64 => 14 - usize::min(nargs, 6),
        }
    }
}

pub struct Program {
    functions: Vec<(Function, Ptr<lir::BlockGraph>)>,
}

impl Program {
    pub fn new(lir: lir::LIR, cconv: CallingConv) -> Self {
        let mut functions = vec![];

        for f in lir.functions {
            functions.push((Function::new(f.nargs, cconv), f.graph));
        }

        Self { functions }
    }

    pub fn emit_asm(&self) {
        for (f, graph) in &self.functions {
            f.allocate_registers(*graph);
        }
    }
}
