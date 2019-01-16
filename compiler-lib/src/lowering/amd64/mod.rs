use crate::lowering::{lir, lir_allocator::Ptr};

mod codegen;
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

pub(super) type VarId = (i64, usize);

fn var_id(op: lir::Operand) -> VarId {
    use super::lir::Operand::*;
    match op {
        Slot(slot) => (slot.allocated_in().num, slot.num()),
        Param { idx } => (-1, idx as usize),
        Imm(_) => unreachable!(),
    }
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

pub struct Program {
    functions: Vec<(Function, Ptr<lir::BlockGraph>)>,
}

impl Program {
    pub fn new(lir: &lir::LIR, cconv: CallingConv) -> Self {
        let mut functions = vec![];

        for f in &lir.functions {
            functions.push((Function::new(f.nargs, cconv, &f.name), f.graph));
        }

        Self { functions }
    }

    pub fn emit_asm(&mut self, out: &mut impl std::io::Write) -> std::io::Result<()> {
        writeln!(out, "\t.text")?;
        for (f, graph) in self.functions.iter_mut() {
            f.emit_asm(*graph, out)?;
        }
        Ok(())
    }
}
