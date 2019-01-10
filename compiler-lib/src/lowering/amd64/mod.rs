#![allow(unused)] // FIXME: I want to see true warnings

use crate::lowering::lir;
use libfirm_rs::Tarval;

mod function;
mod linear_scan;
mod register;

pub use self::function::Function;
use self::register::Amd64Reg;

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
