#![allow(unused)] // FIXME: I want to see true warnings

use crate::lowering::lir;
use libfirm_rs::Tarval;

pub mod function_call;
mod register;

use self::register::Amd64Reg;

pub enum Instruction {
    Movq { src: MoveOperand, dst: MoveOperand },
    Push { src: Operand },
    Pop { dst: Operand },
    // For the Stack pointer
    Addq { src: Operand, dst: Operand },
    Subq { src: Operand, dst: Operand },
    Ret,
}

pub enum Operand {
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

pub enum MoveOperand {
    Operand(Operand),
    Addr(lir::AddressComputation<Operand>),
}

#[derive(Copy, Clone)]
pub enum CallingConv {
    X86_64,
    Stack,
}
