use crate::lowering::lir;

pub(crate) mod basic_block_scheduling;
pub(crate) mod codegen;
// pub(crate) mod codegen_new;
// pub(crate) use codegen_new as codegen;
pub(crate) mod function;
pub(crate) mod linear_scan;
pub(crate) mod live_variable_analysis;
mod register;

pub(crate) use self::register::Amd64Reg;
use std::convert::TryFrom;

pub(crate) type VarId = (usize);

/// FIXME refactor
fn var_id(op: lir::Operand) -> VarId {
    use super::lir::Operand::*;
    match op {
        Var(var) => (var.num()),
        Imm(_) => unreachable!(),
    }
}

#[derive(Debug, Copy, Clone, PartialEq)]
pub enum CallingConv {
    X86_64,
    Stack,
}

impl CallingConv {
    pub(self) fn num_arg_regs(self) -> usize {
        match self {
            CallingConv::Stack => 0,
            CallingConv::X86_64 => 6,
        }
    }
}

#[derive(Debug, Hash, PartialEq, Eq, Copy, Clone)]
pub(crate) enum Size {
    One,
    Four,
    Eight,
}

impl TryFrom<u32> for Size {
    type Error = String;

    fn try_from(size: u32) -> Result<Self, Self::Error> {
        match size {
            1 => Ok(Size::One),
            4 => Ok(Size::Four),
            8 => Ok(Size::Eight),
            x => Err(format!("only sizes 1,4 and 8 are supported, got {:?}", x)),
        }
    }
}
