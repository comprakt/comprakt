use crate::lowering::lir;

mod codegen;
mod function;
mod linear_scan;
mod live_variable_analysis;
mod register;

pub use self::function::Function;
use self::register::{Amd64Reg, Reg};
use std::convert::TryFrom;

pub(super) type VarId = (usize);

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

pub struct Program {
    functions: Vec<Function>,
}

impl Program {
    pub fn new(lir: &lir::LIR, cconv: CallingConv) -> Self {
        let mut functions = vec![];

        for f in &lir.functions {
            let mut amd64_func = Function::new(f.nargs, cconv, &f.name);
            amd64_func.gen_code(f.graph);
            functions.push(amd64_func);
        }

        Self { functions }
    }

    pub fn emit_asm(&mut self, out: &mut impl std::io::Write) -> std::io::Result<()> {
        writeln!(out, "\t.text")?;
        for f in self.functions.iter() {
            f.emit_asm(out)?;
        }
        Ok(())
    }
}

#[derive(Debug, Hash, PartialEq, Eq, Copy, Clone)]
pub(self) enum Size {
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
