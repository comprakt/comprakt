use crate::lowering::lir;

mod codegen;
mod function;
mod linear_scan;
mod live_variable_analysis;
mod register;

pub use self::function::Function;
use self::register::Amd64Reg;

pub(super) type VarId = (i64, usize);

fn var_id(op: lir::Operand) -> VarId {
    use super::lir::Operand::*;
    match op {
        Slot(slot) => (slot.allocated_in().num, slot.num()),
        Param { idx } => (-1, idx as usize),
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
