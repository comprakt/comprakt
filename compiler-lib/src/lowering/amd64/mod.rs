use crate::lowering::{lir, lir_allocator::Ptr};

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

#[derive(Copy, Clone, PartialEq)]
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
