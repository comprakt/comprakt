#![allow(unused)]
use super::{
    linear_scan,
    lir::{self, JmpKind},
    live_variable_analysis,
    register::Amd64Reg,
    VarId,
};
use libfirm_rs::Tarval;
use std::collections::HashMap;

pub(super) struct Codegen {
    blocks: Vec<live_variable_analysis::Block>,
    var_location: HashMap<VarId, linear_scan::Location>,
    fn_name: String,

    pub(super) instrs: Vec<Instruction>,
}

impl Codegen {
    pub(super) fn new(
        blocks: Vec<live_variable_analysis::Block>,
        var_location: HashMap<VarId, linear_scan::Location>,
        fn_name: &str,
    ) -> Self {
        Self {
            blocks,
            var_location,
            fn_name: fn_name.to_string(),
            instrs: vec![],
        }
    }

    pub(super) fn run(&mut self) {
        for block in &self.blocks {
            use self::Instruction::*;
            self.instrs.push(Label {
                label: self.gen_label(block.num),
            });

            for instr in &block.instrs {
                match instr {
                    // Match over every instruction and generate amd64 instructions
                    x => unimplemented!("{:?}", x),
                }
            }
        }
    }

    fn gen_label(&self, block_num: usize) -> String {
        format!(".L{}B{}", self.fn_name, block_num)
    }
}

#[derive(Display)]
pub(super) enum Instruction {
    #[display(fmt = "\tmovq {}, {}", src, dst)]
    Movq { src: SrcOperand, dst: DstOperand },
    #[display(fmt = "\taddq {}, {}", src, dst)]
    Addq { src: SrcOperand, dst: DstOperand },
    #[display(fmt = "\tsubq {}, {}", src, dst)]
    Subq { src: SrcOperand, dst: DstOperand },
    #[display(fmt = "\timulq {}, {}", src, dst)]
    Mulq { src: SrcOperand, dst: DstOperand },
    #[display(fmt = "\tandq {}, {}", src, dst)]
    Andq { src: SrcOperand, dst: DstOperand },
    #[display(fmt = "\torq {}, {}", src, dst)]
    Orq { src: SrcOperand, dst: DstOperand },
    #[display(fmt = "\txorq {}, {}", src, dst)]
    Xorq { src: SrcOperand, dst: DstOperand },
    #[display(fmt = "\tidivq {}", src)]
    Divq { src: SrcOperand },
    #[display(fmt = "\tidivq {}", src)]
    Modq { src: SrcOperand },
    #[display(fmt = "\tpushq {}", src)]
    Pushq { src: SrcOperand },
    #[display(fmt = "\tpopq {}", dst)]
    Popq { dst: DstOperand },
    #[display(fmt = "\tcmpq {}, {}", src1, src2)]
    Cmpq { src1: SrcOperand, src2: SrcOperand },
    #[display(fmt = "\t{} {}", kind, label)]
    Jmp { label: String, kind: lir::JmpKind },
    #[display(fmt = "\tcall {}", label)]
    Call { label: String },
    #[display(fmt = "\tret")]
    Ret,
    #[display(fmt = "{}", label)]
    Label { label: String },
}

pub(super) enum SrcOperand {
    Mem(lir::AddressComputation<AddrOperand>),
    Reg(Amd64Reg),
    Imm(Tarval),
}

pub(super) enum DstOperand {
    Mem(lir::AddressComputation<AddrOperand>),
    Reg(Amd64Reg),
}

pub(super) enum AddrOperand {
    Reg(Amd64Reg),
    Imm(Tarval),
}

impl std::fmt::Display for SrcOperand {
    fn fmt(&self, fmt: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            SrcOperand::Mem(addr) => write!(fmt, "{}", addr),
            SrcOperand::Reg(reg) => write!(fmt, "{}", reg),
            SrcOperand::Imm(c) => write!(fmt, "${}", c.get_long()),
        }
    }
}

impl std::fmt::Display for DstOperand {
    fn fmt(&self, fmt: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            DstOperand::Mem(addr) => write!(fmt, "{}", addr),
            DstOperand::Reg(reg) => write!(fmt, "{}", reg),
        }
    }
}

impl std::fmt::Display for AddrOperand {
    fn fmt(&self, fmt: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            AddrOperand::Reg(reg) => write!(fmt, "{}", reg),
            AddrOperand::Imm(c) => write!(fmt, "${}", c.get_long()),
        }
    }
}
