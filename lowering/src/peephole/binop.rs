use super::*;
use crate::codegen::{Instruction, OperandTrait};
use std::convert::TryInto;

pub struct BinOpScratch;

impl GlobalPeepholeOptimization for BinOpScratch {
    fn run(&mut self, asm: &mut Vec<Instruction>) {
        for window in UnsafeSlidingWindow::new(asm, 3) {
            use crate::{
                codegen::{DstOperand, MovInstruction, SrcOperand},
                register::{Amd64Reg, Reg},
            };
            use Instruction::*;

            if let [Mov(MovInstruction {
                src,
                dst:
                    DstOperand::Reg(Reg {
                        reg: Amd64Reg::A, ..
                    }),
                ..
            }), op, Mov(MovInstruction {
                src:
                    SrcOperand::Reg(Reg {
                        reg: Amd64Reg::A, ..
                    }),
                dst,
                ..
            })] = window
            {
                if let Some((
                    op_src,
                    DstOperand::Reg(Reg {
                        reg: Amd64Reg::A, ..
                    }),
                )) = op.binop_operands()
                {
                    if let Ok(src) = op_src.try_into() {
                        if dst.eq_ignore_size(src) {
                            continue;
                        }
                    }
                    let instr1;
                    let instr2;
                    let instr3 = PeepholedOut;
                    match dst {
                        // mov _, Rax
                        // op _, Rax
                        // mov Rax, Reg
                        DstOperand::Reg(..) => {
                            instr1 = Mov(MovInstruction {
                                src: *src,
                                dst: *dst,
                                comment: "peepholed binop".to_string(),
                            });
                            instr2 = op.binop_with_dst(*dst).unwrap();
                        }
                        DstOperand::Ar(..) => match op_src {
                            // mov _, Rax (*)
                            // op Reg/Imm, Rax
                            // mov Rax, Ar
                            SrcOperand::Reg(..) | SrcOperand::Imm(..) => {
                                // We can't have Ar at the rhs of a mul instruction
                                if let Mul { .. } = op {
                                    continue;
                                }
                                match src {
                                    // mov Reg/Imm, Rax (*)
                                    SrcOperand::Reg(..) | SrcOperand::Imm(..) => {
                                        instr1 = Mov(MovInstruction {
                                            src: *src,
                                            dst: *dst,
                                            comment: "peepholed binop".to_string(),
                                        });
                                        instr2 = op.binop_with_dst(*dst).unwrap()
                                    }
                                    // mov Ar, Rax (*)
                                    SrcOperand::Ar(..) => continue,
                                }
                            }
                            // mov _, Rax
                            // op Ar, Rax
                            // mov Rax, Ar
                            SrcOperand::Ar(..) => continue,
                        },
                    }
                    window[0] = instr1;
                    window[1] = instr2;
                    window[2] = instr3;
                }
            }
        }
    }
}

pub struct BinOpIdentity;

impl GlobalPeepholeOptimization for BinOpIdentity {
    fn run(&mut self, asm: &mut Vec<Instruction>) {
        for window in UnsafeSlidingWindow::new(asm, 1) {
            use crate::codegen::{DstOperand, SrcOperand};
            use Instruction::*;

            if let [op] = window {
                match op {
                    Add {
                        src: SrcOperand::Imm(tv),
                        ..
                    }
                    | Sub {
                        subtrahend: SrcOperand::Imm(tv),
                        ..
                    } if tv.get_long() == 0 => window[0] = PeepholedOut,
                    Mul {
                        src: SrcOperand::Imm(tv),
                        ..
                    } if tv.get_long() == 1 => window[0] = PeepholedOut,
                    Or {
                        src: SrcOperand::Imm(tv),
                        ..
                    } if tv.get_long() == 0 => window[0] = PeepholedOut,
                    Cmp {
                        subtrahend: SrcOperand::Imm(tv),
                        minuend: DstOperand::Reg(reg),
                    } if tv.get_long() == 0 => {
                        window[0] = Test {
                            src1: SrcOperand::Reg(*reg),
                            src2: DstOperand::Reg(*reg),
                        };
                    }
                    _ => (),
                }
            }
        }
    }
}
