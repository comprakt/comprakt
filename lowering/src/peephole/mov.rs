use super::*;
use crate::codegen::Instruction;

pub struct MovSame;

impl GlobalPeepholeOptimization for MovSame {
    fn run(&mut self, asm: &mut Vec<Instruction>) {
        for window in UnsafeSlidingWindow::new(asm, 1) {
            use crate::codegen::{DstOperand, MovInstruction, SrcOperand};
            use Instruction::*;

            if let [Mov(MovInstruction { src, dst, .. })] = window {
                let remove_mov = match (src, dst) {
                    (SrcOperand::Reg(reg1), DstOperand::Reg(reg2)) => reg1 == reg2,
                    _ => false,
                };

                if remove_mov {
                    window[0] = PeepholedOut;
                }
            }
        }
    }
}
