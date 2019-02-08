use super::*;
use crate::{codegen::Instruction, lir};

pub struct CondJmpSwapTargets;

impl GlobalPeepholeOptimization for CondJmpSwapTargets {
    fn run(&mut self, asm: &mut Vec<Instruction>) {
        for window in UnsafeSlidingWindow::new(asm, 4) {
            use lir::JmpKind;
            use Instruction::*;

            if let [Cmp { .. }, Jmp {
                label: target1,
                kind: kind1,
            }, Jmp {
                label: target2,
                kind: kind2,
            }, Label { label: next_block }] = window
            {
                if next_block != target1 && next_block != target2 {
                    // no chance, also not very likely
                    continue;
                }
                if let Some(cond_op_1) = kind1.conditional() {
                    if kind2.conditional().is_some() {
                        // ???
                        continue;
                    } else if target1 == next_block {
                        // swap the two jmp targets, invert the jmp condition
                        let new1 = Jmp {
                            label: target2.clone(),
                            kind: JmpKind::Conditional(cond_op_1.invert()),
                        };
                        let new2 = Jmp {
                            label: target1.clone(),
                            kind: JmpKind::Unconditional,
                        };
                        window[1] = new1;
                        window[2] = new2;
                    }
                }
            }
        }
    }
}

pub struct Fallthrough;

impl GlobalPeepholeOptimization for Fallthrough {
    fn run(&mut self, asm: &mut Vec<Instruction>) {
        for window in UnsafeSlidingWindow::new(asm, 2) {
            use lir::JmpKind;
            use Instruction::*;

            if let [Jmp {
                label,
                kind: JmpKind::Unconditional,
            }, Label { label: next_block }] = window
            {
                if next_block == label {
                    window[0] = PeepholedOut;
                }
            }
        }
    }
}
