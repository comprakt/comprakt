use super::{
    linear_scan, register::RegisterAllocator, Amd64Reg, CallingConv, Instruction, MoveOperand,
    Operand,
};
use crate::lowering::{lir, lir_allocator::Ptr};
use libfirm_rs::Tarval;

macro_rules! save_regs {
    ([$($reg:ident),*], $save_instrs:expr, $restore_instrs:expr) => {{
        $(
            $save_instrs.push(Instruction::Pushq {
                src: Operand::Reg(Amd64Reg::$reg),
            });
            $restore_instrs.push(Instruction::Popq {
                dst: Operand::Reg(Amd64Reg::$reg),
            });
        )*
    }};
}

type Label = String;

#[derive(Default)]
pub(super) struct FunctionCall {
    arg_save: Vec<Instruction>,
    setup: Vec<Instruction>,
    label: Label,
    move_res: Option<Instruction>,
    recover: Option<Instruction>,
    arg_recover: Vec<Instruction>,
}

impl FunctionCall {
    pub(super) fn new(
        cconv: CallingConv,
        call_instr: lir::Instruction,
        caller: lir::Function,
    ) -> Self {
        let mut call = Self::default();

        match cconv {
            CallingConv::X86_64 => call.setup_x86_64_cconv(call_instr, caller),
            CallingConv::Stack => call.setup_stack_cconv(call_instr),
        }

        call
    }

    fn setup_x86_64_cconv(&mut self, call: lir::Instruction, caller: lir::Function) {
        if let lir::Instruction::Call { func, args, dst } = call {
            self.label = func;

            for i in 0..usize::min(caller.nargs, 6) {
                // push the args of the caller on the stack
                self.arg_save.push(Instruction::Pushq {
                    src: Operand::Reg(Amd64Reg::arg(i)),
                });
                // pop the arguments from the stack after the call
                self.arg_recover.push(Instruction::Popq {
                    dst: Operand::Reg(Amd64Reg::arg(i)),
                })
            }

            let mut push_setup = vec![];
            for (i, arg) in args.into_iter().enumerate() {
                if i < 6 {
                    // Fill the function argument registers
                    self.setup.push(Instruction::Movq {
                        src: MoveOperand::Operand(Operand::LirOperand(arg)),
                        dst: MoveOperand::Operand(Operand::Reg(Amd64Reg::arg(i))),
                    });
                } else {
                    // Push the other args on the stack
                    push_setup.push(Instruction::Pushq {
                        src: Operand::LirOperand(arg),
                    });
                }
            }
            if !push_setup.is_empty() {
                // Remove the pushed args from stack after the call
                self.recover = Some(Instruction::Addq {
                    src: Operand::LirOperand(lir::Operand::Imm(Tarval::mj_int(
                        (push_setup.len() * 8) as i64,
                    ))),
                    dst: Operand::Reg(Amd64Reg::Rsp),
                });

                // Rev the pushed args order: .., 8, 7, 6
                self.setup
                    .append(&mut push_setup.into_iter().rev().collect());
            }

            self.move_res = dst.map(|dst| Instruction::Movq {
                src: MoveOperand::Operand(Operand::Reg(Amd64Reg::Rax)),
                dst: MoveOperand::Operand(Operand::LirOperand(lir::Operand::Slot(dst))),
            });
        } else {
            unreachable!("A FunctionCall can only be setup for a Call instruction")
        }
    }

    fn setup_stack_cconv(&mut self, call: lir::Instruction) {
        if let lir::Instruction::Call { func, args, dst } = call {
            self.label = func;

            for arg in args.into_iter().rev() {
                self.setup.push(Instruction::Pushq {
                    src: Operand::LirOperand(arg),
                });
            }

            // Remove the pushed args from stack after the call
            self.recover = Some(Instruction::Addq {
                src: Operand::LirOperand(lir::Operand::Imm(Tarval::mj_int(
                    (self.setup.len() * 8) as i64,
                ))),
                dst: Operand::Reg(Amd64Reg::Rsp),
            });

            self.move_res = dst.map(|dst| Instruction::Movq {
                src: MoveOperand::Operand(Operand::Reg(Amd64Reg::Rax)),
                dst: MoveOperand::Operand(Operand::LirOperand(lir::Operand::Slot(dst))),
            });
        } else {
            unreachable!("A FunctionCall can only be setup for a Call instruction")
        }
    }
}

pub struct Function {
    /// Number of arguments
    pub(super) nargs: usize,
    /// Calling convention
    pub(super) cconv: CallingConv,
    /// Setup of the function. Get's filled initially
    prolog: Vec<Instruction>,
    /// Save callee-save registers. A call to `self.save_callee_save_regs` is
    /// needed after the register allocation
    save_regs: Vec<Instruction>,
    /// Allocates stack memory. An extra function needs to be called
    allocate: Option<Instruction>,
    /// Restore callee-save registers. This will be setup together with
    /// `save_regs`
    restore_regs: Vec<Instruction>,
    /// Restore of previous stack pointer and return. Get's filled initially
    epilog: Vec<Instruction>,
}

impl Function {
    pub fn new(nargs: usize, cconv: CallingConv) -> Self {
        let mut function = Self {
            nargs,
            cconv,
            prolog: vec![],
            save_regs: vec![],
            allocate: None,
            restore_regs: vec![],
            epilog: vec![],
        };

        function.prolog.push(Instruction::Pushq {
            src: Operand::Reg(Amd64Reg::Rbp),
        });
        function.prolog.push(Instruction::Movq {
            src: MoveOperand::Operand(Operand::Reg(Amd64Reg::Rsp)),
            dst: MoveOperand::Operand(Operand::Reg(Amd64Reg::Rbp)),
        });

        function.epilog.push(Instruction::Movq {
            src: MoveOperand::Operand(Operand::Reg(Amd64Reg::Rbp)),
            dst: MoveOperand::Operand(Operand::Reg(Amd64Reg::Rsp)),
        });
        function.epilog.push(Instruction::Popq {
            dst: Operand::Reg(Amd64Reg::Rbp),
        });
        function.epilog.push(Instruction::Ret);

        function
    }

    pub(super) fn allocate_stack(&mut self, slots: usize) {
        self.allocate = Some(Instruction::Subq {
            src: Operand::LirOperand(lir::Operand::Imm(Tarval::mj_int(8 * slots as i64))),
            dst: Operand::Reg(Amd64Reg::Rsp),
        });
    }

    /// This function should be called by the register allocator, after
    /// determining how many registers will be required for a function. If
    /// callee_save registers are needed to satisfy the register pressure,
    /// it will push these registers on the stack, before the function code
    /// is executed and restores them, after the function finished.
    ///
    /// Make sure to use the `Amd64Reg::reg(n, ..)` function in combination with
    /// this function to always get the registers in the right order.
    ///
    /// # Panics
    ///
    /// This function panics, if the number of required registers is higher,
    /// than the total available registers.
    pub fn save_callee_save_regs(&mut self, num_regs_required: usize) {
        // There are 5 callee save registers: %rbx, %r12-r15
        // %rbp is also callee save, but we never allocate this register
        let regs_to_save = num_regs_required - (self.cconv.max_regs_available(self.nargs) - 5);
        match regs_to_save {
            0 => (),
            1 => save_regs!([Rbx], self.save_regs, self.restore_regs),
            2 => save_regs!([Rbx, R12], self.save_regs, self.restore_regs),
            3 => save_regs!([Rbx, R12, R13], self.save_regs, self.restore_regs),
            4 => save_regs!([Rbx, R12, R13, R14], self.save_regs, self.restore_regs),
            5 => save_regs!([Rbx, R12, R13, R14, R15], self.save_regs, self.restore_regs),
            _ => unreachable!("More registers required than available"),
        }
    }

    pub fn allocate_registers(&self, graph: lir::BlockGraph) {
        for block in graph.postorder_blocks().iter().rev() {
            gen_instrs(&block);
        }
    }
}

fn gen_instrs(block: &lir::BasicBlock) -> linear_scan::Block {
    let code = &block.code;
    let mut reg_block = linear_scan::Block::default();
    for lir::CopyPropagation { src, dst } in &code.copy_in {
        reg_block.instrs.push(Instruction::Movq {
            src: MoveOperand::Operand(Operand::LirOperand(lir::Operand::Slot(*src))),
            dst: MoveOperand::Operand(Operand::LirOperand(lir::Operand::Slot(
                dst.allocated_in.regs[dst.num],
            ))),
        })
    }
    for instr in &code.body {
        reg_block.instrs.append(&mut gen_instr(instr));
    }
    for lir::CopyPropagation { src, dst } in &code.copy_out {
        reg_block.instrs.push(Instruction::Movq {
            src: MoveOperand::Operand(Operand::LirOperand(lir::Operand::Slot(*src))),
            dst: MoveOperand::Operand(Operand::LirOperand(lir::Operand::Slot(
                dst.allocated_in.regs[dst.num],
            ))),
        })
    }
    for leave in &code.leave {
        reg_block.instrs.append(&mut gen_leave(&leave));
    }

    reg_block
}

fn gen_leave(leave: &lir::Leave) -> Vec<Instruction> {
    use super::lir::Leave::*;
    match leave {
        CondJmp {
            op,
            lhs,
            rhs,
            true_target,
            false_target,
        } => vec![
            // FIXME: (Imm, Imm) case (other errorprone combinations?)
            Instruction::Cmpq {
                lhs: Operand::LirOperand(*lhs),
                rhs: Operand::LirOperand(*rhs),
            },
            Instruction::Jmp {
                target: lir::gen_label(true_target),
                cond: lir::JmpKind::Conditional(*op),
            },
            Instruction::Jmp {
                target: lir::gen_label(false_target),
                cond: lir::JmpKind::Unconditional,
            },
        ],
        Jmp { target } => vec![Instruction::Jmp {
            target: lir::gen_label(target),
            cond: lir::JmpKind::Unconditional,
        }],
        Return { value, end_block } => {
            let mut ret = vec![];
            if let Some(value) = value {
                ret.push(Instruction::Movq {
                    src: MoveOperand::Operand(Operand::LirOperand(*value)),
                    dst: MoveOperand::Operand(Operand::Reg(Amd64Reg::Rax)),
                });
            }
            ret.push(Instruction::Jmp {
                target: lir::gen_label(end_block),
                cond: lir::JmpKind::Unconditional,
            });
            ret
        }
    }
}

/// This function generates a `amd64::Instruction` from a `lir::Instruction`.
/// This function needs to take care of following points:
///
/// - a `lir::Instruction::Binop/Div/Mod` is a 3-address instruction.
///   - For all instructions `op src1, src2 -> dst(Slot)`: `dst != src1, src2`,
///     which means that a
///   move to `dst` is sometimes required:
///     - (slot1, slot2)   -> move slot2
///     - (slot, param)    -> move slot (commutative?)
///     - (slot, imm)      -> no move
///     - (param, slot)    -> move slot
///     - (param1, param2) -> see below
///     - (param, imm)     -> move imm
///     - (imm, slot)      -> no move
///     - (imm, param)     -> move imm (commutative?)
///     - (imm1, imm2)     -> move imm2
/// - (param1, param2): Params should never be moved before a operation, because
///   the register allocator should handle parameters. The instruction selection
///   can't know which parameter should be moved from the stack, if a parameter
///   is already/not anymore in a register, ... Maybe a special case in the
///   `amd64::Instruction` enum operands is required for this.
/// - Call instruction need to be destructured. This can be easily done by the
///   FuncntionCall struct, it just needs to be "flattened" afterwards.
///
/// A remodelling of the `amd64::Instruction`+`Operand` enums is probably
/// required.
fn gen_instr(instr: &lir::Instruction) -> Vec<Instruction> {
    vec![]
}
