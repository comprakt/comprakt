use super::*;

macro_rules! save_regs {
    ([$($reg:ident),*], $save_instrs:expr, $restore_instrs:expr) => {{
        $(
            $save_instrs.push(Instruction::Push {
                src: Operand::Reg(Amd64Reg::$reg),
            });
            $restore_instrs.push(Instruction::Pop {
                dst: Operand::Reg(Amd64Reg::$reg),
            });
        )*
    }};
}

type Label = String;

#[derive(Default)]
pub struct FunctionCall {
    arg_save: Vec<Instruction>,
    setup: Vec<Instruction>,
    label: Label,
    move_res: Option<Instruction>,
    recover: Option<Instruction>,
    arg_recover: Vec<Instruction>,
}

impl FunctionCall {
    pub fn new(cconv: CallingConv, call_instr: lir::Instruction, caller: lir::Function) -> Self {
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
                self.arg_save.push(Instruction::Push {
                    src: Operand::Reg(Amd64Reg::arg(i)),
                });
                // pop the arguments from the stack after the call
                self.arg_recover.push(Instruction::Pop {
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
                    push_setup.push(Instruction::Push {
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
                self.setup.push(Instruction::Push {
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
    nargs: usize,
    /// Calling convention
    cconv: CallingConv,
    /// Setup of the function. Get's filled initially
    prolog: Vec<Instruction>,
    /// Save callee-save registers. A call to `self.save_callee_save_regs` is
    /// needed after the register allocation
    save_regs: Vec<Instruction>,
    /// Allocates stack memory. An extra function needs to be called
    allocate: Option<Instruction>,
    /// The code inside the function. This will not be touched
    code: Vec<lir::Instruction>,
    /// Restore callee-save registers. This will be setup together with
    /// `save_regs`
    restore_regs: Vec<Instruction>,
    /// Restore of previous stack pointer and return. Get's filled initially
    epilog: Vec<Instruction>,
}

impl Function {
    pub fn new(nargs: usize, cconv: CallingConv, code: &[lir::Instruction]) -> Self {
        let mut function = Self {
            nargs,
            cconv,
            prolog: vec![],
            save_regs: vec![],
            allocate: None,
            code: code.to_vec(),
            restore_regs: vec![],
            epilog: vec![],
        };

        function.prolog.push(Instruction::Push {
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
        function.epilog.push(Instruction::Pop {
            dst: Operand::Reg(Amd64Reg::Rbp),
        });
        function.epilog.push(Instruction::Ret);

        function
    }

    pub fn allocate(&mut self, slots: usize) {
        self.allocate = Some(Instruction::Subq {
            src: Operand::LirOperand(lir::Operand::Imm(Tarval::mj_int(8 * slots as i64))),
            dst: Operand::Reg(Amd64Reg::Rsp),
        });
    }

    /// Depending on the calling convention some args are in registers. This
    /// function returns the Movq instruction from either a register or an
    /// address into the dst.
    pub fn arg(&self, idx: usize, dst: &lir::Operand) -> Instruction {
        self.arg_from_reg(idx, dst)
            .unwrap_or_else(|| self.arg_from_stack(idx, dst))
    }

    fn arg_from_reg(&self, idx: usize, dst: &lir::Operand) -> Option<Instruction> {
        if idx < 6 {
            match self.cconv {
                CallingConv::Stack => None,
                CallingConv::X86_64 => Some(Instruction::Movq {
                    src: MoveOperand::Operand(Operand::Reg(Amd64Reg::arg(idx))),
                    dst: MoveOperand::Operand(Operand::LirOperand(dst.clone())),
                }),
            }
        } else {
            None
        }
    }

    fn arg_from_stack(&self, idx: usize, dst: &lir::Operand) -> Instruction {
        let offset = match self.cconv {
            CallingConv::Stack => (idx + 1) * 8 + 8,
            CallingConv::X86_64 => {
                debug_assert!(idx >= 6);
                (idx + 1 - 6) * 8 + 8
            }
        } as isize;
        Instruction::Movq {
            src: MoveOperand::Addr(lir::AddressComputation {
                offset,
                base: Operand::Reg(Amd64Reg::Rbp),
                index: lir::IndexComputation::Zero,
            }),
            dst: MoveOperand::Operand(Operand::LirOperand(dst.clone())),
        }
    }

    /// This function gives the number of maximum available registers depending
    /// on the calling convention. For `CallingConv::Stack` it is always 13
    /// (since %rbp, %rsp and %rax) are reserved. For `CallingConv::X86_64`
    /// the number of reserved argument registers is subtracted.
    pub fn max_regs_available(&self) -> usize {
        match self.cconv {
            CallingConv::Stack => 13, // We can't use %rbp, %rsp and %rax
            CallingConv::X86_64 => 13 - usize::min(self.nargs, 6),
        }
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
        let regs_to_save = num_regs_required - (self.max_regs_available() - 5);
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
}
