use super::{
    function,
    linear_scan::{self, Location},
    lir::{self, MultiSlot},
    live_variable_analysis,
    register::Amd64Reg,
    var_id, CallingConv, VarId,
};
use crate::lowering::lir_allocator::Ptr;
use libfirm_rs::{nodes::NodeTrait, Tarval};
use std::{
    collections::{HashMap, HashSet},
    convert::{TryFrom, TryInto},
};

pub(super) struct Codegen {
    var_location: HashMap<VarId, linear_scan::Location>,
    cconv: CallingConv,
    params_moved_from_stack: HashSet<u32>,
}

impl Codegen {
    pub(super) fn new(
        var_location: HashMap<VarId, linear_scan::Location>,
        cconv: CallingConv,
    ) -> Self {
        Self {
            var_location,
            cconv,
            params_moved_from_stack: HashSet::new(),
        }
    }

    pub(super) fn run(
        &mut self,
        function: &function::Function,
        blocks: Vec<live_variable_analysis::Block>,
    ) -> Vec<Instruction> {
        use self::Instruction::*;
        let mut instrs = vec![];

        if self.cconv == CallingConv::X86_64 {
            for i in 0..6 {
                if let Some(Location::Mem(idx)) = self.var_location.get(&(-1, i)) {
                    instrs.push(Comment {
                        comment: format!("spill argument register {}", i),
                    });
                    instrs.push(Movq {
                        src: SrcOperand::Reg(Amd64Reg::arg(i)),
                        dst: DstOperand::Mem(lir::AddressComputation {
                            offset: -(*idx as isize) * 8,
                            base: AddrOperand(Amd64Reg::Rbp),
                            index: lir::IndexComputation::Zero,
                        }),
                    });
                }
            }
        }

        instrs.push(Comment {
            comment: "function prolog".to_string(),
        });
        for instr in &function.prolog {
            match instr {
                function::FnInstruction::Pushq { src } => {
                    let src = self.fn_to_src_operand(*src, &mut instrs);
                    instrs.push(Pushq { src });
                }
                function::FnInstruction::Movq { src, dst } => {
                    let src = self.fn_to_src_operand(*src, &mut instrs);
                    let dst = self
                        .fn_to_src_operand(*dst, &mut instrs)
                        .try_into()
                        .unwrap();
                    instrs.push(Movq { src, dst });
                }
                _ => unreachable!(),
            }
        }

        instrs.push(Comment {
            comment: "function save regs".to_string(),
        });
        for instr in &function.save_regs {
            if let function::FnInstruction::Pushq { src } = instr {
                let src = self.fn_to_src_operand(*src, &mut instrs);
                instrs.push(Pushq { src });
            } else {
                unreachable!();
            }
        }

        if let Some(instr) = function.allocate {
            instrs.push(Comment {
                comment: "function allocate stack space".to_string(),
            });
            if let function::FnInstruction::Subq { src, dst } = instr {
                instrs.push(Subq {
                    src: SrcOperand::Imm(src),
                    dst: DstOperand::Reg(dst),
                });
            } else {
                unreachable!();
            }
        }

        let mut block_iter = blocks.iter().peekable();

        while let Some(block) = block_iter.next() {
            instrs.push(Label {
                label: self.gen_label(block.firm_num),
            });

            for instr in &block.instrs {
                match instr {
                    // Match over every instruction and generate amd64 instructions
                    live_variable_analysis::Instruction::Call(call) => {
                        instrs.push(Comment {
                            comment: "call instruction".to_string(),
                        });
                        self.gen_call(call, &mut instrs);
                    }
                    live_variable_analysis::Instruction::Lir(lir) => {
                        self.gen_lir(lir, &mut instrs);
                    }
                    live_variable_analysis::Instruction::Leave(leave) => {
                        instrs.push(Comment {
                            comment: "leave instruction".to_string(),
                        });
                        self.gen_leave(
                            leave,
                            block_iter.peek().map_or(-1, |block| block.firm_num),
                            &mut instrs,
                        );
                    }
                    live_variable_analysis::Instruction::Mov { src, dst } => {
                        instrs.push(Comment {
                            comment: "copy instruction".to_string(),
                        });
                        let src = self.lir_to_src_operand(lir::Operand::Slot(*src), &mut instrs);
                        let dst = self
                            .lir_to_src_operand(lir::Operand::Slot(*dst), &mut instrs)
                            .try_into()
                            .unwrap();
                        match (src, dst) {
                            (SrcOperand::Reg(_), _) | (_, DstOperand::Reg(_)) => {
                                instrs.push(Movq { src, dst });
                            }
                            (SrcOperand::Mem(_), DstOperand::Mem(_)) => {
                                instrs.push(Pushq {
                                    src: SrcOperand::Reg(Amd64Reg::Rax),
                                });
                                instrs.push(Movq {
                                    src,
                                    dst: DstOperand::Reg(Amd64Reg::Rax),
                                });
                                instrs.push(Movq {
                                    src: SrcOperand::Reg(Amd64Reg::Rax),
                                    dst,
                                });
                                instrs.push(Popq {
                                    dst: DstOperand::Reg(Amd64Reg::Rax),
                                });
                            }
                            (SrcOperand::Imm(_), _) => unreachable!(),
                        }
                    }
                }
            }
        }

        instrs.push(Comment {
            comment: "function restore regs".to_string(),
        });
        for instr in &function.restore_regs {
            if let function::FnInstruction::Popq { dst } = instr {
                let dst = self
                    .fn_to_src_operand(*dst, &mut instrs)
                    .try_into()
                    .unwrap();
                instrs.push(Popq { dst });
            }
        }

        instrs.push(Comment {
            comment: "function epilog".to_string(),
        });
        for instr in &function.epilog {
            match instr {
                function::FnInstruction::Leave => instrs.push(Leave),
                function::FnInstruction::Ret => instrs.push(Ret),
                _ => unreachable!(),
            }
        }

        instrs
    }

    fn gen_lir(&mut self, lir: &lir::Instruction, instrs: &mut Vec<Instruction>) {
        use self::Instruction::*;

        log::debug!("Gen lir: {:?}", lir);
        match lir {
            lir::Instruction::Binop {
                kind,
                src1,
                src2,
                dst,
            } => self.gen_binop(instrs, kind, src1, src2, *dst),
            lir::Instruction::Div { src1, src2, dst } => {
                let dst = self
                    .lir_to_src_operand(lir::Operand::Slot(*dst), instrs)
                    .try_into()
                    .unwrap();
                self.gen_div(instrs, src1, src2, || Movq {
                    src: SrcOperand::Reg(Amd64Reg::Rax),
                    dst,
                });
            }
            lir::Instruction::Mod { src1, src2, dst } => {
                let dst = self
                    .lir_to_src_operand(lir::Operand::Slot(*dst), instrs)
                    .try_into()
                    .unwrap();
                self.gen_div(instrs, src1, src2, || Movq {
                    src: SrcOperand::Reg(Amd64Reg::Rdx),
                    dst,
                });
            }
            lir::Instruction::Conv { src, dst } => {
                let src = self.lir_to_src_operand(*src, instrs);
                let dst = self
                    .lir_to_src_operand(lir::Operand::Slot(*dst), instrs)
                    .try_into()
                    .unwrap();
                instrs.push(Movq { src, dst })
            }
            lir::Instruction::Unop { kind, src, dst } => self.gen_unop(instrs, kind, src, *dst),
            lir::Instruction::StoreMem { src, dst } => {
                let src = self.lir_to_src_operand(*src, instrs);
                self.gen_load_store(instrs, dst, |addr| Movq {
                    src,
                    dst: DstOperand::Mem(addr),
                })
            }
            lir::Instruction::LoadMem { src, dst } => {
                let dst = self
                    .lir_to_src_operand(lir::Operand::Slot(*dst), instrs)
                    .try_into()
                    .unwrap();
                self.gen_load_store(instrs, src, |addr| Movq {
                    src: SrcOperand::Mem(addr),
                    dst,
                })
            }
            lir::Instruction::Call { .. } => unreachable!("Call already converted"),
            lir::Instruction::Comment(_) => (),
        }
    }

    fn gen_binop(
        &mut self,
        instrs: &mut Vec<Instruction>,
        kind: &lir::BinopKind,
        src1: &lir::Operand,
        src2: &lir::Operand,
        dst: Ptr<MultiSlot>,
    ) {
        use self::Instruction::*;
        macro_rules! push_binop {
            ($kind:expr, $src:expr, $dst:expr) => {{
                match $kind {
                    lir::BinopKind::Add => instrs.push(Addq {
                        src: $src,
                        dst: $dst,
                    }),
                    lir::BinopKind::Mul => instrs.push(Mulq {
                        src: $src,
                        dst: $dst,
                    }),
                    lir::BinopKind::And => instrs.push(Andq {
                        src: $src,
                        dst: $dst,
                    }),
                    lir::BinopKind::Or => instrs.push(Orq {
                        src: $src,
                        dst: $dst,
                    }),
                    lir::BinopKind::Xor => instrs.push(Xorq {
                        src: $src,
                        dst: $dst,
                    }),
                    lir::BinopKind::Sub => instrs.push(Subq {
                        src: $src,
                        dst: $dst,
                    }),
                }
            }};
            (SWAPPED, $kind:expr, $src:expr, $dst:expr) => {{
                push_binop!($kind, $src, $dst);
                // subq $src, $dst => $dst = $src - $dst
                // but if $src1 was moved into $dst and $src2 is used for $src1, then we want
                // $dst = $dst - $src = -($src - $dst) = -($dst'):
                //
                // subq $src, $dst
                // negq $dst
                if let lir::BinopKind::Sub = $kind {
                    instrs.push(Negq { src: $dst })
                }
            }};
        }

        let src1 = self.lir_to_src_operand(*src1, instrs);
        let src2 = self.lir_to_src_operand(*src2, instrs);
        let dst = self
            .lir_to_src_operand(lir::Operand::Slot(dst), instrs)
            .try_into()
            .unwrap();
        match dst {
            DstOperand::Reg(_) => {
                // When the dst is a Reg, we just have to move the second operand in this
                // Reg. It doesn't matter if the `src`s are Reg, Mem or Imm
                instrs.push(Movq { src: src2, dst });
                push_binop!(kind, src1, dst);
            }
            DstOperand::Mem(_) => match (src1, src2) {
                // Enforce that we always get `op reg/imm, mem`
                (SrcOperand::Reg(_), SrcOperand::Reg(_))
                | (SrcOperand::Reg(_), SrcOperand::Imm(_))
                | (SrcOperand::Imm(_), SrcOperand::Reg(_))
                | (SrcOperand::Imm(_), SrcOperand::Imm(_)) => {
                    instrs.push(Movq { src: src2, dst });
                    push_binop!(kind, src1, dst);
                }
                (SrcOperand::Mem(_), SrcOperand::Mem(_)) => {
                    // This case is bad. There aren't any (mem, mem) ops. This means we
                    // need to spill one register:
                    // `op mem, mem -> mem` => `op reg, mem`
                    // We want to move src2 -> dst, but first we need to move src2 -> %rax:
                    instrs.push(Pushq {
                        src: SrcOperand::Reg(Amd64Reg::Rax),
                    });
                    instrs.push(Movq {
                        src: src2,
                        dst: DstOperand::Reg(Amd64Reg::Rax),
                    });
                    // Now we can move %rax -> dst
                    instrs.push(Movq {
                        src: SrcOperand::Reg(Amd64Reg::Rax),
                        dst,
                    });
                    // We're now at a 2-address code state: `op mem, mem`
                    // Now we need to move src1 -> %rax
                    instrs.push(Movq {
                        src: src1,
                        dst: DstOperand::Reg(Amd64Reg::Rax),
                    });
                    // Now the instruction `op %rax, mem`:
                    push_binop!(kind, SrcOperand::Reg(Amd64Reg::Rax), dst);
                    // And last but not least: recover %rax
                    instrs.push(Popq {
                        dst: DstOperand::Reg(Amd64Reg::Rax),
                    });
                }
                // src2 is Reg or Imm
                (SrcOperand::Mem(_), _) => {
                    instrs.push(Pushq {
                        src: SrcOperand::Reg(Amd64Reg::Rax),
                    });
                    // src1 -> %rax
                    instrs.push(Movq {
                        src: src1,
                        dst: DstOperand::Reg(Amd64Reg::Rax),
                    });
                    // %rax -> dst
                    instrs.push(Movq {
                        src: SrcOperand::Reg(Amd64Reg::Rax),
                        dst,
                    });
                    // Now the instruction `op src2, mem(src1)`:
                    push_binop!(SWAPPED, kind, src2, dst);
                    // And last but not least: recover %rax
                    instrs.push(Popq {
                        dst: DstOperand::Reg(Amd64Reg::Rax),
                    });
                }
                (_, SrcOperand::Mem(_)) => {
                    instrs.push(Pushq {
                        src: SrcOperand::Reg(Amd64Reg::Rax),
                    });
                    // src1 -> %rax
                    instrs.push(Movq {
                        src: src2,
                        dst: DstOperand::Reg(Amd64Reg::Rax),
                    });
                    // %rax -> dst
                    instrs.push(Movq {
                        src: SrcOperand::Reg(Amd64Reg::Rax),
                        dst,
                    });
                    // Now the instruction `op src1, mem(src2)`:
                    push_binop!(kind, src1, dst);
                    // And last but not least: recover %rax
                    instrs.push(Popq {
                        dst: DstOperand::Reg(Amd64Reg::Rax),
                    });
                }
            },
        }
    }

    fn gen_div<F>(
        &mut self,
        instrs: &mut Vec<Instruction>,
        src1: &lir::Operand,
        src2: &lir::Operand,
        f: F,
    ) where
        F: FnOnce() -> Instruction,
    {
        use self::Instruction::*;
        instrs.push(Pushq {
            src: SrcOperand::Reg(Amd64Reg::Rdx),
        });
        instrs.push(Pushq {
            src: SrcOperand::Reg(Amd64Reg::Rax),
        });
        let src = self.lir_to_src_operand(*src1, instrs);
        instrs.push(Movq {
            src,
            dst: DstOperand::Reg(Amd64Reg::Rax),
        });
        instrs.push(Cqto);
        if let lir::Operand::Imm(_) = src2 {
            instrs.push(Pushq {
                src: SrcOperand::Reg(Amd64Reg::Rsi),
            });
            let src = self.lir_to_src_operand(*src2, instrs);
            instrs.push(Movq {
                src,
                dst: DstOperand::Reg(Amd64Reg::Rsi),
            });
            instrs.push(Divq {
                src: DstOperand::Reg(Amd64Reg::Rsi),
            });
            instrs.push(Popq {
                dst: DstOperand::Reg(Amd64Reg::Rsi),
            });
        } else {
            let src = self.lir_to_src_operand(*src2, instrs).try_into().unwrap();
            instrs.push(Divq { src });
        }
        instrs.push(f());
        instrs.push(Popq {
            dst: DstOperand::Reg(Amd64Reg::Rax),
        });
        instrs.push(Popq {
            dst: DstOperand::Reg(Amd64Reg::Rdx),
        });
    }

    fn gen_unop(
        &mut self,
        instrs: &mut Vec<Instruction>,
        kind: &lir::UnopKind,
        src: &lir::Operand,
        dst: Ptr<MultiSlot>,
    ) {
        use self::Instruction::*;
        macro_rules! push_unop {
            ($kind:expr, $dst:expr) => {{
                match $kind {
                    lir::UnopKind::Not => instrs.push(Notq { src: $dst }),
                    lir::UnopKind::Neg => instrs.push(Negq { src: $dst }),
                }
            }};
        }

        let src = self.lir_to_src_operand(*src, instrs);
        let dst = self
            .lir_to_src_operand(lir::Operand::Slot(dst), instrs)
            .try_into()
            .unwrap();
        match dst {
            DstOperand::Reg(_) => {
                instrs.push(Movq { src, dst });
                push_unop!(kind, dst);
            }
            DstOperand::Mem(_) => match src {
                SrcOperand::Reg(_) | SrcOperand::Imm(_) => {
                    instrs.push(Movq { src, dst });
                    push_unop!(kind, dst);
                }
                SrcOperand::Mem(_) => {
                    instrs.push(Pushq {
                        src: SrcOperand::Reg(Amd64Reg::Rax),
                    });
                    // src -> %rax
                    instrs.push(Movq {
                        src,
                        dst: DstOperand::Reg(Amd64Reg::Rax),
                    });
                    // Now the instruction `op %rax`:
                    push_unop!(kind, DstOperand::Reg(Amd64Reg::Rax));
                    // %rax -> dst
                    instrs.push(Movq {
                        src: SrcOperand::Reg(Amd64Reg::Rax),
                        dst,
                    });
                    // And last but not least: recover %rax
                    instrs.push(Popq {
                        dst: DstOperand::Reg(Amd64Reg::Rax),
                    });
                }
            },
        }
    }

    fn gen_load_store<F>(
        &mut self,
        instrs: &mut Vec<Instruction>,
        addr: &lir::AddressComputation<lir::Operand>,
        f: F,
    ) where
        F: FnOnce(lir::AddressComputation<AddrOperand>) -> Instruction,
    {
        use self::Instruction::*;
        let lir::AddressComputation {
            offset,
            base,
            index,
        } = addr;
        let base = self.lir_to_src_operand(*base, instrs);
        let (base, base_new_reg) = match base {
            SrcOperand::Reg(reg) => (AddrOperand(reg), false),
            _ => {
                instrs.push(Pushq {
                    src: SrcOperand::Reg(Amd64Reg::Rax),
                });
                instrs.push(Movq {
                    src: base,
                    dst: DstOperand::Reg(Amd64Reg::Rax),
                });
                (AddrOperand(Amd64Reg::Rax), true)
            }
        };
        let (index, index_new_reg) = match index {
            lir::IndexComputation::Zero => (lir::IndexComputation::Zero, false),
            lir::IndexComputation::Displacement(op, s) => {
                let op = self.lir_to_src_operand(*op, instrs);
                match op {
                    SrcOperand::Reg(reg) => (
                        lir::IndexComputation::Displacement(AddrOperand(reg), *s),
                        false,
                    ),
                    _ => {
                        instrs.push(Pushq {
                            src: SrcOperand::Reg(Amd64Reg::Rdx),
                        });
                        instrs.push(Movq {
                            src: op,
                            dst: DstOperand::Reg(Amd64Reg::Rdx),
                        });
                        (
                            lir::IndexComputation::Displacement(AddrOperand(Amd64Reg::Rdx), *s),
                            true,
                        )
                    }
                }
            }
        };

        instrs.push(f(lir::AddressComputation {
            offset: *offset,
            base,
            index,
        }));

        if base_new_reg {
            instrs.push(Popq {
                dst: DstOperand::Reg(Amd64Reg::Rax),
            });
        }
        if index_new_reg {
            instrs.push(Popq {
                dst: DstOperand::Reg(Amd64Reg::Rdx),
            });
        }
    }

    fn gen_call(&mut self, call: &function::FunctionCall, instrs: &mut Vec<Instruction>) {
        use self::Instruction::*;

        log::debug!("Gen call: {:?}", call);
        for instr in &call.arg_save {
            if let function::FnInstruction::Pushq { src } = instr {
                let src = self.fn_to_src_operand(*src, instrs);
                instrs.push(Pushq { src });
            } else {
                unreachable!();
            }
        }

        for instr in &call.setup {
            match instr {
                function::FnInstruction::Movq { src, dst } => {
                    let src = self.fn_to_src_operand(*src, instrs);
                    let dst = self.fn_to_src_operand(*dst, instrs).try_into().unwrap();
                    instrs.push(Movq { src, dst });
                }
                function::FnInstruction::Pushq { src } => {
                    let src = self.fn_to_src_operand(*src, instrs);
                    instrs.push(Pushq { src });
                }
                _ => unreachable!(),
            }
        }

        instrs.push(Call {
            label: call.label.clone(),
        });

        if let Some(function::FnInstruction::Movq { src, dst }) = call.move_res {
            let src = self.fn_to_src_operand(src, instrs);
            let dst = self.fn_to_src_operand(dst, instrs).try_into().unwrap();
            instrs.push(Movq { src, dst });
        }

        if let Some(function::FnInstruction::Addq { src, dst }) = call.recover {
            instrs.push(Addq {
                src: SrcOperand::Imm(src),
                dst: DstOperand::Reg(dst),
            });
        }

        for instr in &call.arg_recover {
            if let function::FnInstruction::Popq { dst } = instr {
                let dst = self.fn_to_src_operand(*dst, instrs).try_into().unwrap();
                instrs.push(Popq { dst });
            } else {
                unreachable!();
            }
        }
    }

    fn gen_leave(
        &mut self,
        leave: &lir::Leave,
        next_block_num: i64,
        instrs: &mut Vec<Instruction>,
    ) {
        use self::Instruction::*;

        macro_rules! push_jmp {
            ($kind:expr, $target:expr, $next_block_num:expr, fall=$fall:expr) => {{
                let target_num = $target.firm.node_id();
                if !$fall || target_num != $next_block_num {
                    instrs.push(Jmp {
                        label: self.gen_label(target_num),
                        kind: $kind,
                    });
                }
            }};
        }

        log::debug!("Gen leave: {:?}", leave);
        match leave {
            lir::Leave::Jmp { target } => push_jmp!(
                lir::JmpKind::Unconditional,
                target,
                next_block_num,
                fall = true
            ),
            lir::Leave::CondJmp {
                op,
                lhs,
                rhs,
                true_target,
                false_target,
            } => {
                let lhs = self.lir_to_src_operand(*lhs, instrs);
                let rhs = self.lir_to_src_operand(*rhs, instrs);
                let op = match (lhs, rhs) {
                    (SrcOperand::Reg(_), SrcOperand::Imm(_))
                    | (SrcOperand::Mem(_), SrcOperand::Imm(_)) => {
                        instrs.push(Cmpq { lhs: rhs, rhs: lhs });
                        op.swap()
                    }
                    (SrcOperand::Reg(_), SrcOperand::Mem(_))
                    | (_, SrcOperand::Reg(_))
                    | (SrcOperand::Imm(_), SrcOperand::Mem(_)) => {
                        instrs.push(Cmpq { lhs, rhs });
                        *op
                    }
                    (SrcOperand::Mem(_), SrcOperand::Mem(_))
                    | (SrcOperand::Imm(_), SrcOperand::Imm(_)) => {
                        // This case is bad we don't know if there is a free
                        // register left. So we have to spill one register.
                        instrs.push(Pushq {
                            src: SrcOperand::Reg(Amd64Reg::Rax),
                        });
                        instrs.push(Movq {
                            src: rhs,
                            dst: DstOperand::Reg(Amd64Reg::Rax),
                        });
                        instrs.push(Cmpq {
                            lhs,
                            rhs: SrcOperand::Reg(Amd64Reg::Rax),
                        });
                        instrs.push(Popq {
                            dst: DstOperand::Reg(Amd64Reg::Rax),
                        });
                        *op
                    }
                };
                push_jmp!(
                    lir::JmpKind::Conditional(op),
                    true_target,
                    next_block_num,
                    fall = false
                );
                push_jmp!(
                    lir::JmpKind::Unconditional,
                    false_target,
                    next_block_num,
                    fall = true
                );
            }
            lir::Leave::Return { value, end_block } => {
                if let Some(value) = value {
                    let src = self.lir_to_src_operand(*value, instrs);
                    instrs.push(Movq {
                        src,
                        dst: DstOperand::Reg(Amd64Reg::Rax),
                    })
                }
                push_jmp!(
                    lir::JmpKind::Unconditional,
                    end_block,
                    next_block_num,
                    fall = true
                );
            }
        }
    }

    fn gen_label(&self, block_num: i64) -> String {
        format!(".L{}", block_num)
    }

    fn lir_to_src_operand(
        &mut self,
        op: lir::Operand,
        instrs: &mut Vec<Instruction>,
    ) -> SrcOperand {
        match op {
            lir::Operand::Imm(c) => SrcOperand::Imm(c),
            lir::Operand::Slot(_) => match self.var_location[&var_id(op)] {
                Location::Reg(reg) => SrcOperand::Reg(reg),
                Location::Mem(idx) => SrcOperand::Mem(lir::AddressComputation {
                    offset: -(idx as isize) * 8,
                    base: AddrOperand(Amd64Reg::Rbp),
                    index: lir::IndexComputation::Zero,
                }),
                Location::ParamMem => unreachable!("a slot never has a ParamMem location"),
            },
            lir::Operand::Param { idx } => match self.var_location[&var_id(op)] {
                Location::Reg(reg) => {
                    if let Some(instr) = self.arg_from_stack(idx, DstOperand::Reg(reg)) {
                        if self.params_moved_from_stack.insert(idx) {
                            instrs.push(instr);
                        }
                    }
                    SrcOperand::Reg(reg)
                }
                // This can happen when the param was originally in a register but got moved on
                // the stack.
                Location::Mem(idx) => SrcOperand::Mem(lir::AddressComputation {
                    offset: -(idx as isize) * 8,
                    base: AddrOperand(Amd64Reg::Rbp),
                    index: lir::IndexComputation::Zero,
                }),
                Location::ParamMem => SrcOperand::Mem(lir::AddressComputation {
                    offset: (idx as isize) * 8,
                    base: AddrOperand(Amd64Reg::Rbp),
                    index: lir::IndexComputation::Zero,
                }),
            },
        }
    }

    fn fn_to_src_operand(
        &mut self,
        op: function::FnOperand,
        instrs: &mut Vec<Instruction>,
    ) -> SrcOperand {
        match op {
            function::FnOperand::Lir(lir) => self.lir_to_src_operand(lir, instrs),
            function::FnOperand::Reg(reg) => SrcOperand::Reg(reg),
        }
    }

    fn arg_from_stack(&self, idx: u32, dst: DstOperand) -> Option<Instruction> {
        let offset = match self.cconv {
            CallingConv::Stack => (idx + 1) * 8 + 8,
            CallingConv::X86_64 => {
                if idx < 6 {
                    return None;
                } else {
                    (idx + 1 - 6) * 8 + 8
                }
            }
        } as isize;
        Some(Instruction::Movq {
            src: SrcOperand::Mem(lir::AddressComputation {
                offset,
                base: AddrOperand(Amd64Reg::Rbp),
                index: lir::IndexComputation::Zero,
            }),
            dst,
        })
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
    Divq { src: DstOperand },
    #[display(fmt = "\tnotq {}", src)]
    Notq { src: DstOperand },
    #[display(fmt = "\tnegq {}", src)]
    Negq { src: DstOperand },
    #[display(fmt = "\tpushq {}", src)]
    Pushq { src: SrcOperand },
    #[display(fmt = "\tpopq {}", dst)]
    Popq { dst: DstOperand },
    #[display(fmt = "\tcmpq {}, {}", lhs, rhs)]
    Cmpq { lhs: SrcOperand, rhs: SrcOperand },
    #[display(fmt = "\t{} {}", kind, label)]
    Jmp { label: String, kind: lir::JmpKind },
    #[display(fmt = "\tcall {}", label)]
    Call { label: String },
    #[display(fmt = "\tleave")]
    Leave,
    #[display(fmt = "\tret")]
    Ret,
    #[display(fmt = "{}:", label)]
    Label { label: String },
    #[display(fmt = "cqto")]
    Cqto,
    #[display(fmt = "\t/* {} */", comment)]
    Comment { comment: String },
}

#[derive(Copy, Clone)]
pub(super) enum SrcOperand {
    Mem(lir::AddressComputation<AddrOperand>),
    Reg(Amd64Reg),
    Imm(Tarval),
}

#[derive(Copy, Clone)]
pub(super) enum DstOperand {
    Mem(lir::AddressComputation<AddrOperand>),
    Reg(Amd64Reg),
}

impl TryFrom<SrcOperand> for DstOperand {
    type Error = ();

    fn try_from(op: SrcOperand) -> Result<Self, ()> {
        match op {
            SrcOperand::Reg(reg) => Ok(DstOperand::Reg(reg)),
            SrcOperand::Mem(addr) => Ok(DstOperand::Mem(addr)),
            SrcOperand::Imm(_) => Err(()),
        }
    }
}

#[derive(Copy, Clone)]
pub(super) struct AddrOperand(Amd64Reg);

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
        write!(fmt, "{}", self.0)
    }
}
