use super::{
    function,
    function::SaveRegList,
    linear_scan::{self, Location},
    lir::{self, AddressOperand, MultiSlot},
    live_variable_analysis,
    register::{Amd64Reg, Amd64RegByte, Amd64RegDouble},
    var_id, CallingConv, VarId,
};
use crate::lowering::lir_allocator::Ptr;
use libfirm_rs::{nodes::NodeTrait, Tarval};
use std::{
    collections::{HashMap, HashSet},
    convert::{TryFrom, TryInto},
    fmt,
};
use std::iter::FromIterator;

pub(super) struct Codegen {
    var_location: HashMap<VarId, linear_scan::Location>,
    cconv: CallingConv,
    params_moved_from_stack: HashSet<u32>,
    num_saved_regs: usize,
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
            num_saved_regs: 0,
        }
    }

    pub(super) fn run(
        &mut self,
        function: &function::Function,
        blocks: Vec<live_variable_analysis::Block>,
    ) -> Vec<Instruction> {
        use self::Instruction::*;
        let mut instrs = vec![];
        self.num_saved_regs = function.saved_regs.len();

        self.gen_meta_comments(&mut instrs);

        self.gen_function_prolog(function, &mut instrs);

        self.gen_spill_arg_regs(&mut instrs);

        let mut block_iter = blocks.iter().peekable();
        while let Some(block) = block_iter.next() {
            instrs.push(Label {
                label: lir::gen_label(block.firm_num),
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
                        self.gen_mov(*src, *dst, &mut instrs);
                    }
                }
            }
        }

        self.gen_function_epilog(function, &mut instrs);

        instrs
    }

    fn gen_meta_comments(&self, instrs: &mut Vec<Instruction>) {
        use self::Instruction::Comment;

        instrs.push(Comment {
            comment: format!("Calling convention: {:?}", self.cconv),
        });
        for (id, location) in &self.var_location {
            instrs.push(Comment {
                comment: format!("Var {:?} in {:?}", id, location),
            });
        }
    }

    fn gen_function_prolog(
        &mut self,
        function: &function::Function,
        instrs: &mut Vec<Instruction>,
    ) {
        use self::Instruction::{Comment, Mov, Pushq, Subq};

        instrs.push(Comment {
            comment: "function prolog".to_string(),
        });
        for instr in &function.prolog {
            match instr {
                function::FnInstruction::Pushq { src } => {
                    let src = self.fn_to_src_operand(*src, instrs);
                    instrs.push(Pushq { src });
                }
                function::FnInstruction::Movq { src, dst } => {
                    let src = self.fn_to_src_operand(*src, instrs);
                    let dst = self.fn_to_src_operand(*dst, instrs).try_into().unwrap();
                    instrs.push(Mov(MovInstruction { src, dst, size: 8 }));
                }
                _ => unreachable!(),
            }
        }

        instrs.push(Comment {
            comment: "function save regs".to_string(),
        });
        for src in function.saved_regs.saves() {
            let src = SrcOperand::Reg(src);
            instrs.push(Pushq { src });
        }

        if let Some(instr) = function.allocate {
            instrs.push(Comment {
                comment: "function allocate stack space".to_string(),
            });
            if let function::FnInstruction::Subq { src, dst } = instr {
                instrs.push(Subq {
                    subtrahend: SrcOperand::Imm(src),
                    acc: DstOperand::Reg(dst),
                });
            } else {
                unreachable!();
            }
        }
    }

    fn gen_spill_arg_regs(&self, instrs: &mut Vec<Instruction>) {
        use self::Instruction::{Comment, Mov};

        for i in 0..self.cconv.num_arg_regs() {
            if let Some(Location::Mem(idx)) = self.var_location.get(&(-1, i as usize)) {
                instrs.push(Comment {
                    comment: format!("spill argument register {}", i),
                });
                instrs.push(Mov(MovInstruction {
                    src: SrcOperand::Reg(Amd64Reg::arg(i)),
                    dst: DstOperand::Mem(lir::AddressOperand::RegisterRelativeOffset {
                        offset: -((*idx + self.num_saved_regs) as i32) * 8,
                        base: Amd64Reg::Rbp,
                    }),
                    size: 8,
                }));
            }
        }
    }

    fn gen_function_epilog(
        &mut self,
        function: &function::Function,
        instrs: &mut Vec<Instruction>,
    ) {
        use self::Instruction::{Comment, Leave, Popq, Ret};

        instrs.push(Comment {
            comment: "function restore regs".to_string(),
        });
        for dst in function.saved_regs.restores() {
            let dst = DstOperand::Reg(dst);
            instrs.push(Popq { dst });
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
    }

    fn gen_mov(
        &mut self,
        src: lir::CopyPropagationSrc,
        dst: Ptr<MultiSlot>,
        instrs: &mut Vec<Instruction>,
    ) {
        use self::Instruction::{Mov, Popq, Pushq};

        let src = self.lir_to_src_operand(src.into(), instrs);
        let dst = self
            .lir_to_src_operand(lir::Operand::Slot(dst), instrs)
            .try_into()
            .unwrap();
        match (src, dst) {
            (SrcOperand::Reg(_), _) | (_, DstOperand::Reg(_)) => {
                instrs.push(Mov(MovInstruction { src, dst, size: 8 }));
            }
            (SrcOperand::Mem(_), DstOperand::Mem(_)) => {
                instrs.push(Pushq {
                    src: SrcOperand::Reg(Amd64Reg::Rax),
                });
                instrs.push(Mov(MovInstruction {
                    src,
                    dst: DstOperand::Reg(Amd64Reg::Rax),
                    size: 8,
                }));
                instrs.push(Mov(MovInstruction {
                    src: SrcOperand::Reg(Amd64Reg::Rax),
                    dst,
                    size: 8,
                }));
                instrs.push(Popq {
                    dst: DstOperand::Reg(Amd64Reg::Rax),
                });
            }
            (SrcOperand::Imm(_), _) => unreachable!(),
        }
    }

    fn gen_lir(&mut self, lir: &lir::Instruction, instrs: &mut Vec<Instruction>) {
        use self::Instruction::Mov;

        log::debug!("Gen lir: {:?}", lir);
        match lir {
            lir::Instruction::Binop {
                kind,
                src1,
                src2,
                dst,
            } => self.gen_binop(instrs, kind, src1, src2, *dst),
            lir::Instruction::Div { src1, src2, dst } => {
                let dst: DstOperand = self
                    .lir_to_src_operand(lir::Operand::Slot(*dst), instrs)
                    .try_into()
                    .unwrap();
                self.gen_div(instrs, src1, src2, dst.reg(), || {
                    Mov(MovInstruction {
                        src: SrcOperand::Reg(Amd64Reg::Rax),
                        dst,
                        size: 8,
                    })
                });
            }
            lir::Instruction::Mod { src1, src2, dst } => {
                let dst: DstOperand = self
                    .lir_to_src_operand(lir::Operand::Slot(*dst), instrs)
                    .try_into()
                    .unwrap();
                self.gen_div(instrs, src1, src2, dst.reg(), || {
                    Mov(MovInstruction {
                        src: SrcOperand::Reg(Amd64Reg::Rdx),
                        dst,
                        size: 8,
                    })
                });
            }
            lir::Instruction::Conv { src, dst } => {
                let src = self.lir_to_src_operand(*src, instrs);
                let dst = self
                    .lir_to_src_operand(lir::Operand::Slot(*dst), instrs)
                    .try_into()
                    .unwrap();
                instrs.push(Mov(MovInstruction { src, dst, size: 8 }))
            }
            lir::Instruction::Unop { kind, src, dst } => self.gen_unop(instrs, kind, src, *dst),

            lir::Instruction::StoreMem { src, dst, size } => {

                let mut constraints = vec![
                    (*src, OperandConstraints::AnyRegister),
                ];
                match dst {
                    lir::AddressOperand::RegisterRelativeOffset { base, .. } => unimplemented!(),
                    lir::AddressOperand::Scaled { base, index, stride, offset } => {
                        let base = lir::Operand::Slot(*base);
                        let index = lir::Operand::Slot(*index);
                        constraints.push((base, OperandConstraints::AnyRegister));
                        constraints.push((index, OperandConstraints::AnyRegister));
                        // FIXME invalid free reg list
                        let free_reg_list = RegFreeList::new(Amd64Reg::all().collect());
                        self.with_operand_constraints(instrs, free_reg_list, &constraints, &move |any, reg| {
                            let src = any(src);
                            let dst = lir::AddressOperand::Scaled {
                                base: reg(&base),
                                index: reg(&index),
                                stride: *stride,
                                offset: *offset,
                            };
                            vec![Mov(MovInstruction{
                                src,
                                dst: DstOperand::Mem(dst),
                                size: *size,
                            })]
                        });
                    }
                }
            }
            lir::Instruction::LoadMem { src, dst, size } => {
                let dst: DstOperand = self
                    .lir_to_src_operand(lir::Operand::Slot(*dst), instrs)
                    .try_into()
                    .unwrap();

                // convert AddressOperand to src operand
                // let mut factors = vec![];
                macro_rules! slot_to_operand {
                    ( $ptr_multi_slot:expr ) => {{
                        let lirop = lir::Operand::Slot($ptr_multi_slot);
                        let x = self.lir_to_src_operand(lirop, instrs);
                        //factors.push(x);
                        match x {
                            SrcOperand::Reg(reg) => reg,
                            _ => panic!("expecting register allocation to allocate reg"),
                        }
                    }};
                }
                let src = match *src {
                    AddressOperand::RegisterRelativeOffset { base, offset } => {
                        let base = slot_to_operand!(base);
                        AddressOperand::RegisterRelativeOffset { base, offset }
                    }
                    AddressOperand::Scaled {
                        base,
                        index,
                        stride,
                        offset,
                    } => {
                        let base = slot_to_operand!(base);
                        let index = slot_to_operand!(index);
                        AddressOperand::Scaled {
                            base,
                            index,
                            stride,
                            offset,
                        }
                    }
                };
                // factors must be in registers for x86 reg-relative addressing
                // the following situation is very unlikely, but because codegen
                // happens after register allocation, and because we do not have a way
                // to require registers for certain lir::Operand::Slot(_) (FIXME @flip1995 )
                // this situation can happen
                // let (factor_regs, factor_notyetregs) = factors.partition(|factor| match
                // factor {     SrcOperand::Reg(_) => true,
                //     _ => false,
                // });
                // let mut free_regs =
                //     Amd64Reg::all().filter(|reg| factor_regs.iter().find(reg).is_none());
                // let mut spills: Vec<(Amd64Reg> = vec![];
                // for factor in &factor_notyetregs {
                //     let slot = match factor {
                //         SrcOperand::Reg(_) => unreachable!(),
                //         SrcOperand::Imm(_) => unreachable!(),
                //         SrcOperand::Mem(slot) => slot,
                //     };
                //     let
                //     spills.push(free_regsfactor,
                // }
                instrs.push(Mov(MovInstruction {
                    src: SrcOperand::Mem(src),
                    dst,
                    size: *size,
                }));

                // let dst: DstOperand = self
                //     .lir_to_src_operand(lir::Operand::Slot(*dst), instrs)
                //     .try_into()
                //     .unwrap();
                // self.gen_load_store(instrs, src, None, dst.reg(), |addr| {
                //     Mov(MovInstruction {
                //         src: SrcOperand::Mem(addr),
                //         dst,
                //         size: *size,
                //     })
                // })
            }
            lir::Instruction::Call { .. } => unreachable!("Call already converted"),
            lir::Instruction::Comment(_) => (),
        }
    }

    fn with_operand_constraints<F>(&self, instrs: &mut Vec<Instruction>, free_regs: RegFreeList, constraints: &[(lir::Operand, OperandConstraints)], f: &F)
    where
        F: FnMut(& dyn Fn(&lir::Operand) -> SrcOperand, & dyn Fn(&lir::Operand) -> Amd64Reg) -> Vec<Instruction>,
    {

            use self::{Location as L, OperandConstraints as C};
        let (mut satisfied_reg, mut unsatisfied_reg_to_specific_reg, mut unsatisfied_mem_to_any_reg, mut unsatisfied_mem_to_specific_reg) =
            (vec![], vec![], vec![], vec![]);
        for (operand, constraint) in constraints.clone() {
            let location = self.var_location[&var_id(*operand)];
            match (location, *constraint) {
                (L::Reg(r), C::AnyRegister)
                | (L::Reg(r), C::MemOrRegister) => satisfied_reg.push((operand, r)),
                (L::Mem(_), C::MemOrRegister)
                | (L::ParamMem, C::MemOrRegister) => (), // nothing to do
                (L::Reg(r_l), C::Register(r_c)) => {
                    if r_l == r_c {
                        satisfied_reg.push((operand, r_l))
                    } else {
                        unsatisfied_reg_to_specific_reg.push((operand, r_l, r_c))
                    }
                }
                (L::Mem(mem), C::AnyRegister) => unsatisfied_mem_to_any_reg.push(operand),
                (L::Mem(mem), C::Register(r)) => unsatisfied_mem_to_specific_reg.push((operand, r)),
                (L::ParamMem, C::AnyRegister) => unsatisfied_mem_to_any_reg.push(operand),
                (L::ParamMem, C::Register(r)) => unsatisfied_mem_to_specific_reg.push((operand, r)),
            }
        }

        // TODO consistency check: do unsatisfied target regs overlap?
        // are target locations disjoint?

        // Any time we write to a register, add it to this list to ensure its original value
        // is restored after the instructions generated by f
        let mut save_reg_list = SaveRegList::default();

        let mut free_regs = free_regs;

        let mut assignments = HashMap::new();

        // satisfied_reg are already satisfied, just make sure they are not available
        // for any_reg constraints
        for (operand, reg) in satisfied_reg {
            free_regs.begin_use(reg);
            assignments.insert(operand, DstOperand::Reg(reg));
        }

        use self::Instruction::*;
        // satisfy reg_to_specific_reg
        {
            // TODO cycle detection, only spill as necessary
            // spill everything on the stack and restore it immediately
            let (mut pushs, mut pops) = (vec![], vec![]);
            for (operand, r_source, r_target) in &unsatisfied_reg_to_specific_reg {
                save_reg_list.add_regs(&[r_target]);
                free_regs.begin_use(r_target);
                assignments.insert(operand, DstOperand::Reg(r_target));
                pushs.push(Pushq{ src: SrcOperand::Reg(*r_source) });
                pops.push(Popq{ dst: DstOperand::Reg(*r_target) });
            }
            instrs.extend(pushs);
            instrs.extend(pops.into_iter().rev());
        }

        let mem_location_address = &|idx| {
            lir::AddressOperand::RegisterRelativeOffset {
                offset: -((idx + self.num_saved_regs) as i32) * 8,
                base: Amd64Reg::Rbp,
            }
        };

        let parammem_location_address = &|param_idx| {
            lir::AddressOperand::RegisterRelativeOffset {
                offset: (param_idx as i32) * 8,
                base: Amd64Reg::Rbp,
            }
        };

        // satisfy unsatisfied_mem_to_specific_reg
        for (operand, reg) in unsatisfied_mem_to_specific_reg {
            let src_address = match self.var_location[&var_id(*operand)] {
                Location::Reg(_) => unreachable!(),
                Location::Mem(idx) => mem_location_address(idx),
                Location::ParamMem => {
                    if let lir::Operand::Param{idx} = operand {
                        parammem_location_address(*idx)
                    } else {
                        unreachable!()
                    }
                }
            };
            save_reg_list.add_regs(&[reg]);
            free_regs.begin_use(reg);
            assignments.insert(operand, DstOperand::Reg(reg));
            instrs.push(Mov(MovInstruction{
                src: SrcOperand::Mem(src_address),
                dst: DstOperand::Reg(reg),
                size: 8,
            }))
        }

        // satisfy unsatisfied_mem_to_any_reg
        // => consult free list, spill every entry in it as the register may be in use
        // outside of this function
        let mut free_regs =  free_regs.into_unused_regs_iter();
        let mut movs = vec![];
        for operand in  unsatisfied_mem_to_any_reg {
            let reg = if let Some(r) = free_regs.next() {
                save_reg_list.add_regs(&[r]);
                assignments.insert(operand, DstOperand::Reg(r));
                r
            } else {
                panic!("unsatisfiable constraints: no free registers left")
            };
            // TODO following stmt copied from above
            let src_address = match self.var_location[&var_id(*operand)] {
                Location::Reg(_) => unreachable!(),
                Location::Mem(idx) => mem_location_address(idx),
                Location::ParamMem => {
                    if let lir::Operand::Param{idx} = operand {
                        parammem_location_address(*idx)
                    } else {
                        unreachable!()
                    }
                }
            };
            movs.push(Mov(MovInstruction{
                src: SrcOperand::Mem(src_address),
                dst: DstOperand::Reg(reg),
                size: 8, // TODO is this always correct?
            }));
        }

        // save the regs whose values are changed by the movs
        instrs.extend(save_reg_list.saves().map(|r| Pushq{ src: SrcOperand::Reg(*r) } ));
        // emit the movs
        instrs.extend(movs);
        // not all operands are in the place they need to be for f, call it
        let any = |operand| {
            assignments.get(operand).expect("invalid use of API")
        };
        let reg = |operand| {
            match any(operand) {
                DstOperand::Reg(r) => r,
                DstOperand::Mem(_) => panic!("invlaid use of API or implementation error"),
            }
        };
        instrs.extend(f(any, reg));
        // restore the regs whose values were changed by the movs
        instrs.extend(save_reg_list.restores().map(|r| Popq{ dst: DstOperand::Reg(*r) }));
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
                        subtrahend: $src,
                        acc: $dst,
                    }),
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
                instrs.push(Mov(MovInstruction {
                    src: src1,
                    dst,
                    size: 8,
                }));
                push_binop!(kind, src2, dst);
            }
            DstOperand::Mem(_) => match (src1, src2) {
                // Enforce that we always get `op reg/imm, mem`
                (SrcOperand::Reg(_), SrcOperand::Reg(_))
                | (SrcOperand::Reg(_), SrcOperand::Imm(_))
                | (SrcOperand::Imm(_), SrcOperand::Reg(_))
                | (SrcOperand::Imm(_), SrcOperand::Imm(_)) => {
                    instrs.push(Mov(MovInstruction {
                        src: src1,
                        dst,
                        size: 8,
                    }));
                    push_binop!(kind, src2, dst);
                }
                (SrcOperand::Mem(_), SrcOperand::Mem(_)) => {
                    // This case is bad. There aren't any (mem, mem) ops. This means we
                    // need to spill one register:
                    // `op mem, mem -> mem` => `op reg, mem`
                    // We want to move src2 -> dst, but first we need to move src2 -> %rax:
                    let spill = Amd64Reg::Rax;
                    instrs.push(Pushq {
                        src: SrcOperand::Reg(spill),
                    });
                    instrs.push(Mov(MovInstruction {
                        src: src1,
                        dst: DstOperand::Reg(spill),
                        size: 8,
                    }));
                    // Now we can move %rax -> dst
                    instrs.push(Mov(MovInstruction {
                        src: SrcOperand::Reg(spill),
                        dst,
                        size: 8,
                    }));
                    // We're now at a 2-address code state: `op mem, mem`
                    // Now we need to move src1 -> %rax
                    instrs.push(Mov(MovInstruction {
                        src: src2,
                        dst: DstOperand::Reg(spill),
                        size: 8,
                    }));
                    // Now the instruction `op %rax, mem`:
                    push_binop!(kind, SrcOperand::Reg(spill), dst);

                    // And last but not least: recover %rax
                    instrs.push(Popq {
                        dst: DstOperand::Reg(spill),
                    });
                }
                // src1 is Reg or Imm, src2 is Mem
                (SrcOperand::Reg(_), SrcOperand::Mem(_))
                | (SrcOperand::Imm(_), SrcOperand::Mem(_)) => {
                    // move src1 into accumulator
                    // (using src1, not src2 for correct sub support)
                    instrs.push(Mov(MovInstruction {
                        src: src1,
                        dst,
                        size: 8,
                    }));
                    let spill = Amd64Reg::Rax;
                    instrs.push(Pushq {
                        src: SrcOperand::Reg(spill),
                    });
                    instrs.push(Mov(MovInstruction {
                        src: src2,
                        dst: DstOperand::Reg(spill),
                        size: 8,
                    }));
                    // Now the instruction `op spill(src2), mem(dst == src1)`:
                    push_binop!(kind, SrcOperand::Reg(spill), dst);
                    // Now mem(dst == src1 OP src2)

                    instrs.push(Popq {
                        dst: DstOperand::Reg(spill),
                    });
                }
                // src1 is Mem, src2 is Reg or Imm
                (SrcOperand::Mem(_), SrcOperand::Reg(_))
                | (SrcOperand::Mem(_), SrcOperand::Imm(_)) => {
                    // The same approach like above, but with src1 and src2 swapped
                    // and a special case for `sub` because it's not commutative (see `if` comment)
                    instrs.push(Mov(MovInstruction {
                        src: src2,
                        dst,
                        size: 8,
                    }));
                    let spill = Amd64Reg::Rax;
                    instrs.push(Pushq {
                        src: SrcOperand::Reg(spill),
                    });
                    instrs.push(Mov(MovInstruction {
                        src: src1,
                        dst: DstOperand::Reg(spill),
                        size: 8,
                    }));
                    // Now the instruction `op spill(src1), mem(dst == src2)`:
                    push_binop!(kind, SrcOperand::Reg(spill), dst);
                    // Now mem(dst == src2 OP src1)
                    if let lir::BinopKind::Sub = kind {
                        // sub is the only binop that is not commutative
                        // OP = -
                        // and in fact, we wanted to compute dst = src1 OP src2
                        // but we computed src2 OP src1
                        //
                        // Let's use: src1-src2 = -(src2-src1)
                        instrs.push(Negq { src: dst })
                    }

                    instrs.push(Popq {
                        dst: DstOperand::Reg(spill),
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
        dst_reg: Option<Amd64Reg>,
        f: F,
    ) where
        F: FnOnce() -> Instruction,
    {
        use self::Instruction::{Cqto, Divq, Mov, Popq, Pushq};

        // when dst is rdx, rax or rsi, we don't need to spill the respective register,
        // because it is overidden anyway
        let (dst_is_rdx, dst_is_rax, dst_is_rsi) =
            dst_reg.map_or((false, false, false), |reg| match reg {
                Amd64Reg::Rdx => (true, false, false),
                Amd64Reg::Rax => (false, true, false),
                Amd64Reg::Rsi => (false, false, true),
                _ => (false, false, false),
            });

        if !dst_is_rdx {
            // Needed for sign extend of rax -> rdx:rax
            instrs.push(Pushq {
                src: SrcOperand::Reg(Amd64Reg::Rdx),
            });
        }
        if !dst_is_rax {
            instrs.push(Pushq {
                src: SrcOperand::Reg(Amd64Reg::Rax),
            });
        }

        let src = self.lir_to_src_operand(*src1, instrs);
        instrs.push(Mov(MovInstruction {
            src,
            dst: DstOperand::Reg(Amd64Reg::Rax),
            size: 8,
        }));
        instrs.push(Cqto);
        if let lir::Operand::Imm(_) = src2 {
            if !dst_is_rsi {
                instrs.push(Pushq {
                    src: SrcOperand::Reg(Amd64Reg::Rsi),
                });
            }
            let src = self.lir_to_src_operand(*src2, instrs);
            instrs.push(Mov(MovInstruction {
                src,
                dst: DstOperand::Reg(Amd64Reg::Rsi),
                size: 8,
            }));
            instrs.push(Divq {
                src: DstOperand::Reg(Amd64Reg::Rsi),
            });
            if !dst_is_rsi {
                instrs.push(Popq {
                    dst: DstOperand::Reg(Amd64Reg::Rsi),
                });
            }
        } else {
            let src = self.lir_to_src_operand(*src2, instrs).try_into().unwrap();
            instrs.push(Divq { src });
        }
        instrs.push(f());

        if !dst_is_rax {
            instrs.push(Popq {
                dst: DstOperand::Reg(Amd64Reg::Rax),
            });
        }
        if !dst_is_rdx {
            instrs.push(Popq {
                dst: DstOperand::Reg(Amd64Reg::Rdx),
            });
        }
    }

    fn gen_unop(
        &mut self,
        instrs: &mut Vec<Instruction>,
        kind: &lir::UnopKind,
        src: &lir::Operand,
        dst: Ptr<MultiSlot>,
    ) {
        use self::Instruction::{Mov, Negq, Notq, Popq, Pushq};

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
                instrs.push(Mov(MovInstruction { src, dst, size: 8 }));
                push_unop!(kind, dst);
            }
            DstOperand::Mem(_) => match src {
                SrcOperand::Reg(_) | SrcOperand::Imm(_) => {
                    instrs.push(Mov(MovInstruction { src, dst, size: 8 }));
                    push_unop!(kind, dst);
                }
                SrcOperand::Mem(_) => {
                    instrs.push(Pushq {
                        src: SrcOperand::Reg(Amd64Reg::Rax),
                    });
                    // src -> %rax
                    instrs.push(Mov(MovInstruction {
                        src,
                        dst: DstOperand::Reg(Amd64Reg::Rax),
                        size: 8,
                    }));
                    // Now the instruction `op %rax`:
                    push_unop!(kind, DstOperand::Reg(Amd64Reg::Rax));
                    // %rax -> dst
                    instrs.push(Mov(MovInstruction {
                        src: SrcOperand::Reg(Amd64Reg::Rax),
                        dst,
                        size: 8,
                    }));
                    // And last but not least: recover %rax
                    instrs.push(Popq {
                        dst: DstOperand::Reg(Amd64Reg::Rax),
                    });
                }
            },
        }
    }

    // fn gen_load_store<F>(
    //     &mut self,
    //     instrs: &mut Vec<Instruction>,
    //     addr: &lir::AddressOperand,
    //     src_reg: Option<Amd64Reg>,
    //     dst_reg: Option<Amd64Reg>,
    //     f: F,
    // ) where
    //     F: FnOnce(lir::AddressComputation<Amd64Reg>) -> Instruction,
    // {
    //     use self::Instruction::{Mov, Popq, Pushq};

    //     debug_assert!(!(src_reg.is_some() && dst_reg.is_some()));
    //     // when dst is rdx or rax, we don't need to spill the respective
    // register,     // because it is overidden anyway
    //     let (dst_is_rdx, dst_is_rax) = dst_reg.map_or((false, false), |reg| match
    // reg {         Amd64Reg::Rdx => (true, false),
    //         Amd64Reg::Rax => (false, true),
    //         _ => (false, false),
    //     });
    //     let (base_spill_reg, index_spill_reg) =
    //         src_reg.map_or((Amd64Reg::Rax, Amd64Reg::Rdx), |reg| match reg {
    //             Amd64Reg::Rdx => (Amd64Reg::Rax, Amd64Reg::Rsi),
    //             Amd64Reg::Rax => (Amd64Reg::Rsi, Amd64Reg::Rdx),
    //             _ => (Amd64Reg::Rax, Amd64Reg::Rdx),
    //         });

    //     let lir::AddressComputation {
    //         offset,
    //         base,
    //         index,
    //     } = addr;
    //     let base = self.lir_to_src_operand(*base, instrs);
    //     let (base, base_new_reg) = match base {
    //         SrcOperand::Reg(reg) => (reg, false),
    //         _ => {
    //             if !dst_is_rax {
    //                 instrs.push(Pushq {
    //                     src: SrcOperand::Reg(base_spill_reg),
    //                 });
    //             }
    //             instrs.push(Mov(MovInstruction {
    //                 src: base,
    //                 dst: DstOperand::Reg(base_spill_reg),
    //                 size: 8,
    //             }));
    //             // Recover rax, iff dst is not rax
    //             (base_spill_reg, !dst_is_rax)
    //         }
    //     };
    //     let (index, index_new_reg) = match index {
    //         lir::IndexComputation::Zero => (lir::IndexComputation::Zero, false),
    //         lir::IndexComputation::Displacement(op, s) => {
    //             let op = self.lir_to_src_operand(*op, instrs);
    //             match op {
    //                 SrcOperand::Reg(reg) =>
    // (lir::IndexComputation::Displacement(reg, *s), false),                 _
    // => {                     if !dst_is_rdx {
    //                         instrs.push(Pushq {
    //                             src: SrcOperand::Reg(index_spill_reg),
    //                         });
    //                     }
    //                     instrs.push(Mov(MovInstruction {
    //                         src: op,
    //                         dst: DstOperand::Reg(index_spill_reg),
    //                         size: 8,
    //                     }));
    //                     // Recover rdx, iff dst is not rdx
    //                     (
    //                         lir::IndexComputation::Displacement(index_spill_reg,
    // *s),                         !dst_is_rdx,
    //                     )
    //                 }
    //             }
    //         }
    //     };

    //     instrs.push(f(lir::AddressComputation {
    //         offset: *offset,
    //         base,
    //         index,
    //     }));

    //     if base_new_reg {
    //         instrs.push(Popq {
    //             dst: DstOperand::Reg(Amd64Reg::Rax),
    //         });
    //     }
    //     if index_new_reg {
    //         instrs.push(Popq {
    //             dst: DstOperand::Reg(Amd64Reg::Rdx),
    //         });
    //     }
    // }

    fn gen_call(&mut self, call: &function::FunctionCall, instrs: &mut Vec<Instruction>) {
        use self::Instruction::{Addq, Call, Comment, Mov, Popq, Pushq};

        instrs.push(Comment {
            comment: "call save args".to_string(),
        });
        for src in call.saved_regs.saves() {
            let src = SrcOperand::Reg(src);
            instrs.push(Pushq { src });
        }

        instrs.push(Comment {
            comment: "call setup".to_string(),
        });
        for instr in &call.setup {
            match instr {
                function::FnInstruction::Movq { src, dst } => {
                    let src = self.fn_to_src_operand(*src, instrs);
                    let dst = self.fn_to_src_operand(*dst, instrs).try_into().unwrap();
                    instrs.push(Mov(MovInstruction { src, dst, size: 8 }));
                }
                function::FnInstruction::Pushq { src } => {
                    let src = self.fn_to_src_operand(*src, instrs);
                    instrs.push(Pushq { src });
                }
                function::FnInstruction::Popq { dst } => {
                    let dst = self.fn_to_src_operand(*dst, instrs).try_into().unwrap();
                    instrs.push(Popq { dst });
                }
                _ => unreachable!(),
            }
        }

        instrs.push(Call(CallInstruction {
            label: call.label.clone(),
        }));

        if let Some(function::FnInstruction::Movq { src, dst }) = call.move_res {
            instrs.push(Comment {
                comment: "call move from %rax".to_string(),
            });
            let src = self.fn_to_src_operand(src, instrs);
            let dst = self.fn_to_src_operand(dst, instrs).try_into().unwrap();
            instrs.push(Mov(MovInstruction { src, dst, size: 8 }));
        }

        if let Some(function::FnInstruction::Addq { src, dst }) = call.recover {
            instrs.push(Comment {
                comment: "call restore %rsp".to_string(),
            });
            instrs.push(Addq {
                src: SrcOperand::Imm(src),
                dst: DstOperand::Reg(dst),
            });
        }

        instrs.push(Comment {
            comment: "call recover args".to_string(),
        });
        for dst in call.saved_regs.restores() {
            let dst = DstOperand::Reg(dst);
            instrs.push(Popq { dst });
        }
    }

    fn gen_leave(
        &mut self,
        leave: &lir::Leave,
        next_block_num: i64,
        instrs: &mut Vec<Instruction>,
    ) {
        use self::Instruction::{Cmpq, Jmp, Mov, Popq, Pushq};

        macro_rules! push_jmp {
            ($kind:expr, $target:expr, $next_block_num:expr, fall=$fall:expr) => {{
                let target_num = $target.firm.node_id();
                if !$fall || target_num != $next_block_num {
                    instrs.push(Jmp {
                        label: lir::gen_label(target_num),
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
                // x86 GAS mini tutorial:
                // cmp subtrahend: a, minuend: b  <=> RES = b - a
                // RES does not really exist, in fact only flags are kept
                // but it makes things easier to explain
                // => `jl target` is taken iff (RES is signed XOR RES is overflowed)
                // see
                // https://en.wikibooks.org/wiki/X86_Assembly/Control_Flow#Comparison_Instructions

                // We use the following recipe: (COP = Comparison Operation)
                // a COP b => cmp a, b
                // In GAS, this is b - a
                //
                //      a LT  b => b-a, jg
                //      a LTE b => b-a, jge
                //
                //      a GT  b => b-a, jl
                //      a GTE b => b-a, jle
                //
                //      a EQ  b => b-a, je / jz
                //      a NEQ b => b-a, jne /jnz
                //
                // As can be seen, we can "swap" the op kind (LT => jg, GT=>jl).
                // Alternatively, we can swap the operands a and b.
                //
                // NOTE: ja, jae, etc. don't work because we require signed comparison

                let lhs = self.lir_to_src_operand(*lhs, instrs);
                let rhs = self.lir_to_src_operand(*rhs, instrs);
                let op = match (lhs, rhs) {
                    (SrcOperand::Reg(_), SrcOperand::Mem(_))
                    | (_, SrcOperand::Reg(_))
                    | (SrcOperand::Imm(_), SrcOperand::Mem(_)) => {
                        instrs.push(Cmpq {
                            subtrahend: lhs,
                            minuend: rhs,
                        });
                        // swap op to account for GAS syntax
                        op.swap()
                    }

                    (SrcOperand::Reg(_), SrcOperand::Imm(_))
                    | (SrcOperand::Mem(_), SrcOperand::Imm(_)) => {
                        // only the subtrahend is allowed to be immediate
                        // thus, swap rhs and lhs on their subtrahend / minuend position
                        instrs.push(Cmpq {
                            subtrahend: rhs,
                            minuend: lhs,
                        });
                        // do not swap op for GAS, we already swapped rhs and lhs
                        *op
                    }
                    (SrcOperand::Mem(_), SrcOperand::Mem(_))
                    | (SrcOperand::Imm(_), SrcOperand::Imm(_)) => {
                        // This case is bad we don't know if there is a free
                        // register left. So we have to spill one register.
                        let spill = Amd64Reg::Rax;
                        instrs.push(Pushq {
                            src: SrcOperand::Reg(spill),
                        });
                        // if we are in (Imm,Imm), lhs must stay Imm
                        instrs.push(Mov(MovInstruction {
                            src: rhs,
                            dst: DstOperand::Reg(spill),
                            size: 8,
                        }));
                        instrs.push(Cmpq {
                            subtrahend: lhs,
                            minuend: SrcOperand::Reg(spill),
                        });
                        instrs.push(Popq {
                            dst: DstOperand::Reg(spill),
                        });
                        op.swap()
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
                    instrs.push(Mov(MovInstruction {
                        src,
                        dst: DstOperand::Reg(Amd64Reg::Rax),
                        size: 8,
                    }))
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

    fn lir_to_src_operand(
        &mut self,
        op: lir::Operand,
        instrs: &mut Vec<Instruction>,
    ) -> SrcOperand {
        match op {
            lir::Operand::Imm(c) => SrcOperand::Imm(c),
            lir::Operand::Slot(_) => match self.var_location[&var_id(op)] {
                Location::Reg(reg) => SrcOperand::Reg(reg),
                Location::Mem(idx) => {
                    SrcOperand::Mem(lir::AddressOperand::RegisterRelativeOffset {
                        offset: -((idx + self.num_saved_regs) as i32) * 8,
                        base: Amd64Reg::Rbp,
                    })
                }
                Location::ParamMem => unreachable!("a slot never has a ParamMem location"),
            },
            lir::Operand::Param { idx } => match self.var_location[&var_id(op)] {
                Location::Reg(reg) => {
                    if let Some(instr) = self.arg_from_stack(idx as usize, DstOperand::Reg(reg)) {
                        if self.params_moved_from_stack.insert(idx) {
                            instrs.push(instr);
                        }
                    }
                    SrcOperand::Reg(reg)
                }
                // This can happen when the param was originally in a register but got moved on
                // the stack.
                Location::Mem(idx) => {
                    SrcOperand::Mem(lir::AddressOperand::RegisterRelativeOffset {
                        offset: -((idx + self.num_saved_regs) as i32) * 8,
                        base: Amd64Reg::Rbp,
                    })
                }
                Location::ParamMem => {
                    SrcOperand::Mem(lir::AddressOperand::RegisterRelativeOffset {
                        offset: (idx as i32) * 8,
                        base: Amd64Reg::Rbp,
                    })
                }
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

    fn arg_from_stack(&self, idx: usize, dst: DstOperand) -> Option<Instruction> {
        if idx < self.cconv.num_arg_regs() {
            return None;
        }
        let offset = ((idx + 1).checked_sub(self.cconv.num_arg_regs()).unwrap() * 8 + 8) as i32;
        Some(Instruction::Mov(MovInstruction {
            src: SrcOperand::Mem(lir::AddressOperand::RegisterRelativeOffset {
                offset,
                base: Amd64Reg::Rbp,
            }),
            dst,
            size: 8,
        }))
    }
}

use std::collections::{BTreeMap,BTreeSet};

#[derive(Clone, Copy, Debug)]
enum OperandConstraints {
    /// Operand may be in a register or memory location.
    MemOrRegister,
    /// Operand must be in a register.
    AnyRegister,
    /// Operand must be placed in a specific register.
    Register(Amd64Reg),
}

struct RegFreeList {
    free: BTreeSet<Amd64Reg>,
    occupied: BTreeMap<Amd64Reg, usize>,
}

impl RegFreeList {
    /// Initializes list to all free
    fn new(free: BTreeSet<Amd64Reg>) -> Self {
        let occupied = BTreeMap::new();
        Self { free, occupied }
    }
    fn begin_use(&mut self, r: &Amd64Reg) {
        if self.free.contains(r) {
            self.free.remove(r);
            assert!(!self.occupied.contains_key(r));
            self.occupied.insert(*r, 1);
        } else {
            let entry = self.occupied.get_mut(r).unwrap();
            *entry += 1;
        }
    }
    fn end_use(&mut self, r: &Amd64Reg) {
        debug_assert!(!self.free.contains(r), "double free");
        let entry = self.occupied.get_mut(r).unwrap();
        *entry -= 1;
        if *entry == 0 {
            drop(entry);
            self.occupied.remove(r);
            let did_overwrite = self.free.insert(*r);
            assert!(!did_overwrite);
        }
    }
    fn into_unused_regs_iter(self) -> impl Iterator<Item=Amd64Reg> {
        self.free.into_iter()
    }
}


#[derive(Display)]
pub(super) enum Instruction {
    #[display(fmt = "\t{}", _0)]
    Mov(MovInstruction),
    #[display(fmt = "\taddq {}, {}", src, dst)]
    Addq { src: SrcOperand, dst: DstOperand },
    #[display(fmt = "\tsubq {}, {}", subtrahend, acc)]
    Subq {
        subtrahend: SrcOperand,
        acc: DstOperand,
    },
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
    #[display(fmt = "\tcmpq {}, {}", subtrahend, minuend)]
    Cmpq {
        subtrahend: SrcOperand,
        minuend: SrcOperand,
    },
    #[display(fmt = "\t{} {}", kind, label)]
    Jmp { label: String, kind: lir::JmpKind },
    // multi-line output by CallInstruction Display impl
    #[display(fmt = "{}", _0)]
    Call(CallInstruction),
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

pub(super) struct CallInstruction {
    label: String,
}

/// Amd64 requires 16-byte aligned stacks.
/// The emitted assembly works for 8-byte aligned stacks.
/// (This works because we always spill quad-words (pushQ, popQ)).
impl fmt::Display for CallInstruction {
    fn fmt(&self, fmt: &mut fmt::Formatter<'_>) -> fmt::Result {
        writeln!(fmt, "\tpushq %rsp")?;
        writeln!(fmt, "\tpushq (%rsp)")?;
        writeln!(fmt, "\tandq $-16, %rsp")?;
        writeln!(fmt, "\tcall {}", self.label)?;
        writeln!(fmt, "\tmovq 8(%rsp), %rsp")?;
        Ok(())
    }
}

pub(super) struct MovInstruction {
    src: SrcOperand,
    dst: DstOperand,
    size: u32,
}

impl fmt::Display for MovInstruction {
    fn fmt(&self, fmt: &mut fmt::Formatter<'_>) -> fmt::Result {
        match (self.src, self.dst) {
            (SrcOperand::Imm(_), DstOperand::Reg(_)) | (SrcOperand::Reg(_), DstOperand::Reg(_)) => {
                write!(fmt, "movq {}, {}", self.src, self.dst)
            }
            (SrcOperand::Reg(reg), DstOperand::Mem(_)) => match self.size {
                1 => write!(fmt, "movb {}, {}", Amd64RegByte::from(reg), self.dst),
                4 => write!(fmt, "movl {}, {}", Amd64RegDouble::from(reg), self.dst),
                8 => write!(fmt, "movq {}, {}", self.src, self.dst),
                _ => unreachable!(),
            },
            (SrcOperand::Mem(_), DstOperand::Reg(_)) => match self.size {
                1 => write!(fmt, "movsbq {}, {}", self.src, self.dst),
                4 => write!(fmt, "movslq {}, {}", self.src, self.dst),
                8 => write!(fmt, "movq {}, {}", self.src, self.dst),
                _ => unreachable!(),
            },
            (SrcOperand::Imm(_), DstOperand::Mem(_)) => match self.size {
                1 => write!(fmt, "movb {}, {}", self.src, self.dst),
                4 => write!(fmt, "movl {}, {}", self.src, self.dst),
                8 => write!(fmt, "movq {}, {}", self.src, self.dst),
                _ => unreachable!(),
            },
            (SrcOperand::Mem(_), DstOperand::Mem(_)) => unreachable!(),
        }
    }
}

#[derive(Copy, Clone)]
pub(super) enum SrcOperand {
    Mem(lir::AddressOperand<Amd64Reg>),
    Reg(Amd64Reg),
    Imm(Tarval),
}

impl SrcOperand {
    fn reg(self) -> Option<Amd64Reg> {
        if let SrcOperand::Reg(reg) = self {
            Some(reg)
        } else {
            None
        }
    }
}

#[derive(Copy, Clone)]
pub(super) enum DstOperand {
    Mem(lir::AddressOperand<Amd64Reg>),
    Reg(Amd64Reg),
}

impl DstOperand {
    fn reg(self) -> Option<Amd64Reg> {
        if let DstOperand::Reg(reg) = self {
            Some(reg)
        } else {
            None
        }
    }
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
