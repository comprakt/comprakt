use super::{
    function::{self, SaveRegList},
    linear_scan::{self, Location},
    lir::{self, MultiSlot},
    live_variable_analysis,
    register::{Amd64Reg, Amd64RegByte, Amd64RegDouble, RegisterAllocator},
    var_id, CallingConv, VarId,
};
use crate::lowering::lir_allocator::Ptr;
use libfirm_rs::{nodes::NodeTrait, Tarval};
use std::{
    collections::{BTreeSet, HashMap, HashSet},
    convert::{TryFrom, TryInto},
    fmt,
    iter::FromIterator,
};

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
                    dst: DstOperand::Mem(lir::AddressComputation {
                        offset: -((*idx + self.num_saved_regs) as isize) * 8,
                        base: Amd64Reg::Rbp,
                        index: lir::IndexComputation::Zero,
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
            }
            (SrcOperand::Imm(_), _) => instrs.push(Mov(MovInstruction { src, dst, size: 8 })),
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
                unimplemented!()
            }
            lir::Instruction::Mod { src1, src2, dst } => {
                let dst: DstOperand = self
                    .lir_to_src_operand(lir::Operand::Slot(*dst), instrs)
                    .try_into()
                    .unwrap();
                unimplemented!()
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
            lir::Instruction::StoreMem(store) => {
                let mut free_regs = BTreeSet::from_iter(Amd64Reg::all_but_rsp_and_rbp());
                self.load_store_mem_occupied_regs(instrs, store)
                    .into_iter()
                    .for_each(|occupied_reg| {
                        free_regs.remove(&occupied_reg);
                    });

                // rev because eax is the last one, and we want to use it first because it is
                // scratch
                let mut free_regs = free_regs.into_iter().rev().collect();
                let mut spill_ctx = SpillContext::new(free_regs);

                use self::{lir::IndexComputation, Instruction::*};

                let dst: DstOperand = {
                    let dst = store.mem_address_computation();
                    let (dst, _) = self.lir_address_computation_to_register_address_computation(
                        instrs,
                        dst,
                        &mut spill_ctx,
                    );
                    SrcOperand::Mem(dst).try_into().unwrap()
                };

                let src: SrcOperand = {
                    let src = store.operand();
                    let src: SrcOperand = self.lir_to_src_operand(src, instrs);
                    match src {
                        // the src is stored in a reg, this is fine
                        SrcOperand::Reg(reg) => src,
                        SrcOperand::Imm(_) => src,
                        // the source is stored in the activation record, move to scratch
                        SrcOperand::Mem(ar_addr_comp) => {
                            // TODO @flip1996 SrcOperand::ActivationRecordEntry refactor
                            assert!(ar_addr_comp.base == Amd64Reg::Rbp);
                            assert!(ar_addr_comp.index.is_zero());
                            let src = spill_ctx.spill_and_load_operand(src);
                            SrcOperand::Reg(src)
                        }
                    }
                };

                // Emit the instructoins
                // This whole function is just about the following statement
                let surrounded = vec![Mov(MovInstruction {
                    src,
                    dst,
                    size: store.size(),
                })];
                instrs.extend(spill_ctx.emit_surrounded_instrs(surrounded));
            }
            lir::Instruction::LoadMem(load) => {
                let mut free_regs = BTreeSet::from_iter(Amd64Reg::all_but_rsp_and_rbp());
                self.load_store_mem_occupied_regs(instrs, load)
                    .into_iter()
                    .for_each(|occupied_reg| {
                        free_regs.remove(&occupied_reg);
                    });

                // rev because eax is the last one, and we want to use it first because it is
                // scratch
                let mut free_regs = free_regs.into_iter().rev().collect();
                let mut spill_ctx = SpillContext::new(free_regs);
                let mut post_mov_instrs = vec![];

                use self::{lir::IndexComputation, Instruction::*};

                let src: SrcOperand = {
                    let src = load.mem_address_computation();
                    let (src, _) = self.lir_address_computation_to_register_address_computation(
                        instrs,
                        src,
                        &mut spill_ctx,
                    );
                    let src = SrcOperand::Mem(src);
                    src
                };

                let dst = {
                    let dst = load.operand();
                    let dst: DstOperand = self.lir_to_src_operand(dst, instrs).try_into().unwrap();
                    let dst: Amd64Reg = match dst {
                        // the dst is stored in a reg, this is fine
                        DstOperand::Reg(reg) => reg,
                        // the dst is stored in the activation record
                        DstOperand::Mem(ar_addr_comp) => {
                            // TODO @flip1996 SrcOperand::ActivationRecordEntry refactor
                            assert!(ar_addr_comp.base == Amd64Reg::Rbp);
                            assert!(ar_addr_comp.index.is_zero());
                            // To avoid mem mem move, use rax as scratch for result.
                            // Using rax without consulting free_regs is safe because
                            // because any usage as spill space for src
                            // ends during the execution of mov
                            post_mov_instrs.push(Mov(MovInstruction {
                                src: SrcOperand::Reg(Amd64Reg::Rax),
                                dst,
                                size: 8,
                            }));
                            Amd64Reg::Rax
                        }
                    };
                    DstOperand::Reg(dst)
                };

                // Emit the instructions
                // This whole function is just about the following statement
                let mut surrounded = vec![Mov(MovInstruction {
                    src,
                    dst,
                    size: load.size(),
                })];
                surrounded.extend(post_mov_instrs);
                instrs.extend(spill_ctx.emit_surrounded_instrs(surrounded));
            }
            lir::Instruction::Call { .. } => unreachable!("Call already converted"),
            lir::Instruction::Comment(_) => (),
        }
    }

    /// Given self.var_location and a spill_ctx, convert a given
    /// AddressComputation that uses lir::Operands into an
    /// AddressComputation that uses Amd64Reg.
    ///
    /// We need the spilling because it is not guaranteed that register
    /// allocation will place all Operands required for address computation
    /// (base, index) into registers.
    ///
    /// The returned vector contains the registers for those lir::Operands that
    /// did not require spilling to be moved to registers (i.e. register
    /// allocation already placed them in registers).
    fn lir_address_computation_to_register_address_computation(
        &mut self,
        instrs: &mut Vec<Instruction>,
        ac: lir::AddressComputation<lir::Operand>,
        spill_ctx: &mut SpillContext,
    ) -> (lir::AddressComputation<Amd64Reg>, Vec<Amd64Reg>) {
        let lir::AddressComputation {
            offset,
            base,
            index,
        } = ac;
        let mut already_in_registers = vec![];
        let base = self.lir_to_src_operand(base, instrs);
        let base: Amd64Reg = match base {
            // the base address of ac is stored in a reg, this is fine
            SrcOperand::Reg(reg) => {
                already_in_registers.push(reg);
                reg
            }
            SrcOperand::Imm(_) => unreachable!(),
            // base address of ac operand is stored in the AR, we need it in a register
            SrcOperand::Mem(ar_addr_comp) => {
                // TODO @flip1996 SrcOperand::ActivationRecordEntry refactor
                assert!(ar_addr_comp.base == Amd64Reg::Rbp);
                assert!(ar_addr_comp.index.is_zero());
                spill_ctx.spill_and_load_operand(base)
            }
        };
        use self::{lir::IndexComputation, Instruction::*};
        let index: IndexComputation<Amd64Reg> = {
            if let IndexComputation::Displacement(index, stride) = index {
                let index = self.lir_to_src_operand(index, instrs);
                match index {
                    // the index is stored in a reg, this is fine
                    SrcOperand::Reg(reg) => {
                        already_in_registers.push(reg);
                        IndexComputation::Displacement(reg, stride)
                    }
                    // the index is an immediate, move it to a spilled reg
                    SrcOperand::Imm(_) => {
                        let index = spill_ctx.spill_and_load_operand(index);
                        IndexComputation::Displacement(index, stride)
                    }
                    // the index is stored in the AR, we need it in a register
                    SrcOperand::Mem(ar_addr_comp) => {
                        // TODO @flip1996 SrcOperand::ActivationRecordEntry refactor
                        assert!(ar_addr_comp.base == Amd64Reg::Rbp);
                        assert!(ar_addr_comp.index.is_zero());
                        let index = spill_ctx.spill_and_load_operand(index);
                        IndexComputation::Displacement(index, stride)
                    }
                }
            } else {
                // the only other variant of IndexComputation
                IndexComputation::Zero
            }
        };
        let reg_ac = lir::AddressComputation {
            base,
            index,
            offset,
        };
        (reg_ac, already_in_registers)
    }

    fn lir_address_computation_operands_already_in_registers(
        &mut self,
        instrs: &mut Vec<Instruction>,
        ac: lir::AddressComputation<lir::Operand>,
    ) -> Vec<Amd64Reg> {
        let mut pseudo_spill = SpillContext::new(Vec::from_iter(Amd64Reg::all_but_rsp_and_rbp()));
        let (_, already_in_registers) = self
            .lir_address_computation_to_register_address_computation(instrs, ac, &mut pseudo_spill);
        already_in_registers
    }

    fn lir_operand_used_registers(
        &mut self,
        instrs: &mut Vec<Instruction>,
        op: lir::Operand,
    ) -> Vec<Amd64Reg> {
        self.lir_to_src_operand(op, instrs).used_regs()
    }

    fn load_store_mem_occupied_regs<I: LoadOrStoreMem>(
        &mut self,
        instrs: &mut Vec<Instruction>,
        load_or_store: &I,
    ) -> Vec<Amd64Reg> {
        let mut regs = vec![];
        let ac = load_or_store.mem_address_computation();
        regs.extend(self.lir_address_computation_operands_already_in_registers(instrs, ac));
        let op = load_or_store.operand();
        regs.extend(self.lir_operand_used_registers(instrs, op));
        regs
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
                    // This case is bad. There aren't any (mem, mem) ops.
                    // This means we need to spill one register, and that is Rax.
                    // `op mem, mem -> mem` => `op reg, mem`
                    // We want to move src2 -> dst, but first we need to move src2 -> %rax:
                    let spill = Amd64Reg::Rax;
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
                    instrs.push(Mov(MovInstruction {
                        src: src2,
                        dst: DstOperand::Reg(spill),
                        size: 8,
                    }));
                    // Now the instruction `op spill(src2), mem(dst == src1)`:
                    push_binop!(kind, SrcOperand::Reg(spill), dst);
                    // Now mem(dst == src1 OP src2)
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
                }
            },
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
                }
            },
        }
    }

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
                Location::Mem(idx) => SrcOperand::Mem(lir::AddressComputation {
                    offset: -((idx + self.num_saved_regs) as isize) * 8,
                    base: Amd64Reg::Rbp,
                    index: lir::IndexComputation::Zero,
                }),
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
                Location::Mem(idx) => SrcOperand::Mem(lir::AddressComputation {
                    offset: -((idx + self.num_saved_regs) as isize) * 8,
                    base: Amd64Reg::Rbp,
                    index: lir::IndexComputation::Zero,
                }),
                Location::ParamMem => SrcOperand::Mem(lir::AddressComputation {
                    offset: (idx as isize) * 8,
                    base: Amd64Reg::Rbp,
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

    fn arg_from_stack(&self, idx: usize, dst: DstOperand) -> Option<Instruction> {
        if idx < self.cconv.num_arg_regs() {
            return None;
        }
        let offset = ((idx + 1).checked_sub(self.cconv.num_arg_regs()).unwrap() * 8 + 8) as isize;
        Some(Instruction::Mov(MovInstruction {
            src: SrcOperand::Mem(lir::AddressComputation {
                offset,
                base: Amd64Reg::Rbp,
                index: lir::IndexComputation::Zero,
            }),
            dst,
            size: 8,
        }))
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

/// SpillContext tracks the temporary spilling of registers, which is usually
/// necessary to satisfy x86 operand constraints.
///
/// Registers from `free_regs` are eligible for spilling, i.e., being pushed
/// onto the stack, then reused for another SrcOperand, and later popped back to
/// their original value.
struct SpillContext {
    used_regs: HashSet<Amd64Reg>,
    free_regs: Box<Iterator<Item = Amd64Reg>>,
    spills: SaveRegList<Amd64Reg>,
    mov_to_spilled: Vec<Instruction>,
}

impl SpillContext {
    /// `free_regs` must return a register at most once
    /// It must not return `Amd64Reg::Rsp` or `Amd64Reg::Rbp`.
    fn new(free_regs: Vec<Amd64Reg>) -> Self {
        SpillContext {
            used_regs: HashSet::new(),
            free_regs: box free_regs.into_iter(),
            spills: SaveRegList::default(),
            mov_to_spilled: vec![],
        }
    }

    /// mark a register as spilled and load the given SrcOperand
    /// into that register, and return that register
    /// If Rax is returned from the free_regs iterator, it is not spilled
    /// because it is assumed to be a scratch register.
    ///
    /// No entry of `src.used_regs()` must ever be returned by free_regs.
    fn spill_and_load_operand(&mut self, src: SrcOperand) -> Amd64Reg {
        let spill = self.free_regs.next().unwrap();
        self.used_regs.insert(spill);

        src.used_regs().into_iter().for_each(|r| {
            let was_unused = self.used_regs.insert(r);
            debug_assert!(
                was_unused,
                "src operand depends on registers returned by free_regs"
            )
        });

        if spill != Amd64Reg::Rax {
            self.spills.add_regs(&[spill]);
        }
        self.mov_to_spilled.push(Instruction::Mov(MovInstruction {
            src,
            dst: DstOperand::Reg(spill),
            size: 8,
        }));
        spill
    }

    /// emit the spill instructions around those contained in `surrounded`
    fn emit_surrounded_instrs(self, surrounded: Vec<Instruction>) -> Vec<Instruction> {
        let mut instrs = vec![];
        instrs.extend(self.spills.saves().map(|r| Instruction::Pushq {
            src: SrcOperand::Reg(r),
        }));
        instrs.extend(self.mov_to_spilled);
        instrs.extend(surrounded);
        instrs.extend(self.spills.restores().map(|r| Instruction::Popq {
            dst: DstOperand::Reg(r),
        }));
        instrs
    }
}

trait OperandUsingRegs {
    fn used_regs(self) -> Vec<Amd64Reg>;
}

impl OperandUsingRegs for lir::AddressComputation<Amd64Reg> {
    fn used_regs(self) -> Vec<Amd64Reg> {
        self.operands() // FIXME move that here
    }
}

#[derive(Copy, Clone)]
pub(super) enum SrcOperand {
    Mem(lir::AddressComputation<Amd64Reg>),
    Reg(Amd64Reg),
    Imm(Tarval),
}

impl OperandUsingRegs for SrcOperand {
    fn used_regs(self) -> Vec<Amd64Reg> {
        match self {
            SrcOperand::Mem(ac) => ac.used_regs(),
            SrcOperand::Reg(reg) => vec![reg],
            SrcOperand::Imm(_) => vec![],
        }
    }
}

#[derive(Copy, Clone)]
pub(super) enum DstOperand {
    Mem(lir::AddressComputation<Amd64Reg>),
    Reg(Amd64Reg),
}

impl OperandUsingRegs for DstOperand {
    fn used_regs(self) -> Vec<Amd64Reg> {
        match self {
            DstOperand::Mem(ac) => ac.used_regs(),
            DstOperand::Reg(reg) => vec![reg],
        }
    }
}

/// A LoadOrStoreMem represents the commonalities of LoadMem and StoreMem
/// instructions:
trait LoadOrStoreMem {
    /// The address computation for the memory address (not a stack address
    /// computation!).
    fn mem_address_computation(&self) -> lir::AddressComputation<lir::Operand>;
    /// The operand into which data is loaded from mem, or the operand
    /// containing data to be stored to mem.
    fn operand(&self) -> lir::Operand;

    fn size(&self) -> u32;
}

impl LoadOrStoreMem for lir::LoadMem {
    // FIXME: rename this to mem_operand
    fn mem_address_computation(&self) -> lir::AddressComputation<lir::Operand> {
        self.src
    }
    // FIXME: rename this to reg_or_stack_operand
    fn operand(&self) -> lir::Operand {
        lir::Operand::Slot(self.dst)
    }
    fn size(&self) -> u32 {
        self.size
    }
}
impl LoadOrStoreMem for lir::StoreMem {
    fn mem_address_computation(&self) -> lir::AddressComputation<lir::Operand> {
        self.dst
    }
    fn operand(&self) -> lir::Operand {
        self.src
    }
    fn size(&self) -> u32 {
        self.size
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
