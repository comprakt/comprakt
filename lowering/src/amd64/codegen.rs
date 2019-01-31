use super::{
    function::{self, RegGraph, RegGraphMinLeftEdgeInstruction, RegToRegTransfer, SaveRegList},
    linear_scan::{self, Location},
    lir::{self, MultiSlot},
    live_variable_analysis,
    register::{Amd64Reg, Reg},
    var_id, CallingConv, Size, VarId,
};
use crate::lowering::lir_allocator::Ptr;
use itertools::Itertools;
use libfirm_rs::{nodes::NodeTrait, Tarval};
use std::{
    collections::{BTreeSet, HashMap, HashSet},
    convert::{TryFrom, TryInto},
    fmt,
    hash::{Hash, Hasher},
    iter::FromIterator,
};

pub(super) struct Codegen {
    var_location: HashMap<VarId, linear_scan::Location>,
    cconv: CallingConv,
    num_saved_regs: usize,
}

type Code = lir::Code<Mov, Mov, Instruction, Instruction>;

impl Codegen {
    pub(super) fn new(
        var_location: HashMap<VarId, linear_scan::Location>,
        cconv: CallingConv,
    ) -> Self {
        Self {
            var_location,
            cconv,
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

        let mut code_instrs = vec![];

        let mut block_iter = blocks.iter().peekable();
        while let Some(block) = block_iter.next() {
            let mut code = Code::default();
            code.body.push(Label {
                label: lir::gen_label(block.firm_num),
            });

            for instr in block.code.iter_unified() {
                use self::{lir::CodeInstruction as CI, live_variable_analysis as lva};
                match instr {
                    // Match over every instruction and generate amd64 instructions
                    CI::Body(lva::Instruction::Call(call)) => {
                        code.body.push(Comment {
                            comment: "call instruction".to_string(),
                        });
                        self.gen_call(call, &mut code.body);
                    }
                    CI::Body(lva::Instruction::Lir(lir)) => {
                        self.gen_lir(lir, &mut code.body);
                    }
                    CI::Leave(leave) => {
                        code.leave.push(Comment {
                            comment: "leave instruction".to_string(),
                        });
                        self.gen_leave(
                            leave,
                            block_iter.peek().map_or(-1, |block| block.firm_num),
                            &mut code.leave,
                        );
                    }
                    CI::CopyIn(lva::Mov { src, dst }) => {
                        self.gen_mov(*src, *dst, &mut code.copy_in);
                    }
                    CI::CopyOut(lva::Mov { src, dst }) => {
                        self.gen_mov(*src, *dst, &mut code.copy_out);
                    }
                }
            }

            code_instrs.push(code);
        }

        instrs.extend(self.resolve_copy_prop_cycles(&mut code_instrs));

        self.gen_function_epilog(function, &mut instrs);

        instrs
    }

    fn resolve_copy_prop_cycles(&self, code_instrs: &mut Vec<Code>) -> Vec<Instruction> {
        let mut instrs = vec![];
        for code in code_instrs {
            let mut code_body = code.body.iter().cloned().peekable();
            if let Some(Instruction::Label { .. }) = code_body.peek() {
                // Push the label of the block first
                instrs.push(code_body.next().unwrap());
            }
            sort_copy_prop(&code.copy_in, &mut instrs);
            instrs.extend(code_body);
            sort_copy_prop(&code.copy_out, &mut instrs);
            instrs.extend(code.leave.iter().cloned());
        }

        instrs
    }

    fn gen_meta_comments(&self, instrs: &mut Vec<Instruction>) {
        use self::Instruction::Comment;

        instrs.push(Comment {
            comment: format!("Calling convention: {:?}", self.cconv),
        });
        for (id, location) in self.var_location.iter().sorted_by_key(|(id, _)| *id) {
            instrs.push(Comment {
                comment: format!("Var {:?} in {:?}", id, location),
            });
        }
    }

    fn stack_slots_space(&mut self, function: &function::Function) -> Tarval {
        let space = (function.num_stackslots as i64).checked_mul(8).unwrap();
        Tarval::mj_int(space)
    }

    fn gen_function_prolog(
        &mut self,
        function: &function::Function,
        instrs: &mut Vec<Instruction>,
    ) {
        use self::Instruction::{Comment, Mov, Pushq, Sub};

        instrs.push(Comment {
            comment: "function prolog".to_string(),
        });
        for instr in &function.prolog {
            match instr {
                function::FnInstruction::Pushq { src } => {
                    let src = self.fn_to_src_operand(*src);
                    instrs.push(Pushq {
                        src: src.into_size(Size::Eight),
                    });
                }
                function::FnInstruction::Movq { src, dst } => {
                    let src = self.fn_to_src_operand(*src);
                    let dst = self.fn_to_src_operand(*dst).try_into().unwrap();
                    instrs.push(Mov(MovInstruction {
                        src,
                        dst,
                        comment: "fn prolog".to_string(),
                    }));
                }
                _ => unreachable!(),
            }
        }

        instrs.push(Comment {
            comment: "function save regs".to_string(),
        });
        for src in function.saved_regs.saves() {
            let src = SrcOperand::Reg(Reg {
                size: Size::Eight,
                reg: src,
            });
            instrs.push(Pushq { src });
        }

        instrs.push(Comment {
            comment: "function allocate stack slots".to_string(),
        });
        instrs.push(Sub {
            subtrahend: SrcOperand::Imm(self.stack_slots_space(function)),
            acc: DstOperand::Reg(Reg {
                size: Size::Eight,
                reg: Amd64Reg::Sp,
            }),
        });
    }

    fn gen_function_epilog(
        &mut self,
        function: &function::Function,
        instrs: &mut Vec<Instruction>,
    ) {
        use self::Instruction::{Add, Comment, Leave, Popq, Ret};

        instrs.push(Comment {
            comment: "function de-allocate stack slots".to_string(),
        });
        instrs.push(Add {
            src: SrcOperand::Imm(self.stack_slots_space(function)),
            dst: DstOperand::Reg(Reg {
                size: Size::Eight,
                reg: Amd64Reg::Sp,
            }),
        });

        instrs.push(Comment {
            comment: "function restore regs".to_string(),
        });
        for dst in function.saved_regs.restores() {
            let dst = DstOperand::Reg(Reg {
                size: Size::Eight,
                reg: dst,
            });
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
        instrs: &mut Vec<Mov>,
    ) {
        let src = self.lir_to_src_operand(src.into());
        let dst = self
            .lir_to_src_operand(lir::Operand::Slot(dst))
            .try_into()
            .unwrap();
        instrs.push(Mov { src, dst });
    }

    fn gen_lir(&mut self, lir: &lir::Instruction, instrs: &mut Vec<Instruction>) {
        use self::Instruction::{Load, Mov, Store};

        log::debug!("Gen lir: {:?}", lir);
        match lir {
            lir::Instruction::LoadParam { idx, size } => self.gen_load_param(*idx, *size, instrs),
            lir::Instruction::Binop {
                kind,
                src1,
                src2,
                dst,
            } => self.gen_binop(instrs, kind, src1, src2, *dst),
            lir::Instruction::Div { src1, src2, dst } => self.gen_div(
                src1,
                src2,
                *dst,
                |dst| {
                    Mov(MovInstruction {
                        src: SrcOperand::Reg(Reg {
                            size: dst.size(),
                            reg: Amd64Reg::A,
                        }),
                        dst,
                        comment: "div result".to_string(),
                    })
                },
                instrs,
            ),
            lir::Instruction::Mod { src1, src2, dst } => self.gen_div(
                src1,
                src2,
                *dst,
                |dst| {
                    Mov(MovInstruction {
                        src: SrcOperand::Reg(Reg {
                            size: dst.size(),
                            reg: Amd64Reg::D,
                        }),
                        dst,
                        comment: "mod result".to_string(),
                    })
                },
                instrs,
            ),
            lir::Instruction::Conv { src, dst } => {
                let src = self.lir_to_src_operand(*src);
                let dst = self
                    .lir_to_src_operand(lir::Operand::Slot(*dst))
                    .try_into()
                    .unwrap();
                let src = match (src, dst) {
                    (SrcOperand::Ar(_), DstOperand::Ar(_)) => {
                        let spill = Reg {
                            size: Size::Eight,
                            reg: Amd64Reg::A,
                        };
                        instrs.push(Mov(MovInstruction {
                            src,
                            dst: DstOperand::Reg(spill),
                            comment: "conv spill".to_string(),
                        }));
                        SrcOperand::Reg(spill)
                    }
                    _ => src,
                };
                instrs.push(Mov(MovInstruction {
                    src,
                    dst,
                    comment: "conv".to_string(),
                }))
            }
            lir::Instruction::Unop { kind, src, dst } => self.gen_unop(instrs, kind, src, *dst),
            lir::Instruction::StoreMem(store) => {
                let mut free_regs = BTreeSet::from_iter(Amd64Reg::all_but_rsp_and_rbp());
                self.load_store_mem_occupied_regs(store)
                    .into_iter()
                    .for_each(|occupied_reg| {
                        free_regs.remove(&occupied_reg);
                    });

                // rev because rax is the last in all_but_rsp_and_rbp
                // but we want it first because spill_ctx doesn't spill for it
                let free_regs = free_regs.into_iter().rev().collect();
                let mut spill_ctx = SpillContext::new(free_regs);

                let dst: lir::AddressComputation<Reg> = {
                    let dst = store.mem_address_computation();
                    let (dst, _) = self.lir_address_computation_to_register_address_computation(
                        dst,
                        &mut spill_ctx,
                    );
                    dst
                };

                let src: SrcOperand = {
                    let src = store.operand();
                    let src: SrcOperand = self.lir_to_src_operand(src);
                    match src {
                        // the src is stored in a reg, this is fine
                        SrcOperand::Reg(_) => src,
                        SrcOperand::Imm(_) => src,
                        // the source is stored in the activation record, move to scratch
                        SrcOperand::Ar(_) => {
                            let src = spill_ctx.spill_and_load_operand(src);
                            SrcOperand::Reg(src)
                        }
                    }
                };

                // Emit the instructoins
                // This whole function is just about the following statement
                let surrounded = vec![Store(StoreInstruction { src, dst })];
                instrs.extend(spill_ctx.emit_surrounded_instrs(surrounded));
            }
            lir::Instruction::LoadMem(load) => {
                let mut free_regs = BTreeSet::from_iter(Amd64Reg::all_but_rsp_and_rbp());
                self.load_store_mem_occupied_regs(load)
                    .into_iter()
                    .for_each(|occupied_reg| {
                        free_regs.remove(&occupied_reg);
                    });

                // rev because rax is the last in all_but_rsp_and_rbp
                // but we want it first because spill_ctx doesn't spill for it
                let free_regs = free_regs.into_iter().rev().collect();
                let mut spill_ctx = SpillContext::new(free_regs);
                let mut post_mov_instrs = vec![];

                let src: lir::AddressComputation<Reg> = {
                    let src = load.mem_address_computation();
                    let (src, _) = self.lir_address_computation_to_register_address_computation(
                        src,
                        &mut spill_ctx,
                    );
                    src
                };

                let dst = {
                    let dst = load.operand();
                    let dst: DstOperand = self.lir_to_src_operand(dst).try_into().unwrap();
                    match dst {
                        // the dst is stored in a reg, this is fine
                        DstOperand::Reg(reg) => reg,
                        // the dst is stored in the activation record
                        DstOperand::Ar(_) => {
                            // To avoid mem mem move, use rax as scratch for result.
                            // Using rax without consulting free_regs is safe because
                            // because any usage as spill space for src
                            // ends during the execution of mov
                            let spill = Reg {
                                size: dst.size(),
                                reg: Amd64Reg::A,
                            };
                            post_mov_instrs.push(Instruction::Mov(MovInstruction {
                                src: SrcOperand::Reg(spill),
                                dst,
                                comment: "load dst mem from rax".to_string(),
                            }));
                            spill
                        }
                    }
                };

                // Emit the instructions
                // This whole function is just about the following statement
                let mut surrounded = vec![Load(LoadInstruction { src, dst })];
                surrounded.extend(post_mov_instrs);
                instrs.extend(spill_ctx.emit_surrounded_instrs(surrounded));
            }
            lir::Instruction::Call { .. } => unreachable!("Call already converted"),
            lir::Instruction::Comment(_) => (),
        }
    }

    fn gen_load_param(&self, idx: u32, size: u32, instrs: &mut Vec<Instruction>) {
        use self::Instruction::{Comment, Mov};

        let param = &self.var_location[&var_id(lir::Operand::Param { idx, size })];
        let size = size.try_into().unwrap();
        match param {
            // Register allocation placed argument into a register.
            Location::Reg(reg) => {
                // However, calling convention might still dictate that the argument is placed
                // on the stack.
                let reg = Reg { size, reg: *reg };
                if let Some(instr) = self.arg_from_stack(idx as usize, DstOperand::Reg(reg)) {
                    instrs.push(Instruction::Comment {
                        comment: "move stack arg in reg".to_string(),
                    });
                    instrs.push(instr);
                    instrs.push(Mov(MovInstruction {
                        src: SrcOperand::Reg(reg),
                        dst: DstOperand::Reg(Reg {
                            size: Size::Eight,
                            reg: reg.reg,
                        }),
                        comment: "sign extend stack arg".to_string(),
                    }));
                }
            }
            // Register allocation placed argument into the AR, but the calling convetion placed
            // the argument into a register, so move it to the AR slot.
            Location::Ar(i) => {
                let arg_reg = Amd64Reg::arg(idx as usize);
                instrs.push(Comment {
                    comment: format!("spill argument register {}", arg_reg),
                });
                instrs.push(Mov(MovInstruction {
                    src: SrcOperand::Reg(Reg { size, reg: arg_reg }),
                    dst: DstOperand::Ar(-((*i + 1 + self.num_saved_regs) as isize)),
                    comment: "move param to stack".to_string(),
                }));
            }
            // param is already on the stack and stays there
            Location::ParamMem => (),
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
        ac: lir::AddressComputation<lir::Operand>,
        spill_ctx: &mut SpillContext,
    ) -> (lir::AddressComputation<Reg>, Vec<Amd64Reg>) {
        use self::Instruction::Mov;

        let lir::AddressComputation {
            offset,
            base,
            index,
        } = ac;
        let mut already_in_registers = vec![];
        let base = self.lir_to_src_operand(base);
        let base: Reg = match base {
            // the base address of ac is stored in a reg, this is fine
            SrcOperand::Reg(reg) => {
                assert!(reg.size() == Size::Eight); // must be a pointer
                already_in_registers.push(reg.reg);
                reg
            }
            SrcOperand::Imm(tv) => {
                assert!(tv.is_long());
                if tv.get_long() == 0 {
                    // some operation relative to the null pointer
                    // let's use the undefined behavior here
                    // for some funny stuff
                    Reg {
                        size: Size::Eight,
                        reg: Amd64Reg::Sp,
                    }
                } else {
                    // hardcoded addresses are impossible in MJ
                    // and with our optimizations
                    unreachable!();
                }
            }
            // base address of ac operand is stored in the AR, we need it in a register
            SrcOperand::Ar(_) => spill_ctx.spill_and_load_operand(base),
        };
        use self::lir::IndexComputation;
        let index: IndexComputation<Reg> = {
            if let IndexComputation::Displacement(index, stride) = index {
                let index = self.lir_to_src_operand(index);
                match index {
                    // the index is stored in a reg, this is fine
                    SrcOperand::Reg(reg) => {
                        already_in_registers.push(reg.reg);
                        let sign_extended = Reg {
                            size: Size::Eight,
                            reg: reg.reg,
                        };
                        spill_ctx.sign_extension.push(Mov(MovInstruction {
                            src: index,
                            dst: DstOperand::Reg(sign_extended),
                            comment: "load store sign extension".to_string(),
                        }));
                        IndexComputation::Displacement(sign_extended, stride)
                    }
                    // the index is an immediate, move it to a spilled reg
                    SrcOperand::Imm(_) => {
                        let index = spill_ctx.spill_and_load_operand(index);
                        IndexComputation::Displacement(index, stride)
                    }
                    // the index is stored in the AR, we need it in a register
                    SrcOperand::Ar(_) => {
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
        ac: lir::AddressComputation<lir::Operand>,
    ) -> Vec<Amd64Reg> {
        let mut pseudo_spill = SpillContext::new(Vec::from_iter(Amd64Reg::all_but_rsp_and_rbp()));
        let (_, already_in_registers) =
            self.lir_address_computation_to_register_address_computation(ac, &mut pseudo_spill);
        already_in_registers
    }

    fn lir_operand_used_registers(&mut self, op: lir::Operand) -> Vec<Amd64Reg> {
        self.lir_to_src_operand(op).used_regs()
    }

    fn load_store_mem_occupied_regs<I: LoadOrStoreMem>(
        &mut self,
        load_or_store: &I,
    ) -> Vec<Amd64Reg> {
        let mut regs = vec![];
        let ac = load_or_store.mem_address_computation();
        regs.extend(self.lir_address_computation_operands_already_in_registers(ac));
        let op = load_or_store.operand();
        regs.extend(self.lir_operand_used_registers(op));
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
                    lir::BinopKind::Add => instrs.push(Add {
                        src: $src,
                        dst: $dst,
                    }),
                    lir::BinopKind::Mul => instrs.push(Mul {
                        src: $src,
                        dst: $dst,
                    }),
                    lir::BinopKind::And => instrs.push(And {
                        src: $src,
                        dst: $dst,
                    }),
                    lir::BinopKind::Or => instrs.push(Or {
                        src: $src,
                        dst: $dst,
                    }),
                    lir::BinopKind::Xor => instrs.push(Xor {
                        src: $src,
                        dst: $dst,
                    }),
                    lir::BinopKind::Sub => instrs.push(Sub {
                        subtrahend: $src,
                        acc: $dst,
                    }),
                }
            }};
        }

        let src1 = self.lir_to_src_operand(*src1);
        let src2 = self.lir_to_src_operand(*src2);
        let dst = self
            .lir_to_src_operand(lir::Operand::Slot(dst))
            .try_into()
            .unwrap();
        match dst {
            DstOperand::Reg(dst_reg) => {
                // When the dst is a Reg, we just have to move the second operand in this
                // Reg. It doesn't matter if the `src`s are Reg, Mem or Imm
                if let SrcOperand::Reg(src2_reg) = src2 {
                    if src2_reg == dst_reg {
                        push_binop!(kind, src1, dst);
                        if let lir::BinopKind::Sub = kind {
                            // sub is the only binop that is not commutative
                            // OP = -
                            // and in fact, we wanted to compute dst = src1 OP src2
                            // but we computed src2 OP src1
                            //
                            // Let's use: src1-src2 = -(src2-src1)
                            instrs.push(Neg { src: dst })
                        }
                    } else {
                        instrs.push(Mov(MovInstruction {
                            src: src1,
                            dst,
                            comment: "binop setup".to_string(),
                        }));
                        push_binop!(kind, src2, dst);
                    }
                } else {
                    instrs.push(Mov(MovInstruction {
                        src: src1,
                        dst,
                        comment: "binop setup".to_string(),
                    }));
                    push_binop!(kind, src2, dst);
                }
            }
            DstOperand::Ar(_) => {
                let dst_spill = if let lir::BinopKind::Mul = kind {
                    DstOperand::Reg(Reg {
                        size: dst.size(),
                        reg: Amd64Reg::A,
                    })
                } else {
                    dst
                };
                match (src1, src2) {
                    // Enforce that we always get `op reg/imm, mem`
                    (SrcOperand::Reg(_), SrcOperand::Reg(_))
                    | (SrcOperand::Reg(_), SrcOperand::Imm(_))
                    | (SrcOperand::Imm(_), SrcOperand::Reg(_))
                    | (SrcOperand::Imm(_), SrcOperand::Imm(_)) => {
                        instrs.push(Mov(MovInstruction {
                            src: src1,
                            dst: dst_spill,
                            comment: "binop setup".to_string(),
                        }));
                        push_binop!(kind, src2, dst_spill);
                    }
                    (SrcOperand::Ar(_), SrcOperand::Ar(_)) => {
                        // This case is bad. There aren't any (mem, mem) ops.
                        // This means we need to spill one register, and that is Rax.
                        // `op mem, mem -> mem` => `op reg, mem`
                        // We want to move src2 -> dst, but first we need to move src2 -> %rax:
                        // TODO: We can't use %rax here if it's a mul instr
                        let spill = Reg {
                            size: Size::Eight,
                            reg: Amd64Reg::A,
                        };
                        instrs.push(Mov(MovInstruction {
                            src: src1,
                            dst: DstOperand::Reg(spill),
                            comment: "binop 3xmem setup".to_string(),
                        }));
                        // Now we can move %rax -> dst
                        instrs.push(Mov(MovInstruction {
                            src: SrcOperand::Reg(spill),
                            dst: dst_spill,
                            comment: "binop 3xmem setup".to_string(),
                        }));
                        // We're now at a 2-address code state: `op mem, mem`
                        // Now we need to move src2 -> %rax
                        instrs.push(Mov(MovInstruction {
                            src: src2,
                            dst: DstOperand::Reg(spill),
                            comment: "binop setup".to_string(),
                        }));
                        // Now the instruction `op %rax, mem`:
                        push_binop!(kind, SrcOperand::Reg(spill), dst_spill);
                    }
                    // src1 is Reg or Imm, src2 is Mem
                    (SrcOperand::Reg(_), SrcOperand::Ar(_))
                    | (SrcOperand::Imm(_), SrcOperand::Ar(_)) => {
                        // move src1 into accumulator
                        // (using src1, not src2 for correct sub support)
                        instrs.push(Mov(MovInstruction {
                            src: src1,
                            dst: dst_spill,
                            comment: "binop setup".to_string(),
                        }));
                        // TODO: We can't use %rax here if it's a mul instr
                        let spill = Reg {
                            size: Size::Eight,
                            reg: Amd64Reg::A,
                        };
                        instrs.push(Mov(MovInstruction {
                            src: src2,
                            dst: DstOperand::Reg(spill),
                            comment: "binop spill".to_string(),
                        }));
                        // Now the instruction `op spill(src2), mem(dst == src1)`:
                        push_binop!(kind, SrcOperand::Reg(spill), dst_spill);
                        // Now mem(dst == src1 OP src2)
                    }
                    // src1 is Mem, src2 is Reg or Imm
                    (SrcOperand::Ar(_), SrcOperand::Reg(_))
                    | (SrcOperand::Ar(_), SrcOperand::Imm(_)) => {
                        // The same approach like above, but with src1 and src2 swapped
                        // and a special case for `sub` because it's not commutative (see `if`
                        // comment)
                        instrs.push(Mov(MovInstruction {
                            src: src2,
                            dst: dst_spill,
                            comment: "binop setup".to_string(),
                        }));
                        // TODO: We can't use %rax here if it's a mul instr
                        let spill = Reg {
                            size: Size::Eight,
                            reg: Amd64Reg::A,
                        };
                        instrs.push(Mov(MovInstruction {
                            src: src1,
                            dst: DstOperand::Reg(spill),
                            comment: "binop spill".to_string(),
                        }));
                        // Now the instruction `op spill(src1), mem(dst == src2)`:
                        push_binop!(kind, SrcOperand::Reg(spill), dst_spill);
                        // Now mem(dst == src2 OP src1)
                        if let lir::BinopKind::Sub = kind {
                            // sub is the only binop that is not commutative
                            // OP = -
                            // and in fact, we wanted to compute dst = src1 OP src2
                            // but we computed src2 OP src1
                            //
                            // Let's use: src1-src2 = -(src2-src1)
                            instrs.push(Neg { src: dst_spill })
                        }
                    }
                }
                if let lir::BinopKind::Mul = kind {
                    instrs.push(Mov(MovInstruction {
                        src: SrcOperand::Reg(Reg {
                            size: dst.size(),
                            reg: Amd64Reg::A,
                        }),
                        dst,
                        comment: "imul dst mem".to_string(),
                    }));
                }
            }
        }
    }

    fn gen_unop(
        &mut self,
        instrs: &mut Vec<Instruction>,
        kind: &lir::UnopKind,
        src: &lir::Operand,
        dst: Ptr<MultiSlot>,
    ) {
        use self::Instruction::{Mov, Neg, Not};

        macro_rules! push_unop {
            ($kind:expr, $dst:expr) => {{
                match $kind {
                    lir::UnopKind::Not => instrs.push(Not { src: $dst }),
                    lir::UnopKind::Neg => instrs.push(Neg { src: $dst }),
                }
            }};
        }

        let src = self.lir_to_src_operand(*src);
        let dst = self
            .lir_to_src_operand(lir::Operand::Slot(dst))
            .try_into()
            .unwrap();
        match dst {
            DstOperand::Reg(_) => {
                instrs.push(Mov(MovInstruction {
                    src,
                    dst,
                    comment: "unop setup".to_string(),
                }));
                push_unop!(kind, dst);
            }
            DstOperand::Ar(_) => match src {
                SrcOperand::Reg(_) | SrcOperand::Imm(_) => {
                    instrs.push(Mov(MovInstruction {
                        src,
                        dst,
                        comment: "unop setup".to_string(),
                    }));
                    push_unop!(kind, dst);
                }
                SrcOperand::Ar(_) => {
                    debug_assert_eq!(src.size(), dst.size());
                    let spill = Reg {
                        size: src.size(),
                        reg: Amd64Reg::A,
                    };
                    // src -> %rax
                    instrs.push(Mov(MovInstruction {
                        src,
                        dst: DstOperand::Reg(spill),
                        comment: "unop spill".to_string(),
                    }));
                    // Now the instruction `op %rax`:
                    push_unop!(kind, DstOperand::Reg(spill));
                    // %rax -> dst
                    instrs.push(Mov(MovInstruction {
                        src: SrcOperand::Reg(spill),
                        dst,
                        comment: "unop spill to dst".to_string(),
                    }));
                }
            },
        }
    }

    fn gen_div<F>(
        &mut self,
        src1: &lir::Operand,
        src2: &lir::Operand,
        dst: Ptr<MultiSlot>,
        f: F,
        instrs: &mut Vec<Instruction>,
    ) where
        F: FnOnce(DstOperand) -> Instruction,
    {
        use self::Instruction::*;
        instrs.push(Comment {
            comment: "div instr start".to_string(),
        });
        // `idivq %r` divides %rdx:%rax by %r
        // the quotient is stored in %rax, the remainder in %rdx

        let dst_operand_used_regs = |op| match op {
            DstOperand::Ar(_) => vec![Amd64Reg::Bp],
            DstOperand::Reg(reg) => vec![reg.reg],
        };

        let mut free_regs = BTreeSet::from_iter(Amd64Reg::all_but_rsp_and_rbp());
        free_regs.remove(&Amd64Reg::A);
        free_regs.remove(&Amd64Reg::D);
        self.lir_to_src_operand(lir::Operand::Slot(dst))
            .try_into()
            .map(dst_operand_used_regs)
            .unwrap_or_else(|_| vec![])
            .into_iter()
            .for_each(|occupied_reg| {
                free_regs.remove(&occupied_reg);
            });
        let mut free_regs = free_regs.into_iter();

        let mut setup_instr = None;
        let mut spills = SaveRegList::default();
        let mut post_div_instr = None;
        let mut rdx_spilled = false;

        let dst: DstOperand = self
            .lir_to_src_operand(lir::Operand::Slot(dst))
            .try_into()
            .unwrap();

        let src1 = self.lir_to_src_operand(*src1);
        match (src1, dst) {
            // We don't need to spill src1, if we overide it anyway
            (SrcOperand::Reg(x), DstOperand::Reg(y)) if x == y => (),
            (
                SrcOperand::Reg(Reg {
                    reg: Amd64Reg::D, ..
                }),
                _,
            ) => {
                // When src1 is in %rdx it would be overidden by cqto
                spills.add_regs(&[Amd64Reg::D]);
                rdx_spilled = true;
            }
            _ => (),
        }

        let src2 = self.lir_to_src_operand(*src2);
        let src2 = match (src2, dst) {
            (
                SrcOperand::Reg(Reg {
                    reg: Amd64Reg::D, ..
                }),
                DstOperand::Reg(Reg {
                    reg: Amd64Reg::D, ..
                }),
            )
            | (SrcOperand::Imm(_), _) => {
                debug_assert_eq!(src2.size(), dst.size());
                let spill = free_regs.next().unwrap();
                spills.add_regs(&[spill]);
                setup_instr = Some(Mov(MovInstruction {
                    src: src2,
                    dst: DstOperand::Reg(Reg {
                        size: Size::Eight,
                        reg: spill,
                    }),
                    comment: "div move Imm/%rdx to spill".to_string(),
                }));
                SrcOperand::Reg(Reg {
                    size: Size::Eight,
                    reg: spill,
                })
            }
            // When src2 is in %rdx it would be overidden by cqto
            (
                SrcOperand::Reg(Reg {
                    reg: Amd64Reg::D, ..
                }),
                _,
            ) => {
                let spill = free_regs.next().unwrap();
                spills.add_regs(&[spill]);
                setup_instr = Some(Mov(MovInstruction {
                    src: src2,
                    dst: DstOperand::Reg(Reg {
                        size: Size::Eight,
                        reg: spill,
                    }),
                    comment: "div spill src2 from %rdx".to_string(),
                }));
                // src2 can be easily recovered by moving it out of the spill register since
                // this register won't be modified by the div instruction
                post_div_instr = Some(Mov(MovInstruction {
                    src: SrcOperand::Reg(Reg {
                        size: Size::Eight,
                        reg: spill,
                    }),
                    dst: DstOperand::Reg(Reg {
                        size: Size::Eight,
                        reg: Amd64Reg::D,
                    }),
                    comment: "div move src2 back to %rdx".to_string(),
                }));
                rdx_spilled = true;
                SrcOperand::Reg(Reg {
                    size: Size::Eight,
                    reg: spill,
                })
            }
            (SrcOperand::Reg(reg), _) => {
                let sign_extended = Reg {
                    size: Size::Eight,
                    reg: reg.reg,
                };
                setup_instr = Some(Mov(MovInstruction {
                    src: src2,
                    dst: DstOperand::Reg(sign_extended),
                    comment: "div sign extend src2".to_string(),
                }));
                SrcOperand::Reg(sign_extended)
            }
            (SrcOperand::Ar(_), _) => src2,
        };
        let src2 = src2.try_into().unwrap();

        // When the dst register is %rdx, we don't need to spill it, since it will be
        // overidden anyway in any other case we need to spill %rdx, since it could be
        // used by another var unrelated to the div instruction
        match dst {
            _ if rdx_spilled => (),
            DstOperand::Reg(Reg {
                reg: Amd64Reg::D, ..
            }) => (),
            DstOperand::Reg(_) | DstOperand::Ar(_) => spills.add_regs(&[Amd64Reg::D]),
        }

        instrs.extend(spills.saves().map(|reg| Pushq {
            src: SrcOperand::Reg(Reg {
                size: Size::Eight,
                reg,
            }),
        }));
        // Move src1 to %rax
        instrs.push(Mov(MovInstruction {
            src: src1,
            dst: DstOperand::Reg(Reg {
                size: Size::Eight,
                reg: Amd64Reg::A,
            }),
            comment: "div move src1 to %rax".to_string(),
        }));
        if let Some(instr) = setup_instr {
            // This moves src2 to spill register
            instrs.push(instr);
        }
        // sign extend
        instrs.push(Cqto);
        // div instructions writes quotient in %rax and remainder in %rdx
        instrs.push(Divq { src: src2 });
        // write result to dst
        instrs.push(f(dst));
        // recover src2, if it was moved from %rdx into a spill register
        if let Some(instr) = post_div_instr {
            instrs.push(instr);
        }
        // recover pushed spill registers
        instrs.extend(spills.restores().map(|reg| Popq {
            dst: DstOperand::Reg(Reg {
                size: Size::Eight,
                reg,
            }),
        }));
        instrs.push(Comment {
            comment: "div instr end".to_string(),
        });
    }

    fn gen_call(&mut self, call: &function::FunctionCall, instrs: &mut Vec<Instruction>) {
        use self::Instruction::{Add, Call, Comment, Mov, Popq, Pushq};

        instrs.push(Comment {
            comment: "call save args".to_string(),
        });
        for src in call.saved_regs.saves() {
            let src = SrcOperand::Reg(Reg {
                size: Size::Eight,
                reg: src,
            });
            instrs.push(Pushq { src });
        }

        instrs.push(Comment {
            comment: "call setup".to_string(),
        });
        for instr in &call.setup {
            match instr {
                function::FnInstruction::Movq { src, dst } => {
                    let src = self.fn_to_src_operand(*src);
                    let dst = self.fn_to_src_operand(*dst).try_into().unwrap();
                    instrs.push(Mov(MovInstruction {
                        src,
                        dst,
                        comment: "call setup move to arg reg".to_string(),
                    }));
                }
                function::FnInstruction::Pushq { src } => {
                    let src = self.fn_to_src_operand(*src);
                    instrs.push(Pushq { src });
                }
                function::FnInstruction::Popq { dst } => {
                    let dst = self.fn_to_src_operand(*dst).try_into().unwrap();
                    instrs.push(Popq { dst });
                }
                _ => unreachable!(),
            }
        }

        instrs.push(Call(CallInstruction {
            label: call.label.clone(),
        }));

        if let Some(function::FnInstruction::Addq { src, dst }) = call.recover {
            instrs.push(Comment {
                comment: "call restore %rsp".to_string(),
            });
            instrs.push(Add {
                src: SrcOperand::Imm(src),
                dst: DstOperand::Reg(dst),
            });
        }

        instrs.push(Comment {
            comment: "call recover args".to_string(),
        });
        for dst in call.saved_regs.restores() {
            assert_ne!(dst, Amd64Reg::A);
            let dst = DstOperand::Reg(Reg {
                size: Size::Eight,
                reg: dst,
            });
            instrs.push(Popq { dst });
        }

        if let Some(function::FnInstruction::Movq { src, dst }) = call.move_res {
            instrs.push(Comment {
                comment: "call move from %rax".to_string(),
            });
            let src = self.fn_to_src_operand(src);
            assert_eq!(
                DstOperand::Reg(Reg {
                    size: src.size(),
                    reg: Amd64Reg::A,
                }),
                src.try_into().unwrap()
            );
            let dst = self.fn_to_src_operand(dst).try_into().unwrap();
            instrs.push(Mov(MovInstruction {
                src,
                dst,
                comment: "call move result from %rax".to_string(),
            }));
        }
    }

    fn gen_leave(
        &mut self,
        leave: &lir::Leave,
        next_block_num: i64,
        instrs: &mut Vec<Instruction>,
    ) {
        use self::Instruction::{Cmp, Jmp, Mov};

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

                let lhs = self.lir_to_src_operand(*lhs);
                let rhs = self.lir_to_src_operand(*rhs);
                let op = match (lhs, rhs) {
                    (SrcOperand::Reg(_), SrcOperand::Ar(_))
                    | (_, SrcOperand::Reg(_))
                    | (SrcOperand::Imm(_), SrcOperand::Ar(_)) => {
                        instrs.push(Cmp {
                            subtrahend: lhs,
                            minuend: rhs.try_into().unwrap(),
                        });
                        // swap op to account for GAS syntax
                        op.swap()
                    }

                    (SrcOperand::Reg(_), SrcOperand::Imm(_))
                    | (SrcOperand::Ar(_), SrcOperand::Imm(_)) => {
                        // only the subtrahend is allowed to be immediate
                        // thus, swap rhs and lhs on their subtrahend / minuend position
                        instrs.push(Cmp {
                            subtrahend: rhs,
                            minuend: lhs.try_into().unwrap(),
                        });
                        // do not swap op for GAS, we already swapped rhs and lhs
                        *op
                    }
                    (SrcOperand::Ar(_), SrcOperand::Ar(_))
                    | (SrcOperand::Imm(_), SrcOperand::Imm(_)) => {
                        // This case is bad we don't know if there is a free
                        // register left. So we have to spill one register.
                        let spill = Reg {
                            size: rhs.size(),
                            reg: Amd64Reg::A,
                        };
                        // if we are in (Imm,Imm), lhs must stay Imm
                        instrs.push(Mov(MovInstruction {
                            src: rhs,
                            dst: DstOperand::Reg(spill),
                            comment: "cond_jmp move rhs to spill".to_string(),
                        }));
                        instrs.push(Cmp {
                            subtrahend: lhs,
                            minuend: DstOperand::Reg(spill),
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
                    let src = self.lir_to_src_operand(*value);
                    instrs.push(Mov(MovInstruction {
                        src,
                        dst: DstOperand::Reg(Reg {
                            size: Size::Eight,
                            reg: Amd64Reg::A,
                        }),
                        comment: "return move result to %rax".to_string(),
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

    fn lir_to_src_operand(&self, op: lir::Operand) -> SrcOperand {
        match op {
            lir::Operand::Imm(c) => SrcOperand::Imm(c),
            lir::Operand::Slot(slot) => match self.var_location[&var_id(op)] {
                Location::Reg(reg) => SrcOperand::Reg(Reg {
                    size: slot.firm().mode().size_bytes().try_into().unwrap(),
                    reg,
                }),
                Location::Ar(idx) => SrcOperand::Ar(-((idx + 1 + self.num_saved_regs) as isize)),
                Location::ParamMem => unreachable!("a slot never has a ParamMem location"),
            },
            lir::Operand::Param { idx, size } => match self.var_location[&var_id(op)] {
                Location::Reg(reg) => SrcOperand::Reg(Reg {
                    size: size.try_into().unwrap(),
                    reg,
                }),
                // This can happen when the param was originally in a register but got moved on
                // the stack.
                Location::Ar(idx) => SrcOperand::Ar(-((idx + 1 + self.num_saved_regs) as isize)),
                Location::ParamMem => SrcOperand::Ar(
                    (idx.checked_sub(self.cconv.num_arg_regs() as u32).unwrap() as isize) + 2,
                ),
            },
        }
    }

    fn fn_to_src_operand(&self, op: function::FnOperand) -> SrcOperand {
        match op {
            function::FnOperand::Lir(lir) => self.lir_to_src_operand(lir),
            function::FnOperand::Reg(reg) => SrcOperand::Reg(Reg {
                size: Size::Eight,
                reg,
            }),
        }
    }

    fn arg_from_stack(&self, idx: usize, dst: DstOperand) -> Option<Instruction> {
        if idx < self.cconv.num_arg_regs() {
            return None;
        }
        let effective_idx = idx.checked_sub(self.cconv.num_arg_regs()).unwrap();
        let idx = (effective_idx + 2) as isize;
        Some(Instruction::Mov(MovInstruction {
            src: SrcOperand::Ar(idx),
            dst,
            comment: "stack args move in register".to_string(),
        }))
    }
}

#[derive(Clone)]
pub(super) enum Instruction {
    Mov(MovInstruction),
    Load(LoadInstruction),
    Store(StoreInstruction),
    Add {
        src: SrcOperand,
        dst: DstOperand,
    },
    Sub {
        subtrahend: SrcOperand,
        acc: DstOperand,
    },
    Mul {
        src: SrcOperand,
        dst: DstOperand,
    },
    And {
        src: SrcOperand,
        dst: DstOperand,
    },
    Or {
        src: SrcOperand,
        dst: DstOperand,
    },
    Xor {
        src: SrcOperand,
        dst: DstOperand,
    },
    Divq {
        src: DstOperand,
    },
    Not {
        src: DstOperand,
    },
    Neg {
        src: DstOperand,
    },
    Pushq {
        src: SrcOperand,
    },
    Popq {
        dst: DstOperand,
    },
    Cmp {
        subtrahend: SrcOperand,
        minuend: DstOperand,
    },
    Jmp {
        label: String,
        kind: lir::JmpKind,
    },
    // multi-line output by CallInstruction Display impl
    Call(CallInstruction),
    Leave,
    Ret,
    Label {
        label: String,
    },
    Cqto,
    Comment {
        comment: String,
    },
}

impl Instruction {
    fn instr_suffix(&self) -> String {
        use self::Instruction::*;
        match self {
            Add { src, dst }
            | Sub {
                subtrahend: src,
                acc: dst,
            }
            | Mul { src, dst }
            | And { src, dst }
            | Or { src, dst }
            | Xor { src, dst }
            | Cmp {
                subtrahend: src,
                minuend: dst,
            } => {
                let relevant_size = match (src, dst) {
                    (SrcOperand::Imm(_), _) => dst.size(),
                    (SrcOperand::Reg(_), DstOperand::Ar(_)) => src.size(),
                    (SrcOperand::Ar(_), DstOperand::Reg(_)) => dst.size(),
                    (SrcOperand::Reg(_), DstOperand::Reg(_)) => {
                        debug_assert_eq!(src.size(), dst.size());
                        src.size()
                    }
                    (SrcOperand::Ar(_), DstOperand::Ar(_)) => unreachable!(),
                };
                match relevant_size {
                    Size::One => "b",
                    Size::Four => "l",
                    Size::Eight => "q",
                }
            }
            Not { src } | Neg { src } => match src.size() {
                Size::One => "b",
                Size::Four => "l",
                Size::Eight => "q",
            },
            Mov(_)
            | Load(_)
            | Store(_)
            | Divq { .. }
            | Pushq { .. }
            | Popq { .. }
            | Jmp { .. }
            | Call(_)
            | Leave
            | Ret
            | Label { .. }
            | Cqto
            | Comment { .. } => "",
        }
        .to_string()
    }
}

impl fmt::Display for Instruction {
    fn fmt(&self, fmt: &mut fmt::Formatter<'_>) -> fmt::Result {
        use self::Instruction::*;
        match self {
            Add { src, dst } => write!(fmt, "\tadd{} {}, {}", self.instr_suffix(), src, dst),
            Sub { subtrahend, acc } => {
                write!(fmt, "\tsub{} {}, {}", self.instr_suffix(), subtrahend, acc)
            }
            Mul { src, dst } => write!(fmt, "\timul{} {}, {}", self.instr_suffix(), src, dst),
            And { src, dst } => write!(fmt, "\tand{} {}, {}", self.instr_suffix(), src, dst),
            Or { src, dst } => write!(fmt, "\tor{} {}, {}", self.instr_suffix(), src, dst),
            Xor { src, dst } => write!(fmt, "\txor{} {}, {}", self.instr_suffix(), src, dst),
            Cmp {
                subtrahend,
                minuend,
            } => write!(
                fmt,
                "\tcmp{} {}, {}",
                self.instr_suffix(),
                subtrahend,
                minuend
            ),
            Not { src } => write!(fmt, "\tnot{} {}", self.instr_suffix(), src),
            Neg { src } => write!(fmt, "\tneg{} {}", self.instr_suffix(), src),
            Mov(mov) => write!(fmt, "\t{}", mov),
            Load(load) => write!(fmt, "\t{}", load),
            Store(store) => write!(fmt, "\t{}", store),
            Divq { src } => write!(fmt, "\tidivq {}", src),
            Pushq { src } => write!(fmt, "\tpushq {}", src),
            Popq { dst } => write!(fmt, "\tpopq {}", dst),
            Jmp { kind, label } => write!(fmt, "\t{} {}", kind, label),
            Call(call) => write!(fmt, "\t{}", call),
            Leave => write!(fmt, "\tleave"),
            Ret => write!(fmt, "\tret"),
            Label { label } => write!(fmt, "{}:", label),
            Cqto => write!(fmt, "\tcqto"),
            Comment { comment } => write!(fmt, "\t/* {} */", comment),
        }
    }
}

#[derive(Debug, Clone)]
pub(super) struct CallInstruction {
    label: String,
}

/// Amd64 requires 16-byte aligned stacks.
/// The emitted assembly works for 8-byte aligned stacks.
/// (This works because we always spill quad-words (pushQ, popQ)).
impl fmt::Display for CallInstruction {
    fn fmt(&self, fmt: &mut fmt::Formatter<'_>) -> fmt::Result {
        match &*self.label {
            "mjrt_system_out_println"
            | "mjrt_system_out_write"
            | "mjrt_system_out_flush"
            | "mjrt_system_in_read"
            | "mjrt_new" => {
                writeln!(fmt, "\tpushq %rsp")?;
                writeln!(fmt, "\tpushq (%rsp)")?;
                writeln!(fmt, "\tandq $-16, %rsp")?;
                writeln!(fmt, "\tcall {}", self.label)?;
                write!(fmt, "\tmovq 8(%rsp), %rsp")?;
                Ok(())
            }
            "mjrt_dumpstack"
            | "mjrt_div_by_zero"
            | "mjrt_null_usage"
            | "mjrt_array_out_of_bounds" => unimplemented!(),
            // We don't need to align the stack pointer for our own functions, since we address
            // always correctly in our own interpretation of System V AMD64 ABI calling convention
            _ => write!(fmt, "\tcall {}", self.label),
        }
    }
}

#[derive(Clone)]
pub(super) struct MovInstruction {
    src: SrcOperand,
    dst: DstOperand,
    comment: String,
}

impl fmt::Display for MovInstruction {
    fn fmt(&self, fmt: &mut fmt::Formatter<'_>) -> fmt::Result {
        use super::Size::*;
        let (src, dst) = (self.src, self.dst);
        match (src, dst) {
            (SrcOperand::Ar(_), DstOperand::Reg(_)) => {
                // we use movq because then assembler will panic if dst has wrong size
                write!(
                    fmt,
                    "movq {}, {}\t\t/* {} */",
                    src,
                    dst.into_size(Eight),
                    self.comment
                )
            }
            (SrcOperand::Imm(_), DstOperand::Reg(_)) => {
                let mov_suffix = match dst.size() {
                    One => "b",
                    Four => "l",
                    Eight => "q",
                };
                write!(
                    fmt,
                    "mov{} {}, {}\t\t/* {} */",
                    mov_suffix, src, dst, self.comment
                )
            }
            (SrcOperand::Reg(_), DstOperand::Ar(_)) => write!(
                fmt,
                "movq {}, {}\t\t/* {} */",
                src.into_size(Eight),
                dst,
                self.comment
            ),
            (SrcOperand::Imm(_), DstOperand::Ar(_)) => {
                write!(fmt, "movq {}, {}\t\t/* {} */", src, dst, self.comment)
            }
            (SrcOperand::Reg(src_reg), DstOperand::Reg(dst_reg)) => {
                let (mov_suffix, src, dst) = match (self.src.size(), self.dst.size()) {
                    (One, One) => ("b", self.src, self.dst),
                    (One, Four) => ("sbl", self.src, self.dst),
                    (One, Eight) => ("sbq", self.src, self.dst),
                    (Four, One) => ("b", self.src.into_size(One), self.dst),
                    (Four, Four) => ("l", self.src, self.dst),
                    (Four, Eight) => ("slq", self.src, self.dst),
                    (Eight, One) => ("b", self.src.into_size(One), self.dst),
                    (Eight, Four) => ("l", self.src.into_size(Four), self.dst),
                    (Eight, Eight) => ("q", self.src, self.dst),
                };
                // FIXME: handling this in the Display impl is wrong. Do this right!
                if src_reg == dst_reg {
                    write!(fmt, "/* mov %r, %r */\t\t/* {} */", self.comment)
                } else {
                    write!(
                        fmt,
                        "mov{} {}, {}\t\t/* {} */",
                        mov_suffix, src, dst, self.comment
                    )
                }
            }
            (SrcOperand::Ar(_), DstOperand::Ar(_)) => unreachable!(),
        }
    }
}

#[derive(Clone)]
pub(super) struct LoadInstruction {
    src: lir::AddressComputation<Reg>,
    dst: Reg,
}

impl fmt::Display for LoadInstruction {
    fn fmt(&self, fmt: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mov_suffix = match self.dst.size {
            Size::One => "sbq",
            Size::Four => "slq",
            Size::Eight => "q",
        };
        write!(
            fmt,
            "mov{} {}, {}\t\t/* load */",
            mov_suffix,
            self.src,
            self.dst.into_size(Size::Eight)
        )
    }
}

#[derive(Clone)]
pub(super) struct StoreInstruction {
    src: SrcOperand,
    dst: lir::AddressComputation<Reg>,
}

impl fmt::Display for StoreInstruction {
    fn fmt(&self, fmt: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mov_suffix = match self.src.size() {
            Size::One => "b",
            Size::Four => "l",
            Size::Eight => "q",
        };
        write!(
            fmt,
            "mov{} {}, {}\t\t/* store */",
            mov_suffix, self.src, self.dst
        )
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
    free_regs: Box<dyn Iterator<Item = Amd64Reg>>,
    spills: SaveRegList<Amd64Reg>,
    mov_to_spilled: Vec<Instruction>,
    sign_extension: Vec<Instruction>,
}

impl SpillContext {
    /// `free_regs` must return a register at most once
    /// It must not return `Amd64Reg::Sp` or `Amd64Reg::Bp`.
    fn new(free_regs: Vec<Amd64Reg>) -> Self {
        SpillContext {
            used_regs: HashSet::new(),
            free_regs: box free_regs.into_iter(),
            spills: SaveRegList::default(),
            mov_to_spilled: vec![],
            sign_extension: vec![],
        }
    }

    /// mark a register as spilled and load the given SrcOperand
    /// into that register, and return that register
    /// If Rax is returned from the free_regs iterator, it is not spilled
    /// because it is assumed to be a scratch register.
    ///
    /// No entry of `src.used_regs()` must ever be returned by free_regs.
    fn spill_and_load_operand(&mut self, src: SrcOperand) -> Reg {
        let spill = self.free_regs.next().unwrap();
        self.used_regs.insert(spill);

        src.used_regs().into_iter().for_each(|r| {
            self.used_regs.insert(r);
        });

        if spill != Amd64Reg::A {
            self.spills.add_regs(&[spill]);
        }
        let spill = Reg {
            size: Size::Eight,
            reg: spill,
        };
        self.mov_to_spilled.push(Instruction::Mov(MovInstruction {
            src,
            dst: DstOperand::Reg(spill),
            comment: "spill context move to spilled".to_string(),
        }));
        spill
    }

    /// emit the spill instructions around those contained in `surrounded`
    fn emit_surrounded_instrs(self, surrounded: Vec<Instruction>) -> Vec<Instruction> {
        let mut instrs = vec![];
        instrs.extend(self.spills.saves().map(|reg| Instruction::Pushq {
            src: SrcOperand::Reg(Reg {
                size: Size::Eight,
                reg,
            }),
        }));
        instrs.extend(self.mov_to_spilled);
        instrs.extend(self.sign_extension);
        instrs.extend(surrounded);
        instrs.extend(self.spills.restores().map(|reg| Instruction::Popq {
            dst: DstOperand::Reg(Reg {
                size: Size::Eight,
                reg,
            }),
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

trait OperandTrait {
    fn size(self) -> Size;
    fn into_size(self, size: Size) -> Self;
}

#[derive(Copy, Clone)]
pub(super) enum SrcOperand {
    Ar(isize),
    Reg(Reg),
    Imm(Tarval),
}

impl OperandTrait for SrcOperand {
    fn size(self) -> Size {
        match self {
            SrcOperand::Ar(_) => Size::Eight,
            SrcOperand::Reg(reg) => reg.size(),
            SrcOperand::Imm(tv) => tv.mode().size_bytes().try_into().unwrap(),
        }
    }
    fn into_size(self, size: Size) -> Self {
        match self {
            // We can't convert the size of an Ar entry, but that's ok
            SrcOperand::Ar(_) => self,
            SrcOperand::Reg(reg) => SrcOperand::Reg(reg.into_size(size)),
            SrcOperand::Imm(_) => self,
        }
    }
}

impl OperandTrait for Reg {
    fn size(self) -> Size {
        self.size
    }
    fn into_size(self, size: Size) -> Self {
        Self {
            size,
            reg: self.reg,
        }
    }
}

impl OperandUsingRegs for SrcOperand {
    fn used_regs(self) -> Vec<Amd64Reg> {
        match self {
            SrcOperand::Reg(reg) => vec![reg.reg],
            SrcOperand::Imm(_) => vec![],
            SrcOperand::Ar(_) => vec![Amd64Reg::Bp],
        }
    }
}

#[derive(Debug, Copy, Clone)]
pub(super) enum DstOperand {
    Ar(isize),
    Reg(Reg),
}

impl OperandTrait for DstOperand {
    fn size(self) -> Size {
        match self {
            DstOperand::Ar(_) => Size::Eight,
            DstOperand::Reg(reg) => reg.size,
        }
    }
    fn into_size(self, size: Size) -> Self {
        match self {
            // We can't convert the size of an Ar entry, but that's ok
            DstOperand::Ar(_) => self,
            DstOperand::Reg(reg) => DstOperand::Reg(reg.into_size(size)),
        }
    }
}

impl OperandUsingRegs for DstOperand {
    fn used_regs(self) -> Vec<Amd64Reg> {
        match self {
            DstOperand::Reg(reg) => vec![reg.reg],
            DstOperand::Ar(_) => vec![Amd64Reg::Bp],
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
            SrcOperand::Ar(idx) => Ok(DstOperand::Ar(idx)),
            SrcOperand::Imm(_) => Err(()),
        }
    }
}

impl std::fmt::Display for SrcOperand {
    fn fmt(&self, fmt: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            SrcOperand::Reg(reg) => write!(fmt, "{}", reg),
            SrcOperand::Ar(idx) => write!(fmt, "{}(%rbp)", idx.checked_mul(8).unwrap()),
            SrcOperand::Imm(c) => write!(fmt, "${}", c.get_long()),
        }
    }
}

impl std::fmt::Display for DstOperand {
    fn fmt(&self, fmt: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            DstOperand::Ar(idx) => write!(fmt, "{}(%rbp)", idx.checked_mul(8).unwrap()),
            DstOperand::Reg(reg) => write!(fmt, "{}", reg),
        }
    }
}

impl Hash for DstOperand {
    fn hash<H: Hasher>(&self, state: &mut H) {
        match self {
            DstOperand::Reg(reg) => reg.hash(state),
            DstOperand::Ar(idx) => idx.hash(state),
        }
    }
}

impl PartialEq for DstOperand {
    fn eq(&self, other: &DstOperand) -> bool {
        use self::DstOperand::*;
        match (self, other) {
            (Reg(reg1), Reg(reg2)) => reg1 == reg2,
            (Ar(idx1), Ar(idx2)) => idx1 == idx2,
            (Ar(_), Reg(_)) | (Reg(_), Ar(_)) => false,
        }
    }
}

impl Eq for DstOperand {}

struct Mov {
    src: SrcOperand,
    dst: DstOperand,
}

impl From<RegGraphMinLeftEdgeInstruction<DstOperand>> for Instruction {
    fn from(i: RegGraphMinLeftEdgeInstruction<DstOperand>) -> Self {
        use self::Instruction::{Mov, Popq, Pushq};
        match i {
            RegGraphMinLeftEdgeInstruction::Push(src) => {
                let src = match src {
                    DstOperand::Reg(reg) => SrcOperand::Reg(reg),
                    DstOperand::Ar(idx) => SrcOperand::Ar(idx),
                };
                Pushq {
                    src: src.into_size(Size::Eight),
                }
            }
            RegGraphMinLeftEdgeInstruction::Pop(dst) => Popq {
                dst: dst.into_size(Size::Eight),
            },
            RegGraphMinLeftEdgeInstruction::Mov(RegToRegTransfer { src, dst }) => {
                let src = match src {
                    DstOperand::Reg(reg) => SrcOperand::Reg(reg),
                    DstOperand::Ar(idx) => SrcOperand::Ar(idx),
                };
                Mov(MovInstruction {
                    src,
                    dst,
                    comment: "copy prop".to_string(),
                })
            }
        }
    }
}

fn sort_copy_prop(copies: &[Mov], instrs: &mut Vec<Instruction>) {
    use self::Instruction::Mov;

    let mut mov_imm = vec![];

    debug_assert_eq!(copies.iter().unique_by(|mov| mov.dst).count(), copies.len());

    let mut transfers = vec![];
    for instr in copies.iter() {
        match instr.src {
            SrcOperand::Imm(_) => mov_imm.push(Mov(MovInstruction {
                src: instr.src,
                dst: instr.dst,
                comment: "copy prop imm move".to_string(),
            })),
            _ => {
                transfers.push(RegToRegTransfer {
                    src: instr.src.try_into().unwrap(),
                    dst: instr.dst,
                });
            }
        }
    }

    let reg_graph = RegGraph::new(transfers);

    let mut copy_instrs: Vec<_> = reg_graph.into_instructions::<Instruction>().collect();
    copy_instrs.extend(mov_imm);
    for instr in copy_instrs {
        match instr {
            Instruction::Pushq { .. } | Instruction::Popq { .. } => instrs.push(instr),
            Instruction::Mov(MovInstruction { src, dst, comment }) => match (src, dst) {
                (SrcOperand::Reg(_), _) | (_, DstOperand::Reg(_)) => {
                    instrs.push(Mov(MovInstruction { src, dst, comment }));
                }
                (SrcOperand::Ar(_), DstOperand::Ar(_)) => {
                    instrs.push(Mov(MovInstruction {
                        src,
                        dst: DstOperand::Reg(Reg {
                            size: src.size(),
                            reg: Amd64Reg::A,
                        }),
                        comment: "copy prop spill".to_string(),
                    }));
                    instrs.push(Mov(MovInstruction {
                        src: SrcOperand::Reg(Reg {
                            size: src.size(),
                            reg: Amd64Reg::A,
                        }),
                        dst,
                        comment,
                    }));
                }
                (SrcOperand::Imm(_), _) => instrs.push(Mov(MovInstruction { src, dst, comment })),
            },
            _ => unreachable!(),
        }
    }
}
