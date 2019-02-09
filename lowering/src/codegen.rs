//! Use linear scan results to convert LIR-instructions to x86 instructions.

use crate::{
    allocator::Ptr,
    cycle_removal::{RegGraph, RegGraphMinLeftEdgeInstruction, RegToRegTransfer},
    linear_scan::{LSAResult, Location},
    lir,
    register::{Amd64Reg, CallingConv, Reg, Size},
};
use itertools::Itertools;
use libfirm_rs::{nodes::NodeTrait, Tarval};
use std::{
    collections::{BTreeSet, HashSet},
    convert::{TryFrom, TryInto},
    fmt,
    hash::{Hash, Hasher},
    iter::FromIterator,
};

/// Fill `lir::BasicBlock::codegen_instrs` from its `lir::BasicBlock::code`.
///
/// Returns the state of code generation as an opaque `Codegen`.
///
/// A peephole optimizer can run over the individual
/// `lir::BasicBlock::codegen_instrs`.
///
/// After the peephole optimizer is done, use `Codegen::to_instr_list` to
/// produce instruction lists.
///
/// A subsequent peephole-optimizer may pass over the complete instruciton list.
/// The Instructions implement `Display`, and valid GNU Assembly is emitted if
/// the returned instruction list is printed line by line.
pub(crate) fn begin_codegen(func: &mut lir::Function, lsa_result: LSAResult) -> Codegen {
    let mut codegen = Codegen::new(func, lsa_result); // TODO remove hardcoded CC
    codegen.run_initial();
    codegen
}

pub(crate) struct Codegen<'f> {
    func: &'f lir::Function,
    lsa_result: LSAResult,
    cconv: CallingConv,

    saved_regs: SaveRegList<Amd64Reg>,
}

type Code = lir::Code<Mov, Mov, Instruction, Instruction>;

impl<'f> Codegen<'f> {
    fn new(func: &'f lir::Function, lsa_result: LSAResult) -> Self {
        // There are 5 callee save registers: %rbx, %r12-r15
        // %rbp is also callee save, but we never allocate this register
        // There are 10 caller save registers, but %rsp is reserved, so we need to save
        // registers if more than 9 registers are required.
        let mut saved_regs = SaveRegList::default();
        use Amd64Reg::*;
        match lsa_result.num_regs_required {
            x if x < 9 => (), // Enough caller save registers available
            9 => saved_regs.add_regs(&[B]),
            10 => saved_regs.add_regs(&[B, R12]),
            11 => saved_regs.add_regs(&[B, R12, R13]),
            12 => saved_regs.add_regs(&[B, R12, R13, R14]),
            13 => saved_regs.add_regs(&[B, R12, R13, R14, R15]),
            _ => unreachable!("More registers required than available"),
        }

        Self {
            func,
            lsa_result,
            saved_regs,
            cconv: CallingConv,
        }
    }

    fn run_initial(&mut self) {
        let mut graph = self.func.graph; // copy for Ptr -,-
        for block in graph.blocks_scheduled.as_mut().unwrap().iter_mut() {
            let code = self.run_block(*block);
            block.codegen_instrs = code;
        }
    }

    fn run_block(&mut self, block: Ptr<lir::BasicBlock>) -> Vec<Instruction> {
        let mut code = Code::default();
        for instr in block.code.iter_unified() {
            use self::{lir::CodeInstruction as CI, Instruction::Comment};
            match instr {
                // Match over every instruction and generate amd64 instructions
                CI::Body(instr) => {
                    if let lir::Instruction::Call(call) = &**instr {
                        code.body.push(Comment {
                            comment: "call instruction".to_string(),
                        });
                        self.gen_call(call, &mut code.body);
                    } else {
                        self.gen_lir(&**instr, &mut code.body);
                    }
                }
                CI::Leave(leave) => {
                    code.leave.push(Comment {
                        comment: "leave instruction".to_string(),
                    });
                    self.gen_leave(leave, &mut code.leave);
                }
                CI::CopyIn(copy_prop) => {
                    self.gen_mov(*copy_prop, &mut code.copy_in);
                }
                CI::CopyOut(copy_prop) => {
                    self.gen_mov(*copy_prop, &mut code.copy_out);
                }
            }
        }

        self.resolve_copy_prop_cycles_and_convert_to_instructions(code)
    }

    /// assumes `run_block` ran for each function
    pub(crate) fn emit_function(&self) -> Vec<Instruction> {
        let mut instrs = vec![];

        macro_rules! push_raw {
            ($($fmt:expr),+) => {
                instrs.push(Instruction::Raw(format!($( $fmt ),+ )));
            }
        }

        push_raw!("# -- Begin  {}", self.func.name);
        // "\t.p2align %u,%s,%u\n", po2alignment, fill_byte, maximum_skip
        push_raw!("\t.p2align  4,,15"); // .p2align 4,,15
        push_raw!("\t.globl  {}", self.func.name); // .globl mj_main
        push_raw!("\t.type\t{}, @function", self.func.name);
        push_raw!("{}:", self.func.name);

        self.gen_meta_comments(&mut instrs);
        self.gen_function_prolog(&mut instrs);
        for block in self.func.graph.blocks_scheduled.as_ref().unwrap() {
            instrs.push(Instruction::Label {
                label: self.gen_jmp_label(*block),
            });
            instrs.extend(block.codegen_instrs.clone());
        }
        self.gen_function_epilog(&mut instrs);

        push_raw!("\t.size\t{}, .-{}", self.func.name, self.func.name);
        push_raw!("# -- End {}\n", self.func.name);

        instrs
    }

    fn resolve_copy_prop_cycles_and_convert_to_instructions(&self, code: Code) -> Vec<Instruction> {
        let mut instrs = vec![];
        let mut code_body = code.body.iter().cloned().peekable();
        if let Some(Instruction::Label { .. }) = code_body.peek() {
            // Push the label of the block first
            instrs.push(code_body.next().unwrap());
        }
        sort_copy_prop(&code.copy_in, &mut instrs);
        instrs.extend(code_body);
        sort_copy_prop(&code.copy_out, &mut instrs);
        instrs.extend(code.leave.iter().cloned());
        instrs
    }

    fn gen_meta_comments(&self, instrs: &mut Vec<Instruction>) {
        use self::Instruction::Comment;

        instrs.push(Comment {
            comment: format!("Calling convention: {:?}", self.cconv),
        });
        for (var, location) in self
            .lsa_result
            .var_location
            .iter()
            .sorted_by_key(|(var, _)| var.num())
        {
            instrs.push(Comment {
                comment: format!("Var {:?} in {:?}", var.num(), location),
            });
        }
    }

    fn stack_slots_space(&self) -> Tarval {
        let space = (self.lsa_result.stack_vars_counter as i64)
            .checked_mul(8)
            .unwrap();
        Tarval::mj_int(space)
    }

    fn gen_function_prolog(&self, instrs: &mut Vec<Instruction>) {
        use self::Instruction::{Comment, Mov, Pushq, Sub};

        instrs.push(Comment {
            comment: "function prolog".to_string(),
        });
        instrs.push(Pushq {
            src: Amd64Reg::Bp.into_reg(Size::Eight).into(),
        });
        instrs.push(Mov(MovInstruction {
            src: Amd64Reg::Sp.into_reg(Size::Eight).into(),
            dst: Amd64Reg::Bp.into_reg(Size::Eight).into(),
            comment: "".to_string(),
        }));

        instrs.push(Comment {
            comment: "function save regs".to_string(),
        });
        for src in self.saved_regs.saves() {
            let src = src.into_reg(Size::Eight).into();
            instrs.push(Pushq { src });
        }

        instrs.push(Comment {
            comment: "function allocate stack slots".to_string(),
        });
        instrs.push(Sub {
            subtrahend: SrcOperand::Imm(self.stack_slots_space()),
            acc: Amd64Reg::Sp.into_reg(Size::Eight).into(),
        });
    }

    fn gen_function_epilog(&self, instrs: &mut Vec<Instruction>) {
        use self::Instruction::{Add, Comment, Leave, Popq, Ret};

        instrs.push(Comment {
            comment: "function de-allocate stack slots".to_string(),
        });
        instrs.push(Add {
            src: SrcOperand::Imm(self.stack_slots_space()),
            dst: DstOperand::Reg(Reg {
                size: Size::Eight,
                reg: Amd64Reg::Sp,
            }),
        });

        instrs.push(Comment {
            comment: "function restore regs".to_string(),
        });
        for dst in self.saved_regs.restores() {
            let dst = DstOperand::Reg(Reg {
                size: Size::Eight,
                reg: dst,
            });
            instrs.push(Popq { dst });
        }

        instrs.push(Comment {
            comment: "function epilog".to_string(),
        });
        instrs.push(Leave);
        instrs.push(Ret);
    }

    fn gen_mov(&self, copy_prop: Ptr<lir::CopyPropagation>, instrs: &mut Vec<Mov>) {
        let lir::CopyPropagation { src, dst } = &*copy_prop;
        let src = self.lir_to_src_operand(src.clone());
        let dst: DstOperand = self
            .lir_to_src_operand(lir::Operand::Var(*dst))
            .try_into()
            .unwrap();
        assert_eq!(src.size(), dst.size(), "src: {:?}, dst: {:?}", src, dst);
        instrs.push(Mov { src, dst });
    }

    fn gen_lir(&self, lir: &lir::Instruction, instrs: &mut Vec<Instruction>) {
        use self::Instruction::{Load, Mov, Store};

        log::debug!("Gen lir: {:?}", lir);
        match lir {
            lir::Instruction::LoadParam { idx, size, dst } => {
                self.gen_load_param(*idx, *size, *dst, instrs)
            }
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
                    .lir_to_src_operand(lir::Operand::Var(*dst))
                    .try_into()
                    .unwrap();

                // TODO peephole this away for regs
                let spill = Reg {
                    size: Size::Eight,
                    reg: Amd64Reg::A,
                };
                instrs.push(Mov(MovInstruction {
                    src,
                    dst: DstOperand::Reg(spill),
                    comment: "conv load".to_string(),
                }));
                instrs.push(Mov(MovInstruction {
                    src: SrcOperand::Reg(spill),
                    dst,
                    comment: "conv store".to_string(),
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
                    let size = store.size();
                    let src = store.operand();
                    let src: SrcOperand = self.lir_to_src_operand(src);
                    match src {
                        // the src is stored in a reg, this is fine
                        SrcOperand::Reg(_) => src,
                        SrcOperand::Imm(_) => src,
                        // the source is stored in the activation record, move to scratch
                        SrcOperand::Ar(_) => {
                            let src = spill_ctx.spill_and_load_operand(src);
                            SrcOperand::Reg(src.into_size(size.try_into().unwrap()))
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
        }
    }

    fn gen_load_param(&self, idx: u32, size: u32, dst: lir::Var, instrs: &mut Vec<Instruction>) {
        use self::Instruction::{Comment, Mov};

        // FIXME confusing
        let param = &self.lsa_result.var_location[&dst];
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
                log::debug!("idx={:?} i={:?} dst={:?}", idx, i, dst);
                // if Amd64Reg::arg panics, idx is too big, location should have been ParamMem
                let arg_reg = Amd64Reg::arg(idx as usize);
                instrs.push(Comment {
                    comment: format!("spill argument register {}", arg_reg),
                });
                instrs.push(Mov(MovInstruction {
                    src: SrcOperand::Reg(Reg { size, reg: arg_reg }),
                    dst: DstOperand::Ar(Ar {
                        size,
                        pos: -((*i + 1 + self.saved_regs.len()) as isize),
                    }),
                    comment: "move param to stack".to_string(),
                }));
            }
            // param is already on the stack and stays there
            Location::ParamMem(_) => (),
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
        &self,
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
                        assert!(
                            reg.size() == Size::Four,
                            "minijava only allows indexing by int"
                        );
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
        &self,
        ac: lir::AddressComputation<lir::Operand>,
    ) -> Vec<Amd64Reg> {
        let mut pseudo_spill = SpillContext::new(Vec::from_iter(Amd64Reg::all_but_rsp_and_rbp()));
        let (_, already_in_registers) =
            self.lir_address_computation_to_register_address_computation(ac, &mut pseudo_spill);
        already_in_registers
    }

    fn lir_operand_used_registers(&self, op: lir::Operand) -> Vec<Amd64Reg> {
        self.lir_to_src_operand(op).used_regs()
    }

    fn load_store_mem_occupied_regs<I: LoadOrStoreMem>(&self, load_or_store: &I) -> Vec<Amd64Reg> {
        let mut regs = vec![];
        let ac = load_or_store.mem_address_computation();
        regs.extend(self.lir_address_computation_operands_already_in_registers(ac));
        let op = load_or_store.operand();
        regs.extend(self.lir_operand_used_registers(op));
        regs
    }

    fn gen_binop(
        &self,
        instrs: &mut Vec<Instruction>,
        kind: &lir::BinopKind,
        src1: &lir::Operand,
        src2: &lir::Operand,
        dst: lir::Var,
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
                    lir::BinopKind::Shr => instrs.push(Shr {
                        val: $dst,
                        by: $src,
                    }),
                    lir::BinopKind::Shrs => instrs.push(Shrs {
                        val: $dst,
                        by: $src,
                    }),
                    lir::BinopKind::Shl => instrs.push(Shl {
                        val: $dst,
                        by: $src,
                    }),
                }
            }};
        }

        let src1 = self.lir_to_src_operand(*src1);
        let src2 = self.lir_to_src_operand(*src2);
        let dst: DstOperand = self
            .lir_to_src_operand(lir::Operand::Var(dst))
            .try_into()
            .unwrap();

        // src1.size == src2.size == dst.size
        assert_eq!(src1.size(), src2.size());
        assert_eq!(src2.size(), dst.size());
        let size = src1.size();

        // all operands are potentially mem operands (in AR)
        //   src1 => rax
        //   op src2, rax /* order is important for mul, which requires rhs to be REG,
        //                   and for sub which requires ordering */
        //   mov rax, dst
        //
        // TODO optimize / peephole this awway for the case where src1 / src2 are in
        // registers => pay attention: src1 and src2 must not be modified

        let spill = Reg {
            size,
            reg: Amd64Reg::A,
        };
        instrs.push(Mov(MovInstruction {
            src: src1,
            dst: DstOperand::Reg(spill),
            comment: "binop step 1".to_string(),
        }));
        push_binop!(kind, src2, DstOperand::Reg(spill));
        instrs.push(Mov(MovInstruction {
            src: SrcOperand::Reg(spill),
            dst,
            comment: "binop step 3".to_string(),
        }));
    }

    fn gen_unop(
        &self,
        instrs: &mut Vec<Instruction>,
        kind: &lir::UnopKind,
        src: &lir::Operand,
        dst: lir::Var,
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
        let dst: DstOperand = self
            .lir_to_src_operand(lir::Operand::Var(dst))
            .try_into()
            .unwrap();
        assert_eq!(src.size(), dst.size());

        // TODO peephole this away if src == rax == Reg::*
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

    fn gen_div<F>(
        &self,
        src1: &lir::Operand,
        src2: &lir::Operand,
        dst: lir::Var,
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
        self.lir_to_src_operand(lir::Operand::Var(dst))
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
            .lir_to_src_operand(lir::Operand::Var(dst))
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

    fn gen_call(&self, call: &lir::Call, instrs: &mut Vec<Instruction>) {
        use self::Instruction::{Mov, Popq, Pushq};

        /* ###############################################################
         General Note on this function: we only fill instrs in the end
         and accumulate all parts of the call setup in separate local
         variables.
        ############################################################### */

        /* ###############################################################
         Save caller-saved registers
        ############################################################### */

        let mut caller_saves = SaveRegList::default();
        caller_saves.add_regs_from_iter(
            call.live_regs_after_call
                .iter()
                .cloned()
                .filter(|reg| reg.is_caller_save()),
        );

        /* ###############################################################
         Place variables (which could be assigned to either Ar or Regs
         to those locations required by the calling convetion
        ############################################################### */

        #[derive(Clone, Copy)]
        enum DesiredLocation<LirOrArch> {
            Register { arg_idx: usize, arg: LirOrArch },
            Stack { arg_idx: usize, arg: LirOrArch },
        }
        impl<T> DesiredLocation<T> {
            fn arg_idx(&self) -> usize {
                match self {
                    DesiredLocation::Stack { arg_idx, .. } => *arg_idx,
                    DesiredLocation::Register { arg_idx, .. } => *arg_idx,
                }
            }
        }
        impl DesiredLocation<lir::Operand> {
            fn into_arch(self, codegen: &Codegen) -> DesiredLocation<SrcOperand> {
                use DesiredLocation::*;
                match self {
                    Register { arg_idx, arg } => Register {
                        arg_idx,
                        arg: codegen.lir_to_src_operand(arg),
                    },
                    Stack { arg_idx, arg } => Stack {
                        arg_idx,
                        arg: codegen.lir_to_src_operand(arg),
                    },
                }
            }
        }
        impl DesiredLocation<SrcOperand> {
            /// the register, if any, that the calling convention devises
            /// for this argument
            fn desired_register(self) -> Option<Amd64Reg> {
                // TODO this should be a property of cconv, not of Amd64Reg
                Amd64Reg::try_arg(self.arg_idx())
            }
        }

        let arg_locations = call
            .args
            .iter()
            .enumerate()
            .map(|(arg_idx, arg)| {
                let arg = *arg;
                if arg_idx < self.cconv.num_arg_regs() {
                    // Fill the function argument registers
                    DesiredLocation::Register { arg_idx, arg }
                } else {
                    // Push the other args on the stack
                    DesiredLocation::Stack { arg_idx, arg }
                }
            })
            .map(|al| al.into_arch(self));

        // at this point, `arg` contains the SrcOperand _before_ the call
        // `DesiredLocation` encodes
        // let's figure out what pushqs and movs we need to do to get

        let mut push_stack_args = vec![];
        let mut movs_from_nonreg = vec![];
        let mut reg_to_reg = vec![];
        for desired_location in arg_locations {
            use DesiredLocation::*;
            match desired_location {
                Stack { arg, .. } => push_stack_args.push(arg),
                Register { arg, .. } => {
                    // let's abuse the used_regs function a bit here
                    let desired_reg = desired_location.desired_register();
                    match (arg, desired_reg) {
                        (SrcOperand::Reg(src), Some(dst)) => reg_to_reg.push((src, dst)),
                        (SrcOperand::Ar(_), Some(dst)) | (SrcOperand::Imm(_), Some(dst)) => {
                            movs_from_nonreg.push(Mov(MovInstruction {
                                src: arg,
                                dst: dst.into_reg(Size::Eight).into(),
                                comment: "NonRegToReg".to_string(),
                            }))
                        }
                        (_, None) => push_stack_args.push(arg),
                    }
                }
            }
        }
        let push_stack_args = push_stack_args
            .into_iter()
            // stack args are passed in reverse order
            .rev()
            .map(|src| Instruction::Pushq { src })
            .collect::<Vec<_>>();
        let reg_to_reg = {
            let transfers = reg_to_reg.into_iter().map(|(src, dst)| RegToRegTransfer {
                src: SortCopyPropEntity(src.into()),
                dst: SortCopyPropEntity(dst.into_reg(Size::Eight).into()),
            });
            let reg_graph = RegGraph::new(transfers.collect());
            reg_graph.into_instructions::<Instruction>()
        };
        let post_call_reset_rsp_of_pushs = Instruction::Add {
            src: Tarval::mj_int((push_stack_args.len() * 8) as i64).into(),
            dst: SrcOperand::Reg(Reg {
                size: Size::Eight,
                reg: Amd64Reg::Sp,
            })
            .try_into()
            .unwrap(),
        };

        // correct
        let move_result_to_dst = call.dst.map(|dst| {
            MovInstruction {
                src: Amd64Reg::A.into_reg(Size::Eight).into(),
                dst: self.lir_to_src_operand(dst).try_into().unwrap(),
                comment: "ResultToDst".to_string(),
            }
            .into()
        });

        /* ###############################################################
         Done with calling convention stuff, now fill instrs
        ############################################################### */

        instrs.extend(caller_saves.saves().map(|reg| Pushq {
            src: reg.into_reg(Size::Eight).into(),
        }));
        instrs.extend(push_stack_args);
        instrs.extend(reg_to_reg);
        instrs.extend(movs_from_nonreg);
        instrs.push(
            CallInstruction {
                label: call.func.clone(),
            }
            .into(),
        );
        instrs.push(post_call_reset_rsp_of_pushs);
        instrs.extend(move_result_to_dst);
        instrs.extend(caller_saves.restores().map(|reg| Popq {
            dst: reg.into_reg(Size::Eight).into(),
        }));
    }

    fn gen_jmp_label(&self, target: Ptr<lir::BasicBlock>) -> String {
        format!(".L{}", target.firm.node_id())
    }

    fn gen_leave(&self, leave: &lir::Leave, instrs: &mut Vec<Instruction>) {
        use self::Instruction::{Cmp, Jmp, Mov};

        macro_rules! push_jmp {
            ($kind:expr, $target:expr, fall=$fall:expr) => {{
                instrs.push(Jmp {
                    label: self.gen_jmp_label($target),
                    kind: $kind,
                });
            }};
        }

        log::debug!("Gen leave: {:?}", leave);
        match leave {
            lir::Leave::Jmp { target } => {
                push_jmp!(lir::JmpKind::Unconditional, *target, fall = true)
            }
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
                push_jmp!(lir::JmpKind::Conditional(op), *true_target, fall = false);
                push_jmp!(lir::JmpKind::Unconditional, *false_target, fall = true);
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
                push_jmp!(lir::JmpKind::Unconditional, *end_block, fall = true);
            }
        }
    }

    fn lir_to_src_operand<OP: Into<lir::Operand>>(&self, op: OP) -> SrcOperand {
        let op: lir::Operand = op.into();
        match op {
            lir::Operand::Imm(c) => SrcOperand::Imm(c),
            lir::Operand::Var(var) => match self.lsa_result.var_location[&var] {
                Location::Reg(reg) => SrcOperand::Reg(Reg {
                    size: { var.firm().mode().size_bytes().try_into().unwrap() },
                    reg,
                }),
                Location::Ar(idx) => SrcOperand::Ar(Ar {
                    size: { var.firm().mode().size_bytes().try_into().unwrap() },
                    pos: -((idx + 1 + self.saved_regs.len()) as isize),
                }),
                Location::ParamMem(idx) => SrcOperand::Ar(Ar {
                    size: var.firm().mode().size_bytes().try_into().unwrap(),
                    pos: (idx.checked_sub(self.cconv.num_arg_regs()).unwrap() as isize) + 2,
                }),
            },
        }
    }

    fn arg_from_stack(&self, idx: usize, dst: DstOperand) -> Option<Instruction> {
        if idx < self.cconv.num_arg_regs() {
            return None;
        }
        let effective_idx = idx.checked_sub(self.cconv.num_arg_regs()).unwrap();
        let idx = (effective_idx + 2) as isize;
        Some(Instruction::Mov(MovInstruction {
            src: SrcOperand::Ar(Ar {
                pos: idx,
                size: dst.size(),
            }),
            dst,
            comment: "stack args move in register".to_string(),
        }))
    }
}

#[derive(Clone)]
pub(crate) enum Instruction {
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
    Shr {
        by: SrcOperand,
        val: DstOperand,
    },
    Shrs {
        by: SrcOperand,
        val: DstOperand,
    },
    Shl {
        by: SrcOperand,
        val: DstOperand,
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
    Raw(String),
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
            | Shr { by: src, val: dst }
            | Shrs { by: src, val: dst }
            | Shl { by: src, val: dst }
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
            | Comment { .. }
            | Raw(_) => "",
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
            Shl { by, val } => write!(fmt, "\tshl{} {}, {}", self.instr_suffix(), by, val),
            Shr { by, val } => write!(fmt, "\tshr{} {}, {}", self.instr_suffix(), by, val),
            Shrs { by, val } => write!(fmt, "\tsar{} {}, {}", self.instr_suffix(), by, val),
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
            Raw(raw) => write!(fmt, "{}", raw),
        }
    }
}

impl From<MovInstruction> for Instruction {
    fn from(mov: MovInstruction) -> Self {
        Instruction::Mov(mov)
    }
}

impl From<CallInstruction> for Instruction {
    fn from(call: CallInstruction) -> Self {
        Instruction::Call(call)
    }
}

#[derive(Debug, Clone)]
pub(crate) struct CallInstruction {
    label: String,
}

/// Amd64 requires 16-byte aligned stacks.
/// The emitted assembly works for 8-byte aligned stacks.
/// (This works because we always spill quad-words (pushQ, popQ)).
impl fmt::Display for CallInstruction {
    fn fmt(&self, fmt: &mut fmt::Formatter<'_>) -> fmt::Result {
        // FIXME
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
pub(crate) struct MovInstruction {
    src: SrcOperand,
    dst: DstOperand,
    comment: String,
}

impl fmt::Display for MovInstruction {
    fn fmt(&self, fmt: &mut fmt::Formatter<'_>) -> fmt::Result {
        use self::Size::*;
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
        match (src, dst) {
            (SrcOperand::Ar(_), DstOperand::Reg(_)) => {
                // we use movq because then assembler will panic if dst has wrong size
                write!(
                    fmt,
                    "mov{} {}, {}\t\t/* {} */",
                    mov_suffix, src, dst, self.comment
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
                "mov{} {}, {}\t\t/* {} */",
                mov_suffix, src, dst, self.comment
            ),
            (SrcOperand::Imm(_), DstOperand::Ar(_)) => write!(
                fmt,
                "mov{} {}, {}\t\t/* {} */",
                mov_suffix, src, dst, self.comment
            ),
            (SrcOperand::Reg(src_reg), DstOperand::Reg(dst_reg)) => {
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

impl fmt::Debug for MovInstruction {
    fn fmt(&self, fmt: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Display::fmt(self, fmt)
    }
}

#[derive(Clone)]
pub(crate) struct LoadInstruction {
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
pub(crate) struct StoreInstruction {
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
            // following two guaranteed by OperandUsingRegs trait
            debug_assert_ne!(r, Amd64Reg::Bp);
            debug_assert_ne!(r, Amd64Reg::Sp);
            let was_unused = self.used_regs.insert(r);
            debug_assert!(
                was_unused,
                "src operand depends on registers returned by free_regs"
            )
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
    // The list of registers used by the operand, excluding rsp and rbp.
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

#[derive(Debug, Copy, Clone)]
pub(crate) enum SrcOperand {
    Ar(Ar),
    Reg(Reg),
    Imm(Tarval),
}

impl From<Tarval> for SrcOperand {
    fn from(tv: Tarval) -> Self {
        SrcOperand::Imm(tv)
    }
}

impl From<Reg> for SrcOperand {
    fn from(reg: Reg) -> Self {
        SrcOperand::Reg(reg)
    }
}

#[derive(Debug, Copy, Clone, Hash, PartialEq, Eq)]
pub(crate) struct Ar {
    pub(crate) pos: isize,
    pub(crate) size: Size,
}

impl OperandTrait for SrcOperand {
    fn size(self) -> Size {
        match self {
            SrcOperand::Ar(ar) => ar.size(),
            SrcOperand::Reg(reg) => reg.size(),
            SrcOperand::Imm(tv) => tv.mode().size_bytes().try_into().unwrap(),
        }
    }
    fn into_size(self, size: Size) -> Self {
        match self {
            SrcOperand::Ar(ar) => SrcOperand::Ar(ar.into_size(size)),
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

impl OperandTrait for Ar {
    fn size(self) -> Size {
        self.size
    }
    fn into_size(self, size: Size) -> Self {
        Self {
            size,
            pos: self.pos,
        }
    }
}

impl OperandUsingRegs for SrcOperand {
    fn used_regs(self) -> Vec<Amd64Reg> {
        match self {
            SrcOperand::Reg(reg) => vec![reg.reg],
            SrcOperand::Imm(_) => vec![],
            SrcOperand::Ar(_) => vec![], // trait definition says rbp is not part of used_regs
        }
    }
}

#[derive(Debug, Copy, Clone)]
pub(crate) enum DstOperand {
    Ar(Ar),
    Reg(Reg),
}

impl<R: Sized + Into<Reg>> From<R> for DstOperand {
    fn from(r: R) -> Self {
        DstOperand::Reg(r.into())
    }
}

impl From<Ar> for DstOperand {
    fn from(ar: Ar) -> Self {
        DstOperand::Ar(ar)
    }
}

impl OperandTrait for DstOperand {
    fn size(self) -> Size {
        match self {
            DstOperand::Ar(ar) => ar.size(),
            DstOperand::Reg(reg) => reg.size,
        }
    }
    fn into_size(self, size: Size) -> Self {
        match self {
            DstOperand::Ar(ar) => DstOperand::Ar(ar.into_size(size)),
            DstOperand::Reg(reg) => DstOperand::Reg(reg.into_size(size)),
        }
    }
}

impl OperandUsingRegs for DstOperand {
    fn used_regs(self) -> Vec<Amd64Reg> {
        match self {
            DstOperand::Reg(reg) => vec![reg.reg],
            DstOperand::Ar(_) => vec![], // per definition is rbp not part of this list
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
        lir::Operand::Var(self.dst)
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
            SrcOperand::Ar(ar) => Ok(DstOperand::Ar(ar)),
            SrcOperand::Imm(_) => Err(()),
        }
    }
}

impl std::fmt::Display for SrcOperand {
    fn fmt(&self, fmt: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            SrcOperand::Reg(reg) => write!(fmt, "{}", reg),
            SrcOperand::Ar(ar) => write!(fmt, "{}(%rbp)", ar.pos.checked_mul(8).unwrap()),
            SrcOperand::Imm(c) => write!(fmt, "${}", c.get_long()),
        }
    }
}

impl std::fmt::Display for DstOperand {
    fn fmt(&self, fmt: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            DstOperand::Ar(ar) => write!(fmt, "{}(%rbp)", ar.pos.checked_mul(8).unwrap()),
            DstOperand::Reg(reg) => write!(fmt, "{}", reg),
        }
    }
}

struct Mov {
    src: SrcOperand,
    dst: DstOperand,
}

/// A newtype around DstOperand for the purpose of copy propagation cycle
/// removal.
#[derive(Debug, Clone, Copy, From)]
struct SortCopyPropEntity(DstOperand);

// keep in sync with Hash
impl PartialEq for SortCopyPropEntity {
    fn eq(&self, other: &Self) -> bool {
        use self::DstOperand::*;
        match (self.0, other.0) {
            // size is not identifying here (rax and eax) **must** be the same for the purpose of
            // cycle removal
            (Reg(reg1), Reg(reg2)) => reg1.reg == reg2.reg,
            // same goes for Ar
            (Ar(ar1), Ar(ar2)) => ar1.pos == ar2.pos,
            (Reg(_), Ar(_)) | (Ar(_), Reg(_)) => false,
        }
    }
}

// keep in sync with PartialEq
impl Hash for SortCopyPropEntity {
    fn hash<H: Hasher>(&self, state: &mut H) {
        match self.0 {
            // size is not identifying here (rax and eax) **must** be the same for the purpose of
            // cycle removal
            DstOperand::Reg(reg) => reg.reg.hash(state),
            DstOperand::Ar(ar) => ar.pos.hash(state),
        }
    }
}

impl Eq for SortCopyPropEntity {}

impl From<RegGraphMinLeftEdgeInstruction<SortCopyPropEntity>> for Instruction {
    fn from(i: RegGraphMinLeftEdgeInstruction<SortCopyPropEntity>) -> Self {
        use self::Instruction::{Mov, Popq, Pushq};
        match i {
            RegGraphMinLeftEdgeInstruction::Push(src) => {
                let src = match src.0 {
                    DstOperand::Reg(reg) => SrcOperand::Reg(reg),
                    DstOperand::Ar(ar) => SrcOperand::Ar(ar),
                };
                Pushq {
                    src: src.into_size(Size::Eight),
                }
            }
            RegGraphMinLeftEdgeInstruction::Pop(dst) => Popq {
                dst: dst.0.into_size(Size::Eight),
            },
            RegGraphMinLeftEdgeInstruction::Mov(RegToRegTransfer { src, dst }) => {
                let src = match src.0 {
                    DstOperand::Reg(reg) => SrcOperand::Reg(reg),
                    DstOperand::Ar(ar) => SrcOperand::Ar(ar),
                };
                Mov(MovInstruction {
                    src: src.into_size(Size::Eight),
                    dst: dst.0.into_size(Size::Eight),
                    comment: "copy prop".to_string(),
                })
            }
        }
    }
}

fn sort_copy_prop(copies: &[Mov], instrs: &mut Vec<Instruction>) {
    use self::Instruction::Mov;

    // Resolve reg-to-reg copy cycles using RegGraph
    let mut mov_imm = vec![];
    let mut transfers: Vec<RegToRegTransfer<SortCopyPropEntity>> = vec![];
    for instr in copies.iter() {
        match instr.src {
            SrcOperand::Imm(tv) => mov_imm.push(Mov(MovInstruction {
                src: SrcOperand::Imm(tv),
                dst: instr.dst,
                comment: "copy prop imm move".to_owned(),
            })),
            _ => {
                transfers.push(RegToRegTransfer {
                    src: SortCopyPropEntity(instr.src.try_into().unwrap()),
                    dst: instr.dst.into(),
                });
            }
        }
    }
    let reg_graph = RegGraph::new(transfers);

    // first move registers, then immediates, because the dsts of the immediates
    // may be src of reg-to-reg-moves
    let mut copy_instrs: Vec<_> = reg_graph.into_instructions::<Instruction>().collect();
    copy_instrs.extend(mov_imm);

    // now emit instrs
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

/// SaveRegList stores a list of registers to be saved before
/// and restored after a function call.
#[derive(Debug, Clone)]
struct SaveRegList<Reg> {
    saved_regs: Vec<Reg>,
}

impl<Reg: Clone> SaveRegList<Reg> {
    pub fn len(&self) -> usize {
        self.saved_regs.len()
    }

    pub fn add_regs(&mut self, regs: &[Reg]) {
        self.saved_regs.extend_from_slice(regs)
    }

    pub fn add_regs_from_iter<Regs>(&mut self, regs: Regs)
    where
        Regs: IntoIterator<Item = Reg>,
    {
        self.saved_regs.extend(regs);
    }
    /// Iterate over registers in the order they were added.
    /// Used to emit `pushq` instructions.
    pub fn saves(&self) -> impl Iterator<Item = Reg> + '_ {
        self.saved_regs.iter().cloned()
    }
    /// Iterate over registers in the **reverse order** in which they were
    /// added. Used to emit `popq` instructions corresponding to `pushq`
    /// instructions.
    pub fn restores(&self) -> impl Iterator<Item = Reg> + '_ {
        self.saved_regs.iter().rev().cloned()
    }
}

impl<T> Default for SaveRegList<T> {
    fn default() -> Self {
        SaveRegList { saved_regs: vec![] }
    }
}
