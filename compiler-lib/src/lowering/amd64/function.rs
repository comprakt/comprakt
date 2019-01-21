use super::{
    codegen::{self, Codegen},
    linear_scan,
    live_variable_analysis::LiveVariableAnalysis,
    register::RegisterAllocator,
    var_id, Amd64Reg, CallingConv, VarId,
};
use crate::lowering::{lir, lir_allocator::Ptr};
use interval::{ops::Range, Interval};
use libfirm_rs::Tarval;
use std::collections::{BTreeSet, HashMap};

/// SaveRegList stores a list of registers to be saved before
/// and restored after a function call.
#[derive(Debug, Clone)]
pub struct SaveRegList<Reg> {
    saved_regs: Vec<Reg>,
}

impl<Reg: Clone> SaveRegList<Reg> {
    pub fn len(&self) -> usize {
        self.saved_regs.len()
    }
    pub fn add_regs(&mut self, save_list: &[Reg]) {
        self.saved_regs.extend_from_slice(save_list);
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

type Label = String;

#[derive(Debug, Copy, Clone)]
pub(super) enum FnInstruction {
    Movq { src: FnOperand, dst: FnOperand },
    Pushq { src: FnOperand },
    Addq { src: Tarval, dst: Amd64Reg },
    Subq { src: Tarval, dst: Amd64Reg },
    Leave,
    Ret,
}

#[derive(Debug, Copy, Clone)]
pub(super) enum FnOperand {
    Lir(lir::Operand),
    Reg(Amd64Reg),
}

impl std::fmt::Display for FnOperand {
    fn fmt(&self, fmt: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            FnOperand::Lir(op) => write!(fmt, "{}", op),
            FnOperand::Reg(reg) => write!(fmt, "{}", reg),
        }
    }
}

#[derive(Debug, Default, Clone)]
pub(super) struct FunctionCall {
    /// Arguments to save before & restore after call.
    pub(super) saved_regs: SaveRegList<Amd64Reg>,
    /// Put the parameters where they belong
    pub(super) setup: Vec<FnInstruction>,
    /// Call label
    pub(super) label: Label,
    /// If a result is produced, move it in the register
    pub(super) move_res: Option<FnInstruction>,
    /// If arguments were put on the stack, reset the stack pointer
    pub(super) recover: Option<FnInstruction>,
}

impl FunctionCall {
    pub(super) fn new(cconv: CallingConv, call_instr: &lir::Instruction) -> Self {
        let mut call = Self::default();

        match cconv {
            CallingConv::X86_64 => call.setup_x86_64_cconv(call_instr),
            CallingConv::Stack => call.setup_stack_cconv(call_instr),
        }

        call
    }

    fn setup_x86_64_cconv(&mut self, call: &lir::Instruction) {
        if let lir::Instruction::Call { func, args, dst } = call {
            self.label = func.clone();

            let mut push_setup = vec![];
            for (i, arg) in args.iter().enumerate() {
                if i < CallingConv::X86_64.num_arg_regs() {
                    // Fill the function argument registers
                    self.setup.push(FnInstruction::Movq {
                        src: FnOperand::Lir(*arg),
                        dst: FnOperand::Reg(Amd64Reg::arg(i)),
                    });
                } else {
                    // Push the other args on the stack
                    push_setup.push(FnInstruction::Pushq {
                        src: FnOperand::Lir(*arg),
                    });
                }
            }
            if !push_setup.is_empty() {
                // Remove the pushed args from stack after the call
                self.recover = Some(FnInstruction::Addq {
                    src: Tarval::mj_int((push_setup.len() * 8) as i64),
                    dst: Amd64Reg::Rsp,
                });

                // Rev the pushed args order: .., 8, 7, 6
                self.setup
                    .append(&mut push_setup.into_iter().rev().collect());
            }

            self.move_res = dst.map(|dst| FnInstruction::Movq {
                src: FnOperand::Reg(Amd64Reg::Rax),
                dst: FnOperand::Lir(lir::Operand::Slot(dst)),
            });
        } else {
            unreachable!("A FunctionCall can only be setup for a Call instruction")
        }
    }

    fn setup_stack_cconv(&mut self, call: &lir::Instruction) {
        if let lir::Instruction::Call { func, args, dst } = call {
            self.label = func.clone();

            for arg in args.iter().rev() {
                self.setup.push(FnInstruction::Pushq {
                    src: FnOperand::Lir(*arg),
                });
            }

            if !self.setup.is_empty() {
                // Remove the pushed args from stack after the call
                self.recover = Some(FnInstruction::Addq {
                    src: Tarval::mj_int((self.setup.len() * 8) as i64),
                    dst: Amd64Reg::Rsp,
                });
            }

            self.move_res = dst.map(|dst| FnInstruction::Movq {
                src: FnOperand::Reg(Amd64Reg::Rax),
                dst: FnOperand::Lir(lir::Operand::Slot(dst)),
            });
        } else {
            unreachable!("A FunctionCall can only be setup for a Call instruction")
        }
    }

    pub(super) fn save_reg(&mut self, reg: Amd64Reg) {
        log::debug!("Save reg: {:?}", reg);
        if reg.is_caller_save() {
            self.saved_regs.add_regs(&[reg]);
        }
    }
}

pub struct Function {
    /// Number of arguments
    pub(super) nargs: usize,
    /// Calling convention
    pub(super) cconv: CallingConv,
    /// Function name
    name: String,
    /// Setup of the function. Get's filled initially
    pub(super) prolog: Vec<FnInstruction>,
    /// Save & restore callee-save registers.
    /// Filled by `self.save_callee_save_regs` after register allocation.
    pub(super) saved_regs: SaveRegList<Amd64Reg>,
    /// Allocates stack memory. An extra function needs to be called
    pub(super) allocate: Option<FnInstruction>,
    /// Restore of previous stack pointer and return. Get's filled initially
    pub(super) epilog: Vec<FnInstruction>,
    /// Instructions of this function generated by calling `gen_code(..)`
    instrs: Vec<codegen::Instruction>,
}

impl Function {
    pub fn new(nargs: usize, cconv: CallingConv, name: &str) -> Self {
        let mut function = Self {
            nargs,
            cconv,
            name: name.to_string(),
            prolog: vec![],
            saved_regs: SaveRegList::default(),
            allocate: None,
            epilog: vec![],
            instrs: vec![],
        };

        function.prolog.push(FnInstruction::Pushq {
            src: FnOperand::Reg(Amd64Reg::Rbp),
        });
        function.prolog.push(FnInstruction::Movq {
            src: FnOperand::Reg(Amd64Reg::Rsp),
            dst: FnOperand::Reg(Amd64Reg::Rbp),
        });

        function.epilog.push(FnInstruction::Leave);
        function.epilog.push(FnInstruction::Ret);

        function
    }

    pub(super) fn allocate_stack(&mut self, slots: usize) {
        if slots > 0 {
            self.allocate = Some(FnInstruction::Subq {
                src: Tarval::mj_int(8 * slots as i64),
                dst: Amd64Reg::Rsp,
            });
        }
    }

    /// This function should be called by the register allocator, after
    /// determining how many registers will be required for a function. If
    /// callee_save registers are needed to satisfy the register pressure,
    /// it will push these registers on the stack, before the function code
    /// is executed and restores them, after the function finished.
    ///
    /// The `num_regs_required` is the amount of registers that are required by
    /// this function, inclusive the reserved argument registers
    ///
    /// # Panics
    ///
    /// This function panics, if the number of required registers is higher,
    /// than the total available registers.
    pub(super) fn save_callee_save_regs(&mut self, num_regs_required: usize) {
        use super::Amd64Reg::*;
        // There are 5 callee save registers: %rbx, %r12-r15
        // %rbp is also callee save, but we never allocate this register
        // There are 10 caller save registers, but %rsp is reserved, so we need to save
        // registers if more than 9 registers are required.
        match num_regs_required {
            x if x < 10 => (), // Enough caller save registers available
            10 => self.saved_regs.add_regs(&[Rbx]),
            11 => self.saved_regs.add_regs(&[Rbx, R12]),
            12 => self.saved_regs.add_regs(&[Rbx, R12, R13]),
            13 => self.saved_regs.add_regs(&[Rbx, R12, R13, R14]),
            14 => self.saved_regs.add_regs(&[Rbx, R12, R13, R14, R15]),
            _ => unreachable!("More registers required than available"),
        }
    }

    pub(super) fn gen_code(&mut self, graph: Ptr<lir::BlockGraph>) {
        let mut lva = LiveVariableAnalysis::new(self.cconv, graph);
        lva.run(graph.end_block);

        let mut lsa = self.build_lsa(&lva);
        lsa.run(&mut lva.postorder_blocks);

        self.save_callee_save_regs(lsa.num_regs_required);
        self.allocate_stack(lsa.stack_vars_counter);

        let mut codegen = Codegen::new(lsa.var_location, self.cconv);
        self.instrs = codegen.run(&self, lva.postorder_blocks);
    }

    pub fn emit_asm(&self, out: &mut impl std::io::Write) -> std::io::Result<()> {
        self.function_prolog(out)?;
        for instr in &self.instrs {
            writeln!(out, "{}", instr)?;
        }
        self.function_epilog(out)?;

        Ok(())
    }

    fn function_prolog(&self, out: &mut impl std::io::Write) -> std::io::Result<()> {
        writeln!(out, "# -- Begin  {}", self.name)?;
        // "\t.p2align %u,%s,%u\n", po2alignment, fill_byte, maximum_skip
        writeln!(out, "\t.p2align  4,,15")?; // .p2align 4,,15

        writeln!(out, "\t.globl  {}", self.name)?; // .globl mj_main
        writeln!(out, "\t.type\t{}, @function", self.name)?;
        writeln!(out, "{}:", self.name)
    }

    fn function_epilog(&self, out: &mut impl std::io::Write) -> std::io::Result<()> {
        writeln!(out, "\t.size\t{name}, .-{name}", name = self.name)?;
        writeln!(out, "# -- End {}\n", self.name)
    }

    fn build_lsa(&self, lva: &LiveVariableAnalysis) -> linear_scan::LinearScanAllocator {
        let mut instr_counter = 0;
        let mut map: HashMap<VarId, Vec<(usize, usize)>> = HashMap::new();
        let mut block_last_instr = vec![];
        for block in &lva.postorder_blocks {
            for instr in &block.instrs {
                for op in instr.src_operands() {
                    match op {
                        lir::Operand::Imm(_) => (),
                        _ => map
                            .entry(var_id(op))
                            .or_default()
                            .push((block.num, instr_counter)),
                    }
                }
                if let Some(dst) = instr.dst_operand() {
                    match dst {
                        lir::Operand::Imm(_) => unreachable!(),
                        _ => map
                            .entry(var_id(dst))
                            .or_default()
                            .push((block.num, instr_counter)),
                    }
                }
                instr_counter += 1;
            }
            block_last_instr.push(instr_counter - 1);
        }

        let mut var_live = BTreeSet::new();
        for (var_id, instrs) in &map {
            let last_instr = instrs.iter().last().unwrap();
            let last_block_alive = lva.liveness.get(&var_id).map_or(last_instr.0, |blocks| {
                blocks.iter().max_by(|a, b| a.num.cmp(&b.num)).unwrap().num
            });
            let interval = Interval::new(
                instrs[0].1,
                if last_block_alive == last_instr.0 {
                    last_instr.1
                } else {
                    block_last_instr[last_block_alive]
                },
            );

            var_live.insert(linear_scan::LiveRange {
                var_id: *var_id,
                interval,
            });
        }

        debug_assert_eq!(var_live.len(), map.len());

        linear_scan::LinearScanAllocator::new(
            RegisterAllocator::new(self.nargs, self.cconv),
            var_live,
        )
    }
}
