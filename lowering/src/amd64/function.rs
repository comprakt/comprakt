use super::{
    codegen::{self, Codegen},
    linear_scan,
    live_variable_analysis::{self, LiveVariableAnalysis, Operands},
    register::RegisterAllocator,
    var_id, Amd64Reg, CallingConv, VarId,
};
use crate::lowering::{lir, lir_allocator::Ptr};
use interval::{ops::Range, Interval};
use libfirm_rs::Tarval;
use std::{
    collections::{BTreeSet, HashMap, HashSet, VecDeque},
    ops::{Deref, DerefMut},
};

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
    Popq { dst: FnOperand },
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
    /// List of parameters that will be passed in registers
    pub(super) reg_setup: Vec<lir::Operand>,
    /// List of parameters that need to be pushed on the stack
    pub(super) push_setup: VecDeque<lir::Operand>,
    /// Setup code generated after the register allocator assigned locations to
    /// the args in `reg_setup` and `push_setup`
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

        if let lir::Instruction::Call { func, args, dst } = call_instr {
            call.label = func.clone();

            for (i, arg) in args.iter().enumerate() {
                if i < cconv.num_arg_regs() {
                    // Fill the function argument registers
                    call.reg_setup.push(*arg);
                } else {
                    // Push the other args on the stack
                    call.push_setup.push_front(*arg);
                }
            }

            if !call.push_setup.is_empty() {
                // Remove the pushed args from stack after the call
                call.recover = Some(FnInstruction::Addq {
                    src: Tarval::mj_int((call.push_setup.len() * 8) as i64),
                    dst: Amd64Reg::Rsp,
                });
            }

            call.move_res = dst.map(|dst| FnInstruction::Movq {
                src: FnOperand::Reg(Amd64Reg::Rax),
                dst: FnOperand::Lir(lir::Operand::Slot(dst)),
            });
        } else {
            unreachable!("A FunctionCall can only be setup for a Call instruction")
        }

        call
    }

    fn gen_setup(&mut self, var_location: &HashMap<VarId, linear_scan::Location>) {
        for op in &self.push_setup {
            self.setup.push(FnInstruction::Pushq {
                src: FnOperand::Lir(*op),
            });
        }

        debug_assert!(self.reg_setup.len() <= 6);

        let mut transfers = vec![];
        for (i, op) in self.reg_setup.iter().enumerate() {
            match op {
                lir::Operand::Imm(_) => self.setup.push(FnInstruction::Movq {
                    src: FnOperand::Lir(*op),
                    dst: FnOperand::Reg(Amd64Reg::arg(i)),
                }),
                _ => match var_location.get(&var_id(*op)).unwrap() {
                    linear_scan::Location::Reg(reg) => transfers.push(RegToRegTransfer {
                        src: *reg,
                        dst: Amd64Reg::arg(i),
                    }),
                    _ => self.setup.push(FnInstruction::Movq {
                        src: FnOperand::Lir(*op),
                        dst: FnOperand::Reg(Amd64Reg::arg(i)),
                    }),
                },
            }
        }

        let reg_graph = RegGraph::new(transfers);
        let swap_instrs = reg_graph.into_instructions::<FnInstruction>();
        self.setup.extend(swap_instrs);
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
            x if x < 9 => (), // Enough caller save registers available
            9 => self.saved_regs.add_regs(&[Rbx]),
            10 => self.saved_regs.add_regs(&[Rbx, R12]),
            11 => self.saved_regs.add_regs(&[Rbx, R12, R13]),
            12 => self.saved_regs.add_regs(&[Rbx, R12, R13, R14]),
            13 => self.saved_regs.add_regs(&[Rbx, R12, R13, R14, R15]),
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
        for block in lva.postorder_blocks.iter_mut() {
            for instr in block.code.iter_unified_mut() {
                if let lir::CodeInstruction::Body(live_variable_analysis::Instruction::Call(call)) =
                    instr
                {
                    call.gen_setup(&lsa.var_location);
                }
            }
        }

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
            for instr in block.code.iter_unified() {
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
            let end = if last_block_alive == instrs[0].0 && last_block_alive == last_instr.0 {
                last_instr.1
            } else {
                block_last_instr[last_block_alive]
            };
            let interval = Interval::new(instrs[0].1, end);

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

#[derive(Clone)]
pub struct Node<R> {
    reg: R,

    in_: Option<R>,
    outs: HashSet<R>,
}

impl<R: std::hash::Hash + Eq> Node<R> {
    fn new(reg: R) -> Self {
        Self {
            reg,
            in_: None,
            outs: HashSet::new(),
        }
    }

    fn set_in_edge(&mut self, reg: R) {
        debug_assert!(self.in_.is_none());
        self.in_ = Some(reg);
    }

    fn add_out_edge(&mut self, reg: R) {
        self.outs.insert(reg);
    }

    fn is_sink(&self) -> bool {
        self.outs.is_empty()
    }

    fn is_source(&self) -> bool {
        self.in_.is_none()
    }
}

#[derive(Default)]
pub struct RegGraph<R: std::hash::Hash + Eq>(HashMap<R, Node<R>>);

pub struct RegToRegTransfer<R> {
    src: R,
    dst: R,
}

pub enum RegGraphMinLeftEdgeInstruction<R> {
    Push(R),
    Pop(R),
    Mov(RegToRegTransfer<R>),
}

impl<R: std::hash::Hash + Eq + Clone + Copy> RegGraph<R> {
    pub fn new(transfers: Vec<RegToRegTransfer<R>>) -> Self {
        let mut reg_graph = Self(HashMap::new());
        for RegToRegTransfer { src, dst } in transfers {
            if src != dst {
                let src_node = Node::new(src);
                let dst_node = Node::new(dst);
                reg_graph.entry(src).or_insert(src_node).add_out_edge(dst);
                reg_graph.entry(dst).or_insert(dst_node).set_in_edge(src);
            }
        }

        reg_graph
    }

    /// This is a greedy cycle removal algorithm from "Graph Drawing: Algorithms
    /// for the Visualization of Graphs" by Eades et al.
    // TODO(flip1995): comment what it does
    // TODO(problame): use Vecs (r_nodes.push_front) can be replace by reversing the
    // vec in the end)
    fn gen_node_list_with_min_left_edges(mut self) -> VecDeque<Node<R>> {
        let mut l_nodes = VecDeque::new();
        let mut r_nodes = VecDeque::new();
        let mut visited = HashSet::new();

        while !self.is_empty() {
            let mut sink_exists = true;
            while sink_exists {
                let sinks: Vec<_> = self
                    .values()
                    .filter(|node| {
                        node.is_sink() || node.outs.iter().all(|reg| visited.contains(reg))
                    })
                    .cloned()
                    .collect();
                sink_exists = false;
                for node in &sinks {
                    self.remove(&node.reg);
                    visited.insert(node.reg);
                    r_nodes.push_front(node.clone());
                    if let Some(pred) = &node.in_ {
                        if let Some(pred_node) = self.get(&pred) {
                            if pred_node.outs.iter().all(|reg| visited.contains(reg)) {
                                if pred_node.in_.is_none() {
                                    // Isolated node
                                    r_nodes.push_front(pred_node.clone());
                                    self.remove(&pred);
                                } else {
                                    sink_exists = true;
                                }
                            }
                        }
                    }
                }
            }

            let mut source_exists = true;
            while source_exists {
                let sources: Vec<_> = self
                    .values()
                    .filter(|node| node.is_source() || visited.contains(&node.in_.unwrap()))
                    .cloned()
                    .collect();
                source_exists = false;
                for node in &sources {
                    self.remove(&node.reg);
                    visited.insert(node.reg);
                    l_nodes.push_back(node.clone());
                    source_exists = true;
                }
            }

            if !self.is_empty() {
                let node = self
                    .values()
                    .max_by_key(|node| {
                        node.outs
                            .iter()
                            .filter(|reg| !visited.contains(reg))
                            .count()
                            - if node.in_.is_some() { 1 } else { 0 }
                    })
                    .cloned()
                    .unwrap();
                self.remove(&node.reg);
                visited.insert(node.reg);
                l_nodes.push_back(node);
            }
        }

        l_nodes.append(&mut r_nodes);
        l_nodes
    }

    pub fn into_instructions<I: From<RegGraphMinLeftEdgeInstruction<R>>>(
        self,
    ) -> impl Iterator<Item = I> {
        let mut instrs = vec![];

        let reg_graph_len = self.len();
        let nodes = self.gen_node_list_with_min_left_edges();

        debug_assert_eq!(nodes.len(), reg_graph_len);

        let mut recover = vec![];
        use self::RegGraphMinLeftEdgeInstruction::*;
        let mut visited = HashSet::new();
        for node in nodes.iter().rev() {
            visited.insert(node.reg);
            for out in node.outs.iter().filter(|reg| !visited.contains(reg)) {
                instrs.push(Push(node.reg));
                recover.push(Pop(*out));
            }

            if let Some(in_) = node.in_ {
                if !visited.contains(&in_) {
                    instrs.push(Mov(RegToRegTransfer {
                        src: in_,
                        dst: node.reg,
                    }));
                }
            }
        }
        instrs.extend(recover);
        instrs.into_iter().map(From::from)
    }
}

impl From<RegGraphMinLeftEdgeInstruction<Amd64Reg>> for FnInstruction {
    fn from(i: RegGraphMinLeftEdgeInstruction<Amd64Reg>) -> Self {
        match i {
            RegGraphMinLeftEdgeInstruction::Push(reg) => FnInstruction::Pushq {
                src: FnOperand::Reg(reg),
            },
            RegGraphMinLeftEdgeInstruction::Pop(reg) => FnInstruction::Popq {
                dst: FnOperand::Reg(reg),
            },
            RegGraphMinLeftEdgeInstruction::Mov(RegToRegTransfer { src, dst }) => {
                FnInstruction::Movq {
                    src: FnOperand::Reg(src),
                    dst: FnOperand::Reg(dst),
                }
            }
        }
    }
}

impl<R: std::hash::Hash + Eq> Deref for RegGraph<R> {
    type Target = HashMap<R, Node<R>>;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl<R: std::hash::Hash + Eq> DerefMut for RegGraph<R> {
    fn deref_mut(&mut self) -> &mut HashMap<R, Node<R>> {
        &mut self.0
    }
}
