use super::{
    function::{FnOperand, FunctionCall},
    lir, var_id, CallingConv, VarId,
};
use crate::lowering::lir_allocator::Ptr;
use libfirm_rs::nodes::NodeTrait;
use std::{
    collections::{HashMap, HashSet, VecDeque},
    hash::{Hash, Hasher},
};

#[derive(Debug, Clone)]
pub(super) enum Instruction {
    // Clippy wants this boxed
    Call(Box<FunctionCall>),
    Lir(lir::Instruction),
}

pub type Mov = lir::CopyPropagation;

type Code = lir::Code<Mov, Mov, Instruction, lir::Leave>;

#[derive(Debug, Clone)]
pub(super) struct Block {
    /// Unique number for a Block. This number matches it's number in a reversed
    /// postorder of the BlockGraph blocks. This reversed postorder is needed by
    /// the linear scan algorithm
    pub(super) num: usize,
    /// For label generation
    pub(super) firm_num: i64,
    pub(super) code: Code,
}

impl PartialEq for Block {
    fn eq(&self, other: &Block) -> bool {
        self.num == other.num
    }
}

impl Eq for Block {}

impl Hash for Block {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.num.hash(state);
    }
}

pub(super) struct LiveVariableAnalysis {
    cconv: CallingConv,
    queue: VecDeque<Ptr<lir::BasicBlock>>,
    graph: Ptr<lir::BlockGraph>,

    pub(super) liveness: HashMap<VarId, HashSet<Block>>,
    pub(super) postorder_blocks: Vec<Block>,
}

impl LiveVariableAnalysis {
    pub fn new(cconv: CallingConv, graph: Ptr<lir::BlockGraph>) -> Self {
        Self {
            cconv,
            queue: VecDeque::new(),
            graph,
            liveness: HashMap::new(),
            postorder_blocks: vec![],
        }
    }

    pub fn run(&mut self, graph: Ptr<lir::BlockGraph>) {
        self.gen_queue(graph, &mut HashSet::new());
        let mut ins: HashMap<libfirm_rs::nodes::Block, HashSet<VarId>> = HashMap::new();
        let mut outs: HashMap<libfirm_rs::nodes::Block, HashSet<VarId>> = HashMap::new();
        let mut block_code_map: HashMap<libfirm_rs::nodes::Block, Block> = HashMap::new();

        for (num, block) in self.graph.postorder_blocks().iter().rev().enumerate() {
            block_code_map.insert(
                block.firm,
                Block {
                    num,
                    firm_num: block.num,
                    code: self.gen_code(&block),
                },
            );
            ins.insert(block.firm, HashSet::new());
            outs.insert(block.firm, HashSet::new());
        }

        while let Some(block) = self.queue.pop_front() {
            let code = &block_code_map[&block.firm].code;

            // ins(b) = f_b(outs) = gen(b)+(outs(b)-kill(b))
            let (gen, kill) = build_gen_kill(code);
            let cur_outs = outs.get_mut(&block.firm).unwrap();
            let cur_ins = ins.get_mut(&block.firm).unwrap();
            log::debug!("Gen: {:?}, Kill: {:?}", gen, kill);

            // outs'(b) = outs(b) - kill(b) => ins(b) = gen(b)+outs'(b)
            for var_id in &kill {
                cur_outs.remove(&var_id);
            }
            // if (outs'(b)+gen(b) != ins(b)) => changed = true
            // sitenote: outs'(b)+gen(b) >= ins(b)
            let mut changed = false;
            // ins(b) += outs'(b)
            for var_id in &outs[&block.firm] {
                changed |= cur_ins.insert(*var_id);
            }
            // ins(b) += gen(b)
            for var_id in gen {
                changed |= cur_ins.insert(var_id);
            }

            if changed {
                for pred in block.preds.iter().cloned() {
                    for var_id in &ins[&block.firm] {
                        outs.get_mut(&pred.firm).unwrap().insert(*var_id);
                    }
                    self.queue.push_back(pred);
                }
            }
        }

        self.postorder_blocks = block_code_map.values().cloned().collect();
        self.postorder_blocks.sort_by(|a, b| a.num.cmp(&b.num));
        for (firm_block, alive_vars) in ins.into_iter() {
            let block = block_code_map
                .remove(&firm_block)
                .expect("Every block is in the code map exactly once");
            for var_id in alive_vars {
                self.liveness
                    .entry(var_id)
                    .or_default()
                    .insert(block.clone());
            }
        }
    }

    fn gen_code(&self, block: &lir::BasicBlock) -> Code {
        let lir_code = &block.code; // lir::Code

        let mut code = Code::default();

        code.copy_in.extend(lir_code.copy_in.iter().cloned());
        for instr in &lir_code.body {
            match instr {
                lir::Instruction::Comment(_) => (), // TODO @flip1995 why ignore comments?
                _ => code.body.push(self.gen_instr(instr)),
            }
        }
        code.copy_out.extend(lir_code.copy_out.iter().cloned());
        code.leave.extend(lir_code.leave.iter().cloned());

        code
    }

    fn gen_instr(&self, instr: &lir::Instruction) -> Instruction {
        if let lir::Instruction::Call { func, .. } = instr {
            let cconv = match &**func {
                "mjrt_system_out_println"
                | "mjrt_system_out_write"
                | "mjrt_system_out_flush"
                | "mjrt_system_in_read"
                | "mjrt_new" => CallingConv::X86_64,
                "mjrt_dumpstack"
                | "mjrt_div_by_zero"
                | "mjrt_null_usage"
                | "mjrt_array_out_of_bounds" => unimplemented!(),
                _ => self.cconv,
            };
            Instruction::Call(box FunctionCall::new(cconv, instr))
        } else {
            Instruction::Lir(instr.clone())
        }
    }

    fn gen_queue(
        &mut self,
        graph: Ptr<lir::BlockGraph>,
        visited: &mut HashSet<libfirm_rs::nodes::Block>,
    ) {
        if graph.end_block.preds.is_empty() {
            // this happens when the function contains an infinite loop and does not return
            // (FIRM end block doesn't have cfg_preds in that case)
            // => see integration-tests/timeout/const_optimizable_while.mj
            //
            // NOTE: This is only the implication
            //        graph.end_block.preds.is_empty() => does not return
            // The reverse implication is not true, according to @hediet
            // => do not extract this into a `fn graph.does_not_return() -> bool`

            // enqueue all blocks in random order: slows the analysis but oh well
            self.queue.extend(graph.iter_blocks());
        } else {
            fn gen_queue_dfs(
                queue: &mut VecDeque<Ptr<lir::BasicBlock>>,
                block: Ptr<lir::BasicBlock>,
                visited: &mut HashSet<libfirm_rs::nodes::Block>,
            ) {
                queue.push_back(block);
                for pred in block.preds.iter().cloned() {
                    if visited.insert(pred.firm) {
                        gen_queue_dfs(queue, pred, visited);
                    }
                }
            }
            gen_queue_dfs(&mut self.queue, graph.end_block, visited);
        }
    }
}

pub(super) trait Operands {
    fn src_operands(&self) -> Vec<lir::Operand>;
    fn dst_operand(&self) -> Option<lir::Operand>;
}

use std::borrow::Borrow;

// note the repeated A (for Mov)
impl<A, B, C> Operands for lir::CodeInstruction<A, A, B, C>
where
    A: Borrow<Mov>,
    B: Borrow<Instruction>,
    C: Borrow<lir::Leave>,
{
    fn src_operands(&self) -> Vec<lir::Operand> {
        use super::lir::{CodeInstruction as CI, Instruction::*, Leave::*, LoadMem, StoreMem};
        match self {
            CI::Body(body) => match body.borrow() {
                Instruction::Lir(lir) => match lir {
                    LoadParam { dst, .. } => vec![lir::Operand::Var(*dst)],
                    Binop { src1, src2, .. } | Div { src1, src2, .. } | Mod { src1, src2, .. } => {
                        vec![*src1, *src2]
                    }
                    Unop { src, .. } | Conv { src, .. } => vec![*src],
                    Call { .. } => vec![], // already converted
                    StoreMem(StoreMem {
                        src,
                        dst: lir::AddressComputation { base, index, .. },
                        ..
                    }) => {
                        let mut ops = vec![*src, *base];
                        match index {
                            lir::IndexComputation::Zero => (),
                            lir::IndexComputation::Displacement(op, _) => ops.push(*op),
                        }
                        ops
                    }
                    LoadMem(LoadMem {
                        src: lir::AddressComputation { base, index, .. },
                        ..
                    }) => {
                        let mut ops = vec![*base];
                        match index {
                            lir::IndexComputation::Zero => (),
                            lir::IndexComputation::Displacement(op, _) => ops.push(*op),
                        }
                        ops
                    }
                    Comment(_) => vec![],
                },
                Instruction::Call(call) => {
                    // arg_save/recover only pushes/pops `Amd64Reg` on/from the stack
                    let mut ops = vec![];

                    for op in &call.reg_setup {
                        ops.push(*op);
                    }
                    for op in &call.push_setup {
                        ops.push(*op);
                    }
                    // src of move_res is always %rax
                    // recover is just an Addq op with a constant and a register
                    ops
                }
            },
            CI::CopyIn(mov) | CI::CopyOut(mov) => {
                let Mov { src, .. } = mov.borrow();
                vec![(*src).into()]
            }
            CI::Leave(leave) => match leave.borrow() {
                CondJmp { lhs, rhs, .. } => vec![*lhs, *rhs],
                Return {
                    value: Some(value), ..
                } => vec![*value],
                Jmp { .. } | Return { value: None, .. } => vec![],
            },
        }
    }

    fn dst_operand(&self) -> Option<lir::Operand> {
        use super::{
            function::FnInstruction,
            lir::{CodeInstruction as CI, Instruction::*, LoadMem, StoreMem},
        };
        match self {
            CI::Body(body) => match body.borrow() {
                Instruction::Lir(lir) => match lir {
                    // LoadParam::dst is a src_operand for the purposes of LVA:
                    LoadParam { .. } => None,
                    Binop { dst, .. } | Div { dst, .. } | Mod { dst, .. } => {
                        Some(lir::Operand::Var(*dst))
                    }
                    Unop { dst, .. } | Conv { dst, .. } => Some(lir::Operand::Var(*dst)),
                    Call { .. } => None,
                    StoreMem(StoreMem { .. }) => None,
                    LoadMem(LoadMem { dst, .. }) => Some(lir::Operand::Var(*dst)),
                    Comment(_) => None,
                },
                Instruction::Call(call) => {
                    // arg_save/recover only pushes/pops `Amd64Reg` on/from the stack

                    // Setup just moves in registers or pushes on the stack

                    if let Some(res) = call.move_res {
                        // src of move_res is always %rax
                        if let FnInstruction::Movq { dst, .. } = res {
                            match dst {
                                FnOperand::Lir(op) => return Some(op),
                                FnOperand::Reg(_) => unreachable!(),
                            }
                        } else {
                            unreachable!()
                        }
                    }

                    // recover is just an Addq op with a constant and a register

                    None
                }
            },
            CI::CopyIn(mov) | CI::CopyOut(mov) => {
                let Mov { dst, .. } = mov.borrow();
                Some(lir::Operand::Var(*dst))
            }
            // No dst for leave instructions
            CI::Leave(_) => None,
        }
    }
}

fn build_gen_kill(code: &Code) -> (Vec<VarId>, Vec<VarId>) {
    use super::lir::Operand::*;

    let mut gen = vec![];
    let mut kill = vec![];

    macro_rules! push {
        ($vec:expr, $op:expr) => {
            match $op {
                Imm(_) => (),
                Var(var) => {
                    debug_assert!(var.firm().mode() != libfirm_rs::Mode::X());
                    if !var.firm().mode().is_mem() {
                        $vec.push(var_id($op))
                    }
                }
            }
        };
    }

    for instr in code.iter_unified() {
        for op in instr.src_operands() {
            match op {
                lir::Operand::Imm(_) => (),
                _ => {
                    if !kill.contains(&var_id(op)) {
                        push!(gen, op);
                    }
                }
            }
        }
        if let Some(op) = instr.dst_operand() {
            push!(kill, op);
        }
    }

    (gen, kill)
}
