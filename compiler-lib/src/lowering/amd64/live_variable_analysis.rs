use super::{
    function::{FnOperand, FunctionCall},
    lir::{self, BasicBlock, MultiSlot},
    var_id, CallingConv, VarId,
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
    Mov {
        src: lir::CopyPropagationSrc,
        dst: Ptr<MultiSlot>,
    },
    Leave(lir::Leave),
}

#[derive(Debug, Clone)]
pub(super) struct Block {
    /// Unique number for a Block. This number matches it's number in a reversed
    /// postorder of the BlockGraph blocks. This reversed postorder is needed by
    /// the linear scan algorithm
    pub(super) num: usize,
    /// For label generation
    pub(super) firm_num: i64,
    pub(super) instrs: Vec<Instruction>,
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
    queue: VecDeque<Ptr<BasicBlock>>,
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

    pub fn run(&mut self, end_block: Ptr<BasicBlock>) {
        self.gen_queue(end_block, &mut HashSet::new());
        let mut ins: HashMap<libfirm_rs::nodes::Block, HashSet<VarId>> = HashMap::new();
        let mut outs: HashMap<libfirm_rs::nodes::Block, HashSet<VarId>> = HashMap::new();
        let mut block_code_map: HashMap<libfirm_rs::nodes::Block, Block> = HashMap::new();

        for (num, block) in self.graph.postorder_blocks().iter().rev().enumerate() {
            block_code_map.insert(
                block.firm,
                Block {
                    num,
                    firm_num: block.num,
                    instrs: self.gen_code(&block),
                },
            );
            ins.insert(block.firm, HashSet::new());
            outs.insert(block.firm, HashSet::new());
        }

        while let Some(block) = self.queue.pop_front() {
            let code = &block_code_map[&block.firm].instrs;

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
                for pred in block.pred_blocks() {
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

    fn gen_code(&self, block: &BasicBlock) -> Vec<Instruction> {
        let code = &block.code;
        let mut instrs = vec![];

        for lir::CopyPropagation { src, dst } in &code.copy_in {
            instrs.push(Instruction::Mov {
                src: *src,
                dst: dst.multislot(),
            });
        }
        for instr in &code.body {
            match instr {
                lir::Instruction::Comment(_) => (),
                _ => instrs.push(self.gen_instr(instr)),
            }
        }
        for lir::CopyPropagation { src, dst } in &code.copy_out {
            instrs.push(Instruction::Mov {
                src: *src,
                dst: dst.multislot(),
            })
        }
        if let Some(leave) = code.leave.get(0) {
            instrs.push(Instruction::Leave(leave.clone()));
        }

        instrs
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
        end_block: Ptr<BasicBlock>,
        visited: &mut HashSet<libfirm_rs::nodes::Block>,
    ) {
        self.queue.push_back(end_block);

        for pred in end_block.pred_blocks() {
            if visited.insert(pred.firm) {
                self.gen_queue(pred, visited);
            }
        }
    }
}

impl Instruction {
    pub(super) fn src_operands(&self) -> Vec<lir::Operand> {
        use super::lir::{Instruction::*, Leave::*};
        match self {
            Instruction::Lir(lir) => match lir {
                Binop { src1, src2, .. } | Div { src1, src2, .. } | Mod { src1, src2, .. } => {
                    vec![*src1, *src2]
                }
                Unop { src, .. } | Conv { src, .. } => vec![*src],
                Call { .. } => vec![], // already converted
                StoreMem {
                    src,
                    dst: lir::AddressComputation { base, index, .. },
                    ..
                } => {
                    let mut ops = vec![*src, *base];
                    match index {
                        lir::IndexComputation::Zero => (),
                        lir::IndexComputation::Displacement(op, _) => ops.push(*op),
                    }
                    ops
                }
                LoadMem {
                    src: lir::AddressComputation { base, index, .. },
                    ..
                } => {
                    let mut ops = vec![*base];
                    match index {
                        lir::IndexComputation::Zero => (),
                        lir::IndexComputation::Displacement(op, _) => ops.push(*op),
                    }
                    ops
                }
                Comment(_) => vec![],
            },
            Instruction::Mov { src, .. } => vec![(*src).into()],
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
            Instruction::Leave(leave) => match leave {
                CondJmp { lhs, rhs, .. } => vec![*lhs, *rhs],
                Return {
                    value: Some(value), ..
                } => vec![*value],
                Jmp { .. } | Return { value: None, .. } => vec![],
            },
        }
    }

    pub(super) fn dst_operand(&self) -> Option<lir::Operand> {
        use super::{function::FnInstruction, lir::Instruction::*};
        match self {
            Instruction::Lir(lir) => match lir {
                Binop { dst, .. } | Div { dst, .. } | Mod { dst, .. } => {
                    Some(lir::Operand::Slot(*dst))
                }
                Unop { dst, .. } | Conv { dst, .. } => Some(lir::Operand::Slot(*dst)),
                Call { .. } => None,
                StoreMem { .. } => None,
                LoadMem { dst, .. } => Some(lir::Operand::Slot(*dst)),
                Comment(_) => None,
            },
            Instruction::Mov { dst, .. } => Some(lir::Operand::Slot(*dst)),
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
            // No dst for leave instructions
            Instruction::Leave(_) => None,
        }
    }
}

fn build_gen_kill(code: &[Instruction]) -> (Vec<VarId>, Vec<VarId>) {
    use super::lir::Operand::*;

    let mut gen = vec![];
    let mut kill = vec![];

    macro_rules! push {
        ($vec:expr, $op:expr) => {
            match $op {
                Imm(_) => (),
                Slot(slot) => {
                    debug_assert!(slot.firm().mode() != libfirm_rs::Mode::X());
                    if !slot.firm().mode().is_mem() {
                        $vec.push(var_id($op))
                    }
                }
                Param { .. } => $vec.push(var_id($op)),
            }
        };
    }

    for instr in code {
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
