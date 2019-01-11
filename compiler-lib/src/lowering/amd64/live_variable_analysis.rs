use super::{
    function::FunctionCall,
    lir::{self, BasicBlock},
    CallingConv, Operand,
};
use crate::lowering::lir_allocator::Ptr;
use std::{
    collections::{HashMap, HashSet, VecDeque},
    hash::{Hash, Hasher},
};

#[derive(Debug, Clone)]
enum Instruction {
    Call(FunctionCall),
    Lir(lir::Instruction),
    Leave(lir::Leave),
}

type VarId = (i64, usize);

#[derive(Debug, Clone)]
pub(super) struct Block {
    /// Unique number for a Block. This number matches it's number in a reversed
    /// postorder of the BlockGraph blocks. This reversed postorder is needed by
    /// the linear scan algorithm
    pub(super) num: usize,
    instrs: Vec<Instruction>,
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

fn var_id(op: lir::Operand) -> VarId {
    use super::lir::Operand::*;
    match op {
        Slot(slot) => (slot.allocated_in().num, slot.num()),
        Param { idx } => (-1, idx as usize),
        Imm(_) => unreachable!(),
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

        let mut block_code_map = HashMap::new();
        for (num, block) in self.graph.postorder_blocks().iter().rev().enumerate() {
            block_code_map.insert(
                block.firm,
                Block {
                    num,
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
            // outs'(b) = outs(b) - kill(b) => ins(b) = gen(b)+outs'(b)
            for var_id in kill {
                outs.get_mut(&block.firm).unwrap().remove(&var_id);
            }
            // if (outs'(b)+gen(b) != ins(b)) => changed = true
            // sitenote: outs'(b)+gen(b) >= ins(b)
            let mut changed = false;
            // ins(b) += outs'(b)
            for var_id in &outs[&block.firm] {
                changed |= ins.get_mut(&block.firm).unwrap().insert(*var_id);
            }
            // ins(b) += gen(b)
            for var_id in gen {
                changed |= ins.get_mut(&block.firm).unwrap().insert(var_id);
            }

            if changed {
                for pred in block.pred_blocks() {
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
            instrs.push(Instruction::Lir(lir::Instruction::Movq {
                src: lir::Operand::Slot(*src),
                dst: lir::Operand::Slot(dst.multislot()),
            }));
        }
        for instr in &code.body {
            match instr {
                lir::Instruction::Comment(_) => (),
                _ => instrs.push(self.gen_instr(instr)),
            }
        }
        for lir::CopyPropagation { src, dst } in &code.copy_out {
            instrs.push(Instruction::Lir(lir::Instruction::Movq {
                src: lir::Operand::Slot(*src),
                dst: lir::Operand::Slot(dst.multislot()),
            }));
        }
        if let Some(leave) = code.leave.get(0) {
            instrs.push(Instruction::Leave(leave.clone()));
        }

        instrs
    }

    fn gen_instr(&self, instr: &lir::Instruction) -> Instruction {
        if let lir::Instruction::Call { .. } = instr {
            Instruction::Call(FunctionCall::new(self.cconv, instr.clone()))
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

#[allow(clippy::cyclomatic_complexity)] // FIXME !!!!
fn build_gen_kill(code: &[Instruction]) -> (Vec<VarId>, Vec<VarId>) {
    use super::{
        function::FnInstruction,
        lir::{Instruction::*, Leave::*, Operand::*},
    };

    let mut gen = vec![];
    let mut kill = vec![];

    for instr in code {
        match instr {
            Instruction::Lir(lir) => match lir {
                Binop {
                    src1, src2, dst, ..
                }
                | Div { src1, src2, dst }
                | Mod { src1, src2, dst } => {
                    match src1 {
                        Imm(_) => (),
                        _ => gen.push(var_id(*src1)),
                    }
                    match src2 {
                        Imm(_) => (),
                        _ => gen.push(var_id(*src2)),
                    }
                    kill.push(var_id(lir::Operand::Slot(*dst)))
                }
                Unop { src, dst, .. } => {
                    match src {
                        Imm(_) => (),
                        _ => gen.push(var_id(*src)),
                    }
                    kill.push(var_id(lir::Operand::Slot(*dst)));
                }
                Movq { src, dst } => {
                    match src {
                        Imm(_) => (),
                        _ => gen.push(var_id(*src)),
                    }
                    // FIXME: assert that dst != Imm
                    kill.push(var_id(*dst));
                }
                Call { .. } => (), // already converted
                StoreMem {
                    src,
                    dst: lir::AddressComputation { base, index, .. },
                } => {
                    match src {
                        Imm(_) => (),
                        _ => gen.push(var_id(*src)),
                    }
                    match base {
                        Imm(_) => (),
                        _ => gen.push(var_id(*base)),
                    }
                    match index {
                        lir::IndexComputation::Zero => (),
                        lir::IndexComputation::Displacement(op, _) => match op {
                            Imm(_) => (),
                            _ => gen.push(var_id(*op)),
                        },
                    }
                }
                LoadMem {
                    src: lir::AddressComputation { base, index, .. },
                    dst,
                } => {
                    match base {
                        Imm(_) => (),
                        _ => gen.push(var_id(*base)),
                    }
                    match index {
                        lir::IndexComputation::Zero => (),
                        lir::IndexComputation::Displacement(op, _) => match op {
                            Imm(_) => (),
                            _ => gen.push(var_id(*op)),
                        },
                    }
                    gen.push(var_id(lir::Operand::Slot(*dst)));
                }
                Comment(_) => (), // Ignore for LVA
            },
            Instruction::Call(call) => {
                // arg_save/recover only pushes/pops `Amd64Reg` on/from the stack
                for instr in &call.setup {
                    match instr {
                        // dst in setup is always a Reg in a Movq instruction
                        FnInstruction::Movq { src, .. } => match src {
                            Operand::LirOperand(op) => match op {
                                Imm(_) => (),
                                _ => gen.push(var_id(*op)),
                            },
                            Operand::Reg(_) => (),
                        },
                        FnInstruction::Pushq { src } => match src {
                            Operand::LirOperand(op) => match op {
                                Imm(_) => (),
                                _ => gen.push(var_id(*op)),
                            },
                            Operand::Reg(_) => (),
                        },
                        _ => unreachable!(), // Popq, Addq and Ret are not in setup
                    }
                }

                if let Some(res) = call.move_res {
                    if let FnInstruction::Movq { dst, .. } = res {
                        match dst {
                            Operand::LirOperand(op) => match op {
                                Imm(_) => (),
                                _ => kill.push(var_id(op)),
                            },
                            Operand::Reg(_) => unreachable!(),
                        }
                    } else {
                        unreachable!()
                    }
                }

                // recover is just an Addq op with a constant and a register
            }
            Instruction::Leave(leave) => match leave {
                CondJmp { lhs, rhs, .. } => {
                    match lhs {
                        Imm(_) => (),
                        _ => gen.push(var_id(*lhs)),
                    }
                    match rhs {
                        Imm(_) => (),
                        _ => gen.push(var_id(*rhs)),
                    }
                }
                Return {
                    value: Some(value), ..
                } => match value {
                    Imm(_) => (),
                    _ => gen.push(var_id(*value)),
                },
                Jmp { .. } | Return { value: None, .. } => (),
            },
        }
    }
    (gen, kill)
}
