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
    LirInstruction(lir::Instruction),
    Leave(lir::Leave),
}

type VarId = (i64, usize);

#[derive(Debug, Clone)]
pub(super) struct Block {
    /// Unique number for a Block. This number matches it's number in a reversed
    /// postorder of the BlockGraph blocks. This reversed postorder is needed by
    /// the linear scan algorithm
    num: usize,
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
        ValueSlot(slot) => (slot.allocated_in.num, slot.num),
        MultiSlot(slot) => (slot.allocated_in().num, slot.num()),
        Param { idx } => (-1, idx as usize),
        Imm(_) => unreachable!(),
    }
}

pub(super) struct LiveVariableAnalysis {
    cconv: CallingConv,
    graph: Ptr<lir::BlockGraph>,

    pub(super) liveness: HashMap<VarId, HashSet<Block>>,
    pub(super) postorder_blocks: Vec<Block>,
}

impl LiveVariableAnalysis {
    pub fn new(cconv: CallingConv, graph: Ptr<lir::BlockGraph>) -> Self {
        Self {
            cconv,
            graph,
            liveness: HashMap::new(),
            postorder_blocks: vec![],
        }
    }

    pub fn run(&mut self) {
        let mut queue: VecDeque<Ptr<lir::BasicBlock>> = VecDeque::new();
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
        }

        while let Some(block) = queue.pop_front() {
            let code = &block_code_map.get(&block.firm).unwrap().instrs;

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
            for var_id in outs.get(&block.firm).unwrap() {
                changed |= ins.get_mut(&block.firm).unwrap().insert(*var_id);
            }
            // ins(b) += gen(b)
            for var_id in gen {
                changed |= ins.get_mut(&block.firm).unwrap().insert(var_id);
            }

            if changed {
                for pred in block.pred_blocks() {
                    queue.push_back(pred);
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
            instrs.push(Instruction::LirInstruction(lir::Instruction::Movq {
                src: lir::Operand::MultiSlot(*src),
                dst: lir::Operand::ValueSlot(*dst),
            }));
        }
        for instr in &code.body {
            instrs.push(self.gen_instr(instr));
        }
        for lir::CopyPropagation { src, dst } in &code.copy_out {
            instrs.push(Instruction::LirInstruction(lir::Instruction::Movq {
                src: lir::Operand::MultiSlot(*src),
                dst: lir::Operand::ValueSlot(*dst),
            }));
        }
        debug_assert_eq!(code.leave.len(), 1);
        instrs.push(Instruction::Leave(code.leave[0].clone()));

        instrs
    }

    fn gen_instr(&self, instr: &lir::Instruction) -> Instruction {
        if let lir::Instruction::Call { .. } = instr {
            Instruction::Call(FunctionCall::new(self.cconv, instr.clone()))
        } else {
            Instruction::LirInstruction(instr.clone())
        }
    }
}

fn build_gen_kill(code: &[Instruction]) -> (Vec<VarId>, Vec<VarId>) {
    use super::{
        function::FnInstruction,
        lir::{Instruction::*, Leave::*, Operand::*},
    };

    let mut gen = vec![];
    let mut kill = vec![];

    for instr in code {
        match instr {
            Instruction::LirInstruction(lir) => match lir {
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
                    kill.push(var_id(lir::Operand::MultiSlot(*dst)))
                }
                Unop { src, dst, .. } => {
                    match src {
                        Imm(_) => (),
                        _ => gen.push(var_id(*src)),
                    }
                    kill.push(var_id(lir::Operand::MultiSlot(*dst)));
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
                    gen.push(var_id(lir::Operand::MultiSlot(*dst)));
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
