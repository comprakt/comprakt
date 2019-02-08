//! Live-variable analysis used by linear-scan.

use super::{
    linear_scan,
    lir::{self, BasicBlock, Var},
};
use crate::{
    allocator::{HashPtr, Ptr},
    lir::Allocator,
};
use gcollections::ops::bounded::Bounded;
use interval::{ops::Range, Interval};
use std::{
    cmp::Ordering,
    collections::{BTreeSet, HashMap, HashSet, VecDeque},
    hash::{Hash, Hasher},
    iter::FromIterator,
};

pub(crate) struct ScheduledInstr {
    preds: Vec<Ptr<ScheduledInstr>>,
    pub(crate) lir: lir::CodeInstruction<
        Ptr<lir::CopyPropagation>,
        Ptr<lir::CopyPropagation>,
        Ptr<lir::Instruction>,
        Ptr<lir::Leave>,
    >,
    idx: usize,
}

impl PartialEq for Ptr<ScheduledInstr> {
    fn eq(&self, other: &Self) -> bool {
        self.idx == other.idx
    }
}

impl Eq for Ptr<ScheduledInstr> {}

impl PartialOrd for Ptr<ScheduledInstr> {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for Ptr<ScheduledInstr> {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.idx.cmp(&other.idx)
    }
}

use std::fmt;

impl fmt::Debug for Ptr<ScheduledInstr> {
    fn fmt(&self, fmt: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.lir.fmt(fmt)
    }
}

pub(crate) struct LVAResult {
    pub(crate) scheduled_instrs: Vec<Ptr<ScheduledInstr>>,
    pub(crate) live_ranges_by_start: BTreeSet<LiveRange>,
    pub(crate) lsa_params_list: Vec<linear_scan::Param>,
}

/// `LiveRange` holds the liveness interval for every `Var`. This implements
/// Ord, so that `LiveRange`s are sorted by the lower bound of their interval.
#[derive(Debug, Eq, PartialEq, Copy, Clone)]
pub(crate) struct LiveRange {
    pub(crate) var: Var,
    pub(crate) interval: Interval<usize>,
}

impl Ord for LiveRange {
    fn cmp(&self, other: &LiveRange) -> Ordering {
        match self.interval.lower().cmp(&other.interval.lower()) {
            Ordering::Equal => self.var.num().cmp(&other.var.num()),
            ord => ord,
        }
    }
}

impl PartialOrd for LiveRange {
    fn partial_cmp(&self, other: &LiveRange) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

pub(crate) fn live_variable_analysis(
    blocks_scheduled: &[Ptr<BasicBlock>],
    alloc: &Allocator,
) -> LVAResult {
    let scheduled_instrs = build_scheduled_instrs(blocks_scheduled, alloc);
    log::debug!("{:#?}", scheduled_instrs);
    let liveness = build_liveness(&scheduled_instrs);
    let (live_ranges_by_start, lsa_params_list) = build_live_ranges(&scheduled_instrs, liveness);
    LVAResult {
        scheduled_instrs,
        live_ranges_by_start,
        lsa_params_list,
    }
}

struct Counter {
    c: usize,
}

impl Counter {
    /// increments counter and returns the previous counter value
    fn inc(&mut self) -> usize {
        let prev = self.c;
        self.c += 1;
        prev
    }
}

fn build_scheduled_instrs<'lir>(
    blocks_scheduled: &'lir [Ptr<BasicBlock>],
    alloc: &Allocator,
) -> Vec<Ptr<ScheduledInstr>> {
    // the basic block B we are at => the list of basic blocks that need B's last
    // instruction in their preds member
    let mut patchup: HashMap<HashPtr<BasicBlock>, Vec<Ptr<ScheduledInstr>>> = HashMap::new();
    let mut leaves: HashMap<HashPtr<BasicBlock>, Ptr<ScheduledInstr>> = HashMap::new();
    let mut out = Vec::new();
    let mut instr_counter = Counter { c: 0 };
    for block in blocks_scheduled.iter().cloned().map(HashPtr::from) {
        let mut pred_in_block: Option<Ptr<ScheduledInstr>> = None;
        for lir_instr in block.code.iter_unified() {
            let sched_instr = ScheduledInstr {
                preds: pred_in_block.iter().cloned().collect(),
                lir: lir_instr.clone_ptred(),
                idx: instr_counter.inc(),
            };
            let sched_instr = alloc.scheduled_instr(sched_instr);
            out.push(sched_instr);
            debug_assert!(instr_counter.c == out.len());
            if pred_in_block.is_none() {
                for pred in block.preds.iter().cloned().map(HashPtr::from) {
                    patchup.entry(pred).or_default().push(sched_instr);
                }
            }
            pred_in_block = Some(sched_instr);
        }
        // leave is always the last instr
        if let Some(leave_instr) = pred_in_block {
            leaves.insert(block, leave_instr);
        }
    }
    for (cur_block, blocks_that_cur_block_is_pred_of) in patchup {
        blocks_that_cur_block_is_pred_of
            .into_iter()
            .for_each(|mut i| {
                let leave = leaves[&cur_block];
                i.preds.push(leave);
            });
    }
    out
}

/// `usize` is the `scheduled_instrs.idx`
type Liveness = HashMap<Var, BTreeSet<usize>>;

fn build_liveness(scheduled_instrs: &[Ptr<ScheduledInstr>]) -> Liveness {
    let mut queue = VecDeque::from_iter(scheduled_instrs.iter().cloned().rev());
    let mut ins: HashMap<HashPtr<ScheduledInstr>, HashSet<Var>> = HashMap::new();
    let mut outs: HashMap<HashPtr<ScheduledInstr>, HashSet<Var>> = HashMap::new();

    while let Some(instr) = queue.pop_front() {
        let gen = instr.lir.src_operands();
        let kill = instr.lir.dst_operand();

        let gen = gen.iter().filter_map(|op| {
            if let lir::Operand::Var(var) = op {
                Some(var)
            } else {
                None
            }
        });

        let cur_ins = ins.entry(instr.into()).or_default();
        let cur_outs = outs.entry(instr.into()).or_default();
        log::debug!("Gen: {:?}, Kill: {:?}", gen, kill);

        // outs'(b) = outs(b) - kill(b) => ins(b) = gen(b)+outs'(b)
        if let Some(var) = kill {
            cur_outs.remove(&var);
        }
        // if (outs'(b)+gen(b) != ins(b)) => changed = true
        // sitenote: outs'(b)+gen(b) >= ins(b)
        let mut changed = false;
        // ins(b) += outs'(b)
        for var in cur_outs.iter() {
            changed |= cur_ins.insert(*var);
        }
        // ins(b) += gen(b)
        for var in gen {
            changed |= cur_ins.insert(*var);
        }

        if changed {
            for pred in &instr.preds {
                for var in cur_ins.iter() {
                    outs.entry((*pred).into()).or_default().insert(*var);
                }
                queue.push_back(*pred);
            }
        }
    }

    let mut liveness = Liveness::new();
    for (instr, alive_vars) in ins.into_iter() {
        for var in alive_vars {
            liveness.entry(var).or_default().insert(instr.idx);
        }
    }

    liveness
}

fn build_live_ranges(
    scheduled_instrs: &[Ptr<ScheduledInstr>],
    liveness: Liveness,
) -> (BTreeSet<LiveRange>, Vec<linear_scan::Param>) {
    let mut defs_and_uses: HashMap<Var, Vec<usize>> = HashMap::new();
    for (i, instr) in scheduled_instrs.iter().enumerate() {
        for op in instr.lir.src_operands() {
            match op {
                lir::Operand::Imm(_) => (),
                lir::Operand::Var(var) => defs_and_uses.entry(var).or_default().push(i),
            }
        }
        if let Some(var) = instr.lir.dst_operand() {
            defs_and_uses.entry(var).or_default().push(i);
        }
    }

    let mut var_live = BTreeSet::new();
    for (var, instr_counters) in defs_and_uses {
        let first_instr_ctr = instr_counters[0];
        let last_instr_ctr = *instr_counters.iter().last().unwrap();
        // The last usage is the last position  last instr_counter or the last
        // instruction in liveness, whichever is later.
        let last_live_ctr = liveness
            .get(&var)
            // if none, the data flow analysis determined that the variable is not live after its
            // definition ("write-only")
            .map_or(last_instr_ctr, |var_uses| *var_uses.iter().last().unwrap());

        log::debug!("first live instr: {:?}", scheduled_instrs[first_instr_ctr]);
        log::debug!("last live instr: {:?}", scheduled_instrs[last_live_ctr]);
        let interval = Interval::new(first_instr_ctr, last_live_ctr);

        var_live.insert(LiveRange { var, interval });
    }

    // TODO hacky
    let params = scheduled_instrs
        .iter()
        .filter_map(|instr| match instr.lir {
            lir::CodeInstruction::Body(body) => match *body {
                lir::Instruction::LoadParam { idx, dst, .. } => Some(linear_scan::Param {
                    pos: idx as usize,
                    var: dst,
                }),
                _ => None,
            },
            _ => None,
        })
        .collect::<Vec<_>>();

    (var_live, params)
}

type Code = lir::Code<lir::CopyPropagation, lir::CopyPropagation, lir::Instruction, lir::Leave>;

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

pub(super) trait Operands {
    fn src_operands(&self) -> Vec<lir::Operand>;
    fn dst_operand(&self) -> Option<lir::Var>;
}

use std::borrow::Borrow;

// note the repeated A (for Mov)
impl<A, B, C> Operands for lir::CodeInstruction<A, A, B, C>
where
    A: Borrow<lir::CopyPropagation>,
    B: Borrow<lir::Instruction>,
    C: Borrow<lir::Leave>,
{
    fn src_operands(&self) -> Vec<lir::Operand> {
        use super::lir::{CodeInstruction as CI, Instruction::*, Leave::*, LoadMem, StoreMem};
        match self {
            CI::Body(body) => match body.borrow() {
                LoadParam { dst, .. } => vec![lir::Operand::Var(*dst)],
                Binop { src1, src2, .. } | Div { src1, src2, .. } | Mod { src1, src2, .. } => {
                    vec![*src1, *src2]
                }
                Unop { src, .. } | Conv { src, .. } => vec![*src],
                Call(lir::Call { args, .. }) => args.clone(),
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
            },
            CI::CopyIn(mov) | CI::CopyOut(mov) => {
                let lir::CopyPropagation { src, .. } = mov.borrow();
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

    fn dst_operand(&self) -> Option<lir::Var> {
        use super::lir::{CodeInstruction as CI, Instruction::*, LoadMem, StoreMem};
        match self {
            CI::Body(body) => match body.borrow() {
                // LoadParam::dst is a src_operand for the purposes of LVA:
                LoadParam { .. } => None,
                Binop { dst, .. } | Div { dst, .. } | Mod { dst, .. } => Some(*dst),
                Unop { dst, .. } | Conv { dst, .. } => Some(*dst),
                StoreMem(StoreMem { .. }) => None,
                LoadMem(LoadMem { dst, .. }) => Some(*dst),
                Call(lir::Call { dst, .. }) => dst.to_owned(),
            },
            CI::CopyIn(mov) | CI::CopyOut(mov) => {
                let lir::CopyPropagation { dst, .. } = mov.borrow();
                Some(*dst)
            }
            // No dst for leave instructions
            CI::Leave(_) => None,
        }
    }
}
