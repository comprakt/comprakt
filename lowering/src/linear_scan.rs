//! Linear-scan register allocation.

use super::{
    lir::{self, Var},
    live_variable_analysis::{self, LiveRange},
    register::{Amd64Reg, RegisterAllocator},
};
use crate::allocator::Ptr;
use gcollections::ops::bounded::Bounded;
use interval::Interval;
use live_variable_analysis::{LVAResult, ScheduledInstr};
use std::{
    cmp::Ordering,
    collections::{BTreeSet, HashMap},
    ops::Deref,
};

pub(crate) struct LSAResult {
    /// Counter of how many memory slots will be needed for spilling
    pub(crate) stack_vars_counter: usize,
    /// Mapping for `Var`s to their location, determined by the linear scan
    /// algorithm
    pub(crate) var_location: HashMap<Var, Location>,
    /// Number of regs that is required after the register allocation
    pub(crate) num_regs_required: usize,
}

impl From<LinearScanAllocator> for LSAResult {
    fn from(lsa: LinearScanAllocator) -> Self {
        let LinearScanAllocator {
            stack_vars_counter,
            var_location,
            num_regs_required,
            ..
        } = lsa;
        Self {
            stack_vars_counter,
            var_location,
            num_regs_required,
        }
    }
}

pub(crate) fn register_allocation(func: &mut lir::Function, lva_result: LVAResult) -> LSAResult {
    let LVAResult {
        scheduled_instrs,
        live_ranges_by_start,
        lsa_params_list,
    } = lva_result;
    let var_live = live_ranges_by_start; // fixme

    // FIXME stack calling convention dropped
    let reg_alloc = RegisterAllocator::new(func.nargs, crate::register::CallingConv);
    let mut linear_scan = LinearScanAllocator::new(reg_alloc, var_live, &lsa_params_list);
    linear_scan.run(&scheduled_instrs);
    linear_scan.into()
}

/// This is a wrapper struct for `LiveRange`s. `LiveRange`s need to be sorted by
/// the lower bound of their interval. We also need a ordering by the upper
/// bound though. So this wraps the `LiveRange` struct to implement Ord for the
/// upper bound.
#[derive(Debug, Eq, PartialEq, Copy, Clone)]
struct Active(LiveRange);

impl Ord for Active {
    fn cmp(&self, other: &Self) -> Ordering {
        match self.interval.upper().cmp(&other.interval.upper()) {
            // If the upper bound is the same take the var that was allocated earlier
            Ordering::Equal => self.var.num().cmp(&other.var.num()),
            ord => ord,
        }
    }
}

impl PartialOrd for Active {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl Deref for Active {
    type Target = LiveRange;

    fn deref(&self) -> &LiveRange {
        &self.0
    }
}

#[derive(Debug, Copy, Clone)]
pub(crate) enum Location {
    Reg(Amd64Reg),
    /// This is for Vars that are stored on the stack. We can pre-allocate a
    /// stack frame for each function, so that we don't need Push/Pop
    /// instructions, but can just Move from/onto the stack. This means,
    /// that we just need the position on the stack and then can access the Var
    /// with `-idx*8(%rbp)`
    Ar(usize),
    /// Params which are already stored on the stack won't need extra allocation
    /// on the stack. The exact address computation for these is already
    /// handled by the instruction selection
    ParamMem(usize),
}

impl Location {
    fn reg_unchecked(self) -> Amd64Reg {
        match self {
            Location::Reg(reg) => reg,
            Location::Ar(_) => panic!("reg_unchecked: Ar"),
            Location::ParamMem(_) => panic!("reg_unchecked: ParamMem"),
        }
    }
}

pub(super) struct LinearScanAllocator {
    /// Register allocator
    reg_alloc: RegisterAllocator,
    /// Live Ranges of all variables. Sorted by shortest interval
    var_live: BTreeSet<LiveRange>,

    /// The list of currently active `LiveRange`s
    active: BTreeSet<Active>,
    /// Map from currently assigned registers to live ranges
    reg_lr_map: HashMap<Amd64Reg, LiveRange>,

    params: HashMap<Var, Param>,

    // FIXME refactor: the following vars are LSAResult
    /// Counter of how many memory slots will be needed for spilling
    pub(super) stack_vars_counter: usize,
    /// Mapping for `Var`s to their location, determined by the linear scan
    /// algorithm
    pub(super) var_location: HashMap<Var, Location>,
    /// Number of regs that is required after the register allocation
    pub(super) num_regs_required: usize,
}

#[derive(Clone, Copy, Debug)]
pub struct Param {
    pub pos: usize,
    pub var: Var,
}

impl LinearScanAllocator {
    pub(super) fn new(
        reg_alloc: RegisterAllocator,
        var_live: BTreeSet<LiveRange>,
        params: &[Param],
    ) -> Self {
        use std::iter::FromIterator;
        let mut active = BTreeSet::new();
        let mut var_location = HashMap::new();
        let mut reg_lr_map = HashMap::new();

        // pre-allocate registers for those parameters passed in registers
        log::debug!("params: {:#?}", params);
        for param in params {
            let live_range = var_live
                .iter()
                .find(|live_range| live_range.var == param.var)
                .expect("lva mut produce a live range for a used parameter");

            if let Some(reg) = reg_alloc.arg_in_reg(param.pos) {
                log::debug!("param {:?} in reg {:?}", param, reg);
                active.insert(Active(*live_range));
                var_location.insert(live_range.var, Location::Reg(reg));
                reg_lr_map.insert(reg, *live_range);
            } else {
                log::debug!("param {:?} on stack", param);
            }
        }

        let num_regs_required = active.len();

        let params = HashMap::from_iter(params.iter().map(|param| (param.var, *param)));
        log::debug!("PARAMS {:#?}", params);

        Self {
            reg_alloc,
            var_live,
            active,
            reg_lr_map,
            stack_vars_counter: 0,
            var_location,
            num_regs_required,
            params,
        }
    }

    // TODO move argument to struct, Ptr yay
    pub(super) fn run(&mut self, scheduled_instrs: &[Ptr<ScheduledInstr>]) {
        let var_live = self.var_live.clone();
        let mut lr_iter = var_live.iter().peekable();

        for (instr_counter, instr) in scheduled_instrs
            .iter()
            .cloned()
            .map(|si| si.lir)
            .enumerate()
        {
            // Special case: caller-saved args used before the call
            // and are live after the call must be saved.
            if let super::lir::CodeInstruction::Body(mut body) = instr {
                if let lir::Instruction::Call(call) = &mut *body {
                    for reg in self.reg_alloc.occupied_regs() {
                        if let Some(lr) = self.reg_lr_map.get(&reg) {
                            if lr.interval.upper() > instr_counter {
                                call.live_regs_after_call.push(reg);
                            }
                        }
                    }
                }
            }

            // Assign registers (and spill if necessary) to all live ranges
            // beginning at the current instruction counter
            while let Some(live_range) = lr_iter.peek() {
                let interval_begins_here = live_range.interval.lower() == instr_counter;
                if interval_begins_here {
                    self.expire_old_intervals(live_range.interval);

                    if self.active.contains(&Active(**live_range)) {
                        // A register was already assigned to this argument. This is the case
                        // when the calling convention uses
                        // registers for arguments and the argument
                        // wasn't already spilled.
                        lr_iter.next();
                        continue;
                    }

                    if let Some(reg) = self.reg_alloc.alloc_reg() {
                        self.active.insert(Active(**live_range));
                        self.var_location.insert(live_range.var, Location::Reg(reg));
                        self.reg_lr_map.insert(reg, **live_range);
                        self.num_regs_required =
                            std::cmp::max(self.active.len(), self.num_regs_required);
                    } else {
                        self.spill_at(**live_range);
                    }

                    lr_iter.next();
                } else {
                    break;
                }
            }
        }
    }

    fn expire_old_intervals(&mut self, interval: Interval<usize>) {
        if let Some(keep) = self
            .active
            .iter()
            .find(|a| a.interval.upper() > interval.lower())
        {
            let new_active = self.active.split_off(&keep.clone());
            for live_range in &self.active {
                let reg = self
                    .var_location
                    .get(&live_range.var)
                    .expect("a register needs to be assigned to vars in the active list")
                    .reg_unchecked();
                self.reg_alloc.free_reg(reg);
                self.reg_lr_map.remove(&reg);
            }

            self.active = new_active;
        }

        // If no register is found in the active list, where the end point excedes the
        // start point of the current interval, no register will be freed and
        // the active list stays the same.
    }

    fn new_stack_location(&mut self, range: LiveRange) -> Location {
        let stack_param = self.params.get(&range.var).and_then(|param| {
            if self.reg_alloc.arg_in_reg(param.pos).is_none() {
                Some(param)
            } else {
                None
            }
        });
        if let Some(param) = stack_param {
            Location::ParamMem(param.pos)
        } else {
            let ar = Location::Ar(self.stack_vars_counter);
            self.stack_vars_counter += 1;
            ar
        }
    }

    fn spill_at(&mut self, live_range: LiveRange) {
        let spill = *self.active.iter().last().unwrap();
        if spill.interval.upper() > live_range.interval.upper() {
            // spill lives longer than live range (live range ends earlier)
            // => spill gets assigned an AR location
            let location = self.new_stack_location(spill.0);
            let spill_reg = self
                .var_location
                .insert(spill.var, location)
                .expect("a register needs to be assigned to vars in the active list");

            self.var_location.insert(live_range.var, spill_reg);
            self.reg_lr_map
                .insert(spill_reg.reg_unchecked(), live_range)
                .expect("reg needs to be overwritten");

            self.active.remove(&spill);
            self.active.insert(Active(live_range));
        } else {
            // live_range lives longer than spill
            // => live_range gets assigned an AR location
            let location = self.new_stack_location(live_range);
            self.var_location.insert(live_range.var, location);
        }
    }
}
