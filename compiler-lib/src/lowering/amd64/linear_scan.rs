use super::{
    live_variable_analysis::{Block, Instruction},
    register::{Amd64Reg, RegisterAllocator},
    VarId,
};
use gcollections::ops::bounded::Bounded;
use interval::Interval;
use std::{
    cmp::Ordering,
    collections::{BTreeSet, HashMap},
    ops::Deref,
};

/// `LiveRange` holds for every `VarId` the liveness interval. This implements
/// Ord, so that `LiveRange`s are sorted by the lower bound of their interval.
#[derive(Debug, Eq, PartialEq, Copy, Clone)]
pub(super) struct LiveRange {
    pub(super) var_id: VarId,
    pub(super) interval: Interval<usize>,
}

impl Ord for LiveRange {
    fn cmp(&self, other: &LiveRange) -> Ordering {
        match self.interval.lower().cmp(&other.interval.lower()) {
            Ordering::Equal => match self.var_id.0.cmp(&other.var_id.0) {
                Ordering::Equal => self.var_id.1.cmp(&other.var_id.1),
                ord => ord,
            },
            ord => ord,
        }
    }
}

impl PartialOrd for LiveRange {
    fn partial_cmp(&self, other: &LiveRange) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

/// This is a wrapper struct for `LiveRange`s. `LiveRange`s need to be sorted by
/// the lower bound of their interval. We also need a ordering by the upper
/// bound though. So this wraps the `LiveRange` struct to implement Ord for the
/// upper bound.
#[derive(Eq, PartialEq, Copy, Clone)]
struct Active(LiveRange);

impl Ord for Active {
    fn cmp(&self, other: &Active) -> Ordering {
        match self.interval.upper().cmp(&other.interval.upper()) {
            // If the upper bound is the same take the var that was allocated earlier
            Ordering::Equal => match self.var_id.0.cmp(&other.var_id.0) {
                Ordering::Equal => self.var_id.1.cmp(&other.var_id.1),
                ord => ord,
            },
            ord => ord,
        }
    }
}

impl PartialOrd for Active {
    fn partial_cmp(&self, other: &Active) -> Option<Ordering> {
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
pub(super) enum Location {
    Reg(Amd64Reg),
    /// This is for Vars that are stored on the stack. We can pre-allocate a
    /// stack frame for each function, so that we don't need Push/Pop
    /// instructions, but can just Move from/onto the stack. This means,
    /// that we just need the position on the stack and then can access the Var
    /// with `-idx*8(%rbp)`
    Mem(usize),
    /// Params which are already stored on the stack won't need extra allocation
    /// on the stack. The exact address computation for these is already
    /// handled by the instruction selection
    ParamMem,
}

impl Location {
    fn reg_unchecked(self) -> Amd64Reg {
        match self {
            Location::Reg(reg) => reg,
            Location::Mem(_) => panic!("reg_unchecked: Mem"),
            Location::ParamMem => panic!("reg_unchecked: ParamMem"),
        }
    }
}

pub(super) struct LinearScanAllocator {
    /// Register allocator
    reg_alloc: RegisterAllocator,
    /// Live Ranges of all variables (ValueSlot/Param). Sorted by shortest
    /// interval
    var_live: BTreeSet<LiveRange>,

    /// The list of currently active `LiveRange`s
    active: BTreeSet<Active>,
    /// Map from currently assigned registers to live ranges
    reg_lr_map: HashMap<Amd64Reg, LiveRange>,

    /// Counter of how many memory slots will be needed for spilling
    pub(super) stack_vars_counter: usize,
    /// Mapping for `VarId`s to their location, determined by the linear scan
    /// algorithm
    pub(super) var_location: HashMap<VarId, Location>,
    /// Number of regs that is required after the register allocation
    pub(super) num_regs_required: usize,
}

impl LinearScanAllocator {
    pub(super) fn new(reg_alloc: RegisterAllocator, var_live: BTreeSet<LiveRange>) -> Self {
        let mut active = BTreeSet::new();
        let mut var_location = HashMap::new();
        let mut reg_lr_map = HashMap::new();
        let mut reg_alloc = reg_alloc;

        for live_range in &var_live {
            if live_range.var_id.0 == -1 {
                if let Some(reg) = reg_alloc.arg_in_reg(live_range.var_id.1) {
                    active.insert(Active(*live_range));
                    var_location.insert(live_range.var_id, Location::Reg(reg));
                    reg_lr_map.insert(reg, *live_range);
                }
                // else: argument is on the Stack. If we have free registers we also want to
                // assign a register to this argument. If the register pressure is too high,
                // these are the first Vars that will be spilled. The later case
                // is handled by `spill_at`
            }
        }

        let used_params = active.iter().map(|lr| lr.var_id.1).collect::<Vec<_>>();
        for i in 0..6 {
            if !used_params.contains(&i) {
                reg_alloc.free_reg(Amd64Reg::arg(i));
            }
        }

        let num_regs_required = active.len();

        Self {
            reg_alloc,
            var_live,
            active,
            reg_lr_map,
            stack_vars_counter: 0,
            var_location,
            num_regs_required,
        }
    }

    pub(super) fn run(&mut self, blocks: &mut [Block]) {
        let var_live = self.var_live.clone();
        let mut lr_iter = var_live.iter().peekable();
        let mut instr_counter = 0;

        for block in blocks.iter_mut() {
            for instr in block.instrs.iter_mut() {
                if let Instruction::Call(call) = instr {
                    for reg in self.reg_alloc.occupied_regs() {
                        if let Some(lr) = self.reg_lr_map.get(&reg) {
                            if lr.interval.upper() > instr_counter {
                                call.save_reg(reg);
                            }
                        }
                    }
                }

                while let Some(live_range) = lr_iter.peek() {
                    if live_range.interval.lower() == instr_counter {
                        self.expire_old_intervals(live_range.interval);

                        if live_range.var_id.0 == -1 && self.active.contains(&Active(**live_range))
                        {
                            // A register was already assigned to this argument. This is the case
                            // when the calling convention uses
                            // registers for arguments and the argument
                            // wasn't already spilled.
                            lr_iter.next();
                            continue;
                        }

                        if let Some(reg) = self.reg_alloc.alloc_reg() {
                            self.active.insert(Active(**live_range));
                            self.var_location
                                .insert(live_range.var_id, Location::Reg(reg));
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
                instr_counter += 1;
            }
        }
    }

    fn expire_old_intervals(&mut self, interval: Interval<usize>) {
        if let Some(keep) = self
            .active
            .iter()
            .find(|a| a.interval.upper() >= interval.lower())
        {
            let new_active = self.active.split_off(&keep.clone());
            for live_range in &self.active {
                let reg = self
                    .var_location
                    .get(&live_range.var_id)
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

    fn spill_at(&mut self, live_range: LiveRange) {
        if live_range.var_id.0 == -1 && self.reg_alloc.arg_in_reg(live_range.var_id.1).is_none() {
            // If the new Var is a argument on the stack, don't assign a register to it
            self.var_location
                .insert(live_range.var_id, Location::ParamMem);
        } else if let Some(arg) = self
            .active
            .iter()
            .find(|lr| lr.var_id.0 == -1 && self.reg_alloc.arg_in_reg(lr.var_id.1).is_none())
            .cloned()
        {
            // If an argument is on the stack and also in the active list, then remove this
            // argument from the active list and assign the new var the free register
            let spill_reg = self
                .var_location
                .insert(arg.var_id, Location::ParamMem)
                .expect("a register needs to be assigned to vars in the active list");
            self.var_location.insert(live_range.var_id, spill_reg);
            self.reg_lr_map
                .insert(spill_reg.reg_unchecked(), live_range)
                .expect("reg needs to be overwritten");

            self.active.remove(&arg);
            self.active.insert(Active(live_range));
        } else {
            let spill = *self.active.iter().last().unwrap();
            let mem_slot = Location::Mem(self.stack_vars_counter);
            self.stack_vars_counter += 1;

            if spill.interval.upper() > live_range.interval.upper() {
                let spill_reg = self
                    .var_location
                    .insert(spill.var_id, mem_slot)
                    .expect("a register needs to be assigned to vars in the active list");

                self.var_location.insert(live_range.var_id, spill_reg);
                self.reg_lr_map
                    .insert(spill_reg.reg_unchecked(), live_range)
                    .expect("reg needs to be overwritten");

                self.active.remove(&spill);
                self.active.insert(Active(live_range));
            } else {
                self.var_location.insert(live_range.var_id, mem_slot);
            }
        }
    }
}
