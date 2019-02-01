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
            Ordering::Equal => self.var_id.cmp(&other.var_id),
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
#[derive(Debug, Eq, PartialEq, Copy, Clone)]
struct Active(LiveRange);

impl Ord for Active {
    fn cmp(&self, other: &Active) -> Ordering {
        match self.interval.upper().cmp(&other.interval.upper()) {
            // If the upper bound is the same take the var that was allocated earlier
            Ordering::Equal => self.var_id.cmp(&other.var_id),
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

    params: HashMap<VarId, Param>,

    /// Counter of how many memory slots will be needed for spilling
    pub(super) stack_vars_counter: usize,
    /// Mapping for `VarId`s to their location, determined by the linear scan
    /// algorithm
    pub(super) var_location: HashMap<VarId, Location>,
    /// Number of regs that is required after the register allocation
    pub(super) num_regs_required: usize,
}

#[derive(Clone, Copy, Debug)]
pub struct Param {
    pub pos: usize,
    pub var_id: VarId,
}

impl LinearScanAllocator {
    pub(super) fn new(
        reg_alloc: RegisterAllocator,
        var_live: BTreeSet<LiveRange>,
        params: &[Param],
    ) -> Self {
        let mut active = BTreeSet::new();
        let mut var_location = HashMap::new();
        let mut reg_lr_map = HashMap::new();

        // pre-allocate registers for those parameters passed in registers
        log::debug!("params: {:#?}", params);
        for param in params {
            let live_range = var_live
                .iter()
                .find(|live_range| live_range.var_id == param.var_id)
                .expect("lva mut produce a live range for a used parameter");

            if let Some(reg) = reg_alloc.arg_in_reg(param.pos) {
                log::debug!("param {:?} in reg {:?}", param, reg);
                active.insert(Active(*live_range));
                var_location.insert(live_range.var_id, Location::Reg(reg));
                reg_lr_map.insert(reg, *live_range);
            } else {
                log::debug!("param {:?} on stack", param);
            }
        }

        let num_regs_required = active.len();

        use std::iter::FromIterator;
        let params = HashMap::from_iter(params.iter().map(|param| (param.var_id, *param)));
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

    pub(super) fn run(&mut self, blocks: &mut [Block]) {
        let var_live = self.var_live.clone();
        let mut lr_iter = var_live.iter().peekable();
        let mut instr_counter = 0;

        for block in blocks.iter_mut() {
            for instr in block.code.iter_unified_mut() {
                // Special case: caller-saved args used before the call
                // and are live after the call must be saved.
                if let super::lir::CodeInstruction::Body(Instruction::Call(call)) = instr {
                    for reg in self.reg_alloc.occupied_regs() {
                        if let Some(lr) = self.reg_lr_map.get(&reg) {
                            if lr.interval.upper() > instr_counter {
                                call.save_reg(reg);
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
            .find(|a| a.interval.upper() > interval.lower())
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

    fn new_stack_location(&mut self, range: LiveRange) -> Location {
        let stack_param = self.params.get(&range.var_id).and_then(|param| {
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
                .insert(spill.var_id, location)
                .expect("a register needs to be assigned to vars in the active list");

            self.var_location.insert(live_range.var_id, spill_reg);
            self.reg_lr_map
                .insert(spill_reg.reg_unchecked(), live_range)
                .expect("reg needs to be overwritten");

            self.active.remove(&spill);
            self.active.insert(Active(live_range));
        } else {
            // live_range lives longer than spill
            // => live_range gets assigned an AR location
            let location = self.new_stack_location(live_range);
            self.var_location.insert(live_range.var_id, location);
        }
    }
}