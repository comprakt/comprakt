use crate::lowering::{
    amd64::{self, CallingConv, MoveOperand, Operand},
    lir,
};
use std::{cmp::Ordering, collections::BTreeMap};

#[derive(Display, Debug, PartialEq, Eq, Copy, Clone)]
pub(super) enum Amd64Reg {
    #[display(fmt = "%rax")]
    Rax,
    #[display(fmt = "%rcx")]
    Rcx,
    #[display(fmt = "%rdx")]
    Rdx,
    #[display(fmt = "%rbx")]
    Rbx,
    #[display(fmt = "%rsi")]
    Rsi,
    #[display(fmt = "%rdi")]
    Rdi,
    #[display(fmt = "%rsp")]
    Rsp,
    #[display(fmt = "%rbp")]
    Rbp,
    #[display(fmt = "%r8")]
    R8,
    #[display(fmt = "%r9")]
    R9,
    #[display(fmt = "%r10")]
    R10,
    #[display(fmt = "%r11")]
    R11,
    #[display(fmt = "%r12")]
    R12,
    #[display(fmt = "%r13")]
    R13,
    #[display(fmt = "%r14")]
    R14,
    #[display(fmt = "%r15")]
    R15,
}

#[rustfmt::skip]
impl From<Amd64Reg> for usize {
    fn from(reg: Amd64Reg) -> usize {
        match reg {
            Amd64Reg::Rdi => 0,  // Caller-save
            Amd64Reg::Rsi => 1,  // Caller-save
            Amd64Reg::Rdx => 2,  // Caller-save
            Amd64Reg::Rcx => 3,  // Caller-save
            Amd64Reg::R8  => 4,  // Caller-save
            Amd64Reg::R9  => 5,  // Caller-save
            Amd64Reg::R10 => 6,  // Caller-save
            Amd64Reg::R11 => 7,  // Caller-save
            Amd64Reg::Rax => 8,  // Caller-save

            Amd64Reg::Rbx => 9,  // Callee-save
            Amd64Reg::R12 => 10, // Callee-save
            Amd64Reg::R13 => 11, // Callee-save
            Amd64Reg::R14 => 12, // Callee-save
            Amd64Reg::R15 => 13, // Callee-save

            Amd64Reg::Rsp => 14, // should not be used
            Amd64Reg::Rbp => 15, // should not be used
        }
    }
}

impl Ord for Amd64Reg {
    fn cmp(&self, other: &Amd64Reg) -> Ordering {
        let l: usize = (*self).into();
        let r: usize = (*other).into();
        l.cmp(&r)
    }
}

impl PartialOrd for Amd64Reg {
    fn partial_cmp(&self, other: &Amd64Reg) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl Amd64Reg {
    /// This function returns the `idx`th register reserved by the X86_64
    /// calling convention for funtion arguments. The order of these
    /// registers is:
    ///
    /// - %rdi
    /// - %rsi
    /// - %rdx
    /// - %rcx
    /// - %r8
    /// - %r9
    ///
    /// # Panics
    ///
    /// This function panics if `idx >= 6`, since X86_64 only reserves 6
    /// registers for function arguments
    pub fn arg(idx: usize) -> Self {
        match idx {
            0 => Amd64Reg::Rdi,
            1 => Amd64Reg::Rsi,
            2 => Amd64Reg::Rdx,
            3 => Amd64Reg::Rcx,
            4 => Amd64Reg::R8,
            5 => Amd64Reg::R9,
            _ => unreachable!("This arg is on the stack"),
        }
    }

    /// This function returns the next unreserved register. This function
    /// is used by the register allocator to always use the registers
    /// in the same order. That is:
    ///
    /// - Caller-Save registers
    ///   - X86_64:
    ///     - if available, first free function argument register (see
    ///       `Amd64Reg::arg()`)
    ///     - %r10, %r11, %rax
    ///   - Stack:
    ///     - the registers that would get reserved for function arguments by
    ///       X86_64
    ///     - %r10, %r11
    /// - Callee-Save registers
    ///   - %rbx, %r12-r15
    ///
    /// This funtion returns `None` when the `idx` (plus the number of reserved function
    /// arguments) points to one of the registers %rbp or %rsp or is
    /// greater or equals 16 (number of total registers)
    #[rustfmt::skip]
    fn reg(idx: usize, nargs: usize, cconv: CallingConv) -> Option<Self> {
        let offset = if let CallingConv::Stack = cconv {
            0
        } else {
            usize::min(nargs, 6)
        };
        match idx + offset {
            0  => Some(Amd64Reg::Rdi), // Caller-save
            1  => Some(Amd64Reg::Rsi), // Caller-save
            2  => Some(Amd64Reg::Rdx), // Caller-save
            3  => Some(Amd64Reg::Rcx), // Caller-save
            4  => Some(Amd64Reg::R8),  // Caller-save
            5  => Some(Amd64Reg::R9),  // Caller-save
            6  => Some(Amd64Reg::R10), // Caller-save
            7  => Some(Amd64Reg::R11), // Caller-save
            8  => Some(Amd64Reg::Rax), // Caller-save

            9  => Some(Amd64Reg::Rbx), // Callee-save
            10 => Some(Amd64Reg::R12), // Callee-save
            11 => Some(Amd64Reg::R13), // Callee-save
            12 => Some(Amd64Reg::R14), // Callee-save
            13 => Some(Amd64Reg::R15), // Callee-save

            14   // `Amd64Reg::Rsp` should not be used
            | 15 // `Amd64Reg::Rbp` should not be used
            | _ => None,
        }
    }
}

#[allow(unused)]
pub(super) struct RegisterAllocator {
    nargs: usize,
    cconv: CallingConv,
    free_list: BTreeMap<Amd64Reg, bool>,
}

#[allow(unused)]
impl RegisterAllocator {
    pub(super) fn new(nargs: usize, cconv: CallingConv) -> Self {
        let mut free_list = BTreeMap::new();
        (0..16)
            .filter_map(|i| Amd64Reg::reg(i, nargs, cconv))
            .for_each(|reg| {
                free_list.insert(reg, true);
            });
        Self {
            nargs,
            cconv,
            free_list,
        }
    }

    /// Returns a register from the `free_list`
    pub(super) fn alloc_reg(&mut self) -> Option<Amd64Reg> {
        if let Some(reg) = self
            .free_list
            .iter()
            .find(|(_, free)| **free)
            .and_then(|(reg, _)| Some(*reg))
        {
            self.free_list.insert(reg, false);
            Some(reg)
        } else {
            None
        }
    }

    /// Inserts a register into the `free_list`. Also registers which were not
    /// initially in the free_list can be inserted. This can be useful for
    /// function argument registers, that won't get used anymore.
    pub(super) fn free_reg(&mut self, reg: Amd64Reg) {
        self.free_list.insert(reg, true);
    }

    /// When the `idx`th arg is in a register, this register will be returned,
    /// otherwise None.
    pub(super) fn arg_in_reg(&self, idx: usize) -> Option<Amd64Reg> {
        if idx < 6 {
            match self.cconv {
                CallingConv::Stack => None,
                CallingConv::X86_64 => Some(Amd64Reg::arg(idx)),
            }
        } else {
            None
        }
    }

    /// Depending on the calling convention some args are in registers. This
    /// function returns the Movq instruction from either a register or an
    /// address into the dst.
    pub(super) fn arg(&self, idx: usize, dst: &lir::Operand) -> amd64::Instruction {
        self.arg_from_reg(idx, dst)
            .unwrap_or_else(|| self.arg_from_stack(idx, dst))
    }

    fn arg_from_reg(&self, idx: usize, dst: &lir::Operand) -> Option<amd64::Instruction> {
        self.arg_in_reg(idx).and_then(|reg| {
            Some(amd64::Instruction::Movq {
                src: MoveOperand::Operand(Operand::Reg(reg)),
                dst: MoveOperand::Operand(Operand::LirOperand(*dst)),
            })
        })
    }

    fn arg_from_stack(&self, idx: usize, dst: &lir::Operand) -> amd64::Instruction {
        let offset = match self.cconv {
            CallingConv::Stack => (idx + 1) * 8 + 8,
            CallingConv::X86_64 => {
                debug_assert!(idx >= 6);
                (idx + 1 - 6) * 8 + 8
            }
        } as isize;
        amd64::Instruction::Movq {
            src: MoveOperand::Addr(lir::AddressComputation {
                offset,
                base: Operand::Reg(Amd64Reg::Rbp),
                index: lir::IndexComputation::Zero,
            }),
            dst: MoveOperand::Operand(Operand::LirOperand(*dst)),
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn free_list_works() {
        let mut free_list = BTreeMap::new();

        free_list.insert(Amd64Reg::R9, true);
        free_list.insert(Amd64Reg::R8, true);

        let free_list: Vec<_> = free_list.into_iter().map(|(reg, _)| reg).collect();

        assert_eq!(free_list, [Amd64Reg::R8, Amd64Reg::R9]);
    }

    #[test]
    fn register_allocation_works() {
        let mut allocator = RegisterAllocator::new(2, CallingConv::X86_64);

        assert_eq!(allocator.alloc_reg().unwrap(), Amd64Reg::Rdx);

        allocator.free_reg(Amd64Reg::Rsi);

        assert_eq!(allocator.alloc_reg().unwrap(), Amd64Reg::Rsi);

        let mut count = 0;
        while let Some(_) = allocator.alloc_reg() {
            count += 1;
        }

        assert_eq!(count, 10);
    }
}
