use super::Size;
use crate::lowering::amd64::CallingConv;
use std::{cmp::Ordering, collections::BTreeMap, convert::TryFrom};

#[derive(Debug, Hash, PartialEq, Eq, Copy, Clone)]
pub(super) struct Reg {
    pub(super) size: Size,
    pub(super) reg: Amd64Reg,
}

impl std::fmt::Display for Reg {
    fn fmt(&self, fmt: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use self::Amd64Reg::*;
        use super::Size::*;
        let (prefix, suffix) = match (self.reg, self.size) {
            (name, One) => match name {
                A | B | C | D => ("", "l"),
                Si | Di | Sp | Bp => ("", "l"),
                R8 | R9 | R10 | R11 | R12 | R13 | R14 | R15 => ("", "b"),
            },
            (name, Four) => match name {
                A | B | C | D => ("e", "x"),
                Si | Di | Sp | Bp => ("e", ""),
                R8 | R9 | R10 | R11 | R12 | R13 | R14 | R15 => ("", "d"),
            },
            (name, Eight) => match name {
                A | B | C | D => ("r", "x"),
                Si | Di | Sp | Bp => ("r", ""),
                R8 | R9 | R10 | R11 | R12 | R13 | R14 | R15 => ("", ""),
            },
        };
        write!(fmt, "%{}{}{}", prefix, self.reg, suffix)
    }
}

#[derive(Display, Debug, Hash, PartialEq, Eq, Copy, Clone)]
pub(super) enum Amd64Reg {
    #[display(fmt = "a")]
    A,
    #[display(fmt = "b")]
    B,
    #[display(fmt = "c")]
    C,
    #[display(fmt = "d")]
    D,

    #[display(fmt = "si")]
    Si,
    #[display(fmt = "di")]
    Di,
    #[display(fmt = "sp")]
    Sp,
    #[display(fmt = "bp")]
    Bp,

    #[display(fmt = "r8")]
    R8,
    #[display(fmt = "r9")]
    R9,
    #[display(fmt = "r10")]
    R10,
    #[display(fmt = "r11")]
    R11,
    #[display(fmt = "r12")]
    R12,
    #[display(fmt = "r13")]
    R13,
    #[display(fmt = "r14")]
    R14,
    #[display(fmt = "r15")]
    R15,
}

#[rustfmt::skip]
impl From<Amd64Reg> for usize {
    fn from(reg: Amd64Reg) -> usize {
        match reg {
            Amd64Reg::Di => 0,   // Caller-save
            Amd64Reg::Si => 1,   // Caller-save
            Amd64Reg::D => 2,    // Caller-save
            Amd64Reg::C => 3,    // Caller-save
            Amd64Reg::R8  => 4,  // Caller-save
            Amd64Reg::R9  => 5,  // Caller-save
            Amd64Reg::R10 => 6,  // Caller-save
            Amd64Reg::R11 => 7,  // Caller-save

            Amd64Reg::B => 8,    // Callee-save
            Amd64Reg::R12 => 9,  // Callee-save
            Amd64Reg::R13 => 10, // Callee-save
            Amd64Reg::R14 => 11, // Callee-save
            Amd64Reg::R15 => 12, // Callee-save

            Amd64Reg::A => 13,   // scratch
            Amd64Reg::Sp => 14,  // should not be used
            Amd64Reg::Bp => 15,  // should not be used
        }
    }
}

#[rustfmt::skip]
impl TryFrom<usize> for Amd64Reg {
    type Error = ();

    fn try_from(idx: usize) -> Result<Self, ()> {
        match idx {
            0  => Ok(Amd64Reg::Di),  // Caller-save
            1  => Ok(Amd64Reg::Si),  // Caller-save
            2  => Ok(Amd64Reg::D),   // Caller-save
            3  => Ok(Amd64Reg::C),   // Caller-save
            4  => Ok(Amd64Reg::R8),  // Caller-save
            5  => Ok(Amd64Reg::R9),  // Caller-save
            6  => Ok(Amd64Reg::R10), // Caller-save
            7  => Ok(Amd64Reg::R11), // Caller-save

            8  => Ok(Amd64Reg::B),   // Callee-save
            9  => Ok(Amd64Reg::R12), // Callee-save
            10 => Ok(Amd64Reg::R13), // Callee-save
            11 => Ok(Amd64Reg::R14), // Callee-save
            12 => Ok(Amd64Reg::R15), // Callee-save

            13 => Ok(Amd64Reg::A),   // scratch
            14 => Ok(Amd64Reg::Sp),  // should not be used
            15 => Ok(Amd64Reg::Bp),  // should not be used
            _  => Err(()),
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
            0 => Amd64Reg::Di,
            1 => Amd64Reg::Si,
            2 => Amd64Reg::D,
            3 => Amd64Reg::C,
            4 => Amd64Reg::R8,
            5 => Amd64Reg::R9,
            _ => unreachable!("This arg is on the stack"),
        }
    }

    pub(super) fn is_caller_save(self) -> bool {
        match self {
            Amd64Reg::Di
            | Amd64Reg::Si
            | Amd64Reg::D
            | Amd64Reg::C
            | Amd64Reg::R8
            | Amd64Reg::R9
            | Amd64Reg::R10
            | Amd64Reg::R11 => true,
            // scratch register
            Amd64Reg::A | _ => false,
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
        let offset = usize::min(nargs, cconv.num_arg_regs());
        match idx + offset {
            0  => Some(Amd64Reg::Di),  // Caller-save
            1  => Some(Amd64Reg::Si),  // Caller-save
            2  => Some(Amd64Reg::D),   // Caller-save
            3  => Some(Amd64Reg::C),   // Caller-save
            4  => Some(Amd64Reg::R8),  // Caller-save
            5  => Some(Amd64Reg::R9),  // Caller-save
            6  => Some(Amd64Reg::R10), // Caller-save
            7  => Some(Amd64Reg::R11), // Caller-save

            8  => Some(Amd64Reg::B),   // Callee-save
            9  => Some(Amd64Reg::R12), // Callee-save
            10 => Some(Amd64Reg::R13), // Callee-save
            11 => Some(Amd64Reg::R14), // Callee-save
            12 => Some(Amd64Reg::R15), // Callee-save

            13   // `Amd64Reg::A` is scratch
            | 14 // `Amd64Reg::Sp` should not be used
            | 15 // `Amd64Reg::Bp` should not be used
            | _ => None,
        }
    }

    pub fn all_but_rsp_and_rbp() -> impl Iterator<Item = Self> {
        // inclusive range
        (0..=13).map(|i| Amd64Reg::try_from(i).unwrap())
    }
}

pub(super) struct RegisterAllocator {
    cconv: CallingConv,
    free_list: BTreeMap<Amd64Reg, bool>,
}

impl RegisterAllocator {
    pub(super) fn new(nargs: usize, cconv: CallingConv) -> Self {
        let mut free_list = BTreeMap::new();
        (0..16)
            .filter_map(|i| Amd64Reg::reg(i, nargs, cconv))
            .for_each(|reg| {
                free_list.insert(reg, true);
            });
        for i in 0..usize::min(nargs, cconv.num_arg_regs()) {
            free_list.insert(Amd64Reg::try_from(i).unwrap(), false);
        }
        Self { cconv, free_list }
    }

    pub(super) fn cconv(&self) -> CallingConv {
        self.cconv
    }

    pub(super) fn occupied_regs(&self) -> impl Iterator<Item = Amd64Reg> + '_ {
        self.free_list
            .iter()
            .filter_map(|(reg, free)| if *free { None } else { Some(*reg) })
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
        if idx < self.cconv.num_arg_regs() {
            match self.cconv {
                CallingConv::Stack => None,
                CallingConv::X86_64 => Some(Amd64Reg::arg(idx)),
            }
        } else {
            None
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

        assert_eq!(allocator.alloc_reg().unwrap(), Amd64Reg::D);

        allocator.free_reg(Amd64Reg::Si);

        assert_eq!(allocator.alloc_reg().unwrap(), Amd64Reg::Si);

        let mut count = 0;
        while let Some(_) = allocator.alloc_reg() {
            count += 1;
        }

        assert_eq!(count, 10);
    }
}
