use crate::lowering::amd64::CallingConv;
use std::{cmp::Ordering, collections::BTreeMap, convert::TryFrom};

#[derive(Display, Debug, Hash, PartialEq, Eq, Copy, Clone)]
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

            Amd64Reg::Rbx => 8,  // Callee-save
            Amd64Reg::R12 => 9, // Callee-save
            Amd64Reg::R13 => 10, // Callee-save
            Amd64Reg::R14 => 11, // Callee-save
            Amd64Reg::R15 => 12, // Callee-save

            Amd64Reg::Rax => 13, // scratch
            Amd64Reg::Rsp => 14, // should not be used
            Amd64Reg::Rbp => 15, // should not be used
        }
    }
}

#[rustfmt::skip]
impl TryFrom<usize> for Amd64Reg {
    type Error = ();

    fn try_from(idx: usize) -> Result<Self, ()> {
        match idx {
            0  => Ok(Amd64Reg::Rdi), // Caller-save
            1  => Ok(Amd64Reg::Rsi), // Caller-save
            2  => Ok(Amd64Reg::Rdx), // Caller-save
            3  => Ok(Amd64Reg::Rcx), // Caller-save
            4  => Ok(Amd64Reg::R8),  // Caller-save
            5  => Ok(Amd64Reg::R9),  // Caller-save
            6  => Ok(Amd64Reg::R10), // Caller-save
            7  => Ok(Amd64Reg::R11), // Caller-save

            8  => Ok(Amd64Reg::Rbx), // Callee-save
            9  => Ok(Amd64Reg::R12), // Callee-save
            10 => Ok(Amd64Reg::R13), // Callee-save
            11 => Ok(Amd64Reg::R14), // Callee-save
            12 => Ok(Amd64Reg::R15), // Callee-save

            13 => Ok(Amd64Reg::Rax), // scratch
            14 => Ok(Amd64Reg::Rsp), // should not be used
            15 => Ok(Amd64Reg::Rbp), // should not be used
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
            0 => Amd64Reg::Rdi,
            1 => Amd64Reg::Rsi,
            2 => Amd64Reg::Rdx,
            3 => Amd64Reg::Rcx,
            4 => Amd64Reg::R8,
            5 => Amd64Reg::R9,
            _ => unreachable!("This arg is on the stack"),
        }
    }

    pub(super) fn is_caller_save(self) -> bool {
        match self {
            Amd64Reg::Rdi
            | Amd64Reg::Rsi
            | Amd64Reg::Rdx
            | Amd64Reg::Rcx
            | Amd64Reg::R8
            | Amd64Reg::R9
            | Amd64Reg::R10
            | Amd64Reg::R11 => true,
            Amd64Reg::Rax // scratch register
            | _ => false,
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
            0  => Some(Amd64Reg::Rdi), // Caller-save
            1  => Some(Amd64Reg::Rsi), // Caller-save
            2  => Some(Amd64Reg::Rdx), // Caller-save
            3  => Some(Amd64Reg::Rcx), // Caller-save
            4  => Some(Amd64Reg::R8),  // Caller-save
            5  => Some(Amd64Reg::R9),  // Caller-save
            6  => Some(Amd64Reg::R10), // Caller-save
            7  => Some(Amd64Reg::R11), // Caller-save

            8  => Some(Amd64Reg::Rbx), // Callee-save
            9  => Some(Amd64Reg::R12), // Callee-save
            10 => Some(Amd64Reg::R13), // Callee-save
            11 => Some(Amd64Reg::R14), // Callee-save
            12 => Some(Amd64Reg::R15), // Callee-save

            13 // `Amd64Reg::Rax` is scratch
            | 14   // `Amd64Reg::Rsp` should not be used
            | 15 // `Amd64Reg::Rbp` should not be used
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

    pub(super) fn alloc_specific_reg(&mut self, reg: Amd64Reg) -> Option<Amd64Reg> {
        let is_free = self.free_list.get_mut(&reg).unwrap();
        if *is_free {
            *is_free = false;
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

#[derive(Display, Debug, Hash, PartialEq, Eq, Copy, Clone)]
pub(super) enum Amd64RegDouble {
    #[display(fmt = "%eax")]
    Eax,
    #[display(fmt = "%ecx")]
    Ecx,
    #[display(fmt = "%edx")]
    Edx,
    #[display(fmt = "%ebx")]
    Ebx,
    #[display(fmt = "%esi")]
    Esi,
    #[display(fmt = "%edi")]
    Edi,
    #[display(fmt = "%esp")]
    Esp,
    #[display(fmt = "%ebp")]
    Ebp,
    #[display(fmt = "%r8d")]
    R8d,
    #[display(fmt = "%r9d")]
    R9d,
    #[display(fmt = "%r10d")]
    R10d,
    #[display(fmt = "%r11d")]
    R11d,
    #[display(fmt = "%r12d")]
    R12d,
    #[display(fmt = "%r13d")]
    R13d,
    #[display(fmt = "%r14d")]
    R14d,
    #[display(fmt = "%r15d")]
    R15d,
}

#[rustfmt::skip]
impl From<Amd64Reg> for Amd64RegDouble {
    fn from(reg: Amd64Reg) -> Self {
        match reg {
            Amd64Reg::Rax => Amd64RegDouble::Eax,
            Amd64Reg::Rcx => Amd64RegDouble::Ecx,
            Amd64Reg::Rdx => Amd64RegDouble::Edx,
            Amd64Reg::Rbx => Amd64RegDouble::Ebx,
            Amd64Reg::Rsi => Amd64RegDouble::Esi,
            Amd64Reg::Rdi => Amd64RegDouble::Edi,
            Amd64Reg::Rsp => Amd64RegDouble::Esp,
            Amd64Reg::Rbp => Amd64RegDouble::Ebp,
            Amd64Reg::R8  => Amd64RegDouble::R8d,
            Amd64Reg::R9  => Amd64RegDouble::R9d,
            Amd64Reg::R10 => Amd64RegDouble::R10d,
            Amd64Reg::R11 => Amd64RegDouble::R11d,
            Amd64Reg::R12 => Amd64RegDouble::R12d,
            Amd64Reg::R13 => Amd64RegDouble::R13d,
            Amd64Reg::R14 => Amd64RegDouble::R14d,
            Amd64Reg::R15 => Amd64RegDouble::R15d,
        }
    }
}

#[derive(Display, Debug, Hash, PartialEq, Eq, Copy, Clone)]
pub(super) enum Amd64RegByte {
    #[display(fmt = "%al")]
    Al,
    #[display(fmt = "%cl")]
    Cl,
    #[display(fmt = "%dl")]
    Dl,
    #[display(fmt = "%bl")]
    Bl,
    #[display(fmt = "%sil")]
    Sil,
    #[display(fmt = "%dil")]
    Dil,
    #[display(fmt = "%spl")]
    Spl,
    #[display(fmt = "%bpl")]
    Bpl,
    #[display(fmt = "%r8b")]
    R8b,
    #[display(fmt = "%r9b")]
    R9b,
    #[display(fmt = "%r10b")]
    R10b,
    #[display(fmt = "%r11b")]
    R11b,
    #[display(fmt = "%r12b")]
    R12b,
    #[display(fmt = "%r13b")]
    R13b,
    #[display(fmt = "%r14b")]
    R14b,
    #[display(fmt = "%r15b")]
    R15b,
}

#[rustfmt::skip]
impl From<Amd64Reg> for Amd64RegByte {
    fn from(reg: Amd64Reg) -> Self {
        match reg {
            Amd64Reg::Rax => Amd64RegByte::Al,
            Amd64Reg::Rcx => Amd64RegByte::Cl,
            Amd64Reg::Rdx => Amd64RegByte::Dl,
            Amd64Reg::Rbx => Amd64RegByte::Bl,
            Amd64Reg::Rsi => Amd64RegByte::Sil,
            Amd64Reg::Rdi => Amd64RegByte::Dil,
            Amd64Reg::Rsp => Amd64RegByte::Spl,
            Amd64Reg::Rbp => Amd64RegByte::Bpl,
            Amd64Reg::R8  => Amd64RegByte::R8b,
            Amd64Reg::R9  => Amd64RegByte::R9b,
            Amd64Reg::R10 => Amd64RegByte::R10b,
            Amd64Reg::R11 => Amd64RegByte::R11b,
            Amd64Reg::R12 => Amd64RegByte::R12b,
            Amd64Reg::R13 => Amd64RegByte::R13b,
            Amd64Reg::R14 => Amd64RegByte::R14b,
            Amd64Reg::R15 => Amd64RegByte::R15b,
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

        assert_eq!(count, 11);
    }
}
