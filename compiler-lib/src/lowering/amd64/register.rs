use super::CallingConv;

#[derive(Display)]
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

    /// This function returns the next "available" register. This function
    /// should be used by the register allocator to always use the registers
    /// in the same order. That is:
    ///
    /// - Caller-Save registers
    ///   - X86_64:
    ///     - if available, first free function argument register (see
    ///       `Amd64Reg::arg()`)
    ///     - %r10, %r11
    ///   - Stack:
    ///     - the registers that would get reserved for function arguments by
    ///       X86_64
    ///     - %r10, %r11
    /// - Callee-Save registers
    ///   - %rbx, %r12-r15
    ///
    /// # Panics
    ///
    /// This funtion panics if the `idx` (plus the number of reserved function
    /// arguments) points to one of the registers %rbp, %rsp or %rax or is
    /// greater or equals 16 (number of total registers)
    #[rustfmt::skip]
    pub fn reg(idx: usize, nargs: usize, cconv: CallingConv) -> Self {
        let offset = if let CallingConv::Stack = cconv {
            0
        } else {
            usize::min(nargs, 6)
        };
        match idx + offset {
            0 => Amd64Reg::Rdi,  // Caller-save
            1 => Amd64Reg::Rsi,  // Caller-save
            2 => Amd64Reg::Rdx,  // Caller-save
            3 => Amd64Reg::Rcx,  // Caller-save
            4 => Amd64Reg::R8,   // Caller-save
            5 => Amd64Reg::R9,   // Caller-save
            6 => Amd64Reg::R10,  // Caller-save
            7 => Amd64Reg::R11,  // Caller-save

            8 => Amd64Reg::Rbx,  // Callee-save
            9 => Amd64Reg::R12,  // Callee-save
            10 => Amd64Reg::R13, // Callee-save
            11 => Amd64Reg::R14, // Callee-save
            12 => Amd64Reg::R15, // Callee-save

            13   // `Amd64Reg::Rsp` should not be used
            | 14 // `Amd64Reg::Rbp` should not be used
            | 15 // `Amd64Reg::Rax` should not be used
            | _ => unreachable!("Register not available"),
        }
    }
}
