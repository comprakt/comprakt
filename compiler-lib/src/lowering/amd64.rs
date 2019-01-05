use super::asm::*;
use std::io;

impl Program {
    pub fn emit_amd64(&mut self, out: &mut impl io::Write) -> io::Result<()> {
        let mut amd64 = Amd64::new();

        writeln!(out, "\t.text")?;

        for function in &mut self.functions {
            amd64.local_register_allocation(function);
            amd64.instruction_scheduling(function);
            amd64.function(function, out)?;
        }

        Ok(())
    }
}

struct Amd64 {}

impl Amd64 {
    fn new() -> Self {
        Self {}
    }

    /// Perform register allocation, inserting
    /// spill and load code
    fn local_register_allocation(&mut self, function: &mut Function) {}

    /// Optimize the order of instructions
    fn instruction_scheduling(&mut self, function: &mut Function) {
        /* NOOP */
    }

    fn function(&mut self, function: &mut Function, out: &mut impl io::Write) -> io::Result<()> {
        self.function_prolog(function, out)?;
        for block in &mut function.blocks {
            self.block(block, out)?;
        }
        self.function_epilog(function, out)?;

        Ok(())
    }

    fn function_prolog(&mut self, function: &mut Function, out: &mut impl io::Write) -> io::Result<()> {
        writeln!(out, "# -- Begin  {}", function.name)?;
        writeln!(out, "\t.p2align  {},,{}", 4, 15)?; // .p2align 4,,15
        // "\t.p2align %u,%s,%u\n", po2alignment, fill_byte, maximum_skip
        writeln!(out, "\t.globl  {}", function.name)?; // .globl mj_main
        writeln!(out, "\t.type\t{}, @function", function.name)?;
        writeln!(out, "{}:", function.name)
    }

    fn function_epilog(&mut self, function: &mut Function, out: &mut impl io::Write) -> io::Result<()> {
        writeln!(out, "\t.size\t{name}, .-{name}", name=function.name)?;
        writeln!(out, "# -- End {}", function.name)
    }

    fn block(&mut self, block: &mut Block, out: &mut impl io::Write) -> io::Result<()> {
        writeln!(out, "{}:", block.label)?;
        for instr in &block.instrs {
            write!(out, "\t")?;
            self.instruction(instr, out)?;
            writeln!(out)?;
        }
        Ok(())
    }

    fn instruction(&mut self, instr: &Instr, out: &mut impl io::Write) -> io::Result<()> {
        match instr {
            Instr::Binop {
                kind,
                src1,
                src2,
                dst,
            } => if let Some(dst) = dst {
                write!(out, "{} {} {} {}", kind, dst, src1, src2)
            } else {
                write!(out, "{} {} {}", kind, src1, src2)
            },
            Instr::Divop {
                kind,
                src1,
                src2,
                dst1,
                dst2,
            } => {
                write!(out, "\tmov %edi, %eax\
                   \tmovslq %eax, %rax\
                   \tcqto\
                   \tmovslq %esi, %rsi\
                   \tidivq %rs\n")
            },
            Instr::Basic { kind, op } => {
                if let Some(op) = op {
                    write!(out, "{} {}", kind, op)
                } else {
                    write!(out, "{}", kind)
                }
            }
            Instr::Movq { src, dst } => write!(out, "movq {} {}", src, dst),
            Instr::Cmpq { lhs, rhs } => write!(out, "cmpq {} {}", lhs, rhs),
            Instr::Call { func, args, dst } => {
                // TODO: args
                write!(out, "call {}", func)
            }
            Instr::Jmp { target, cond } => {
                let instr = match cond {
                    Cond::True => "jmp",
                    Cond::LessEqual => "jle",
                };
                write!(out, "{} {}", instr, target)
            }
            Instr::Comment(c) => {
                write!(out, "#{}", c.replace("\n", "\n#"))
            }
        }
    }
}
