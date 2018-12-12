type Label = String;

pub struct Program {
    // name => function
    functions: Vec<Function>,
}

pub struct Function {
    name: String,
    nargs: usize,
    reg_counter: usize,
    returns: bool,
    label_counter: usize,
    pending_blocks: usize,
    blocks: Vec<Block>,
}

pub struct Block {
    label: Label,
    instrs: Vec<Instr>,
}

#[derive(Clone, Copy)]
pub enum Operand {
    Reg(Reg),
    Imm(isize),
}

#[derive(Clone, Copy)]
pub enum Reg {
    /// Magic register that contains the return value of a function call (rax).
    R0,
    N(usize),
}

impl Reg {
    pub fn into_operand(self) -> Operand {
        Operand::Reg(self)
    }
}

pub enum Instr {
    Binop {
        kind: BinopKind,
        src1: Operand,
        src2: Operand,
        dst: Option<Reg>,
    },
    Divop {
        kind: DivKind,
        src1: Operand,
        src2: Operand,
        dst1: Reg,
        dst2: Reg,
    },
    Cmpq {
        lhs: Operand,
        rhs: Operand,
    },
    Movq {
        src: Operand,
        dst: Reg,
    },
    /// If dst is None, result is in register r0, which cannot be accessed
    /// using molki register names.
    Call {
        func: String,
        args: Vec<Operand>,
        dst: Option<Reg>,
    },
    Jmp {
        target: Label,
        cond: Cond,
    },
}

#[derive(Display)]
pub enum Cond {
    #[display(fmt = "jmp")]
    True,
    #[display(fmt = "jle")]
    LessEqual,
}

#[derive(Display)]
pub enum BinopKind {
    #[display(fmt = "addq")]
    Addq,
    #[display(fmt = "subq")]
    Subq,
}

#[derive(Display)]
pub enum UnopKind {}

#[derive(Display)]
pub enum DivKind {
    /// unsigned
    #[display(fmt = "div")]
    Div,
    /// signed
    #[display(fmt = "idiv")]
    IDiv,
}

use std::io;

impl Program {
    pub fn emit_molki(&self, out: &mut impl io::Write) -> io::Result<()> {
        for f in &self.functions {
            f.emit_molki(out)?;
        }
        Ok(())
    }
}

impl Function {
    fn emit_molki(&self, out: &mut impl io::Write) -> io::Result<()> {
        let nret = if self.returns { 1 } else { 0 };
        writeln!(out, ".function {} {} {}", self.name, self.nargs, nret)?;
        for block in self.blocks.iter() {
            block.emit_molki(out)?;
        }
        writeln!(out, ".endfunction")
    }
}

impl Block {
    fn emit_molki(&self, out: &mut impl io::Write) -> io::Result<()> {
        writeln!(out, "{}:", self.label);
        for i in &self.instrs {
            write!(out, "\t");
            i.emit_molki(out)?;
            writeln!(out, "");
        }
        Ok(())
    }
}

impl Instr {
    fn emit_molki(&self, out: &mut impl io::Write) -> io::Result<()> {
        use self::Instr::*;
        match self {
            Binop {
                kind,
                src1,
                src2,
                dst,
            } => {
                write!(out, "{} [ {} | {} ]", kind, src1, src2)?;
                if let Some(dst) = dst {
                    write!(out, " -> {}", dst)?;
                }
                Ok(())
            }
            Divop {
                kind,
                src1,
                src2,
                dst1,
                dst2,
            } => write!(
                out,
                "{} [ {} | {} ] -> [ {} | {} ]",
                kind, src1, src2, dst1, dst2
            ),
            Movq { src, dst } => write!(out, "movq {}, {}", src, dst),
            Cmpq { lhs, rhs } => write!(out, "cmpq {}, {}", lhs, rhs),
            Call { func, args, dst } => {
                let args = args.iter().map(|a| format!("{}", a)).collect::<Vec<_>>();
                write!(out, "call {} [ {} ]", func, args[..].join(","))?;
                if let Some(dst) = dst {
                    write!(out, " -> {}", dst)?;
                }
                Ok(())
            }
            Jmp { target, cond } => {
                use self::Cond::*;
                let instr = match cond {
                    True => "jmp",
                    LessEqual => "jle",
                };
                write!(out, "{} {}", instr, target)
            }
        }
    }
}

impl std::fmt::Display for Operand {
    fn fmt(&self, fmt: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Operand::Reg(reg) => write!(fmt, "{}", reg),
            Operand::Imm(val) => write!(fmt, "${}", val),
        }
    }
}

impl std::fmt::Display for Reg {
    fn fmt(&self, fmt: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Reg::R0 => write!(fmt, "%@r0"),
            Reg::N(num) => write!(fmt, "%@{}", num),
        }
    }
}

impl Program {
    pub fn new() -> Program {
        Program { functions: vec![] }
    }
    pub fn add_function(&mut self, f: Function) {
        assert!(!f.name.chars().any(|c| c.is_whitespace()));
        assert_eq!(f.pending_blocks, 0);
        self.functions.push(f);
    }
}

impl Function {
    pub fn new(name: String, nargs: usize, returns: bool) -> Function {
        Function {
            name,
            label_counter: 0,
            nargs,
            reg_counter: nargs,
            returns,
            pending_blocks: 0,
            blocks: vec![],
        }
    }
    pub fn name(&self) -> String {
        self.name.clone()
    }
    pub fn arg_reg(&self, arg: usize) -> Reg {
        assert!(arg < self.nargs);
        Reg::N(arg)
    }
    pub fn arg_reg_operand(&self, arg: usize) -> Operand {
        Operand::Reg(self.arg_reg(arg))
    }

    pub fn new_reg(&mut self) -> Reg {
        let reg = Reg::N(self.reg_counter);
        self.reg_counter += 1;
        reg
    }

    fn gen_label(&mut self) -> Label {
        let label = format!("L{}", self.label_counter);
        self.label_counter += 1;
        label
    }

    pub fn begin_block(&mut self) -> Block {
        self.pending_blocks += 1;
        Block::new(self.gen_label())
    }

    pub fn complete_entry_block(&mut self, block: Block) {
        assert!(self.blocks.len() == 0);
        self.pending_blocks -= 1;
        self.blocks.insert(0, block);
    }

    pub fn complete_block(&mut self, block: Block) {
        self.pending_blocks -= 1;
        self.blocks.push(block);
    }
}

impl Block {
    pub fn new(label: Label) -> Block {
        Block {
            label,
            instrs: vec![],
        }
    }
    pub fn label(&self) -> String {
        self.label.to_owned()
    }
    pub fn push(&mut self, instr: Instr) {
        self.instrs.push(instr);
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn works() {
        let expected = r"
.function fib 1 1
L0:
	cmpq $1, %@0
	jle L1
	subq [ $1 | %@0 ] -> %@1
	subq [ $2 | %@0 ] -> %@2
	call fib [ %@1 ] -> %@3
	call fib [ %@2 ] -> %@4
	addq [ %@3 | %@4 ] -> %@r0
	jmp L2
L1:
	movq %@0, %@r0
	jmp L2
L2:
.endfunction
         "
        .trim();

        let mut prog = Program::new();
        let mut fib = Function::new("fib".to_string(), 1, true);

        let mut entry = fib.begin_block();
        let mut fib_basecase = fib.begin_block();
        let mut end = fib.begin_block();

        use self::{BinopKind::*, Instr::*};

        entry.push(Cmpq {
            lhs: Operand::Imm(1),
            rhs: fib.arg_reg_operand(0),
        });
        entry.push(Jmp {
            cond: Cond::LessEqual,
            target: fib_basecase.label(),
        });
        let r1 = fib.new_reg();
        entry.push(Binop {
            kind: Subq,
            src1: Operand::Imm(1),
            src2: fib.arg_reg(0).into_operand(),
            dst: Some(r1),
        });
        let r2 = fib.new_reg();
        entry.push(Binop {
            kind: Subq,
            src1: Operand::Imm(2),
            src2: fib.arg_reg(0).into_operand(),
            dst: Some(r2),
        });

        let r3 = fib.new_reg();
        entry.push(Call {
            func: fib.name(),
            args: vec![r1.into_operand()],
            dst: Some(r3),
        });
        let r4 = fib.new_reg();
        entry.push(Call {
            func: fib.name(),
            args: vec![r2.into_operand()],
            dst: Some(r4),
        });

        entry.push(Binop {
            kind: Addq,
            src1: r3.into_operand(),
            src2: r4.into_operand(),
            dst: Some(Reg::R0),
        });
        entry.push(Jmp {
            cond: Cond::True,
            target: end.label(),
        });
        fib.complete_entry_block(entry);

        fib_basecase.push(Movq {
            src: fib.arg_reg(0).into_operand(),
            dst: Reg::R0,
        });
        fib_basecase.push(Jmp {
            cond: Cond::True,
            target: end.label(),
        });

        fib.complete_block(fib_basecase);
        fib.complete_block(end);

        prog.add_function(fib);

        let mut res = Vec::new();
        prog.emit_molki(&mut res).unwrap();
        let res = String::from_utf8(res).unwrap();
        let res = res.trim();

        println!("if things seem equal, check for tabs vs spaces!");
        println!("expected:\n{}\n\nresult:\n{}", expected, res);

        assert_eq!(expected, res);
    }
}
