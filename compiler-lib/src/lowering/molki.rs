// TODO: this is a temporary fix to avoid merge conflicts
// by renaming everything from molki::* to asm::*
pub use super::asm::*;
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
        writeln!(out, "{}:", self.label)?;
        for i in &self.instrs {
            write!(out, "\t")?;
            i.emit_molki(out)?;
            writeln!(out)?;
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
            Basic { kind, op } => {
                if let Some(op) = op {
                    write!(out, "{} {}", kind, op)
                } else {
                    write!(out, "{}", kind)
                }
            }
            Movq { src, dst } => write!(out, "movq {}, {}", src, dst),
            Cmpq { lhs, rhs } => write!(out, "cmpq {}, {}", lhs, rhs),
            Call { func, args, dst } => {
                let args = args.iter().map(|a| format!("{}", a)).collect::<Vec<_>>();
                write!(out, "call {} [ {} ]", func, args[..].join(" | "))?;
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
            Comment(c) => {
                assert!(!c.contains("/*"));
                assert!(!c.contains("*/"));
                write!(out, "/* {} */", c)
            }
        }
    }
}

#[cfg(test)]
#[allow(clippy::print_stdout)]
#[rustfmt::skip]
mod tests {
    use super::*;
    #[test]
    fn works() {

        libfirm_rs::init();

        let expected = r"
.function fib 1 1
entry:
	/* some comment */
	cmpq $1, %@0
	jle fib_basecase
	subq [ $1 | %@0 ] -> %@1
	subq [ $2 | %@0 ] -> %@2
	call fib [ %@1 ] -> %@3
	call fib [ %@2 ] -> %@4
	addq [ %@3 | %@4 ] -> %@r0
	jmp end
fib_basecase:
	movq %@0, %@r0
	jmp end
end:
.endfunction
         "
        .trim();

        let mut prog = Program::new();
        let mut fib = Function::new("fib".to_string(), 1, true);

        let mut entry = fib.begin_block("entry".to_owned());
        let mut fib_basecase = fib.begin_block("fib_basecase".to_owned());
        let end = fib.begin_block("end".to_owned());

        use self::{BinopKind::*, Instr::*};

        entry.push(Comment("some comment".to_owned()));
        entry.push(Cmpq {
            lhs: Operand::Imm(Tarval::mj_int(1)),
            rhs: fib.arg_reg_operand(0),
        });
        entry.push(Jmp {
            cond: Cond::LessEqual,
            target: fib_basecase.label(),
        });
        let r1 = fib.new_reg();
        entry.push(Binop {
            kind: Sub,
            src1: Operand::Imm(Tarval::mj_int(1)),
            src2: fib.arg_reg(0).into_operand(),
            dst: Some(r1),
        });
        let r2 = fib.new_reg();
        entry.push(Binop {
            kind: Sub,
            src1: Operand::Imm(Tarval::mj_int(2)),
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
            kind: Add,
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
            dst: Reg::R0.into_operand(),
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
