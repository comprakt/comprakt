#![allow(clippy::new_without_default_derive)]
use super::lir_allocator::Ptr;
use crate::lowering::lir;
use libfirm_rs::Tarval;
use std::{collections::HashMap, io};

type Label = String;

#[derive(Debug)]
pub struct Program {
    // name => function
    functions: Vec<Function>,
}

#[derive(Debug)]
pub struct Function {
    name: String,
    nargs: usize,
    reg_counter: usize,
    returns: bool,
    issued_blocks: usize,
    blocks: Vec<Block>,
}

#[derive(Debug)]
pub struct Block {
    label: Label,
    instrs: Vec<Instr>,
}

#[derive(Debug, Clone, Copy)]
pub enum Operand {
    Reg(Reg),
    /// NOTE: Tarcval contains a raw pointer, thus Imm(t) is only valid for the
    /// lifetime of that pointer (the FIRM graph).
    Imm(Tarval),
}

impl Operand {
    fn from(op: lir::Operand, slot_reg_map: &HashMap<(i64, usize), usize>) -> Self {
        use super::lir::Operand::*;
        match op {
            Slot(slot) => Reg::from(slot, slot_reg_map).into_operand(),
            Imm(val) => Operand::Imm(val),
            Param { idx } => Reg::N(idx as usize).into_operand(),
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub enum Reg {
    /// Magic register that contains the return value of a function call (rax).
    R0,
    /// NOTE: Tarcval contains a raw pointer, thus Imm(t) is only valid for the
    /// lifetime of that pointer (the FIRM graph).
    N(usize),
}

impl Reg {
    fn from(op: Ptr<lir::MultiSlot>, slot_reg_map: &HashMap<(i64, usize), usize>) -> Self {
        Reg::N(
            *slot_reg_map
                .get(&(op.allocated_in().num, op.num()))
                .expect("No register for ValueSlot"),
        )
    }
}

impl Reg {
    pub fn into_operand(self) -> Operand {
        Operand::Reg(self)
    }
}

#[derive(Debug, Clone)]
pub enum Instr {
    Binop {
        kind: BinopKind,
        src1: Operand,
        src2: Operand,
        dst: Reg,
    },
    Divop {
        src1: Operand,
        src2: Operand,
        dst1: Reg,
        dst2: Reg,
    },
    Unop {
        kind: UnopKind,
        src: Operand,
        dst: Reg,
    },
    Cmpq {
        lhs: Operand,
        rhs: Operand,
    },
    Movq {
        src: MoveOperand,
        dst: MoveOperand,
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
        kind: JmpKind,
    },
    Comment(String),
}

#[derive(Debug, Clone, Copy)]
pub enum JmpKind {
    Unconditional,
    Conditional(lir::CondOp),
}

#[derive(Debug, Clone)]
pub enum MoveOperand {
    Operand(Operand),
    Address(lir::AddressComputation<Operand>),
}

impl Instr {
    fn from(instr: lir::Instruction, slot_reg_map: &HashMap<(i64, usize), usize>) -> Instr {
        use super::lir::Instruction::*;
        match instr {
            Binop {
                kind,
                src1,
                src2,
                dst,
            } => Instr::Binop {
                kind: kind.into(),
                src1: Operand::from(src1, slot_reg_map),
                src2: Operand::from(src2, slot_reg_map),
                dst: Reg::from(dst, slot_reg_map),
            },
            Div { src1, src2, dst } => Instr::Divop {
                src1: Operand::from(src1, slot_reg_map),
                src2: Operand::from(src2, slot_reg_map),
                dst1: Reg::from(dst, slot_reg_map),
                dst2: Reg::N(usize::max_value()),
            },
            Mod { src1, src2, dst } => Instr::Divop {
                src1: Operand::from(src1, slot_reg_map),
                src2: Operand::from(src2, slot_reg_map),
                dst1: Reg::N(usize::max_value()),
                dst2: Reg::from(dst, slot_reg_map),
            },
            Unop { kind, src, dst } => Instr::Unop {
                kind: kind.into(),
                src: Operand::from(src, slot_reg_map),
                dst: Reg::from(dst.into(), slot_reg_map),
            },
            Conv { src, dst } => Instr::Movq {
                src: MoveOperand::Operand(Operand::from(src, slot_reg_map)),
                dst: MoveOperand::Operand(Operand::from(dst, slot_reg_map)),
            },
            Call { func, args, dst } => Instr::Call {
                func,
                args: args
                    .into_iter()
                    .map(|arg| Operand::from(arg, slot_reg_map))
                    .collect(),
                dst: dst.map(|dst| Reg::from(dst, slot_reg_map)),
            },
            LoadParam { idx, dst } => Instr::Movq {
                src: MoveOperand::Operand(Reg::N(idx).into_operand()),
                dst: MoveOperand::Operand(Reg::from(dst, slot_reg_map).into_operand()),
            },
            StoreMem { src, dst } => Instr::Movq {
                src: MoveOperand::Operand(Operand::from(src, slot_reg_map)),
                dst: MoveOperand::Address(lir::AddressComputation {
                    offset: dst.offset,
                    base: Operand::from(dst.base, slot_reg_map),
                    index: match dst.index {
                        lir::IndexComputation::Zero => lir::IndexComputation::Zero,
                        lir::IndexComputation::Displacement(op, s) => {
                            lir::IndexComputation::Displacement(Operand::from(op, slot_reg_map), s)
                        }
                    },
                }),
            },
            LoadMem { src, dst } => Instr::Movq {
                src: MoveOperand::Address(lir::AddressComputation {
                    offset: src.offset,
                    base: Operand::from(src.base, slot_reg_map),
                    index: match src.index {
                        lir::IndexComputation::Zero => lir::IndexComputation::Zero,
                        lir::IndexComputation::Displacement(op, s) => {
                            lir::IndexComputation::Displacement(Operand::from(op, slot_reg_map), s)
                        }
                    },
                }),
                dst: MoveOperand::Operand(Reg::from(dst, slot_reg_map).into_operand()),
            },
            Comment(c) => Instr::Comment(c),
        }
    }
}

#[derive(Debug, Display, Clone)]
pub enum Cond {
    #[display(fmt = "jmp")]
    True,
    #[display(fmt = "jle")]
    LessEqual,
}

impl From<lir::Cond> for Cond {
    fn from(op: lir::Cond) -> Self {
        use super::lir::Cond::*;
        match op {
            True => Cond::True,
            LessEqual => Cond::LessEqual,
        }
    }
}

#[derive(Debug, Display, Clone)]
pub enum BinopKind {
    #[display(fmt = "addq")]
    Add,
    #[display(fmt = "subq")]
    Sub,
    // We only multiply signed integers, so we can always use `imul`
    #[display(fmt = "imul")]
    Mul,
    #[display(fmt = "andq")]
    And,
    #[display(fmt = "orq")]
    Or,
    #[display(fmt = "xor")]
    Xor,
}

impl From<lir::BinopKind> for BinopKind {
    fn from(bk: lir::BinopKind) -> Self {
        use super::lir::BinopKind::*;
        match bk {
            Add => BinopKind::Add,
            Sub => BinopKind::Sub,
            Mul => BinopKind::Mul,
            And => BinopKind::And,
            Or => BinopKind::Or,
            Xor => BinopKind::Xor,
        }
    }
}

#[derive(Debug, Display, Clone)]
pub enum UnopKind {
    #[display(fmt = "negq")]
    Neg,
    #[display(fmt = "notq")]
    Not,
}

impl From<lir::UnopKind> for UnopKind {
    fn from(bk: lir::UnopKind) -> Self {
        use super::lir::UnopKind::*;
        match bk {
            Neg => UnopKind::Neg,
            Not => UnopKind::Not,
        }
    }
}

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
                write!(out, "{} [ {} | {} ] -> {}", kind, src1, src2, dst)?;
                Ok(())
            }
            Divop {
                src1,
                src2,
                dst1,
                dst2,
            } => write!(
                out,
                "idivq [ {} | {} ] -> [ {} | {} ]",
                src1, src2, dst1, dst2
            ),
            Unop { kind, src, dst } => write!(
                out,
                "{} {src}\nmovq {src} {dst}",
                kind,
                src = src,
                dst = dst
            ),
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
            Jmp { target, kind } => {
                use self::{lir::CondOp::*, JmpKind::*};
                let instr = match kind {
                    Unconditional => "jmp",
                    Conditional(Equals) => "je",
                    Conditional(NotEquals) => "jne",
                    Conditional(LessThan) => "jl",
                    Conditional(LessEquals) => "jle",
                    Conditional(GreaterThan) => "jg",
                    Conditional(GreaterEquals) => "jge",
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

impl std::fmt::Display for Operand {
    fn fmt(&self, fmt: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Operand::Reg(reg) => write!(fmt, "{}", reg),
            Operand::Imm(val) => write!(fmt, "${}", val.get_long()), // TODO render suitable value
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

impl std::fmt::Display for MoveOperand {
    fn fmt(&self, fmt: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            MoveOperand::Operand(op) => write!(fmt, "{}", op),
            MoveOperand::Address(addr) => write!(fmt, "{}", addr),
        }
    }
}

impl Program {
    pub fn new() -> Program {
        Program { functions: vec![] }
    }

    pub fn add_function(&mut self, f: Function) {
        assert!(!f.name.chars().any(|c| c.is_whitespace()));
        assert_eq!(f.issued_blocks, f.blocks.len());
        self.functions.push(f);
    }
}

impl Function {
    pub fn new(name: String, nargs: usize, returns: bool) -> Function {
        Function {
            name,
            nargs,
            reg_counter: nargs,
            returns,
            issued_blocks: 0,
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

    pub fn begin_block(&mut self, label: String) -> Block {
        self.issued_blocks += 1;
        Block::new(label)
    }

    pub fn complete_entry_block(&mut self, block: Block) {
        self.blocks.insert(0, block);
    }

    pub fn complete_block(&mut self, block: Block) {
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
    pub fn append(&mut self, instrs: &mut Vec<Instr>) {
        self.instrs.append(instrs);
    }
}

#[inline]
fn gen_label(block: Ptr<lir::BasicBlock>) -> String {
    format!(".L{}", block.num)
}

fn gen_leave(leave: &lir::Leave, slot_reg_map: &HashMap<(i64, usize), usize>) -> Vec<Instr> {
    use super::lir::Leave::*;
    match leave {
        CondJmp {
            op,
            lhs,
            rhs,
            true_target,
            false_target,
        } => vec![
            Instr::Cmpq {
                lhs: Operand::from(*lhs, slot_reg_map),
                rhs: Operand::from(*rhs, slot_reg_map),
            },
            Instr::Jmp {
                target: gen_label(*true_target),
                kind: JmpKind::Conditional(*op),
            },
            Instr::Jmp {
                target: gen_label(*false_target),
                kind: JmpKind::Unconditional,
            },
        ],
        Jmp { target } => vec![Instr::Jmp {
            target: gen_label(*target),
            kind: JmpKind::Unconditional,
        }],
        Return { value, end_block } => {
            let mut ret = vec![];
            if let Some(value) = value {
                ret.push(Instr::Movq {
                    src: MoveOperand::Operand(Operand::from(*value, slot_reg_map)),
                    dst: MoveOperand::Operand(Reg::R0.into_operand()),
                });
            }
            ret.push(Instr::Jmp {
                target: gen_label(*end_block),
                kind: JmpKind::Unconditional,
            });
            ret
        }
    }
}

impl From<lir::LIR> for Program {
    fn from(p: lir::LIR) -> Program {
        let mut mp = Program::new();
        for f in &p.functions {
            let mut slot_reg_map = HashMap::new();
            f.graph
                .iter_blocks()
                .flat_map(|b| {
                    // FIXME: EndBlock has `regs: [[]]`
                    // log::debug!("{:#?}", b.borrow());
                    b.regs
                        .iter()
                        .map(|multi_slot| match &(**multi_slot) {
                            lir::MultiSlot::Multi { slots, .. } if slots.is_empty() => None,
                            _ => Some((multi_slot.allocated_in().num, multi_slot.num())),
                        })
                        .collect::<Vec<_>>()
                })
                .enumerate()
                .for_each(|(i, key)| {
                    if let Some(key) = key {
                        slot_reg_map.entry(key).or_insert(i + f.nargs);
                    }
                });
            let mf_name = f.name.to_owned();
            let mut mf = Function::new(mf_name, f.nargs, f.returns);
            let mut mblocks = HashMap::new();
            let mut is_entry_block = true;

            for block in f.graph.iter_blocks() {
                let mut mblock = mf.begin_block(gen_label(block));
                let code = &block.code;
                mblock.append(
                    &mut code
                        .copy_in
                        .iter()
                        .map(|cp| Instr::Movq {
                            src: MoveOperand::Operand(
                                Reg::from(cp.src, &slot_reg_map).into_operand(),
                            ),
                            dst: MoveOperand::Operand(
                                Reg::from(cp.dst.allocated_in.regs[cp.dst.num], &slot_reg_map)
                                    .into_operand(),
                            ),
                        })
                        .collect(),
                );
                mblock.append(
                    &mut code
                        .body
                        .iter()
                        .cloned()
                        .map(|instr| Instr::from(instr, &slot_reg_map))
                        .collect(),
                );
                mblock.append(
                    &mut code
                        .copy_out
                        .iter()
                        .map(|cp| Instr::Movq {
                            src: MoveOperand::Operand(
                                Reg::from(cp.src, &slot_reg_map).into_operand(),
                            ),
                            dst: MoveOperand::Operand(
                                Reg::from(cp.dst.allocated_in.regs[cp.dst.num], &slot_reg_map)
                                    .into_operand(),
                            ),
                        })
                        .collect(),
                );
                mblock.append(
                    &mut code
                        .leave
                        .iter()
                        .flat_map(|instr| gen_leave(instr, &slot_reg_map))
                        .collect(),
                );

                // TODO assert that there is a jump at the end of each instr list
                mblocks.insert(block.firm, (mblock, is_entry_block));
                is_entry_block = false;
            }

            for (_, (mblock, is_entry_block)) in mblocks {
                if is_entry_block {
                    mf.complete_entry_block(mblock);
                } else {
                    mf.complete_block(mblock)
                }
            }
            mp.add_function(mf);
        }
        mp
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
            kind: JmpKind::Conditional(lir::CondOp::LessEquals),
            target: fib_basecase.label(),
        });
        let r1 = fib.new_reg();
        entry.push(Binop {
            kind: Sub,
            src1: Operand::Imm(Tarval::mj_int(1)),
            src2: fib.arg_reg(0).into_operand(),
            dst: r1,
        });
        let r2 = fib.new_reg();
        entry.push(Binop {
            kind: Sub,
            src1: Operand::Imm(Tarval::mj_int(2)),
            src2: fib.arg_reg(0).into_operand(),
            dst: r2,
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
            dst: Reg::R0,
        });
        entry.push(Jmp {
            kind: JmpKind::Unconditional,
            target: end.label(),
        });
        fib.complete_entry_block(entry);

        fib_basecase.push(Movq {
            src: MoveOperand::Operand(fib.arg_reg(0).into_operand()),
            dst: MoveOperand::Operand(Reg::R0.into_operand()),
        });
        fib_basecase.push(Jmp {
            kind: JmpKind::Unconditional,
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
