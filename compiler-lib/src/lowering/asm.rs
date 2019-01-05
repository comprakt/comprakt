//! This is the final compiler stage, transforms LIR
//! into assembly code in a String or file.
//!
//! Steps taken in this file
//! - Transform LIR instructions on virtual registers to machine instructions
use std::io;

pub type Label = String;

#[derive(Debug)]
pub struct Program {
    // name => function
    pub functions: Vec<Function>,
}

#[derive(Debug)]
pub struct Function {
    pub name: String,
    pub nargs: usize,
    pub reg_counter: usize,
    pub returns: bool,
    pub issued_blocks: usize,
    pub blocks: Vec<Block>,
}

#[derive(Debug)]
pub struct Block {
    pub label: Label,
    pub instrs: Vec<Instr>,
}

use libfirm_rs::Tarval;

#[derive(Debug, Clone, Copy)]
pub enum Operand {
    Addr {
        offset: isize,
        reg: Reg,
    },
    Reg(Reg),
    /// NOTE: Tarcval contains a raw pointer, thus Imm(t) is only valid for the
    /// lifetime of that pointer (the FIRM graph).
    Imm(Tarval),
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
        dst: Option<Reg>,
    },
    Divop {
        kind: DivKind,
        src1: Operand,
        src2: Operand,
        dst1: Reg,
        dst2: Reg,
    },
    Basic {
        kind: BasicKind,
        op: Option<Operand>,
    },
    Cmpq {
        lhs: Operand,
        rhs: Operand,
    },
    Movq {
        src: Operand,
        dst: Operand,
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
    Comment(String),
}

#[derive(Debug, Display, Clone)]
pub enum Cond {
    #[display(fmt = "jmp")]
    True,
    #[display(fmt = "jle")]
    LessEqual,
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
}

#[derive(Debug, Display)]
pub enum UnopKind {
    #[display(fmt = "negq")]
    Neg,
    #[display(fmt = "notq")]
    Not,
}

#[derive(Debug, Display, Clone)]
pub enum DivKind {
    /// unsigned
    #[display(fmt = "div")]
    Div,
    /// signed
    #[display(fmt = "idiv")]
    IDiv,
}

#[derive(Debug, Display, Clone)]
pub enum BasicKind {
    #[display(fmt = "ret")]
    Ret,
    #[display(fmt = "notq")]
    Not,
    #[display(fmt = "negq")]
    Neg,
    #[display(fmt = "popq")]
    Pop,
}

pub enum Language {
    Molki,
    Asm64,
}

impl Program {
    fn emit(&self, lang: Language, out: &mut impl io::Write) -> io::Result<()> {
        match lang {
            Language::Molki => self.emit_molki(out),
            Language::Asm64 => self.emit_asm64(out),
        }
    }
}
