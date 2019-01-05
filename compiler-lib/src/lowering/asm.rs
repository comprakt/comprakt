//! This is the final compiler stage, transforms LIR
//! into assembly code in a string or file.
#![allow(clippy::new_without_default_derive)]

use libfirm_rs::nodes::NodeTrait;
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
    Amd64,
}

impl std::fmt::Display for Operand {
    fn fmt(&self, fmt: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Operand::Addr { offset, reg } => write!(fmt, "{}({})", offset, reg),
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

impl Program {
    pub fn new() -> Program {
        Program { functions: vec![] }
    }

    /// Write the program to a string or file in the given assembly
    /// language
    ///
    /// Instruction scheduling and register allocation might
    /// reorder the instructions in the program
    fn emit(&mut self, lang: Language, out: &mut impl io::Write) -> io::Result<()> {
        match lang {
            Language::Molki => self.emit_molki(out),
            Language::Amd64 => self.emit_amd64(out),
        }
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
}

use crate::lowering::lir;
use std::collections::HashMap;

impl From<lir::LIR> for Program {
    fn from(p: lir::LIR) -> Program {
        let mut mp = Program::new();
        for f in &p.functions {
            let mf_name = f.name.to_owned();
            let mut mf = Function::new(mf_name, f.nargs, f.returns);
            let mut mblocks = HashMap::new();
            let mut is_entry_block = true;

            for block in f.graph.iter_blocks() {
                let mut mblock = mf.begin_block(format!(".L{}", block.borrow().firm.node_id()));
                for instr in &block.borrow().code {
                    mblock.push(instr.clone());
                }
                // TODO assert that there is a jump at the end of each instr list
                mblocks.insert(block.borrow().firm, (mblock, is_entry_block));
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
