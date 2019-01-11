#![allow(unused)]

use super::Instruction;

#[derive(Default)]
pub(super) struct Block {
    pub(super) instrs: Vec<Instruction>,
}

pub(super) struct LinearScanAllocator {
    blocks: Vec<Block>,
}
