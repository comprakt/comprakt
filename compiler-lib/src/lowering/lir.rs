//! Low level intermediate representation

use crate::{
    firm,
    type_checking::type_system::CheckedType,
    utils::cell::{MutRc, MutWeak},
};
use libfirm_rs::{
    graph::VisitTime,
    nodes::{Node, NodeTrait},
};
use std::collections::HashMap;

#[derive(Debug)]
pub struct LIR {
    pub functions: Vec<Function>,
}

impl From<&firm::Program<'_, '_>> for LIR {
    fn from(prog: &firm::Program<'_, '_>) -> Self {
        let mut functions = Vec::new();

        for class in prog.classes.values() {
            for method in class.borrow().methods.values() {
                functions.push((&*method.borrow()).into());
            }
        }

        LIR { functions }
    }
}

#[derive(Debug)]
pub struct Function {
    pub name: String,
    pub nargs: usize,
    pub returns: bool,
    pub graph: BlockGraph,
}

impl From<&firm::Method<'_, '_>> for Function {
    fn from(method: &firm::Method<'_, '_>) -> Self {
        let graph: libfirm_rs::Graph = method
            .graph
            .expect(&format!(
                "Cannot lower function without a graph {}",
                method.def.name
            ))
            .into();
        let graph: libfirm_rs::graph::Graph = graph.into();

        let returns = method.def.return_ty != CheckedType::Void;
        log::debug!("Generating block graph for {}", method.def.name);
        Function {
            name: method._name.clone().into_string().unwrap(),
            nargs: method.def.params.len(),
            returns,
            graph: graph.into(),
        }
    }
}

#[derive(Debug)]
/// A graph of basic blocks. Each block is a list of instructions and a set of
/// pseudo-registers called `ValueSlots`. This is a more localized
/// represantation of SSA, as the value
/// slots (or variable names) are
/// namespaced per block (and can only be
/// refered to by adjacent blocks) and
/// the sources of the values are
/// annotated on each edge, instead of
/// being phi-nodes pointing to some far
/// away firm-node.
pub struct BlockGraph {
    pub firm: libfirm_rs::graph::Graph,
    blocks: HashMap<libfirm_rs::nodes::Block, MutRc<BasicBlock>>,
    pub head: MutRc<BasicBlock>,
}

/// This is a vertex in the basic-block graph
#[derive(Debug)]
pub struct BasicBlock {
    /// The Pseudo-registers used by the Block
    pub regs: Vec<ValueSlot>,
    /// The instructions (using arbitrarily many registers) of the block
    pub code: Vec<Instruction>,
    /// Control flow-transfers *to* this block.
    /// Usually at most 2
    pub preds: Vec<MutWeak<ControlFlowTransfer>>,
    /// Control flow-transfers *out of* this block
    /// Usually at most 2
    pub succs: Vec<MutRc<ControlFlowTransfer>>,

    /// The firm structure of this block
    pub firm: libfirm_rs::nodes::Block,
}

/// TODO Tie to problames instruction stuff
pub type Instruction = !;

/// An abstract pseudo-register
#[derive(Debug)]
pub struct ValueSlot {
    /// The block in which this slot is allocated
    //block: MutWeak<BasicBlock>,
    /// The slot number. Uniqe only per Block, not globally
    num: usize,
    kind: ValueSlotKind,
    /// The firm node that corresponds to this value
    firm: Node,
}

#[derive(Debug)]
pub enum ValueSlotKind {
    /// The value in this slot is used in the instructions of the block
    Used,
    /// Ghost slots are unused in this block, but contain values that must be
    /// kept alive for the duration of this block. This is created for values
    /// that are calculated in an earlier block, and used in a later block (but
    /// not in this block).
    Ghost,
}

/// Transfer control-flow from one block to another. This is an edge in the
/// basic-block graph
#[derive(Debug)]
pub struct ControlFlowTransfer {
    /// How do value slots used in the preceeding block map to value slots in
    /// the next block? SSA-information is encoded in this.
    ///
    /// ## How to use this in a register allocator
    /// A register-allocator can use this information to figure out which
    /// registers to use in the in the adjacent blocks (so the transitions have
    /// minimal "mismatches"). If there is copy-code needed to match the
    /// the registers for a given value slot, the
    /// following cases need to be considered:
    ///
    ///  - The `source` block has multiple `succ` edges for the slot: copy-code
    /// needs to be placed in each `target` block
    ///
    ///  - The `target` block has multiple `pred` edges for the slot: copy-code
    /// needs to be placed in each `source` block
    ///
    ///  - Both of the above (can happen in loops): An additional block needs
    /// to be introduced right on this edge containing the copy-code (This is
    /// exactly the *copy problem* from the lecture)
    ///
    /// The *swap problem* is handled by there being no semantic order between
    /// the register transitions.
    register_transitions: Vec<(ValueSlot, ValueSlot)>,

    source: MutWeak<BasicBlock>,
    pub target: MutRc<BasicBlock>,
}

impl From<libfirm_rs::graph::Graph> for BlockGraph {
    fn from(firm_graph: libfirm_rs::graph::Graph) -> Self {
        let mut graph = Self::build_skeleton(firm_graph);
        log::debug!("Head: {:?}", graph.head.borrow().firm);
        graph.fill_blocks();
        graph
    }
}

impl BlockGraph {
    fn build_skeleton(firm_graph: libfirm_rs::graph::Graph) -> Self {
        let mut blocks = HashMap::new();

        // This is basically a `for each edge "firm_target -> firm_source"`
        firm_graph.walk_blocks(|visit, firm_target| match visit {
            VisitTime::BeforePredecessors => {
                let target = BasicBlock::skeleton_block(&mut blocks, *firm_target);

                for firm_source in firm_target.cfg_preds() {
                    let source = BasicBlock::skeleton_block(&mut blocks, firm_source);

                    let edge = MutRc::new(ControlFlowTransfer {
                        register_transitions: Vec::new(),
                        source: MutRc::downgrade(&source),
                        target: MutRc::clone(&target),
                    });

                    log::debug!("Visiting edge: {:?}->{:?}", firm_source, firm_target);

                    source.borrow_mut().succs.push(MutRc::clone(&edge));
                    target.borrow_mut().preds.push(MutRc::downgrade(&edge));
                }
            }

            VisitTime::AfterPredecessors => (),
        });

        let head = MutRc::clone(
            blocks
                .get(&firm_graph.start_block())
                .expect("All blocks (including start block) should have been generated"),
        );

        BlockGraph {
            firm: firm_graph,
            blocks,
            head,
        }
    }

    fn fill_blocks(&mut self) {
        self.firm.walk_blocks(|visit, firm_target| match visit {
            VisitTime::BeforePredecessors => {
                log::debug!("Nodes in: {:?}", firm_target);
                for node_in_block in firm_target.out_nodes() {
                    log::debug!("\tGreen points of {:?}", node_in_block);
                    match node_in_block {
                        Node::Phi(phi) => {
                            for (cfg_pred, value) in phi.preds() {
                                log::debug!("\t\t {:?} via {:?}", value, cfg_pred);
                            }
                        }
                        _ => {
                            for child in node_in_block.in_nodes() {
                                if child.block() != *firm_target {
                                    log::debug!("\t\t{:?}", child)
                                }
                            }
                        }
                    }
                }
            }

            VisitTime::AfterPredecessors => (),
        });
    }
}

impl BasicBlock {
    fn skeleton_block(
        known_blocks: &mut HashMap<libfirm_rs::nodes::Block, MutRc<BasicBlock>>,
        firm: libfirm_rs::nodes::Block,
    ) -> MutRc<Self> {
        known_blocks
            .entry(firm)
            .or_insert_with(|| {
                MutRc::new(BasicBlock {
                    regs: Vec::new(),
                    code: Vec::new(),
                    preds: Vec::new(),
                    succs: Vec::new(),
                    firm,
                })
            })
            .clone()
    }
}
