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
    pub regs: Vec<Vec<MutRc<ValueSlot>>>,
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
    /// The slot number. Uniqe only per Block, not globally
    num: usize,
    /// The firm node that corresponds to this value
    firm: Node,

    /// The block in which this slot is allocated
    allocated_in: MutWeak<BasicBlock>,
    /// The block in which the value of this slot originates
    originates_in: MutWeak<BasicBlock>,
    /// The block in which this value is used
    terminates_in: MutWeak<BasicBlock>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Display)]
pub enum ValueSlotKind {
    /// The value in this slot originates in the local block, but is used in a
    /// later block
    ///
    /// Original values must appear in the transitions of all `succs`.
    #[display(fmt = "original")]
    Original,
    /// The value in this slot is used in the instructions of the local block,
    /// but originiates in a previous block
    ///
    /// Terminal values must appear in the transitions of all `preds`.
    #[display(fmt = "terminal")]
    Terminal,
    /// The slot is unused in the local block, but contains a value that must be
    /// kept alive for the duration of this block. This is created for values
    /// that are calculated in an earlier block, and used in a later block (but
    /// not in this block).
    ///
    /// Pass-through values must appear in the transitions both all `preds` and
    /// `succs`.
    #[display(fmt = "pass-through")]
    PassThrough,
    /// The value of the slot both originates and is used (terminates) in the
    /// local block.
    ///
    /// Private values must appear neither in the transitions of any `preds`,
    /// nor `succs`.
    #[display(fmt = "private")]
    Private,
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
    register_transitions: Vec<(MutRc<ValueSlot>, MutRc<ValueSlot>)>,

    source: MutWeak<BasicBlock>,
    pub target: MutRc<BasicBlock>,
}

impl From<libfirm_rs::graph::Graph> for BlockGraph {
    fn from(firm_graph: libfirm_rs::graph::Graph) -> Self {
        let mut graph = Self::build_skeleton(firm_graph);
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
        self.firm.walk_blocks(|visit, firm_block| match visit {
            VisitTime::BeforePredecessors => {
                log::debug!("VISIT {:?}", firm_block);
                let local_block = self.get_block(*firm_block);

                // Foreign values are the green points in yComp (inter-block edges)
                for node_in_block in firm_block.out_nodes() {
                    log::debug!("---");
                    match node_in_block {
                        Node::Phi(phi) => {
                            let multislot = local_block.new_terminating_multislot();
                            phi.preds()
                                // What TODO with mem edges?
                                .filter(|(_, value)| {
                                    value.mode() != unsafe { libfirm_rs::bindings::mode::M }
                                })
                                // The next two 'maps' need to be seperated in two closures
                                // because we wan't to selectively `move` `value` and
                                // `multislot` into the closure, but take `self` by reference
                                .map(|(cfg_pred, value)| {
                                    (
                                        cfg_pred,
                                        value,
                                        MutRc::downgrade(&self.get_block(value.block())),
                                    )
                                })
                                .map(move |(cfg_pred, value, original_block)| {
                                    (
                                        cfg_pred,
                                        multislot.add_possible_value(value, original_block),
                                    )
                                })
                                .for_each(|(cfg_pred, value_slot)| {
                                    log::debug!("");
                                    local_block
                                        .borrow()
                                        .find_incoming_edge_from(cfg_pred)
                                        .unwrap()
                                        .borrow_mut()
                                        .add_incoming_value_flow(value_slot);
                                });
                        }

                        _ => node_in_block
                            .in_nodes()
                            // What TODO with mem edges?
                            .filter(|value| {
                                value.mode() != unsafe { libfirm_rs::bindings::mode::M }
                            })
                            .filter_map(|value| {
                                if value.block() != *firm_block {
                                    Some(value)
                                } else {
                                    // This is a value produced by our block, so no need to get
                                    // it from somewhere
                                    None
                                }
                            })
                            // Foreign values that are not phi, flow in from each cfg pred
                            // => values x cfg_preds
                            .for_each(|value| {
                                log::debug!("");
                                // Do this here, because we don't want to move `self` or
                                // `local_block` into closure
                                let original_block = self.get_block(value.block());
                                let local_block = MutRc::clone(&local_block);
                                local_block.new_terminating_slot(value, original_block.downgrade());
                            }),
                    }
                }
            }

            VisitTime::AfterPredecessors => (),
        });
    }

    fn get_block(&self, firm_block: libfirm_rs::nodes::Block) -> MutRc<BasicBlock> {
        self.blocks
            .get(&firm_block)
            .expect("BlockGraph is incomplete")
            .clone()
    }
}

impl ControlFlowTransfer {
    /// Only call this from target
    fn add_incoming_value_flow(&mut self, target_slot: MutRc<ValueSlot>) {
        let source_slot = self
            .source
            .upgrade()
            .unwrap()
            .new_forwarding_slot(&target_slot.borrow());

        assert_eq!(target_slot.borrow().firm, source_slot.borrow().firm);
        log::debug!(
            "\tTRANSFER: from='{:?}' to='{:?}' value='{:?}'",
            upborrow!(self.source).firm,
            self.target.borrow().firm,
            target_slot.borrow().firm,
        );

        self.register_transitions.push((source_slot, target_slot));
    }
}

impl MutRc<BasicBlock> {
    fn new_multislot(&self, terminates_in: MutWeak<BasicBlock>) -> MultiSlotBuilder {
        MultiSlotBuilder::new(MutRc::clone(self), terminates_in)
    }

    fn new_terminating_multislot(&self) -> MultiSlotBuilder {
        self.new_multislot(MutRc::downgrade(self))
    }

    fn new_slot(
        &self,
        value: libfirm_rs::nodes::Node,
        originates_in: MutWeak<BasicBlock>,
        terminates_in: MutWeak<BasicBlock>,
    ) -> MutRc<ValueSlot> {
        let this = self.borrow();
        let possibly_existing_slot = this
            .regs
            .iter()
            .filter_map(|multislot| multislot.iter().find(|slot| slot.borrow().firm == value))
            .next();

        let slot = if let Some(slot) = possibly_existing_slot {
            log::debug!(
                "\tREUSE: slot={} in='{:?}' kind={} value='{:?}'",
                slot.borrow().num,
                this.firm,
                slot.borrow().value_kind(),
                slot.borrow().firm
            );

            MutRc::clone(slot)
        } else {
            drop(possibly_existing_slot);
            drop(this);
            self.new_multislot(terminates_in)
                .add_possible_value(value, originates_in)
        };

        // If the value is foreign, we need to "get it" from each blocks above us,
        // to pass it through to the block below us. This only happens if this function
        // is called from `ControlFlowtransfer::add_incoming_value_flow`.
        if !slot.borrow().value_kind().originates_here() {
            for pred in &self.borrow().preds {
                upborrow!(mut pred).add_incoming_value_flow(MutRc::clone(&slot));
            }
        }

        slot
    }

    fn new_forwarding_slot(&self, target_slot: &ValueSlot) -> MutRc<ValueSlot> {
        let local_slot = self.new_slot(
            target_slot.firm,
            MutWeak::clone(&target_slot.originates_in),
            MutWeak::clone(&target_slot.terminates_in),
        );

        assert!(!local_slot.borrow().value_kind().terminates_here());
        local_slot
    }

    fn new_terminating_slot(
        &self,
        value: libfirm_rs::nodes::Node,
        origin: MutWeak<BasicBlock>,
    ) -> MutRc<ValueSlot> {
        let local_slot = self.new_slot(value, origin, MutRc::downgrade(self));

        assert!(local_slot.borrow().value_kind().terminates_here());
        local_slot
    }

    #[allow(dead_code)]
    fn new_private_slot(&self, value: libfirm_rs::nodes::Node) -> MutRc<ValueSlot> {
        assert_eq!(value.block(), self.borrow().firm);
        let slot = self.new_slot(value, MutRc::downgrade(self), MutRc::downgrade(self));

        assert!(slot.borrow().value_kind().is_private());
        slot
    }
}

impl BasicBlock {
    fn find_incoming_edge_from(
        &self,
        cfg_pred: libfirm_rs::nodes::Block,
    ) -> Option<MutRc<ControlFlowTransfer>> {
        self.preds
            .iter()
            .map(|edge| MutWeak::upgrade(edge).unwrap())
            .find(|edge| upborrow!(edge.borrow().source).firm == cfg_pred)
    }

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

#[derive(Debug)]
pub struct MultiSlotBuilder {
    num: usize,
    allocated_in: MutWeak<BasicBlock>,
    terminates_in: MutWeak<BasicBlock>,
}

impl MultiSlotBuilder {
    fn new(allocated_in: MutRc<BasicBlock>, terminates_in: MutWeak<BasicBlock>) -> Self {
        let num = allocated_in.borrow().regs.len();
        allocated_in.borrow_mut().regs.push(Vec::new());
        MultiSlotBuilder {
            num,
            allocated_in: MutRc::downgrade(&allocated_in),
            terminates_in,
        }
    }

    fn add_possible_value(
        &self,
        value: libfirm_rs::nodes::Node,
        originates_in: MutWeak<BasicBlock>,
    ) -> MutRc<ValueSlot> {
        assert!(upborrow!(self.allocated_in).regs.get(self.num).is_some());

        {
            let allocated_in = MutWeak::upgrade(&self.allocated_in).unwrap();
            let allocated_in = allocated_in.borrow();
            let is_duplicate = allocated_in.regs[self.num]
                .iter()
                .any(|slot| slot.borrow().firm == value);

            assert!(!is_duplicate);
        }

        let slot = ValueSlot {
            num: self.num,
            firm: value,
            allocated_in: MutWeak::clone(&self.allocated_in),
            originates_in,
            terminates_in: MutWeak::clone(&self.terminates_in),
        };

        log::debug!(
            "\tALLOC: slot={} in='{:?}' kind={} value='{:?}'",
            slot.num,
            upborrow!(self.allocated_in).firm,
            slot.value_kind(),
            slot.firm
        );

        let slot = MutRc::new(slot);
        upborrow!(mut self.allocated_in).regs[self.num].push(MutRc::clone(&slot));

        slot
    }
}

impl ValueSlot {
    pub fn value_kind(&self) -> ValueSlotKind {
        use self::ValueSlotKind::*;
        if upborrow!(self.originates_in).firm == upborrow!(self.terminates_in).firm {
            assert_eq!(
                upborrow!(self.allocated_in).firm,
                upborrow!(self.originates_in).firm
            );
            Private
        } else if upborrow!(self.allocated_in).firm == upborrow!(self.originates_in).firm {
            Original
        } else if upborrow!(self.allocated_in).firm == upborrow!(self.terminates_in).firm {
            Terminal
        } else {
            PassThrough
        }
    }
}

impl ValueSlotKind {
    pub fn is_private(self) -> bool {
        use self::ValueSlotKind::*;
        self == Private
    }

    pub fn is_original(self) -> bool {
        use self::ValueSlotKind::*;
        self == Original
    }

    pub fn is_terminal(self) -> bool {
        use self::ValueSlotKind::*;
        self == Terminal
    }

    pub fn is_pass_through(self) -> bool {
        use self::ValueSlotKind::*;
        self == PassThrough
    }

    pub fn terminates_here(self) -> bool {
        self.is_private() || self.is_terminal()
    }

    pub fn originates_here(self) -> bool {
        self.is_private() || self.is_original()
    }
}
