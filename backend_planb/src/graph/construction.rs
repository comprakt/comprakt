use super::*;
use firm_construction as firm;
use firm_construction::FirmProgram;
use libfirm_rs::{
    self, bindings,
    nodes::{self, Node, NodeTrait, ProjKind},
    Tarval, VisitTime,
};
use optimization::{Local, RemoveCriticalEdges};
use std::collections::{HashMap, HashSet, VecDeque};
use type_checking::type_system::CheckedType;

impl<'alloc> Program<'alloc> {
    pub fn from_firm(alloc: &'alloc Alloc, firm_prog: &FirmProgram<'_, '_>) -> Program<'alloc> {
        let mut functions = Vec::new();

        for method in firm_prog.methods.values() {
            let function = Function::from_firm(&*method.borrow(), &alloc);
            functions.push(function);
        }

        Program { alloc, functions }
    }
}

impl Function {
    fn from_firm(method: &firm::FirmMethod<'_, '_>, alloc: &Alloc) -> Self {
        let graph: libfirm_rs::Graph = method
            .graph
            .unwrap_or_else(|| panic!("Cannot lower function without a graph {}", method.def.name));

        let returns = method.def.return_ty != CheckedType::Void;
        Function {
            ld_name: method.entity.ld_name().to_str().unwrap().to_owned(),
            nargs: method.def.params.len() + if method.def.is_static { 0 } else { 1 },
            returns,
            graph: BlockGraph::from_firm(graph, alloc),
            ar: ActivationRecord::default(),
        }
    }
}

impl BlockGraph {
    fn from_firm(firm_graph: libfirm_rs::Graph, alloc: &Alloc) -> Ptr<Self> {
        RemoveCriticalEdges::optimize_function(firm_graph);
        firm_graph.assure_outs();

        let mut graph = BlockGraph::build_skeleton(firm_graph, alloc);
        graph.build_fill_instrs();
        graph
    }

    fn build_skeleton(firm_graph: libfirm_rs::Graph, alloc: &Alloc) -> Ptr<Self> {
        let mut blocks = HashMap::new();

        // This is basically a `for each edge "firm_target -> firm_source"`
        firm_graph.walk_blocks(|visit, firm_target| match visit {
            VisitTime::BeforePredecessors => {
                let mut target = Block::build_skeleton(&mut blocks, *firm_target, alloc);

                for firm_source in firm_target.cfg_preds() {
                    let mut source = Block::build_skeleton(&mut blocks, firm_source.block(), alloc);
                    source.succs.push(target);
                    target.preds.push(source);
                }
            }

            VisitTime::AfterPredecessors => (),
        });

        let start_block = *blocks
            .get(&firm_graph.start_block())
            .expect("All blocks (including start block) should have been generated");

        let end_block = *blocks
            .get(&firm_graph.end_block())
            .expect("All blocks (including end block) should have been generated");

        let graph = alloc.graph.alloc(BlockGraph {
            firm: firm_graph,
            blocks,
            start_block,
            end_block,
        });

        graph
    }

    fn block(&self, libfirm_block: libfirm_rs::nodes::Block) -> Ptr<Block> {
        *self.blocks.get(&libfirm_block).unwrap()
    }

    fn build_fill_instrs(&mut self) {}
}

impl Block {
    fn build_skeleton(
        known_blocks: &mut HashMap<libfirm_rs::nodes::Block, Ptr<Block>>,
        firm: libfirm_rs::nodes::Block,
        alloc: &Alloc,
    ) -> Ptr<Self> {
        *known_blocks.entry(firm).or_insert_with(|| {
            alloc.block.alloc(Block {
                code: Code::default(),
                succs: Vec::new(),
                preds: Vec::new(),
                firm: firm,
            })
        })
    }
}
