use crate::{
    analysis::CallGraph,
    dot::Label,
    firm::{FirmMethodP, FirmProgram},
    optimization::{self, Outcome},
    ref_eq::RefEq,
};
use libfirm_rs::{nodes::*, Graph};
use petgraph::algo::{condensation, toposort};
use std::collections::hash_map::HashMap;

pub struct Inlining;

impl optimization::Interprocedural for Inlining {
    fn optimize<'src, 'ast>(program: &FirmProgram<'src, 'ast>) -> Outcome {
        let g = CallGraph::new(program).construct();
        let mut g = condensation(g, false);

        g.reverse();
        g.retain_edges(|g, e| {
            let (n1, n2) = g.edge_endpoints(e).unwrap();
            n1 != n2
        });

        let topo_sort = toposort(&g, None).unwrap();
        let mut map = HashMap::new();
        for (idx, node_id) in topo_sort.iter().enumerate() {
            map.insert(RefEq(g.node_weight(*node_id).unwrap()), idx);
        }

        breakpoint!("inline graph", g, &|n: &Vec<FirmMethodP<'src, 'ast>>| {
            Label::from_text(format!(
                "{:?} {}",
                n.iter()
                    .map(|n| n.borrow().def.name.as_str().to_owned())
                    .collect::<Vec<_>>(),
                map[&RefEq(n)]
            ))
        });

        for node in topo_sort {
            if g.node_weight(node).unwrap().len() != 1 {
                continue;
            }

            for edge in g.edges_directed(node, petgraph::Direction::Outgoing) {
                Inline::inline(*edge.weight()).unwrap();
            }
        }

        Outcome::Unchanged
    }
}

struct Inline {
    graph: Graph,
    marked: HashMap<Node, bool>,
}

#[derive(Debug, Display)]
struct InlineError {}

impl From<std::option::NoneError> for InlineError {
    fn from(_err: std::option::NoneError) -> Self {
        InlineError {}
    }
}

impl Inline {
    pub fn inline(call: Call) -> Result<(), InlineError> {
        let graph = call.graph();
        graph.assure_outs();
        let mut i = Self::new(graph);
        i.internal_inline(call)
    }

    fn new(graph: Graph) -> Self {
        let mut marked = HashMap::new();
        for n in graph.nodes().iter() {
            marked.insert(*n, false);
        }

        Inline { graph, marked }
    }

    fn internal_inline(&mut self, call: Call) -> Result<(), InlineError> {
        let graph = self.graph;
        let proj_m = call.out_proj_m().unwrap();
        let proj_first_result = call.out_proj_t_result().and_then(|r| r.out_nodes().idx(0));
        let has_result = proj_first_result.is_some();

        log::debug!("Call to inline: {:?}, has result: {}", call, has_result);

        let address = Node::as_address(call.ptr())?;
        let entity = address.entity();
        let graph_to_inline: Graph = entity.graph()?;

        let (block, target_block) = self.split_block_at(graph, call.into());

        //self.text.insert(block.into(), "new block".into());
        self.marked.insert(target_block.into(), true);

        let mut map = HashMap::new();
        map.insert(graph_to_inline.start().block().into(), block.into());

        let returns: Vec<_> = graph_to_inline
            .end()
            .block()
            .in_nodes()
            .filter(|n| Node::is_return(*n))
            .map(|ret| {
                let updated_ret =
                    Node::as_return(self.copy(ret, &mut map, call, graph.start_block())).unwrap();
                let res = updated_ret.return_res().idx(0);
                let mem = updated_ret.mem();
                let block = updated_ret.block();
                log::debug!("res: {:?}, mem: {:?}, block: {:?}", res, mem, block);
                graph.mark_as_bad(updated_ret);

                self.marked.insert(block.into(), true);
                self.marked.insert(updated_ret.into(), true);
                (block.new_jmp(), res, mem)
            })
            .collect();

        let jmps: Vec<Node> = returns
            .iter()
            .map(|(jmp, _res, _mem)| {
                let n: Node = (*jmp).into();
                n
            })
            .collect();
        target_block.set_in_nodes(&jmps[..]);

        let mems: Vec<Node> = returns.iter().map(|(_jmp, _res, mem)| *mem).collect();
        let mode = mems[0].mode();
        let new_mem = if mems.len() == 1 {
            mems[0]
        } else {
            target_block.new_phi(&mems[..], mode).into()
        };
        Graph::exchange(proj_m, new_mem);
        if has_result {
            let ress: Vec<_> = returns
                .iter()
                .map(|(_jmp, res, _mem)| res.unwrap())
                .collect();
            let ress_mode = ress[0].mode();
            let new_res = if ress.len() == 1 {
                ress[0]
            } else {
                target_block.new_phi(&ress[..], ress_mode).into()
            };
            Graph::exchange(proj_first_result.unwrap(), new_res);
        }

        graph.mark_as_bad(call);
        graph.remove_bads();

        graph.dump("inline");

        Ok(())
    }

    fn split_block_at(&mut self, graph: Graph, node: Node) -> (Block, Block) {
        let block = node.block();

        // move cfg_preds from block to new_block
        let cfg_preds: Vec<_> = block.cfg_preds().collect();
        let new_block = self.graph.new_block(&cfg_preds[..]);
        block.set_in_nodes(&[]);

        self.move_node(node, block, new_block);
        if block == graph.start_block() {
            log::debug!("Update start block");
            graph.set_start_block(new_block);
            // no_mem must be set
            graph.no_mem().set_block(new_block);

            // for some reason, this projs are not considered in out_nodes.
            // graph.frame().set_block(new_block);

            // all address and const nodes must have the start block as predecessor.
            for node in block.out_nodes() {
                match node {
                    Node::Address(_) | Node::Const(_) => node.set_block(new_block),
                    _ => {}
                }
            }
        }
        (new_block, block)
    }

    fn move_node(&self, node: Node, from_block: Block, to_block: Block) {
        log::debug!(
            "Move node {:?} from {:?} to {:?}",
            node,
            from_block,
            to_block
        );
        if Node::is_block(node) || node.block() != from_block {
            return;
        }

        node.set_block(to_block);

        for proj in node.all_out_projs() {
            log::debug!("... considering proj {:?}", proj);
            proj.set_block(to_block);
        }

        for in_node in node.in_nodes() {
            self.move_node(in_node, from_block, to_block);
        }
    }

    fn copy(
        &mut self,
        old: Node,
        map: &mut HashMap<Node, Node>,
        call_to_inline: Call,
        start_block: Block,
    ) -> Node {
        if let Some(nd) = map.get(&old) {
            return *nd;
        }

        match old {
            Node::Proj(_proj, ProjKind::Start_TArgs_Arg(idx, _start, _)) => {
                return call_to_inline.args().idx(idx as i32).unwrap();
            }
            Node::Proj(_proj, ProjKind::Start_M(_start)) => return call_to_inline.mem(),
            _ => {}
        }

        let graph = self.graph;
        let new_node = graph.copy_node(old, |n| match old {
            Node::Address(_) if Node::is_block(n) => start_block.into(),
            Node::Const(_) if Node::is_block(n) => start_block.into(),
            _ => self.copy(n, map, call_to_inline, start_block),
        });

        map.insert(old, new_node);
        new_node
    }
}
