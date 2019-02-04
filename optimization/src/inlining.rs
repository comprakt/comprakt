use crate::{
    analysis::CallGraph,
    firm::{program_generator::Spans, FirmMethodP, FirmProgram},
    optimization::{self, Outcome},
    ref_eq::RefEq,
};
use debugging::dot::Label;
use libfirm_rs::{bindings, nodes::*, types::ClassTy, Graph};
use petgraph::algo::{condensation, toposort};
use std::collections::{HashMap, HashSet};

pub struct Inlining;

impl optimization::Interprocedural for Inlining {
    fn optimize<'src, 'ast>(program: &mut FirmProgram<'src, 'ast>) -> Outcome {
        let g = CallGraph::new(program).construct();
        breakpoint!("before inline: graph", g, &|n: &FirmMethodP<'src, 'ast>| {
            Label::from_text(format!("{:?}", n.borrow().entity.name_string()))
        });

        let mut g = condensation(g, false);

        g.reverse();

        let mut is_recursive = HashSet::new();

        // remove all loops after condensation
        g.retain_edges(|g, e| {
            let (n1, n2) = g.edge_endpoints(e).unwrap();
            if n1 == n2 {
                is_recursive.insert(n1);
            }
            n1 != n2
        });

        let topo_sort = toposort(&g, None).unwrap();
        let mut map = HashMap::new();
        for (idx, node_id) in topo_sort.iter().enumerate() {
            map.insert(RefEq(g.node_weight(*node_id).unwrap()), idx);
        }

        // FIXME
        // breakpoint!("inline graph", g, &|n: &Vec<FirmMethodP<'src, 'ast>>| {
        //     Label::from_text(format!(
        //         "{:?} {}",
        //         n.iter()
        //             .map(|n| n.borrow().def.name.as_str().to_owned())
        //             .collect::<Vec<_>>(),
        //         map[&RefEq(n)]
        //     ))
        // });

        for node in topo_sort {
            if is_recursive.contains(&node) {
                // TODO: skip this continue to provoke another bug.
                // We should investigate this.
                continue;
            }

            if g.node_weight(node).unwrap().len() != 1 {
                continue;
            }

            let method = g.node_weight(node).unwrap().first().unwrap();

            for edge in g.edges_directed(node, petgraph::Direction::Outgoing) {
                Inline::inline(*edge.weight()).unwrap();
            }

            if !is_recursive.contains(&node) {
                let method_entity = method.borrow().entity;
                let class_ty = ClassTy::from(method_entity.owner()).unwrap();
                class_ty.remove_member(method_entity);
                if !method.borrow().def.is_main {
                    program.methods.remove(&RefEq(method.borrow().def.clone()));
                    if let Some(method_graph) = method.borrow().graph {
                        unsafe { bindings::free_ir_graph(method_graph.into()) }
                    }
                }
            }
        }

        breakpoint!(
            "after inline: graph",
            CallGraph::new(program).construct(),
            &|n: &FirmMethodP<'src, 'ast>| Label::from_text(format!(
                "{:?}",
                n.borrow().entity.name_string()
            ))
        );

        log::debug!("Inline finished");

        Outcome::Unchanged
    }
}

#[derive(Debug, Display)]
struct InlineError {}

impl From<std::option::NoneError> for InlineError {
    fn from(_err: std::option::NoneError) -> Self {
        InlineError {}
    }
}

struct Inline {
    graph: Graph,
    depth: usize,
}

struct MoveResult {
    child_of_split_node: bool,
}

impl Inline {
    pub fn inline(call: Call) -> Result<(), InlineError> {
        let graph = call.graph();
        graph.assure_outs();

        let mut i = Inline { graph, depth: 0 };
        i.inline_with_context(call)
    }

    fn inline_with_context(&mut self, call: Call) -> Result<(), InlineError> {
        let graph = self.graph;
        let proj_m = call.out_proj_m().unwrap();
        let call_result = call.out_proj_t_result().and_then(|r| r.out_nodes().idx(0));

        log::debug!("Call to inline: {:?}, result: {:?}", call, call_result);

        let address = Node::as_address(call.ptr())?;
        let entity = address.entity();
        let graph_to_inline: Graph = entity.graph()?;

        let (block, target_block) = self.split_block_at(graph, call.into());
        log::debug!("Block splitted into {:?} and {:?}", block, target_block);

        let mut map = HashMap::new();
        map.insert(graph_to_inline.start().block().into(), block.into());
        struct Return {
            jmp: Jmp,
            res: Option<Node>,
            mem: Node,
        };
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
                let ret_block = updated_ret.block();
                log::debug!("Return {:?} with mem {:?} from {:?}", res, mem, ret_block);

                log::debug!("Delete copied return {:?}", updated_ret);
                graph.mark_as_bad(updated_ret);

                Return {
                    jmp: ret_block.new_jmp(),
                    res,
                    mem,
                }
            })
            .collect();

        for kept_alive_node in graph_to_inline.end_keep_alives() {
            let new_node = self.copy(kept_alive_node, &mut map, call, graph.start_block());
            new_node.keep_alive();
        }

        let jmps: Vec<Node> = returns.iter().map(|r| Node::from(r.jmp)).collect();
        log::debug!("Set jmp targets of {:?} to {:?}", jmps, target_block);
        target_block.set_in_nodes(&jmps);

        let mems: Vec<Node> = returns.iter().map(|r| r.mem).collect();
        let new_mem = target_block.phi_or_node(&mems);
        log::debug!("Exchange call mem {:?} with mem {:?}", proj_m, new_mem);
        Graph::exchange(proj_m, new_mem);

        if let Some(call_result) = call_result {
            let ress: Vec<_> = returns.iter().map(|r| r.res.unwrap()).collect();
            let new_res = target_block.phi_or_node(&ress);
            log::debug!("Exchange call res {:?} with res {:?}", call_result, new_res);
            Graph::exchange(call_result, new_res);
        }

        log::debug!("Delete {:?}", call);
        graph.mark_as_bad(call);

        graph.remove_bads();

        Ok(())
    }

    fn split_block_at(&mut self, graph: Graph, split_node: Node) -> (Block, Block) {
        let block = split_node.block();
        let phis = block.phis();

        // move cfg_preds from block to new_block
        let cfg_preds: Vec<_> = block.cfg_preds().collect();
        let new_block = self.graph.new_block(&cfg_preds[..]);
        block.set_in_nodes(&[]);

        self.move_node(split_node, block, new_block, split_node);
        for phi in phis {
            self.move_node(phi.into(), block, new_block, split_node);
        }

        if block == graph.start_block() {
            log::debug!("Update start block");
            graph.set_start_block(new_block);
            // no_mem must be set
            graph.no_mem().set_block(new_block);

            // these projs must be set manually
            graph.frame().set_block(new_block);
            graph.args().set_block(new_block);

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

    fn move_node(
        &self,
        node: Node,
        from_block: Block,
        to_block: Block,
        split_node: Node,
    ) -> MoveResult {
        if Node::is_block(node) || node.block() != from_block {
            return MoveResult {
                child_of_split_node: false,
            };
        }

        // check wether we or our users use `split_node`
        let mut child_of_split_node = false;
        for in_node in node.in_nodes() {
            if in_node == split_node {
                child_of_split_node = true;
            } else {
                let move_result = self.move_node(in_node, from_block, to_block, split_node);
                child_of_split_node |= move_result.child_of_split_node;
            }
        }

        if child_of_split_node && !Node::is_phi(node) {
            // phi nodes are allowed to use `split_node`
            log::debug!("Don't move node {:?} because its child of split_node", node,);
            MoveResult {
                child_of_split_node: true,
            }
        } else {
            log::debug!(
                "Move node {:?} from {:?} to {:?}",
                node,
                from_block,
                to_block
            );
            for proj in node.all_out_projs() {
                log::debug!("... considering {:?}", proj);
                proj.set_block(to_block);
            }
            node.set_block(to_block);
            MoveResult {
                child_of_split_node: false,
            }
        }
    }

    fn depth_str(&self) -> String {
        "| ".repeat(self.depth)
    }

    fn copy(
        &mut self,
        old: Node,
        map: &mut HashMap<Node, Node>,
        call_to_inline: Call,
        start_block: Block,
    ) -> Node {
        match old {
            Node::Proj(_proj, ProjKind::Start_TArgs_Arg(idx, _start, _)) => {
                return call_to_inline.args().idx(idx as i32).unwrap();
            }
            Node::Proj(_proj, ProjKind::Start_M(_start)) => return call_to_inline.mem(),
            _ => {}
        }

        let target_block = match old {
            Node::Block(_) => None,
            Node::Address(_) | Node::Const(_) => Some(start_block),
            node => Some(
                Node::as_block(self.copy(node.block().into(), map, call_to_inline, start_block))
                    .unwrap(),
            ),
        };

        if let Some(nd) = map.get(&old) {
            log::debug!(
                "{} Reused already copied {} for {:?}",
                self.depth_str(),
                nd.node_id(),
                old
            );
            return *nd;
        }

        let new_node = self.graph.copy_node_without_ins(old, target_block);
        if let Some(span) = Spans::lookup_span(old) {
            Spans::add_span(new_node, span);
        }

        log::debug!(
            "{} Copied {:?} to {}",
            self.depth_str(),
            old,
            new_node.node_id()
        );
        self.depth += 1;
        let existing = map.insert(old, new_node);
        assert!(existing.is_none());

        let new_in_nodes: Vec<_> = old
            .in_nodes()
            .map(|n| self.copy(n, map, call_to_inline, start_block))
            .collect();

        new_node.set_in_nodes(&new_in_nodes);
        // new_in_nodes might not be finished yet
        self.depth -= 1;
        log::debug!(
            "{} Completed copying from {:?} to {}",
            self.depth_str(),
            old,
            new_node.node_id()
        );

        new_node
    }
}
