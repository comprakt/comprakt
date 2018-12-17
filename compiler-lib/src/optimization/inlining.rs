use super::{OptimizationResult, OptimizationResultCollector};
use crate::{debugging, firm::Program};
use libfirm_rs::{
    graph::{Graph, NodeData},
    nodes::{Node, ProjKind, Block, NodeTrait},
    nodes,
    bindings,
};
use std::{collections::hash_map::HashMap, path::PathBuf};

pub fn run(program: &Program<'_, '_>) -> OptimizationResult {
    let mut collector = OptimizationResultCollector::new();
    for class in program.classes.values() {
        for method in class.borrow().methods.values() {
            if let Some(graph) = method.borrow().graph {
                let graph: Graph = graph.into();
                log::debug!("Graph for Method: {:?}", method.borrow().entity.name());

                process(graph);
            }
        }
    }
    collector.result()
}

fn process(graph: Graph) {
    if graph.entity().name_string() != "Main.M.run" { return }
    graph.dump("inline");

    graph.assure_outs();

    graph.dump_dot_data(
        &PathBuf::from(format!(
            "./dot-out/{}.dot",
            graph.entity().name_string()
        )),
        |n| {
            let mut nd = NodeData::new(format!(
                "{:?}",
                n,
            ));
            nd
        },
    );

    debugging::wait();

    for n in graph.nodes().iter() {
        let mut f = Foo::new(graph);
        f.handle_node(*n);
    }
}

struct Foo {
    marked: HashMap<Node, bool>,
    graph: Graph,
    text: HashMap<Node, String>,
}

impl Foo {
    pub fn new(graph: Graph) -> Foo {
        let mut marked = HashMap::new();
        for n in graph.nodes().iter() {
            marked.insert(*n, false);
        }
        Foo { marked, graph, text: HashMap::new() }
    }

    fn move_proj_recursive(&self, node: Node, from_block: Block, to_block: Block) {
        if Node::is_block(node) { return; }
        log::debug!("set out_node {:?}", node);
        if node.block() != from_block { return; }
        node.set_block(to_block);
        for n in node.out_nodes() {
            if let Node::Proj(proj, _) = n {
                self.move_proj_recursive(proj.into(), from_block, to_block);
            }
        }
    }

    fn move_recursive(&self, node: Node, from_block: Block, to_block: Block) {
        if Node::is_block(node) { return; }
        if node.block() != from_block { return; }
        if Node::is_phi(node) { return; }

        self.move_proj_recursive(node, from_block, to_block);

        for in_node in node.in_nodes() {
            self.move_recursive(in_node, from_block, to_block);
        }
    }

    fn split_block_at(&mut self, graph: Graph, node: Node) -> (Block, Block) {
        let block = node.block();
        let new_block = self.graph.new_block(block.cfg_preds().collect());
        block.set_in_nodes(&[]);
        self.move_recursive(node, block, new_block);
        if block == graph.start_block() {
            log::debug!("Update start block");
            graph.set_start_block(new_block);
            // for some reason, these projs are not considered in out_nodes.
            graph.no_mem().set_block(new_block);
            graph.frame().set_block(new_block);

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

    fn copy(&mut self, old: Node, map: &mut HashMap<Node, Node>, call_to_inline: nodes::Call, start_block: Block) -> Node {
        if let Some(nd) = map.get(&old) {
            return *nd;
        }

        match old {
            Node::Proj(proj, ProjKind::Start_TArgs_Arg(idx, start, _)) => {
                return call_to_inline.args().idx(idx as i32).unwrap();
            },
            Node::Proj(proj, ProjKind::Start_M(start)) => return call_to_inline.mem(),
            _ => {},
        }

        let graph = self.graph;
        let new_node = graph.copy_node(
            old,
            |n| {
                match old {
                    Node::Address(_) if Node::is_block(n) => start_block.into(),
                    Node::Const(_) if Node::is_block(n) => start_block.into(),
                    _ => self.copy(n, map, call_to_inline, start_block),
                }
            },
        );

        map.insert(old, new_node);

        self.text.insert(new_node, "new".to_string());
        new_node
    }

    fn handle_node(&mut self, n: Node) -> Option<()> {
        let graph = self.graph;
        let call = Node::as_call(n)?;
        let proj_m = call.out_proj_m().unwrap();
        let proj_result = call.out_proj_t_result();
        let proj_first_result = proj_result.and_then(|r| r.out_nodes().idx(0));
        let has_result = proj_first_result.is_some();

        log::debug!("{:?} has result: {}", call, has_result);

        let address = Node::as_address(call.ptr())?;
        let entity = address.entity();
        let graph_to_inline: Graph = entity.graph()?;


        let (block, target_block) = self.split_block_at(graph, call.into());

        //target_block.set_in_nodes(&[block.new_jmp().into()]);
        //self.dump_graph(Some(block.into()), Some(&self.text));

        self.text.insert(block.into(), "new block".into());
        self.marked.insert(target_block.into(), true);

        let mut map = HashMap::new();
        map.insert(graph_to_inline.start().block().into(), block.into());

        let returns: Vec<_> = graph_to_inline
            .end()
            .block()
            .in_nodes()
            .filter(|n| Node::is_return(*n))
            .map(|ret| {
                let updated_ret = Node::as_return(self.copy(ret, &mut map, call, graph.start_block())).unwrap();
                let res = updated_ret.return_res().idx(0);
                let mem = updated_ret.mem();
                let block = updated_ret.block();
                log::debug!("res: {:?}, mem: {:?}, block: {:?}", res, mem, block);
                graph.mark_as_bad(&updated_ret);

                self.marked.insert(block.into(), true);
                self.marked.insert(updated_ret.into(), true);
                (block.new_jmp(), res, mem)
            })
            .collect();

        let jmps: Vec<Node> = returns.iter().map(|(jmp, res, mem)| { let n: Node = (*jmp).into(); n }).collect();
        target_block.set_in_nodes(&jmps[..]);

        let mems: Vec<Node> = returns.iter().map(|(jmp, res, mem)| *mem).collect();
        let mode = mems[0].mode();
        let new_mem = if mems.len() == 1 { mems[0] } else { target_block.new_phi(mems, mode).into() };
        Graph::exchange(&proj_m, &new_mem);
        if has_result {
            let ress: Vec<_> = returns.iter().map(|(jmp, res, mem)| res.unwrap()).collect();
            let ress_mode = ress[0].mode();
            let new_res = if ress.len() == 1 { ress[0] } else { target_block.new_phi(ress, ress_mode).into() };
            Graph::exchange(&proj_first_result.unwrap(), &new_res);
        }

        graph.mark_as_bad(&call);
        graph.remove_bads();

        self.dump_graph(Some(block.into()), Some(&self.text));

        Some(())
    }

    fn dump_graph(&self, cur_node: Option<Node>, text: Option<&HashMap<Node, String>>) {
        let graph = self.graph;

        graph.dump("inline");

        log::debug!("dump_graph");
        graph.dump_dot_data(
            &PathBuf::from(format!(
                "./dot-out/{}.dot",
                graph.entity().name_string()
            )),
            |n| {
                let more_text = if let Some(hm) = text {
                    if let Some(s) = hm.get(&n) {
                        format!("\n{}", s)
                    } else {
                        "".to_string()
                    }
                } else {
                    "".to_string()
                };

                let mut nd = NodeData::new(format!(
                    "{:?}{}",
                    n,
                    more_text,
                ));

                nd.filled(Some(n) == cur_node);
                nd.bold(*self.marked.get(&n).unwrap_or(&false));
                nd
            },
        );

        debugging::wait();
    }
}

