use crate::firm::Program;
use libfirm_rs::{
    bindings,
    graph::Graph,
    nodes::NodeTrait,
    nodes_gen::{Node, ProjKind},
    tarval::Tarval,
};

struct UnreachableCodeElimination {
    graph: Graph,
}

pub fn run(program: &Program<'_, '_>) {
    for class in program.classes.values() {
        for method in class.borrow().methods.values() {
            if let Some(graph) = method.borrow().graph {
                log::debug!("Graph for Method: {:?}", method.borrow().entity.name());
                UnreachableCodeElimination::new(graph.into()).run();
            }
        }
    }
}

impl UnreachableCodeElimination {
    fn new(graph: Graph) -> Self {
        Self { graph }
    }

    fn run(&mut self) {
        unsafe {
            bindings::assure_irg_outs(self.graph.into());
        }

        let mut replacements = Vec::new();
        self.graph.walk_topological(|node| {
            if let Node::Cond(cond) = node {
                if let Node::Const(c) = cond.selector() {
                    let tarval = Tarval::from(c.tarval());

                    let used_proj = node
                        .out_nodes()
                        .filter_map(|out_node| match out_node {
                            Node::Proj(proj, ProjKind::Cond_True(_)) if tarval.is_bool_true() => {
                                Some(proj)
                            }
                            Node::Proj(proj, ProjKind::Cond_False(_)) if !tarval.is_bool_true() => {
                                Some(proj)
                            }
                            _ => None,
                        })
                        .next()
                        .expect("Cond node must have Cond_{True,False} projection");

                    let unused_proj = node
                        .out_nodes()
                        .filter_map(|out_node| match out_node {
                            Node::Proj(proj, ProjKind::Cond_True(_)) if !tarval.is_bool_true() => {
                                Some(proj)
                            }
                            Node::Proj(proj, ProjKind::Cond_False(_)) if tarval.is_bool_true() => {
                                Some(proj)
                            }
                            _ => None,
                        })
                        .next()
                        .expect("Cond node must have Cond_{True,False} projection");

                    let target_block = used_proj
                        .out_nodes()
                        .next()
                        .expect("Proj node must have successor");
                    let target_block = if let Node::Block(block) = target_block {
                        block
                    } else {
                        unreachable!("Target of a Proj must be a Block")
                    };

                    let jmp = cond.block().new_jmp();
                    replacements.push((used_proj, jmp, unused_proj, target_block));
                    log::debug!(
                        "Replace Cond(id={}) with Jmp(id={}) to Block(id={})",
                        node.node_id(),
                        jmp.node_id(),
                        target_block.node_id()
                    );
                }
            }
        });

        for (used_proj, jmp, unused_proj, target_block) in replacements {
            // Now we replace the always-taken path with an unconditional jump ...
            Graph::exchange(used_proj, jmp);

            // ... and mark the never-taken as "bad", so it will be later removed
            self.graph.mark_as_bad(unused_proj);

            // We need this because if we have a while(true) loop, the code will be
            // unreachable (libfirm-edge wise) from the end block (because the end block is
            // never reached control-flow wise), but libfirm needs to find the
            // loop (and it starts searching from the end block)
            target_block.keep_alive();
        }

        // Now we actually remove the unreachable code
        self.graph.remove_bads();
    }
}
