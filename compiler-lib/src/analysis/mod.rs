use super::firm::*;
use crate::ref_eq::RefEq;
use libfirm_rs::{
    nodes::{Call, Node},
    Graph,
};
use petgraph::Graph as G;
use std::{collections::HashMap, rc::Rc};

mod bounds;

pub struct CallGraph<'prog, 'src, 'ast> {
    program: &'prog FirmProgram<'src, 'ast>,
}

impl<'prog, 'src, 'ast> CallGraph<'prog, 'src, 'ast> {
    pub fn new(program: &'prog FirmProgram<'src, 'ast>) -> CallGraph<'prog, 'src, 'ast> {
        CallGraph { program }
    }

    pub fn construct(&self) -> G<FirmMethodP<'src, 'ast>, Call> {
        // Vec<Call>
        let mut map = HashMap::new(); // : HashMap<RefEq<FirmMethodP<'src, 'ast>>, _>
        let mut g = G::new();
        for method in self.program.methods.values() {
            map.insert(RefEq(Rc::clone(method)), g.add_node(Rc::clone(method)));
        }
        for method in self.program.methods.values() {
            let caller_id = map[&RefEq(Rc::clone(method))];
            if let Some(graph) = method.borrow().graph {
                for (call, firm_method) in self.usages(graph).into_iter() {
                    let callee_id = map[&RefEq(firm_method)];
                    g.add_edge(caller_id, callee_id, call);
                }
            }
        }
        g
    }

    pub fn usages(&self, graph: Graph) -> Vec<(Call, FirmMethodP<'src, 'ast>)> {
        let mut result = Vec::new();
        graph.walk_topological(|n: &Node| {
            let mut inner = || -> Option<()> {
                let call = Node::as_call(*n)?;
                let addr = Node::as_address(call.ptr())?;
                let entity = addr.entity();
                let firm_method = self.program.method_by_entity(entity)?;
                result.push((call, firm_method));
                Some(())
            };
            inner();
        });
        result
    }
}
