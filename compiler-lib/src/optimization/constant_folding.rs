use crate::firm::Program;
use libfirm_rs::{
    bindings,
    nodes::NodeTrait,
    nodes_gen::{Node, NodeDiscriminants, NodeFactory},
    other::Graph,
    tarval::{Binop, Lattice, Tarval},
};
use std::collections::{hash_map::HashMap, VecDeque};

struct ConstantFolding {
    values: HashMap<Node, Tarval>,
    list: VecDeque<Node>,
    graph: Graph,
}

pub fn run(program: &Program<'_, '_>) {
    for class in program.classes.values() {
        for method in class.borrow().methods.values() {
            if let Some(graph) = method.borrow().graph {
                println!("Graph for Method: {:?}", method.borrow().entity.name());
                let mut cf = ConstantFolding::new(graph.into());
                cf.run();
            }
        }
    }
}

impl ConstantFolding {
    fn new(graph: Graph) -> Self {
        let mut list = VecDeque::new();
        graph.walk_topological(|node| {
            list.push_back(*node);
        });

        let mut values = HashMap::new();

        for node in &list {
            values.insert(*node, Tarval::unknown());
        }

        // the first node is always the start _block_
        //   it also contains all const nodes
        // the second node is always the start node
        // let's start with the start node
        list.pop_front().expect("graph must contain start block");

        Self {
            list,
            values,
            graph,
        }
    }

    fn run(&mut self) {
        unsafe {
            bindings::assure_irg_outs(self.graph.into());
        }

        loop {
            let cur = match self.list.pop_front() {
                Some(n) => n,
                None => break,
            };

            log::debug!(
                "VISIT NODE kind={} id={:?}",
                NodeDiscriminants::from(cur),
                cur.node_id()
            );

            match cur {
                Node::Const(constant) => {
                    let tv = constant.tarval().into();
                    log::debug!("constant: {:?}", tv);
                    let prev = self.values.insert(cur, tv).unwrap();
                    debug_assert_eq!(
                        prev.mode(),
                        Tarval::unknown().mode(),
                        "only set const nodes once"
                    );
                    // no need to queue anything because of initial topological sort
                }
                Node::Add(add) => {
                    let lhs = self.values[&add.left()];
                    let rhs = self.values[&add.right()];
                    let res = Lattice::binop(lhs, rhs, Binop::Add);
                    log::debug!("result is: {:?}", res);
                    let prev = self.values.insert(cur, res).unwrap();
                    if res != prev {
                        let followers = cur.reverse_edges();
                        log::debug!(
                            "queuing edges: node-ids: {:?}",
                            followers.iter().map(|x| x.node_id()).collect::<Vec<_>>()
                        );
                        self.list.extend(followers);
                    }
                }
                Node::Phi(phi) => unsafe {
                    let npreds = bindings::get_Phi_n_preds(phi.into());
                    let preds = (0..npreds)
                        .map(|pos| bindings::get_Phi_pred(phi.into(), pos))
                        .map(|irn| NodeFactory::node(irn))
                        .map(|p| self.values[&p])
                        .collect::<Vec<_>>();
                    log::debug!("preds: {:?}", preds);
                    let res = Lattice::phi(preds.into_iter());
                    log::debug!("result is: {:?}", res);
                    let prev = self.values.insert(cur, res).unwrap();
                    if res != prev {
                        let followers = cur.reverse_edges();
                        log::debug!(
                            "queuing edges: node-ids: {:?}",
                            followers.iter().map(|x| x.node_id()).collect::<Vec<_>>()
                        );
                        self.list.extend(followers);
                    }
                },
                _ => (),
            }
        }

        // TODO annotate graph?
        let mut values = self.values.iter().collect::<Vec<_>>();
        values.sort_by_key(|(l, _)| l.node_id());
        for (n, v) in values {
            log::debug!(
                "RESULT NODE kind={} id={:?} val={:?}",
                NodeDiscriminants::from(n),
                n.node_id(),
                v
            );
        }
    }
}
