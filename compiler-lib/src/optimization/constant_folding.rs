use crate::firm::Program;
use libfirm_rs::{
    bindings,
    nodes::NodeTrait,
    nodes_gen::{Node, NodeDiscriminants, NodeFactory},
    other::Graph,
    tarval::{mode_name, Tarval},
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

    fn queue_followers_if_changed(&mut self, node: Node, prev: Tarval, new: Tarval) {
        if prev != new {
            let followers = node.reverse_edges();
            log::debug!(
                "queuing edges: node-ids: {:?}",
                followers.iter().map(|x| x.node_id()).collect::<Vec<_>>()
            );
            self.list.extend(followers);
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

            macro_rules! tarval_binop {
                ($thing: ident, $op: ident) => {{
                    let lhs = self.values[&$thing.left()];
                    let rhs = self.values[&$thing.right()];
                    let res = Lattice::binop(lhs, rhs, Binop::$op);
                    log::debug!("result is: {:?}", res);
                    let prev = self.values.insert(cur, res).unwrap();
                    self.queue_followers_if_changed(cur, prev, res);
                }};
            }

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
                    tarval_binop!(add, Add);
                }
                Node::Sub(sub) => {
                    tarval_binop!(sub, Sub);
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
                    self.queue_followers_if_changed(cur, prev, res);
                },
                _ => (),
            }
        }

        // sort to have reproducible replacement order
        let mut values = self.values.iter().collect::<Vec<_>>();
        values.sort_by_key(|(l, _)| l.node_id());

        // now apply the values
        for (n, v) in values {
            if v.is_constant() {
                log::debug!(
                    "EXCHANGE NODE kind={} id{:?} val={:?}",
                    NodeDiscriminants::from(n),
                    n.node_id(),
                    v
                );
                let const_node = Node::Const(self.graph.new_const((*v).into()));
                Graph::exchange(*n, const_node);
            }
        }

        // TODO move this to a general feature of the optimization CLI
        unsafe {
            use std::ffi::CString;
            self::bindings::dump_all_ir_graphs(
                CString::new("postconstantfolding").unwrap().as_ptr(),
            );
        }
    }
}

#[derive(Clone, Copy, From, Into)]
pub struct Lattice(Tarval);

// TODO impl TryFrom<Node>
pub enum Binop {
    Add,
    Sub,
    Phi,
}

impl Lattice {
    pub fn binop(lhs: Tarval, rhs: Tarval, op: Binop) -> Tarval {
        let unknown = Tarval::unknown().mode();
        let bad = Tarval::bad().mode();
        if lhs.mode() == unknown || rhs.mode() == unknown {
            return Tarval::unknown();
        }
        if lhs.mode() == bad || rhs.mode() == bad {
            return Tarval::bad();
        }
        if lhs.mode() != rhs.mode() {
            panic!(
                "lhs mode {:?} != {:?} rhs mode",
                mode_name(lhs.mode()),
                mode_name(rhs.mode())
            )
        }
        match op {
            Binop::Add => lhs + rhs,
            Binop::Sub => lhs - rhs,
            Binop::Phi => {
                if lhs == rhs {
                    lhs
                } else {
                    Tarval::bad()
                }
            }
        }
    }
    pub fn phi<I: Iterator<Item = Tarval>>(mut vals: I) -> Tarval {
        let first = vals
            .next()
            .expect("phi must have at least one predecessor (two in fact, but we only need one)");
        vals.fold(first, |r, v| Lattice::binop(r, v, Binop::Phi))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    macro_rules! assert_eq_modes {
        ($l:expr, $r:expr) => {{
            let lhs = $l;
            let rhs = $r;
            debug_assert_eq!(
                lhs.mode(),
                rhs.mode(),
                "modes do not match: {:?} != {:?}",
                mode_name(lhs.mode()),
                mode_name(rhs.mode())
            );
        }};
    }

    #[test]
    fn lattice_binop_modes_like_lecture_slides() {
        libfirm_rs::init();
        let uk = Tarval::unknown();
        let bad = Tarval::bad();
        let i = Tarval::mj_int(23);
        let j = Tarval::mj_int(42);
        let x = Lattice::binop(j, i, Binop::Add);

        assert_eq_modes!(x, i);
        assert_eq_modes!(x, j);

        let x = Lattice::binop(uk, uk, Binop::Add);
        assert_eq_modes!(x, uk);

        let x = Lattice::binop(bad, bad, Binop::Add);
        assert_eq_modes!(x, bad);

        // mixed
        let x = Lattice::binop(i, bad, Binop::Add);
        assert_eq_modes!(x, bad);

        let x = Lattice::binop(i, uk, Binop::Add);
        assert_eq_modes!(x, uk);
    }

}
