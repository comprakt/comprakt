use crate::firm::Program;
use libfirm_rs::{
    bindings,
    graph::Graph,
    nodes::NodeTrait,
    nodes_gen::{Node, NodeDiscriminants, ProjKind},
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
                log::debug!("Graph for Method: {:?}", method.borrow().entity.name());
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
            log::debug!(
                "queuing edges: node-ids: {:?}",
                node.out_nodes().map(|x| x.node_id()).collect::<Vec<_>>()
            );
            self.list.extend(node.out_nodes());
        }
    }

    fn run(&mut self) {
        self.graph.assure_outs();

        while let Some(cur) = self.list.pop_front() {
            log::debug!(
                "VISIT NODE kind={} id={:?}",
                NodeDiscriminants::from(cur),
                cur.node_id()
            );

            macro_rules! tarval_binop {
                ($thing: ident, $op: expr) => {{
                    use self::Binop::*;
                    let lhs = self.values[&$thing.left()];
                    let rhs = self.values[&$thing.right()];
                    let res = Lattice::binop(lhs, rhs, $op);
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
                    // FIXME queue nethertheless to prevent accidental bugs.
                }
                Node::Add(add) => {
                    tarval_binop!(add, Add);
                }
                Node::Sub(sub) => {
                    tarval_binop!(sub, Sub);
                }
                Node::Mul(mul) => {
                    tarval_binop!(mul, Mul);
                }
                Node::Div(div) => {
                    tarval_binop!(div, Div);
                }
                Node::Mod(modulo) => {
                    tarval_binop!(modulo, Mod);
                }
                Node::Eor(eor) => {
                    tarval_binop!(eor, Xor);
                }
                Node::Minus(minus) => {
                    let inner = self.values[&minus.op()];
                    let res = Lattice::unary_op(inner, UnaryOp::Minus);
                    log::debug!("result is: {:?}", res);
                    let prev = self.values.insert(cur, res).unwrap();
                    self.queue_followers_if_changed(cur, prev, res);
                }
                Node::Phi(phi) => {
                    let preds = phi.phi_preds().map(|p| self.values[&p]).collect::<Vec<_>>();
                    log::debug!("preds: {:?}", preds);
                    let res = Lattice::phi(preds.into_iter());
                    log::debug!("result is: {:?}", res);
                    let prev = self.values.insert(cur, res).unwrap();
                    self.queue_followers_if_changed(cur, prev, res);
                }

                Node::Cmp(cmp) => tarval_binop!(cmp, Rel(cmp.relation())),

                _ => (),
            }
        }

        // sort to have reproducible replacement order
        let mut values = self.values.iter().collect::<Vec<_>>();
        values.sort_by_key(|(l, _)| l.node_id());

        // now apply the values
        for (node, v) in values {
            if v.is_constant() {
                log::debug!(
                    "EXCHANGE NODE kind={} id{:?} val={:?}",
                    NodeDiscriminants::from(node),
                    node.node_id(),
                    v
                );
                let const_node = Node::Const(self.graph.new_const((*v).into()));

                match node {
                    /* IMPROVEMENT?
                    This might be more elegant, but does not do the exact same:
                    It fails if there are multiple projects to that pin!
                    Node::Div(div) => {
                        if Some(res) = div.out_proj_res() {
                            Graph::exchange(res, const_node);
                        }
                        if Some(mem) = div.out_proj_m {
                            Graph::exchange(mem, div.mem());
                        }
                    }
                    Node::Mod(modulo) => {
                        if Some(res) = modulo.out_proj_res() {
                            Graph::exchange(res, const_node);
                        }
                        if Some(m) = modulo.out_proj_m() {
                            Graph::exchange(m, modulo.mem());
                        }
                    }
                    */
                    Node::Div(div) => {
                        for out_node in node.out_nodes() {
                            match out_node {
                                Node::Proj(res_proj, ProjKind::Div_Res(_)) => {
                                    Graph::exchange(res_proj, const_node);
                                }
                                Node::Proj(m_proj, ProjKind::Div_M(_)) => {
                                    Graph::exchange(m_proj, div.mem());
                                }
                                _ => {}
                            }
                        }
                    }
                    Node::Mod(modulo) => {
                        for out_node in node.out_nodes() {
                            match out_node {
                                Node::Proj(res_proj, ProjKind::Mod_Res(_)) => {
                                    Graph::exchange(res_proj, const_node);
                                }
                                Node::Proj(m_proj, ProjKind::Mod_M(_)) => {
                                    Graph::exchange(m_proj, modulo.mem());
                                }
                                _ => {}
                            }
                        }
                    }
                    _ => {
                        Graph::exchange(*node, const_node);
                    }
                }
            }
        }
    }
}

#[derive(Clone, Copy, From, Into)]
pub struct Lattice(Tarval);

// TODO impl TryFrom<Node>
#[derive(Clone, Copy, Debug)]
pub enum Binop {
    Add,
    Sub,
    Mul,
    Div,
    Xor,
    Mod,
    Phi,
    Rel(bindings::ir_relation::Type),
}

#[derive(Clone, Copy, Debug)]
pub enum UnaryOp {
    Minus,
}

impl Lattice {
    pub fn unary_op(val: Tarval, op: UnaryOp) -> Tarval {
        let unknown = Tarval::unknown().mode();
        let bad = Tarval::bad().mode();

        // TODO: these checks might be unnecessary
        if val.mode() == unknown {
            return Tarval::unknown();
        }

        if val.mode() == bad {
            return Tarval::bad();
        }

        match op {
            UnaryOp::Minus => -val,
        }
    }

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
            Binop::Mul => lhs * rhs,
            Binop::Div => lhs / rhs,
            Binop::Mod => lhs % rhs,
            Binop::Xor => lhs ^ rhs,
            Binop::Phi => {
                if lhs == rhs {
                    lhs
                } else {
                    Tarval::bad()
                }
            }

            Binop::Rel(rel) => lhs.cmp(rel, rhs),
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
