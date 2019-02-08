//! Some simple node local optimizations, e.g.
//! - algebraic simplifications,
//! - div of power of two to shift
use super::Outcome;
use crate::optimization;
use libfirm_rs::{
    nodes::{Node, NodeTrait},
    Graph, Mode, Tarval, TarvalKind,
};

pub struct NodeLocal {
    graph: Graph,
    changed: Outcome,
}

impl optimization::Local for NodeLocal {
    fn optimize_function(graph: Graph) -> Outcome {
        NodeLocal::new(graph).run()
    }
}

impl NodeLocal {
    fn new(graph: Graph) -> Self {
        Self {
            graph,
            changed: Outcome::Unchanged,
        }
    }

    fn visit_node(&mut self, current_node: Node) {
        // do not put a breakpoint here, this will result in a walker in walker
        // which just does random shit in libfirm.
        log::debug!("Div2Shift: visiting {:?}", current_node);

        match current_node {
            Node::Div(div) => {
                log::debug!(
                    "Div2Shift: {:?}[left:{:?},right:{:?}] ",
                    div,
                    div.left(),
                    div.right(),
                );
                if let Node::Conv(conv) = div.right() {
                    log::debug!("Div2Shift: conv {:?}!", conv);
                    if let Node::Const(divisor) = conv.op() {
                        log::debug!("Div2Shift: divisor {:?}!", divisor);
                        if let TarvalKind::Long(divisor_value) = divisor.tarval().kind() {
                            log::debug!("Div2Shift: divisor value {:?}!", divisor_value);
                            if is_power_of_two(divisor_value) {
                                log::debug!("Div2Shift: is power of two!");
                                let shift_amount = (divisor_value as f64).log2() as i64;
                                let shift_amount_node =
                                    self.graph.new_const(Tarval::val(shift_amount, Mode::Iu()));

                                let div_proj_res = if let Some(res) = div.out_proj_res() {
                                    res
                                } else {
                                    return;
                                };

                                if div_proj_res.out_nodes().len() != 1 {
                                    return;
                                }

                                // this is the conv hanging at the end of the div
                                let div_end = div_proj_res.out_nodes().nth(0).unwrap();

                                // reach through the conv node in front of the div
                                if div.left().in_nodes().len() != 1 {
                                    return;
                                }
                                let real_left = div.left().in_nodes().nth(0).unwrap();

                                let shr = div.block().new_shr(real_left, shift_amount_node);

                                log::debug!(
                                    "Div2Shift: memory edge through div {:?} is {:?} -> {:?}",
                                    div,
                                    div.mem(),
                                    div.out_proj_m(),
                                );

                                let div_proj_mem = if let Some(mem) = div.out_proj_m() {
                                    mem
                                } else {
                                    return;
                                };

                                // drop the mem edge from the div
                                Graph::exchange(div_proj_mem, div.mem());

                                log::debug!(
                                    "Div2Shift: {:?}[left:{:?},right:{:?}] replaced by '>> {}'",
                                    div,
                                    div.left(),
                                    div.right(),
                                    shift_amount
                                );

                                Graph::exchange(div_end, shr);

                                self.changed = Outcome::Changed;
                            }
                        }
                    }
                }
            }
            _ => {}
        }
    }

    fn run(&mut self) -> Outcome {
        self.changed = Outcome::Unchanged;

        self.graph.assure_outs();

        self.graph.walk_topological(|node| {
            // this is run for each node "after all predecessors are visited"
            self.visit_node(*node);
        });

        if self.changed == Outcome::Changed {
            self.graph.remove_unreachable_code();
            self.graph.remove_bads();
        }

        self.changed
    }
}

fn is_power_of_two(x: i64) -> bool {
    // TODO: signed x!!!
    // popcount
    x.count_ones() == 1
    //return (x & (x - 1)) == 0;
}
