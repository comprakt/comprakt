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

        #[allow(clippy::single_match)]
        match current_node {
            Node::Add(add) => {
                let left = add.left();
                let right = add.right();

                // checking for equality of const nodes is unnecessary since const additions
                // were already folded
                if left == right {
                    // convert (a + a) to (a << 2)
                    log::debug!(
                        "Div2Shift: self addition {:?} [left:{:?},right:{:?}] \
                         replaced by '{:?} << 2'",
                        add,
                        left,
                        right,
                        left
                    );

                    let tarval_1 = self.graph.new_const(Tarval::val(1, Mode::Iu()));

                    let shl = add.block().new_shl(left, tarval_1);

                    Graph::exchange(add, shl);
                }
            }
            Node::Mul(mul) => {
                let left = mul.left();
                let right = mul.right();

                let (other, power) = {
                    if let Node::Const(op1) = left {
                        // power of two * a
                        if let TarvalKind::Long(val) = op1.tarval().kind() {
                            (right, val)
                        } else {
                            return;
                        }
                    } else if let Node::Const(op2) = right {
                        // a * power of two
                        if let TarvalKind::Long(val) = op2.tarval().kind() {
                            (left, val)
                        } else {
                            return;
                        }
                    } else {
                        return;
                    }
                };

                let abs_power = power.abs() as u64;
                let has_minus = power < 0;

                if abs_power.is_power_of_two() {
                    let shift_amount = 64 - 1 - abs_power.leading_zeros();

                    let shift_amount_node = self
                        .graph
                        .new_const(Tarval::val(i64::from(shift_amount), Mode::Iu()));

                    let shl = mul.block().new_shl(other, shift_amount_node);
                    let shl_end = if has_minus {
                        Node::Minus(mul.block().new_minus(shl))
                    } else {
                        Node::Shl(shl)
                    };

                    log::debug!(
                        "Div2Shift: {:?} [left:{:?},right:{:?}] replaced by '{:?} << {}'",
                        mul,
                        left,
                        right,
                        shl,
                        shift_amount
                    );

                    Graph::exchange(mul, shl_end);
                }
            }
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

                            // cannot crash since min_val is not a power of two
                            let abs_divisor_value = divisor_value.abs() as u64;
                            let has_minus = divisor_value < 0;

                            if abs_divisor_value.is_power_of_two() {
                                log::debug!("Div2Shift: is power of two!");
                                let shift_amount = 64 - 1 - abs_divisor_value.leading_zeros();
                                let shift_amount_node = self
                                    .graph
                                    .new_const(Tarval::val(i64::from(shift_amount), Mode::Iu()));

                                let const_31 = self.graph.new_const(Tarval::val(31, Mode::Iu()));
                                let const_1 = self.graph.new_const(Tarval::mj_int(1));

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

                                let block = div.block();
                                let shr_by_31 = block.new_shrs(real_left, const_31);
                                let shl_1_by_shift = block.new_shl(const_1, shift_amount_node);
                                let shl_1_by_shift_minus_1 = block.new_sub(shl_1_by_shift, const_1);
                                let binary_and = block.new_and(shr_by_31, shl_1_by_shift_minus_1);
                                let add_binary_and = block.new_add(real_left, binary_and);
                                let shift_to_result =
                                    block.new_shrs(add_binary_and, shift_amount_node);

                                let shr_end = if has_minus {
                                    Node::Minus(div.block().new_minus(shift_to_result))
                                } else {
                                    Node::Shrs(shift_to_result)
                                };

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

                                Graph::exchange(div_end, shr_end);

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
