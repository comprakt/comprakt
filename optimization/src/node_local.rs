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
        log::debug!("LO: visiting {:?}", current_node);

        match current_node {
            Node::Add(add) => {
                let left = add.left();
                let right = add.right();

                // checking for equality of const nodes is unnecessary since const additions
                // were already folded
                if left == right {
                    // convert (a + a) to (a << 2)
                    log::debug!(
                        "LO: self addition {:?} [left:{:?},right:{:?}] \
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
                        "LO: Mul2Shift: {:?} [left:{:?},right:{:?}] replaced by '{:?} << {}'",
                        mul,
                        left,
                        right,
                        shl,
                        shift_amount
                    );

                    Graph::exchange(mul, shl_end);
                }
            }
            Node::Mod(modulo) => {
                // TODO: deduplicate with Div
                // this substitutes a % b to a - (a/b) * b, which is how modulo is defined in
                // the java standard the division (a/b) is replaced by ((a + ((a
                // >> 31) & ((1 << b) - 1))) >> shift)
                log::debug!(
                    "LO: Mod2Shift: {:?}[left:{:?},right:{:?}] ",
                    modulo,
                    modulo.left(),
                    modulo.right(),
                );

                // we expect either
                // - a Conv with a Const Is as operand [which is the output of firm
                //   construction]
                // - or a constant with Const Ls [which is the output of constant folding]
                let divisor = match modulo.right() {
                    Node::Conv(conv) => {
                        log::debug!("Mod2Shift: with conv {:?}!", conv);
                        if let Node::Const(divisor) = conv.op() {
                            divisor
                        } else {
                            return;
                        }
                    }
                    Node::Const(divisor) => divisor,
                    _ => {
                        return;
                    }
                };

                log::debug!("Mod2Shift: divisor {:?}!", divisor);

                if let TarvalKind::Long(divisor_value) = divisor.tarval().kind() {
                    log::debug!("Mod2Shift: divisor value {:?}!", divisor_value);

                    if divisor_value > std::i32::MAX.into() || divisor_value < std::i32::MIN.into()
                    {
                        log::debug!("Mod2Shift: aborting since value is not in i32 range!",);
                        // this can only happen through constant folding, which might
                        // generate a Const Ls out of the i32 range
                        return;
                    }

                    // cannot crash since min_val is not a power of two
                    let abs_divisor_value = divisor_value.abs() as u64;
                    let has_minus = divisor_value < 0;

                    if abs_divisor_value.is_power_of_two() {
                        log::debug!("Mod2Shift: is power of two!");

                        let shift_amount = 64 - 1 - abs_divisor_value.leading_zeros();
                        let shift_amount_node = self
                            .graph
                            .new_const(Tarval::val(i64::from(shift_amount), Mode::Iu()));

                        let const_31 = self.graph.new_const(Tarval::val(31, Mode::Iu()));
                        let const_1 = self.graph.new_const(Tarval::mj_int(1));

                        let modulo_proj_res = if let Some(res) = modulo.out_proj_res() {
                            res
                        } else {
                            return;
                        };

                        if modulo_proj_res.out_nodes().len() != 1 {
                            return;
                        }

                        // this is the conv hanging at the end of the modulo
                        let modulo_end = modulo_proj_res.out_nodes().nth(0).unwrap();

                        // reach through the conv node in front of the modulo
                        if modulo.left().in_nodes().len() != 1 {
                            return;
                        }

                        let real_left = modulo.left().in_nodes().nth(0).unwrap();

                        let block = modulo.block();
                        let shr_by_31 = block.new_shrs(real_left, const_31);
                        let shl_1_by_shift = block.new_shl(const_1, shift_amount_node);
                        let shl_1_by_shift_minus_1 = block.new_sub(shl_1_by_shift, const_1);
                        let binary_and = block.new_and(shr_by_31, shl_1_by_shift_minus_1);
                        let add_binary_and = block.new_add(real_left, binary_and);
                        let shift_to_result = block.new_shrs(add_binary_and, shift_amount_node);

                        let inner_div_end = if has_minus {
                            Node::Minus(block.new_minus(shift_to_result))
                        } else {
                            Node::Shrs(shift_to_result)
                        };

                        // TODO: substitute this mul by a with shift since divisor_amount_node is a
                        // power of two (or a negative power of two)
                        //let mul_result_by_divisor =
                        //block.new_mul(inner_div_end, divisor_amount_node);
                        let mul_result_by_divisor = block.new_shl(inner_div_end, shift_amount_node);
                        let mul_result_by_divisor_end = if has_minus {
                            Node::Minus(block.new_minus(mul_result_by_divisor))
                        } else {
                            Node::Shl(mul_result_by_divisor)
                        };

                        let modulo_subst_end = block.new_sub(real_left, mul_result_by_divisor_end);

                        log::debug!(
                            "LO: Mod2Shift: memory edge through modulo {:?} is {:?} -> {:?}",
                            modulo,
                            modulo.mem(),
                            modulo.out_proj_m(),
                        );

                        let modulo_proj_mem = if let Some(mem) = modulo.out_proj_m() {
                            mem
                        } else {
                            return;
                        };

                        // drop the mem edge from the modulo
                        Graph::exchange(modulo_proj_mem, modulo.mem());

                        log::debug!(
                            "LO: Mod2Shift: {:?}[left:{:?},right:{:?}] replaced",
                            modulo,
                            modulo.left(),
                            modulo.right(),
                        );

                        Graph::exchange(modulo_end, modulo_subst_end);

                        self.changed = Outcome::Changed;
                    }
                }
            }
            Node::Div(div) => {
                log::debug!(
                    "LO: Div2Shift: {:?}[left:{:?},right:{:?}] ",
                    div,
                    div.left(),
                    div.right(),
                );

                // we expect either
                // - a Conv with a Const Is as operand [which is the output of firm
                //   construction]
                // - or a constant with Const Ls [which is the output of constant folding]
                let divisor = match div.right() {
                    Node::Conv(conv) => {
                        log::debug!("Div2Shift: with conv {:?}!", conv);
                        if let Node::Const(divisor) = conv.op() {
                            divisor
                        } else {
                            return;
                        }
                    }
                    Node::Const(divisor) => divisor,
                    _ => {
                        return;
                    }
                };

                log::debug!("Div2Shift: divisor {:?}!", divisor);

                if let TarvalKind::Long(divisor_value) = divisor.tarval().kind() {
                    log::debug!("Div2Shift: divisor value {:?}!", divisor_value);

                    if divisor_value > std::i32::MAX.into() || divisor_value < std::i32::MIN.into()
                    {
                        log::debug!("Div2Shift: aborting since value is not in i32 range!",);
                        // this can only happen through constant folding, which might
                        // generate a Const Ls out of the i32 range
                        return;
                    }

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
                        let shift_to_result = block.new_shrs(add_binary_and, shift_amount_node);

                        let shr_end = if has_minus {
                            Node::Minus(div.block().new_minus(shift_to_result))
                        } else {
                            Node::Shrs(shift_to_result)
                        };

                        log::debug!(
                            "LO: Div2Shift: memory edge through div {:?} is {:?} -> {:?}",
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
                            "LO: Div2Shift: {:?}[left:{:?},right:{:?}] replaced by '>> {}'",
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
