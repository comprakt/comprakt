//! Injects the class 'Assert' into the mini java runtime, which
//! contains functions that are evaluated at compile-time.
use crate::{firm::FirmProgram, optimization};
use firm_construction::program_generator::Spans;
use libfirm_rs::{
    nodes::{self, Node},
    Graph,
};
use std::fmt;

/// Set this environment variable to enable these special functions
pub const ENV_VAR: &'static str = "COMPRAKT_COMPILE_TIME_MJ_ASSERTS";

#[derive(Default, Clone, Debug)]
pub struct CompileTimeAssertions {}

#[derive(Clone, Copy, Debug)]
pub struct Phase {
    pub opt_idx: usize,
    pub opt_kind: optimization::Kind,
    pub is_already_applied: bool,
}

impl fmt::Display for Phase {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "{} optimization number {}. {}",
            if self.is_already_applied {
                "after"
            } else {
                "before"
            },
            self.opt_idx + 1,
            self.opt_kind
        )
    }
}

impl Phase {
    fn is_match(&self, filter: &str) -> bool {
        if self.opt_kind.to_string() == filter {
            return true;
        }

        if let Ok(num) = filter.parse::<usize>() {
            return self.opt_idx == num;
        }

        return false;
    }
}

impl CompileTimeAssertions {
    pub fn new() -> Self {
        Self {}
    }

    fn disabled() -> bool {
        !std::env::var(ENV_VAR).is_ok()
    }

    pub fn check_program(&self, program: &FirmProgram<'_, '_>, phase: Phase) {
        if CompileTimeAssertions::disabled() {
            return;
        }

        log::debug!("checking assertions for phase {:?}", phase);

        for method in program.methods.values() {
            if let Some(graph) = method.borrow().graph {
                self.check_graph(graph, phase);
            }
        }
    }

    // check all assertions, panic if the an assertion is
    // wrong.
    pub fn check_graph(&self, graph: Graph, phase: Phase) {
        if CompileTimeAssertions::disabled() {
            return;
        }

        graph.walk(|node| {
            if let Some(call) = Node::as_call(*node) {
                if let Some(method_name) = call.method_name() {
                    match method_name.as_str() {
                        "Assert$M$is_const_after" if phase.is_already_applied => {
                            assert_const(call, &phase)
                        }
                        "Assert$M$is_const_before" if !phase.is_already_applied => {
                            assert_const(call, &phase)
                        }
                        "Assert$M$is_not_const_after" if phase.is_already_applied => {
                            assert_not_const(call, &phase)
                        }
                        "Assert$M$is_not_const_before" if !phase.is_already_applied => {
                            assert_not_const(call, &phase)
                        }
                        _ => {}
                    }
                }
            }
        });
    }
}

fn assert_const(call: nodes::Call, phase: &Phase) {
    let phase_filter_node = call.args().nth(1).unwrap();
    // TODO: somehow suppport specifying optimizations by name instead of indices
    let phase_filter = Node::as_const(phase_filter_node)
        .unwrap()
        .tarval()
        .get_long()
        .to_string();
    let node = call.args().nth(2).unwrap();

    if phase.is_match(&phase_filter) {
        assert!(
            Node::is_const(node),
            build_assert_msg(
                format!("expected {:?} to be a constant {}", node, phase),
                node
            )
        );
    }
}

fn assert_not_const(call: nodes::Call, phase: &Phase) {
    let phase_filter_node = call.args().nth(1).unwrap();
    // TODO: somehow suppport specifying optimizations by name instead of indices
    let phase_filter = Node::as_const(phase_filter_node)
        .unwrap()
        .tarval()
        .get_long()
        .to_string();
    let node = call.args().nth(2).unwrap();

    if phase.is_match(&phase_filter) {
        assert!(
            !Node::is_const(node),
            build_assert_msg(
                format!("expected {:?} to NOT be a constant {}", node, phase),
                node
            )
        );
    }
}

fn build_assert_msg(msg: String, node: Node) -> String {
    format!(
        "{}{}",
        if let Some(pos) = Spans::lookup_span(node) {
            format!("{}: ", pos)
        } else {
            "".to_string()
        },
        msg
    )
}
