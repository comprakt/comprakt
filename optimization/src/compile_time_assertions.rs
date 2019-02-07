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
pub const ENV_VAR_NAME: &'static str = "COMPRAKT_COMPILE_TIME_MJ_ASSERTS";

lazy_static::lazy_static! {
    static ref ENABLED: bool = std::env::var(ENV_VAR_NAME).is_ok();
}

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
        !(*ENABLED)
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

    /// Check if a call is a match for the given test, this allows for
    /// method overloading by adding a suffix, e.g. a `check_call("is_const",
    /// assert_const, call, phase)` will match method
    /// `Assert$M$is_const_int_after`, `Assert$M$is_const_4_before`, ...
    pub fn check_call<T: Fn(nodes::Call, &Phase)>(
        &self,
        basename: &str,
        checker: T,
        call: nodes::Call,
        phase: &Phase,
    ) {
        if let Some(method_name) = call.method_name() {
            let prefix = format!("Assert$M${}", basename);
            let is_before = method_name.ends_with("_before");
            let is_after = method_name.ends_with("_after");
            if method_name.starts_with(&prefix) {
                if (is_before && !phase.is_already_applied)
                    || (is_after && phase.is_already_applied)
                {
                    checker(call, phase);
                }
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
                self.check_call("is_const", assert_const, call, &phase);
                self.check_call("is_not_const", assert_not_const, call, &phase);
                self.check_call("is_same_node", assert_same_node, call, &phase);
                self.check_call("is_different_node", assert_different_node, call, &phase);
            }
        });
    }
}

fn assert_const(call: nodes::Call, phase: &Phase) {
    if let Some(nodes) = check_phase_filter(call, phase) {
        for node in nodes {
            assert!(
                Node::is_const(node),
                build_assert_msg(
                    format!(
                        "expected {:?} ({}) to be a constant {}",
                        node,
                        Spans::span_str(node),
                        phase
                    ),
                    call
                )
            );
        }
    }
}

fn assert_not_const(call: nodes::Call, phase: &Phase) {
    if let Some(nodes) = check_phase_filter(call, phase) {
        for node in nodes {
            assert!(
                !Node::is_const(node),
                build_assert_msg(
                    format!(
                        "expected {:?} ({}) to NOT be a constant {}",
                        node,
                        Spans::span_str(node),
                        phase
                    ),
                    call
                )
            );
        }
    }
}

fn assert_same_node(call: nodes::Call, phase: &Phase) {
    assert_node_equality(call, phase, true)
}
fn assert_different_node(call: nodes::Call, phase: &Phase) {
    assert_node_equality(call, phase, false)
}
fn assert_node_equality(call: nodes::Call, phase: &Phase, expect_same: bool) {
    if let Some(nodes) = check_phase_filter(call, phase) {
        assert!(
            nodes.len() >= 2,
            build_assert_msg(
                format!(
                    "same node checks need at least 2 nodes, {} given.",
                    nodes.len()
                ),
                call
            )
        );

        let first = nodes[0];
        for node in &nodes[1..] {
            assert!(
                if expect_same {
                    first == *node
                } else {
                    first != *node
                },
                build_assert_msg(
                    format!(
                        "expected {:?} ({}) and {:?} ({}) to be the same node {}",
                        first,
                        Spans::lookup_span(first)
                            .map(|span| span.as_str())
                            .unwrap_or(""),
                        node,
                        Spans::lookup_span(*node)
                            .map(|span| span.as_str())
                            .unwrap_or(""),
                        phase
                    ),
                    call,
                )
            );
        }
    }
}

/// Returns `None` if the assertion should not be run, returns
/// `Some(node)`, if the assertion should be run on `node`.
fn check_phase_filter(call: nodes::Call, phase: &Phase) -> Option<Vec<Node>> {
    let phase_filter_node = call.args().nth(1).unwrap();
    let phase_filter = Node::as_const(phase_filter_node)
        .unwrap()
        .tarval()
        .get_long()
        .to_string();

    if phase.is_match(&phase_filter) {
        // skip phase filter and `this` argument.
        let nodes = call.args().skip(2).collect();
        Some(nodes)
    } else {
        None
    }
}

fn build_assert_msg(msg: String, node: nodes::Call) -> String {
    format!(
        "{}{}",
        if let Some(pos) = Spans::lookup_span(Node::Call(node)) {
            format!("{}: ", pos)
        } else {
            "".to_string()
        },
        msg
    )
}
