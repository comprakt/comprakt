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

    /// Check if a call is a match for the given test
    ///
    /// This allows for method overloading by adding a suffix, e.g.
    /// a `check_call("is_const", assert_const, call, phase)` before and after
    /// the first optimization `ConstantFolding` will match:
    ///
    /// - `Opt0Before$M$is_const`,
    /// - `Opt0After$M$is_const_4`,
    /// - `ConstantFoldingBefore$M$is_const`
    /// - ...
    pub fn check_call<T: Fn(nodes::Call, &Phase)>(
        &self,
        basename: &str,
        checker: T,
        call: nodes::Call,
        phase: &Phase,
    ) {
        if let Some(method_name) = call.method_name() {
            let optidx_class_name = format!("Opt{}", phase.opt_idx);
            let optkind_class_name = format!("{}", phase.opt_kind);
            let is_before = method_name
                .starts_with(&format!("{}Before$M${}", optidx_class_name, basename))
                || method_name.starts_with(&format!("{}Before$M${}", optkind_class_name, basename));
            let is_after = method_name
                .starts_with(&format!("{}After$M${}", optidx_class_name, basename))
                || method_name.starts_with(&format!("{}After$M${}", optkind_class_name, basename));

            if (is_before && !phase.is_already_applied) || (is_after && phase.is_already_applied) {
                checker(call, phase);
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
    for node in get_args(call) {
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

fn assert_not_const(call: nodes::Call, phase: &Phase) {
    for node in get_args(call) {
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

fn assert_same_node(call: nodes::Call, phase: &Phase) {
    assert_node_equality(call, phase, true)
}
fn assert_different_node(call: nodes::Call, phase: &Phase) {
    assert_node_equality(call, phase, false)
}
fn assert_node_equality(call: nodes::Call, phase: &Phase, expect_same: bool) {
    let nodes = get_args(call);
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

    for (idx, curr_node) in nodes.iter().enumerate() {
        for other in &nodes[(idx + 1)..] {
            assert!(
                if expect_same {
                    other == curr_node
                } else {
                    other != curr_node
                },
                build_assert_msg(
                    format!(
                        "expected {:?} ({}) and {:?} ({}) to be the same node {}",
                        curr_node,
                        Spans::lookup_span(*curr_node)
                            .map(|span| span.as_str())
                            .unwrap_or(""),
                        other,
                        Spans::lookup_span(*other)
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
fn get_args(call: nodes::Call) -> Vec<Node> {
    // skip `this` argument.
    call.args().skip(1).collect()
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
