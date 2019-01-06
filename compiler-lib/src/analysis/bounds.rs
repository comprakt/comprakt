//! Computes value range for all nodes
//!
//! This can be used for
//!
//! a.) bounds check elimination (BCE) in our safe variant. For example
//!     this code will not emit an if-check that verifies the bounds
//!
//!     ```
//!     int a[] = new int[5];
//!     System.out.println(a[2]);
//!     ```
use crate::{
    breakpoint,
    dot::*,
    optimization::{self, Outcome},
};
use libfirm_rs::{
    nodes::{Node, NodeTrait},
    Graph,
};
use std::collections::HashMap;

type MjInt = i32;

enum Range {
    Empty,
    /// Both ranges inclusive, e.g. the constant 1 is represented as
    /// `Range::Interval { lower: 1, upper: 1 }`
    Interval {
        lower: MjInt,
        upper: MjInt,
    },
}

impl Range {
    fn empty() -> Self {
        Range::Empty
    }
    fn constant(val: MjInt) -> Self {
        Range::Interval {
            lower: val,
            upper: val,
        }
    }
    fn byte() -> Self {
        Range::Interval {
            lower: 0,
            upper: 255,
        }
    }
    fn full() -> Self {
        Range::Interval {
            lower: std::i32::MIN,
            upper: std::i32::MAX,
        }
    }
}

pub struct ValueRangeAnalysis {
    graph: Graph,
    ranges: HashMap<Node, Range>,
}

impl optimization::Local for ValueRangeAnalysis {
    fn optimize_function(graph: Graph) -> Outcome {
        ValueRangeAnalysis::new(graph).run()
    }
}

impl ValueRangeAnalysis {
    fn new(graph: Graph) -> Self {
        graph.assure_outs();
        Self {
            graph,
            ranges: HashMap::new(),
        }
    }

    fn run(&mut self) -> Outcome {
        self.graph.walk_topological(|current: &Node| {
            breakpoint!("Jump Threading: iteration", self.graph, &|node: &Node| {
                let mut label = default_label(node);

                if node == current {
                    label = label
                        .style(Style::Filled)
                        .fillcolor(X11Color::Blue)
                        .fontcolor(X11Color::White);
                }

                label
            });

            match current {
                //Node::Jmp(jmp) => {
                //}
                //Node::Block(block) => {
                //}
                _ => {}
            }
        });

        Outcome::Unchanged
    }
}
