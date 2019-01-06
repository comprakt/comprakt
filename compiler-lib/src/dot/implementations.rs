use super::{
    dot_string, Color, Dot, GraphData, GraphState, Label, LabelMaker, Named, Shape, X11Color,
};
use crate::{
    firm::FirmProgram,
    lowering::lir::{self, BasicBlock},
};
use itertools::Itertools;
use libfirm_rs::{
    nodes::{Node, NodeTrait},
    Graph,
};
use petgraph::visit::{EdgeRef, IntoNodeReferences, NodeIndexable, NodeRef};
use std::{collections::HashMap, hash::BuildHasher, io::Write};

// == GraphData ==

impl<TStruct, TNode> GraphData<TNode> for TStruct
where
    TStruct: Dot<TNode> + Named,
{
    fn graph_data<T>(&self, label_maker: &T) -> HashMap<String, GraphState>
    where
        Self: Sized,
        T: LabelMaker<TNode>,
    {
        let graph_name = self.name();
        let mut dot_files = HashMap::new();
        dot_files.insert(
            graph_name.clone(),
            GraphState {
                name: graph_name.clone(),
                dot_content: self.into_dot_format_string(&graph_name, label_maker),
            },
        );
        dot_files
    }
}

impl<'a, 'b> GraphData<Node> for FirmProgram<'a, 'b> {
    fn graph_data<T>(&self, label_maker: &T) -> HashMap<String, GraphState>
    where
        Self: Sized,
        T: LabelMaker<Node>,
    {
        let mut dot_files = HashMap::new();

        for method in self.methods.values() {
            let class = method.borrow().owning_class.upgrade().unwrap();
            if let Some(graph) = method.borrow().graph {
                let internal_name = method.borrow().entity.name_string();
                let class_name = class.borrow().def.name.to_string();
                let method_name = method.borrow().def.name.to_string();
                dot_files.insert(
                    internal_name.clone(),
                    GraphState {
                        name: format!("{}.{}", class_name, method_name),
                        dot_content: graph.into_dot_format_string(&internal_name, label_maker),
                    },
                );
            }
        }

        dot_files
    }
}

impl GraphData<lir::BasicBlock> for lir::LIR {
    fn graph_data<T>(&self, label_maker: &T) -> HashMap<String, GraphState>
    where
        Self: Sized,
        T: LabelMaker<lir::BasicBlock>,
    {
        let mut dot_files = HashMap::new();

        for function in &self.functions {
            let name = function.name.to_string();
            dot_files.insert(
                name.to_string(),
                GraphState {
                    dot_content: function.into_dot_format_string(&name, label_maker),
                    name,
                },
            );
        }

        dot_files
    }
}

// == Dot ==

impl Named for Graph {
    fn name(&self) -> String {
        self.entity().name_string()
    }
}

impl Dot<Node> for Graph {
    fn into_dot_format<T>(&self, writer: &mut dyn Write, graph_name: &str, label_maker: &T)
    where
        Self: Sized,
        T: LabelMaker<Node>,
    {
        let mut list = Vec::new();
        self.walk_topological(|node| {
            list.push(*node);
        });

        writeln!(writer, "digraph {} {{", dot_string(graph_name)).unwrap();
        for node in list.iter() {
            let label = label_maker.label_for_node(node);
            label.write_dot_format(node.node_id(), writer);

            if !Node::is_block(*node) {
                writeln!(
                    writer,
                    "{:?} -> {:?} [color=blue];",
                    node.block().node_id(),
                    node.node_id()
                )
                .unwrap();
            }
            for ref_node in node.in_nodes() {
                writeln!(
                    writer,
                    "{:?} -> {:?} [color={}];",
                    ref_node.node_id(),
                    node.node_id(),
                    if ref_node.mode().is_mem() || node.mode().is_mem() {
                        Color::X11Color(X11Color::Red)
                    } else {
                        Color::X11Color(X11Color::Black)
                    }
                )
                .unwrap();
            }
        }
        writeln!(writer, "}}").unwrap();

        writer.flush().unwrap();
    }
}

impl<TNode, TEdge> Named for petgraph::Graph<TNode, TEdge> {
    fn name(&self) -> String {
        "graph".to_owned()
    }
}

impl<TNode, TEdge> Dot<TNode> for petgraph::Graph<TNode, TEdge> {
    fn into_dot_format<T>(&self, writer: &mut dyn Write, graph_name: &str, label_maker: &T)
    where
        Self: Sized,
        T: LabelMaker<TNode>,
    {
        writeln!(writer, "digraph {} {{", dot_string(graph_name)).unwrap();
        for node in self.node_references() {
            let label = label_maker.label_for_node(node.weight());
            label.write_dot_format(self.to_index(node.id()), writer);
        }
        for edge in self.edge_references() {
            writeln!(
                writer,
                "{:?} -> {:?};",
                self.to_index(edge.source()),
                self.to_index(edge.target())
            )
            .unwrap();
        }
        writeln!(writer, "}}").unwrap();

        writer.flush().unwrap();
    }
}

// == LIR ==

impl Dot<BasicBlock> for lir::Function {
    fn into_dot_format<T>(&self, writer: &mut dyn Write, graph_name: &str, label_maker: &T)
    where
        Self: Sized,
        T: LabelMaker<BasicBlock>,
    {
        self.graph
            .borrow()
            .into_dot_format(writer, graph_name, label_maker)
    }
}

impl Dot<BasicBlock> for lir::BlockGraph {
    fn into_dot_format<T>(&self, writer: &mut dyn Write, graph_name: &str, label_maker: &T)
    where
        Self: Sized,
        T: LabelMaker<BasicBlock>,
    {
        writeln!(writer, "digraph {} {{", dot_string(graph_name)).unwrap();
        for block_rc in self.blocks.values() {
            let block = block_rc.borrow();
            let label = label_maker.label_for_node(&block);
            label.write_dot_format(block.firm.node_id(), writer);

            log::debug!(
                "blocks preds: {}, blocks succs: {}",
                block.preds.len(),
                block.succs.len()
            );

            for control_flow_transfer in &block.succs {
                log::debug!(
                    "num transfers: {}",
                    control_flow_transfer.borrow().register_transitions.len(),
                );

                for (source_slot, target_slot) in
                    &control_flow_transfer.borrow().register_transitions
                {
                    let source_num = source_slot.borrow().num();
                    let target_num = target_slot.borrow().num;

                    writeln!(
                        writer,
                        " {:?} -> {:?} [label=\"  {}>{}\", color=\"{};0.5:{}\", fontcolor={}];",
                        block.firm.node_id(),
                        control_flow_transfer
                            .borrow()
                            .target
                            .borrow()
                            .firm
                            .node_id(),
                        source_num,
                        target_num,
                        Color::from(source_num),
                        Color::from(target_num),
                        Color::from(source_num),
                    )
                    .unwrap();
                }
            }
        }

        writeln!(writer, "}}").unwrap();

        writer.flush().unwrap();
    }
}

// == LabelMaker ==

impl<F, TNode> LabelMaker<TNode> for F
where
    F: Fn(&TNode) -> Label,
{
    fn label_for_node(&self, node: &TNode) -> Label {
        self(node)
    }
}

pub fn default_label(node: &Node) -> Label {
    let mut label = Label::from_text(format!("{:?}", node));
    if Node::is_proj(*node) {
        label = label.shape(Shape::Ellipse);
    }
    label
}

pub fn default_lir_label(block: &BasicBlock) -> Label {
    Label::from_text(format!(
        "Basic Block {:?}\n{}\\l",
        block.firm.node_id(),
        block
            .code
            .body
            .iter()
            .map(|instr| format!("{:?}", instr))
            .join("\\l")
    ))
}

impl<S: BuildHasher> LabelMaker<Node> for HashMap<Node, Label, S> {
    fn label_for_node(&self, node: &Node) -> Label {
        self.get(&node)
            .cloned()
            .unwrap_or_else(|| Label::from_text("".to_string()))
    }
}
