use super::{dot_string, Color, Dot, GraphData, GraphState, Label, LabelMaker, Named, X11Color, Shape};
use crate::firm::FirmProgram;
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

impl<S: BuildHasher> LabelMaker<Node> for HashMap<Node, Label, S> {
    fn label_for_node(&self, node: &Node) -> Label {
        self.get(&node)
            .cloned()
            .unwrap_or_else(|| Label::from_text("".to_string()))
    }
}
