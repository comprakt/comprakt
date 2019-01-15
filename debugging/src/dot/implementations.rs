use super::{
    dot_string, Color, Dot, GraphData, GraphState, Label, LabelMaker, Named, Shape, Style, X11Color,
};
use crate::firm::{program_generator::Spans, FirmProgram};
use libfirm_rs::{
    bindings,
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
                    format!("{}-PDG", internal_name.clone()),
                    GraphState {
                        name: format!("{}.{}[PDG]", class_name, method_name),
                        dot_content: graph.into_dot_format_string(&internal_name, label_maker),
                    },
                );
                // TODO: this should be implemented on Graph directly, the
                // information is available without the firm program context
                let mut dot_dom_tree: Vec<u8> = Vec::new();
                let graph_name = format!("{}-Dominance-Tree", internal_name.clone());
                dominance_tree_in_dot_format(&mut dot_dom_tree, &graph_name, graph);

                dot_files.insert(
                    graph_name.clone(),
                    GraphState {
                        name: format!("{}.{}[Dominance Tree]", class_name, method_name),
                        dot_content: String::from_utf8_lossy(&dot_dom_tree).to_string(),
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
        self.walk(|node| {
            list.push(*node);
        });
        self.assure_outs();

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

            for (in_idx, pred) in node.in_nodes().enumerate() {
                let pred_out_idx = pred
                    .out_nodes()
                    .position(|current| node == &current)
                    .map(|idx| idx.to_string())
                    .unwrap_or_else(|| "?".to_string());

                writeln!(
                    writer,
                    "{:?} -> {:?} [color={}, label=\"{} -> {}\"];",
                    pred.node_id(),
                    node.node_id(),
                    if pred.mode().is_mem() || node.mode().is_mem() {
                        Color::X11Color(X11Color::Red)
                    } else {
                        Color::X11Color(X11Color::Black)
                    },
                    pred_out_idx,
                    in_idx,
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
        label = label.shape(Shape::Note);
    }
    if let Some(span) = Spans::lookup_span(*node) {
        label = label.append(format!(" [src:{}]", span));
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

fn dominance_tree_in_dot_format(writer: &mut dyn Write, graph_name: &str, graph: Graph) {
    // TODO: onyl render if dominators are computed. Side effects in
    // debugging code is a bad idea
    graph.compute_doms();

    let mut list = Vec::new();
    graph.walk_dom_tree_postorder(|block| {
        list.push(*block);
    });

    writeln!(writer, "digraph {} {{", dot_string(graph_name)).unwrap();
    for block in list.iter() {
        let label = dom_info_box(&Node::Block(*block));
        label.write_dot_format(block.node_id(), writer);

        if let Some(idom) = block.immediate_dominator() {
            writeln!(
                writer,
                "{:?} -> {:?} [color=blue];",
                idom.node_id(),
                block.node_id()
            )
            .unwrap();
        }
    }
    writeln!(writer, "}}").unwrap();
}

pub fn dom_info_box(node: &Node) -> Label {
    if let Node::Block(block) = node {
        let dom_depth = unsafe { bindings::get_Block_dom_depth(node.internal_ir_node()) };
        Label::from_text(format!(
            r#"{{{body}|{{Dom Depth|{}}}}}"#,
            dom_depth = dom_depth,
            body = escape_record_content(&format!("{:?}", block)),
        ))
        .shape(Shape::Record)
        .styles(vec![Style::Rounded, Style::Filled])
    } else {
        default_label(node)
    }
}

// TODO: deduplicate after 172 is merged
pub fn escape_record_content(text: &str) -> String {
    text.replace("|", "\\|")
        .replace("{", "\\{")
        .replace("}", "\\}")
        .replace("<", "\\<")
        .replace(">", "\\>")
}
