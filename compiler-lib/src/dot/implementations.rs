use super::{
    dot_string, Color, Dot, GraphData, GraphState, Label, LabelMaker, Named, Shape, Style, X11Color,
};
use crate::{
    firm::{program_generator::Spans, FirmProgram},
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

// == LIR ==

impl Dot<BasicBlock> for lir::Function {
    fn into_dot_format<T>(&self, writer: &mut dyn Write, graph_name: &str, label_maker: &T)
    where
        Self: Sized,
        T: LabelMaker<BasicBlock>,
    {
        self.graph.into_dot_format(writer, graph_name, label_maker)
    }
}

impl Dot<BasicBlock> for lir::BlockGraph {
    fn into_dot_format<T>(&self, writer: &mut dyn Write, graph_name: &str, label_maker: &T)
    where
        Self: Sized,
        T: LabelMaker<BasicBlock>,
    {
        writeln!(writer, "digraph {} {{", dot_string(graph_name)).unwrap();
        for block in self.blocks.values() {
            let label = label_maker.label_for_node(block);
            label.write_dot_format(block.firm.node_id(), writer);

            log::debug!(
                "blocks preds: {}, blocks succs: {}",
                block.preds.len(),
                block.succs.len()
            );

            for control_flow_transfer in &block.succs {
                log::debug!(
                    "num transfers: {}",
                    control_flow_transfer.register_transitions.len(),
                );

                writeln!(
                    writer,
                    " {block_out} -> {block_in}\
                     [color=\"{color}\", penwidth=2];",
                    block_out = block.firm.node_id(),
                    block_in = control_flow_transfer.target.firm.node_id(),
                    color = X11Color::Black,
                )
                .unwrap();

                for (source_slot, target_slot) in &control_flow_transfer.register_transitions {
                    let source_num = source_slot.num();
                    let target_num = target_slot.num;

                    writeln!(
                        writer,
                        " {block_out}:out{out_slot} -> {block_in}:in{in_slot} \
                         [color=\"{color_out};0.5:{color_in}\"];",
                        block_out = block.firm.node_id(),
                        block_in = control_flow_transfer.target.firm.node_id(),
                        out_slot = source_num,
                        in_slot = target_num,
                        color_out = Color::from(source_num),
                        color_in = Color::from(target_num),
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
        label = label.shape(Shape::Note);
    }
    if let Some(span) = Spans::lookup_span(*node) {
        label = label.append(format!(" [src:{}]", span));
    }
    label
}

pub fn default_lir_label(block: &BasicBlock) -> Label {
    let mut s = Vec::new();

    write!(&mut s, "\\lCOPY IN\\l").unwrap();
    for instr in block.code.copy_in.iter() {
        write!(&mut s, "{:?}\\l", instr).unwrap();
    }

    write!(&mut s, "\\lBODY\\l").unwrap();
    for instr in block.code.body.iter() {
        write!(&mut s, "{:?}\\l", instr).unwrap();
    }

    write!(&mut s, "\\lCOPY OUT\\l").unwrap();
    for instr in block.code.copy_out.iter() {
        write!(&mut s, "{:?}\\l", instr).unwrap();
    }

    write!(&mut s, "\\lLEAVE\\l").unwrap();
    for instr in block.code.leave.iter() {
        write!(&mut s, "{:?}\\l", instr).unwrap();
    }

    lir_box(block, &format!("{}\\l", String::from_utf8(s).unwrap()))
}

pub fn lir_box(block: &BasicBlock, body: &str) -> Label {
    let pins = 0..block.regs.len();

    Label::from_text(format!(
        r#"{{{{{input_slots}}}|<header> Block {block_id}|<code>{code}|{{{out_slots}}}}}"#,
        block_id = block.firm.node_id(),
        code = escape_record_content(body),
        input_slots = pins
            .clone()
            .map(|index| format!("<in{idx}>{idx}", idx = index))
            .join("|"),
        out_slots = pins
            .map(|index| format!("<out{idx}>{idx}", idx = index))
            .join("|"),
    ))
    .shape(Shape::Record)
    .styles(vec![Style::Rounded, Style::Filled])
}

pub fn escape_record_content(text: &str) -> String {
    text.replace("|", "\\|")
        .replace("{", "\\{")
        .replace("}", "\\}")
        .replace("<", "\\<")
        .replace(">", "\\>")
}

impl<S: BuildHasher> LabelMaker<Node> for HashMap<Node, Label, S> {
    fn label_for_node(&self, node: &Node) -> Label {
        self.get(&node)
            .cloned()
            .unwrap_or_else(|| Label::from_text("".to_string()))
    }
}
