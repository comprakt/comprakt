use super::BasicBlock;
use crate::lir;
use debugging::dot::{
    dot_string, escape_record_content, Dot, GraphData, GraphState, Label, LabelMaker, Shape, Style,
    X11Color,
};
use libfirm_rs::nodes::NodeTrait;
use std::{collections::HashMap, io::Write};

pub fn default_lir_label(block: &BasicBlock) -> Label {
    let mut s = Vec::new();

    write!(&mut s, "\\lCOPY IN\\l").unwrap();
    for instr in block.code.copy_in.iter() {
        write!(&mut s, "{:?}\\l", &**instr).unwrap();
    }

    write!(&mut s, "\\lBODY\\l").unwrap();
    for instr in block.code.body.iter() {
        write!(&mut s, "{:?}\\l", &**instr).unwrap();
    }

    write!(&mut s, "\\lCOPY OUT\\l").unwrap();
    for instr in block.code.copy_out.iter() {
        write!(&mut s, "{:?}\\l", &**instr).unwrap();
    }

    write!(&mut s, "\\lLEAVE\\l").unwrap();
    for instr in block.code.leave.iter() {
        write!(&mut s, "{:?}\\l", &**instr).unwrap();
    }

    lir_box(block, &format!("{}\\l", String::from_utf8(s).unwrap()))
}

pub fn lir_box(block: &BasicBlock, body: &str) -> Label {
    Label::from_text(format!(
        r#"{{|<header> Block {block_id}|<code>{code}|}}"#,
        block_id = block.firm.node_id(),
        code = escape_record_content(body),
    ))
    .shape(Shape::Record)
    .styles(vec![Style::Rounded, Style::Filled])
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

            for succ in &block.succs {
                writeln!(
                    writer,
                    " {block_out} -> {block_in}\
                     [color=\"{color}\", penwidth=2];",
                    block_out = block.firm.node_id(),
                    block_in = succ.firm.node_id(),
                    color = X11Color::Black,
                )
                .unwrap();
            }
        }

        writeln!(writer, "}}").unwrap();

        writer.flush().unwrap();
    }
}
