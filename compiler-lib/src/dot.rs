//! Converts FIRM or LIR graphs into dot graph description language for easy debugging.
use libfirm_rs::nodes::NodeTrait;
use std::fs::File;
use std::path::PathBuf;
use std::collections::hash_map::HashMap;
use libfirm_rs::entity::Entity;
use libfirm_rs::graph::Graph;
use crate::firm::Program;
use libfirm_rs::nodes_gen::Node;
use std::io::{Write, BufWriter};
use serde_derive::{Serialize};

#[derive(Debug,Clone,Serialize)]
pub struct GraphState {
    class_name: String,
    method_name: String,
    dot_file: String
}

pub fn default_label(node :Node) -> Label {
    Label::from_text(node.node_id().to_string(), format!("{:?}", node))
}

pub trait GraphData {
    /// Transform the object into a map of unique function names
    /// to graph information in dot format
    fn graph_data<T>(&self, label_maker: &T) -> HashMap<String, GraphState>
        where Self: Sized, T : LabelMaker;
}

impl<'a, 'b> GraphData for Program<'a, 'b> {
    fn graph_data<T>(&self, label_maker: &T) -> HashMap<String, GraphState> 
        where Self: Sized, T : LabelMaker
    {
        let mut dot_files = HashMap::new();

        for (class_name, class) in &self.classes {
            for (method_name, method) in &class.borrow().methods {
                if let Some(graph) = method.borrow().graph {
                    let graph : Graph = graph.into();
                    let internal_name = Entity::new(method.borrow().entity.into()).name_string();
                    dot_files.insert(internal_name.clone(), GraphState {
                        class_name: class_name.to_string(),
                        method_name: method_name.to_string(),
                        dot_file: graph.into_dot_format_string(&internal_name, label_maker)
                    });
                }
            }
        }

        dot_files
    }
}

impl Dot for Graph {
    fn into_dot_format<T>(&self, writer: &mut dyn Write, graph_name:&str, label_maker: &T)
        where Self: Sized, T : LabelMaker
    {
        let mut list = Vec::new();
        self.walk_topological(|node| {
            list.push(*node);
        });

        writeln!(writer, "digraph {} {{", graph_name).unwrap();
        for node in list.iter() {
            let label = label_maker.label_for_node(*node);
            label.write_dot_format(writer);


            if !node.is_block() {
                writeln!(
                    writer,
                    "{:?} -> {:?} [color=blue];",
                    node.block().node_id(),
                    node.node_id()
                )
                .unwrap();
            }
            for ref_node in node.in_nodes() {
                writeln!(writer, "{:?} -> {:?};", ref_node.node_id(), node.node_id()).unwrap();
            }
        }
        writeln!(writer, "}}").unwrap();

        writer.flush().unwrap();
    }
}


/// Abstraction over anything that can be transformed into labels for 
/// each node in a graph
pub trait LabelMaker {
    fn label_for_node(&self, node :Node) -> Label;
}


impl<F> LabelMaker for F where F: Fn(Node) -> Label {
    fn label_for_node(&self, node: Node) -> Label {
        self(node)
    }
}

impl LabelMaker for HashMap<Node, Label> {
    fn label_for_node(&self, node :Node) -> Label {
        self.get(&node).map(|v| v.clone()).unwrap_or_else(|| {
            Label::from_text(node.node_id().to_string(), "".to_string())
        })
    }
}

pub trait Dot {
    fn dump_as_dot_file<T>(&self, filename: &PathBuf, graph_name:&str, data: &T)
        where Self: Sized, T : LabelMaker
    {
        let write_file = File::create(filename).unwrap();
        let mut writer = BufWriter::new(&write_file);
        self.into_dot_format(&mut writer, graph_name, data)
    }

    fn into_dot_format_string<T>(&self, graph_name:&str, data: &T) -> String
        where Self: Sized, T : LabelMaker {
        let mut dot_data :Vec<u8> = Vec::new();
        self.into_dot_format(&mut dot_data, graph_name, data/*|node| {
            Label::from_text(node.node_id().to_string(), format!("{:?}", node))
        }*/);
        String::from_utf8_lossy(&dot_data).to_string()
    }

    fn into_dot_format<T>(&self, writer: &mut dyn Write, graph_name:&str, data: &T)
        where Self: Sized, T : LabelMaker;
}

#[derive(Debug, Clone)]
enum LabelText {
    Plain(String),
    Html(String)
}

impl Default for LabelText {
    fn default() -> Self {
        LabelText::Plain("".to_string())
    }
}

#[derive(Debug, Clone)]
enum LabelColor {
    HSB(f32,f32,f32),
    RGB(u8,u8,u8),
    // TODO: there are also predefined colors
    //Orchid,
}

#[derive(Default, Debug, Clone)]
pub struct Label {
    text: LabelText,
    id: String,
    filled: Option<bool>,
    dotted: Option<bool>,
    bold: Option<bool>,
    skew: Option<f32>,
    polygon: Option<i32>,
    distortion: Option<f32>,
}

#[derive(Debug, Clone)]
enum SpecialLabels {
    Current,
    Other,
}

impl Label {
    pub fn from_text(id: String, text: String) -> Label {
        let mut label :Label = Default::default();
        label.text(text);
        label.id(id);
        label
    }

    pub fn from_html(id: String, text: String) -> Label {
        let mut label :Label = Default::default();
        label.html(text);
        label.id(id);
        label
    }

    //pub fn mark(&mut self, val: bool) {
        //self.filled = val;
    //}

    //pub fn color(&mut self, hsb: (f32,f32,f32)) {
    //}

    pub fn filled(&mut self, val:bool) {
        self.filled = Some(val);
    }

    pub fn dotted(&mut self, val:bool) {
        self.dotted = Some(val);
    }

    pub fn bold(&mut self, val:bool) {
        self.bold = Some(val);
    }

    pub fn shape(&mut self, num_edges: u32) {
        //self.shape = Some(num_edges);
    }

    pub fn outlines(&mut self, count: u32) {
        // peripheries=
    }

    pub fn skew(&mut self, val:f32) {
        self.skew = Some(val);
    }

    pub fn text(&mut self, text:String) {
        self.text = LabelText::Plain(text);
    }

    pub fn html(&mut self, text:String) {
        self.text = LabelText::Html(text);
    }

    pub fn id(&mut self, id:String) {
        self.id = id;
    }

    pub fn write_dot_format(&self, writer: &mut dyn Write) {
        writeln!(
            writer,
            "{id} [label={label}{style}, shape=box];",
            id=self.id,
            label=match self.text {
                // TODO: escape 
                LabelText::Plain(ref text) => dot_string(&text),
                LabelText::Html(ref text) => format!("<{}>", text),
            },
            style={
                let val = (vec![
                    self.bold.map(|_| "bold"),
                    self.dotted.map(|_| "dotted"),
                    self.filled.map(|_| "filled"),
                ]).iter().filter_map(|v| *v).collect::<Vec<_>>().join(",");

                if val != "" {
                    format!(",{}",val)
                } else {
                    "".to_string()
                }
            },
        )
        .unwrap();
    }
}

fn dot_string(string:&str) -> String {
    format!("\"{}\"", string.replace("\"", "'").replace("\n", "\\n"))
}
