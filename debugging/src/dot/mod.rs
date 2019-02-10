//! Converts FIRM or LIR graphs into dot graph description language for easy
//! debugging.

mod colors;
mod implementations;

pub use self::{colors::*, implementations::*};

use itertools::Itertools;
use serde_derive::Serialize;
use std::{
    collections::hash_map::HashMap,
    fs::File,
    io::{BufWriter, Write},
    path::PathBuf,
};

pub trait GraphData<TNode> {
    /// Transform the object into a map of unique function names
    /// to graph information in dot format
    fn graph_data<T>(&self, label_maker: &T) -> HashMap<String, GraphState>
    where
        Self: Sized,
        T: LabelMaker<TNode>;
}

#[derive(Debug, Clone, Serialize)]
pub struct GraphState {
    pub name: String,
    pub dot_content: String,
}

/// Abstraction over anything that can be transformed into labels for
/// each node in a graph
pub trait LabelMaker<TNode> {
    fn label_for_node(&self, node: &TNode) -> Label;
}

#[derive(Default, Debug, Clone)]
pub struct Label {
    text: LabelText,
    id: String,
    style: Option<Vec<Style>>,
    fillcolor: Option<Color>,
    fontcolor: Option<Color>,
    shape: Option<Shape>,
    sides: Option<u32>,
    peripheries: Option<u32>,
    distortion: Option<f32>,
    skew: Option<f32>,
}

pub trait Dot<TNode>: Sized {
    fn into_dot_format<T>(&self, writer: &mut dyn Write, graph_name: &str, data: &T)
    where
        Self: Sized,
        T: LabelMaker<TNode>;

    fn dump_as_dot_file<T>(&self, filename: &PathBuf, graph_name: &str, data: &T)
    where
        Self: Sized,
        T: LabelMaker<TNode>,
    {
        let write_file = File::create(filename).unwrap();
        let mut writer = BufWriter::new(&write_file);
        self.into_dot_format(&mut writer, graph_name, data)
    }

    fn into_dot_format_string<T>(&self, graph_name: &str, data: &T) -> String
    where
        Self: Sized,
        T: LabelMaker<TNode>,
    {
        let mut dot_data: Vec<u8> = Vec::new();
        self.into_dot_format(
            &mut dot_data,
            graph_name,
            data, /*|node| {
                      Label::from_text(node.node_id().to_string(), format!("{:?}", node))
                  }*/
        );
        String::from_utf8_lossy(&dot_data).to_string()
    }

    fn with_name(&self, name: String) -> NamedDot<'_, Self, TNode> {
        NamedDot {
            dot: self,
            name,
            node_marker: std::marker::PhantomData,
        }
    }
}

pub trait Named {
    fn name(&self) -> String;
}

pub struct NamedDot<'dot, TDot, TNode>
where
    TDot: Dot<TNode>,
{
    dot: &'dot TDot,
    name: String,
    node_marker: std::marker::PhantomData<TNode>,
}

impl<'dot, TDot, TNode> Named for NamedDot<'dot, TDot, TNode>
where
    TDot: Dot<TNode>,
{
    fn name(&self) -> String {
        self.name.clone()
    }
}

impl<'dot, TDot, TNode> Dot<TNode> for NamedDot<'dot, TDot, TNode>
where
    TDot: Dot<TNode>,
{
    fn into_dot_format<T>(&self, writer: &mut dyn Write, graph_name: &str, data: &T)
    where
        Self: Sized,
        T: LabelMaker<TNode>,
    {
        self.dot.into_dot_format(writer, graph_name, data);
    }
}

#[derive(Debug, Clone)]
enum LabelText {
    Plain(String),
    Html(String),
}

impl Default for LabelText {
    fn default() -> Self {
        LabelText::Plain("".to_string())
    }
}

#[derive(Display, Debug, Clone, Copy)]
pub enum Style {
    Dashed,
    Dotted,
    Solid,
    Bold,
    Invisible,
    Filled,
    Diagonals,
    Rounded,
}

#[derive(Display, Debug, Clone, Copy)]
pub enum Shape {
    Box,
    Polygon,
    Ellipse,
    Circle,
    Point,
    Egg,
    Triangle,
    PlainText,
    Diamond,
    Trapezium,
    Parallelogram,
    House,
    Pentagon,
    Hexagon,
    Septagon,
    Octagon,
    DoubleCircle,
    DoubleOctagon,
    TripleOctagon,
    InvTriangle,
    InvTrapezium,
    InvHouse,
    MDiamond,
    MSquare,
    MCircle,
    Note,
    Tab,
    Folder,
    Box3D,
    Component,
    Record,
    MRecord,
}

impl Label {
    pub fn from_text(text: String) -> Self {
        Self::default().text(text)
    }

    pub fn from_html(text: String) -> Self {
        Self::default().html(text)
    }

    pub fn style(mut self, val: Style) -> Self {
        self.style = Some(vec![val]);
        self
    }

    pub fn styles(mut self, val: Vec<Style>) -> Self {
        self.style = Some(val);
        self
    }

    pub fn add_style(self, val: Style) -> Self {
        match self.style {
            Some(ref style) => {
                let mut new_style = style.clone();
                new_style.push(val);
                self.styles(new_style)
            }
            None => self.style(val),
        }
    }

    pub fn shape(mut self, shape: Shape) -> Self {
        self.shape = Some(shape);
        self
    }

    pub fn fillcolor<T: Into<Color>>(mut self, color: T) -> Self {
        self.fillcolor = Some(color.into());
        self
    }

    pub fn fontcolor<T: Into<Color>>(mut self, color: T) -> Self {
        self.fontcolor = Some(color.into());
        self
    }

    /// Number of outlines. When filled one outline is hidden
    /// by the shape of the filling.
    pub fn peripheries(mut self, count: u32) -> Self {
        self.peripheries = Some(count);
        self
    }

    pub fn skew(mut self, val: f32) -> Self {
        self.skew = Some(val);
        self
    }

    pub fn sides(mut self, val: u32) -> Self {
        self.sides = Some(val);
        self
    }

    pub fn text(mut self, text: String) -> Self {
        self.text = LabelText::Plain(text);
        self
    }

    pub fn append(mut self, new_text: String) -> Self {
        let new_label = match &self.text {
            LabelText::Plain(text) => LabelText::Plain(format!("{}{}", text, new_text)),
            LabelText::Html(text) => LabelText::Html(format!("{}{}", text, new_text)),
        };

        self.text = new_label;
        self
    }

    pub fn html(mut self, text: String) -> Self {
        self.text = LabelText::Html(text);
        self
    }

    pub fn write_dot_format(&self, id: impl std::fmt::Display, writer: &mut dyn Write) {
        writeln!(
            writer,
            "{id} [label={label}{style}{fillcolor}{fontcolor}{shape}\
             {peripheries}{skew}{distortion}{sides}];",
            id = id,
            label = match self.text {
                // TODO: escape
                LabelText::Plain(ref text) => dot_string(&text),
                LabelText::Html(ref text) => format!("<{}>", text),
            },
            style = {
                self.style
                    .clone()
                    .map(|v| {
                        format!(
                            ",style=\"{}\"",
                            v.iter().map(|v| v.to_string().to_lowercase()).join(",")
                        )
                    })
                    .unwrap_or_else(|| "".to_string())
            },
            fillcolor = {
                self.fillcolor
                    .map(|v| format!(",fillcolor={}", v.to_dot_string()))
                    .unwrap_or_else(|| "".to_string())
            },
            fontcolor = {
                self.fontcolor
                    .map(|v| format!(",fontcolor={}", v.to_dot_string()))
                    .unwrap_or_else(|| "".to_string())
            },
            shape = {
                format!(
                    ",shape={}",
                    self.shape.unwrap_or(Shape::Box).to_string().to_lowercase()
                )
            },
            peripheries = {
                self.peripheries
                    .map(|v| format!(",peripheries={}", v))
                    .unwrap_or_else(|| "".to_string())
            },
            skew = {
                self.skew
                    .map(|v| format!(",skew={}", v))
                    .unwrap_or_else(|| "".to_string())
            },
            distortion = {
                self.distortion
                    .map(|v| format!(",distortion={}", v))
                    .unwrap_or_else(|| "".to_string())
            },
            sides = {
                self.sides
                    .map(|v| format!(",sides={}", v))
                    .unwrap_or_else(|| "".to_string())
            },
        )
        .unwrap();
    }
}

pub fn dot_string(string: &str) -> String {
    format!("\"{}\"", string.replace("\"", "\\\"").replace("\n", "\\n"))
}
