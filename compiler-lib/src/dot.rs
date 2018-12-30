//! Converts FIRM or LIR graphs into dot graph description language for easy
//! debugging.
use crate::firm::FirmProgram;
use libfirm_rs::{
    nodes::{Node, NodeTrait},
    Graph,
};
use serde_derive::Serialize;
use std::{
    collections::hash_map::HashMap,
    fs::File,
    hash::BuildHasher,
    io::{BufWriter, Write},
    path::PathBuf,
};

#[derive(Debug, Clone, Serialize)]
pub struct GraphState {
    pub name: String,
    dot_content: String,
}

pub fn default_label(node: Node) -> Label {
    Label::from_text(node.node_id().to_string(), format!("{:?}", node))
}

pub trait GraphData {
    /// Transform the object into a map of unique function names
    /// to graph information in dot format
    fn graph_data<T>(&self, label_maker: &T) -> HashMap<String, GraphState>
    where
        Self: Sized,
        T: LabelMaker;
}

impl<'a, 'b> GraphData for FirmProgram<'a, 'b> {
    fn graph_data<T>(&self, label_maker: &T) -> HashMap<String, GraphState>
    where
        Self: Sized,
        T: LabelMaker,
    {
        let mut dot_files = HashMap::new();

        for method in self.methods.values() {
            let class = method.borrow().owning_class.upgrade().unwrap();
            if let Some(graph) = method.borrow().graph {
                let internal_name = method.borrow().entity.name_string();
                dot_files.insert(
                    internal_name.clone(),
                    GraphState {
                        name: format!(
                            "{}.{}",
                            class.borrow().def.name.to_string(),
                            method.borrow().def.name.to_string()
                        ),
                        dot_content: graph.into_dot_format_string(&internal_name, label_maker),
                    },
                );
            }
        }

        dot_files
    }
}

impl<'a, 'b> GraphData for Graph {
    fn graph_data<T>(&self, label_maker: &T) -> HashMap<String, GraphState>
    where
        Self: Sized,
        T: LabelMaker,
    {
        let name = self.entity().name_string();
        let mut dot_files = HashMap::new();
        dot_files.insert(
            name.clone(),
            GraphState {
                name: name.clone(),
                dot_content: self.into_dot_format_string(&name.clone(), label_maker),
            },
        );
        dot_files
    }
}

impl Dot for Graph {
    fn into_dot_format<T>(&self, writer: &mut dyn Write, graph_name: &str, label_maker: &T)
    where
        Self: Sized,
        T: LabelMaker,
    {
        let mut list = Vec::new();
        self.walk_topological(|node| {
            list.push(*node);
        });

        writeln!(writer, "digraph {} {{", dot_string(graph_name)).unwrap();
        for node in list.iter() {
            let label = label_maker.label_for_node(*node);
            label.write_dot_format(writer);

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
    fn label_for_node(&self, node: Node) -> Label;
}

impl<F> LabelMaker for F
where
    F: Fn(Node) -> Label,
{
    fn label_for_node(&self, node: Node) -> Label {
        self(node)
    }
}

impl<S: BuildHasher> LabelMaker for HashMap<Node, Label, S> {
    fn label_for_node(&self, node: Node) -> Label {
        self.get(&node)
            .cloned()
            .unwrap_or_else(|| Label::from_text(node.node_id().to_string(), "".to_string()))
    }
}

pub trait Dot {
    fn dump_as_dot_file<T>(&self, filename: &PathBuf, graph_name: &str, data: &T)
    where
        Self: Sized,
        T: LabelMaker,
    {
        let write_file = File::create(filename).unwrap();
        let mut writer = BufWriter::new(&write_file);
        self.into_dot_format(&mut writer, graph_name, data)
    }

    fn into_dot_format_string<T>(&self, graph_name: &str, data: &T) -> String
    where
        Self: Sized,
        T: LabelMaker,
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

    fn into_dot_format<T>(&self, writer: &mut dyn Write, graph_name: &str, data: &T)
    where
        Self: Sized,
        T: LabelMaker;
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

#[derive(Debug, Clone, Copy)]
pub enum Color {
    Hsv(f32, f32, f32),
    Rgb(u8, u8, u8),
    X11Color(X11Color),
}

impl Color {
    fn to_dot_string(&self) -> String {
        match self {
            Color::Hsv(h, s, v) => format!("\"{} {} {}\"", h, s, v),
            Color::Rgb(r, g, b) => format!("\"#{:02x}{:02x}{:02x}\"", r, g, b),
            Color::X11Color(color) => color.to_string().to_lowercase(),
        }
    }
}

#[derive(Display, Debug, Clone, Copy)]
pub enum X11Color {
    AliceBlue,
    AntiqueWhite,
    AntiqueWhite1,
    AntiqueWhite2,
    AntiqueWhite3,
    AntiqueWhite4,
    Aquamarine,
    Aquamarine1,
    Aquamarine2,
    Aquamarine3,
    Aquamarine4,
    Azure,
    Azure1,
    Azure2,
    Azure3,
    Azure4,
    Beige,
    Bisque,
    Bisque1,
    Bisque2,
    Bisque3,
    Bisque4,
    Black,
    BlanchedAlmond,
    Blue,
    Blue1,
    Blue2,
    Blue3,
    Blue4,
    BlueViolet,
    Brown,
    Brown1,
    Brown2,
    Brown3,
    Brown4,
    Burlywood,
    Burlywood1,
    Burlywood2,
    Burlywood3,
    Burlywood4,
    CadetBlue,
    CadetBlue1,
    CadetBlue2,
    CadetBlue3,
    CadetBlue4,
    Chartreuse,
    Chartreuse1,
    Chartreuse2,
    Chartreuse3,
    Chartreuse4,
    Chocolate,
    Chocolate1,
    Chocolate2,
    Chocolate3,
    Chocolate4,
    Coral,
    Coral1,
    Coral2,
    Coral3,
    Coral4,
    CornFlowerBlue,
    CornSilk,
    CornSilk1,
    CornSilk2,
    CornSilk3,
    CornSilk4,
    Crimson,
    Cyan,
    Cyan1,
    Cyan2,
    Cyan3,
    Cyan4,
    DarkGoldenrod,
    DarkGoldenrod1,
    DarkGoldenrod2,
    DarkGoldenrod3,
    DarkGoldenrod4,
    DarkGreen,
    Darkkhaki,
    DarkOliveGreen,
    DarkOliveGreen1,
    DarkOliveGreen2,
    DarkOliveGreen3,
    DarkOliveGreen4,
    DarkOrange,
    DarkOrange1,
    DarkOrange2,
    DarkOrange3,
    DarkOrange4,
    DarkOrchid,
    DarkOrchid1,
    DarkOrchid2,
    DarkOrchid3,
    DarkOrchid4,
    DarkSalmon,
    DarkSeaGreen,
    DarkSeaGreen1,
    DarkSeaGreen2,
    DarkSeaGreen3,
    DarkSeaGreen4,
    DarkSlateBlue,
    DarkSlateGray,
    DarkSlateGray1,
    DarkSlateGray2,
    DarkSlateGray3,
    DarkSlateGray4,
    DarkTurquoise,
    DarkViolet,
    DeepPink,
    DeepPink1,
    DeepPink2,
    DeepPink3,
    DeepPink4,
    DeepSkyBlue,
    DeepSkyBlue1,
    DeepSkyBlue2,
    DeepSkyBlue3,
    DeepSkyBlue4,
    DimGray,
    DodgerBlue,
    DodgerBlue1,
    DodgerBlue2,
    DodgerBlue3,
    DodgerBlue4,
    Firebrick,
    Firebrick1,
    Firebrick2,
    Firebrick3,
    Firebrick4,
    FloralWhite,
    ForestGreen,
    Gainsboro,
    GhostWhite,
    Gold,
    Gold1,
    Gold2,
    Gold3,
    Gold4,
    Goldenrod,
    Goldenrod1,
    Goldenrod2,
    Goldenrod3,
    Goldenrod4,
    Gray,
    Gray0,
    Gray1,
    Gray2,
    Gray3,
    Gray4,
    Gray5,
    Gray6,
    Gray7,
    Gray8,
    Gray9,
    Gray10,
    Gray11,
    Gray12,
    Gray13,
    Gray14,
    Gray15,
    Gray16,
    Gray17,
    Gray18,
    Gray19,
    Gray20,
    Gray21,
    Gray22,
    Gray23,
    Gray24,
    Gray25,
    Gray26,
    Gray27,
    Gray28,
    Gray29,
    Gray30,
    Gray31,
    Gray32,
    Gray33,
    Gray34,
    Gray35,
    Gray36,
    Gray37,
    Gray38,
    Gray39,
    Gray40,
    Gray41,
    Gray42,
    Gray43,
    Gray44,
    Gray45,
    Gray46,
    Gray47,
    Gray48,
    Gray49,
    Gray50,
    Gray51,
    Gray52,
    Gray53,
    Gray54,
    Gray55,
    Gray56,
    Gray57,
    Gray58,
    Gray59,
    Gray60,
    Gray61,
    Gray62,
    Gray63,
    Gray64,
    Gray65,
    Gray66,
    Gray67,
    Gray68,
    Gray69,
    Gray70,
    Gray71,
    Gray72,
    Gray73,
    Gray74,
    Gray75,
    Gray76,
    Gray77,
    Gray78,
    Gray79,
    Gray80,
    Gray81,
    Gray82,
    Gray83,
    Gray84,
    Gray85,
    Gray86,
    Gray87,
    Gray88,
    Gray89,
    Gray90,
    Gray91,
    Gray92,
    Gray93,
    Gray94,
    Gray95,
    Gray96,
    Gray97,
    Gray98,
    Gray99,
    Gray100,
    Green,
    Green1,
    Green2,
    Green3,
    Green4,
    GreenYellow,
    HoneyDew,
    HoneyDew1,
    HoneyDew2,
    HoneyDew3,
    HoneyDew4,
    HotPink,
    HotPink1,
    HotPink2,
    HotPink3,
    HotPink4,
    IndianRed,
    IndianRed1,
    IndianRed2,
    IndianRed3,
    IndianRed4,
    Indigo,
    Ivory,
    Ivory1,
    Ivory2,
    Ivory3,
    Ivory4,
    Khaki,
    Khaki1,
    Khaki2,
    Khaki3,
    Khaki4,
    Lavender,
    LavenderBlush,
    LavenderBlush1,
    LavenderBlush2,
    LavenderBlush3,
    LavenderBlush4,
    LawnGreen,
    LemonChiffon,
    LemonChiffon1,
    LemonChiffon2,
    LemonChiffon3,
    LemonChiffon4,
    LightBlue,
    LightBlue1,
    LightBlue2,
    LightBlue3,
    LightBlue4,
    LightCoral,
    LightCyan,
    LightCyan1,
    LightCyan2,
    LightCyan3,
    LightCyan4,
    LightGoldenrod,
    LightGoldenrod1,
    LightGoldenrod2,
    LightGoldenrod3,
    LightGoldenrod4,
    LightGoldenrodYellow,
    LightGray,
    LightPink,
    LightPink1,
    LightPink2,
    LightPink3,
    LightPink4,
    LightSalmon,
    LightSalmon1,
    LightSalmon2,
    LightSalmon3,
    LightSalmon4,
    LightSeaGreen,
    LightSkyBlue,
    LightSkyBlue1,
    LightSkyBlue2,
    LightSkyBlue3,
    LightSkyBlue4,
    LightSlateBlue,
    LightSlateGray,
    LightSteelBlue,
    LightSteelBlue1,
    LightSteelBlue2,
    LightSteelBlue3,
    LightSteelBlue4,
    LightYellow,
    LightYellow1,
    LightYellow2,
    LightYellow3,
    LightYellow4,
    LimeGreen,
    Linen,
    Magenta,
    Magenta1,
    Magenta2,
    Magenta3,
    Magenta4,
    Maroon,
    Maroon1,
    Maroon2,
    Maroon3,
    Maroon4,
    MediumAquamarine,
    MediumBlue,
    MediumOrchid,
    MediumOrchid1,
    MediumOrchid2,
    MediumOrchid3,
    MediumOrchid4,
    MediumPurple,
    MediumPurple1,
    MediumPurple2,
    MediumPurple3,
    MediumPurple4,
    MediumSeaGreen,
    MediumSlateBlue,
    MediumSpringGreen,
    MediumTurquoise,
    MediumVioletRed,
    MidnightBlue,
    MintCream,
    MistyRose,
    MistyRose1,
    MistyRose2,
    MistyRose3,
    MistyRose4,
    Moccasin,
    NavajoWhite,
    NavajoWhite1,
    NavajoWhite2,
    NavajoWhite3,
    NavajoWhite4,
    Navy,
    NavyBlue,
    OldLace,
    OliveDrab,
    OliveDrab1,
    OliveDrab2,
    OliveDrab3,
    OliveDrab4,
    Orange,
    Orange1,
    Orange2,
    Orange3,
    Orange4,
    OrangeRed,
    OrangeRed1,
    OrangeRed2,
    OrangeRed3,
    OrangeRed4,
    Orchid,
    Orchid1,
    Orchid2,
    Orchid3,
    Orchid4,
    PaleGoldenrod,
    PaleGreen,
    PaleGreen1,
    PaleGreen2,
    PaleGreen3,
    PaleGreen4,
    PaleTurquoise,
    PaleTurquoise1,
    PaleTurquoise2,
    PaleTurquoise3,
    PaleTurquoise4,
    PaleVioletRed,
    PaleVioletRed1,
    PaleVioletRed2,
    PaleVioletRed3,
    PaleVioletRed4,
    PapayaWhip,
    PeachPuff,
    PeachPuff1,
    PeachPuff2,
    PeachPuff3,
    PeachPuff4,
    Peru,
    Pink,
    Pink1,
    Pink2,
    Pink3,
    Pink4,
    Plum,
    Plum1,
    Plum2,
    Plum3,
    Plum4,
    PowderBlue,
    Purple,
    Purple1,
    Purple2,
    Purple3,
    Purple4,
    Red,
    Red1,
    Red2,
    Red3,
    Red4,
    RosyBrown,
    RosyBrown1,
    RosyBrown2,
    RosyBrown3,
    RosyBrown4,
    RoyalBlue,
    RoyalBlue1,
    RoyalBlue2,
    RoyalBlue3,
    RoyalBlue4,
    SaddleBrown,
    Salmon,
    Salmon1,
    Salmon2,
    Salmon3,
    Salmon4,
    SandyBrown,
    SeaGreen,
    SeaGreen1,
    SeaGreen2,
    SeaGreen3,
    SeaGreen4,
    SeaShell,
    SeaShell1,
    SeaShell2,
    SeaShell3,
    SeaShell4,
    Sienna,
    Sienna1,
    Sienna2,
    Sienna3,
    Sienna4,
    SkyBlue,
    SkyBlue1,
    SkyBlue2,
    SkyBlue3,
    SkyBlue4,
    SlateBlue,
    SlateBlue1,
    SlateBlue2,
    SlateBlue3,
    SlateBlue4,
    SlateGray,
    SlateGray1,
    SlateGray2,
    SlateGray3,
    SlateGray4,
    Snow,
    Snow1,
    Snow2,
    Snow3,
    Snow4,
    SpringGreen,
    SpringGreen1,
    SpringGreen2,
    SpringGreen3,
    SpringGreen4,
    SteelBlue,
    SteelBlue1,
    SteelBlue2,
    SteelBlue3,
    SteelBlue4,
    Tan,
    Tan1,
    Tan2,
    Tan3,
    Tan4,
    Thistle,
    Thistle1,
    Thistle2,
    Thistle3,
    Thistle4,
    Tomato,
    Tomato1,
    Tomato2,
    Tomato3,
    Tomato4,
    Transparent,
    Turquoise,
    Turquoise1,
    Turquoise2,
    Turquoise3,
    Turquoise4,
    Violet,
    VioletRed,
    VioletRed1,
    VioletRed2,
    VioletRed3,
    VioletRed4,
    Wheat,
    Wheat1,
    Wheat2,
    Wheat3,
    Wheat4,
    White,
    WhiteSmoke,
    Yellow,
    Yellow1,
    Yellow2,
    Yellow3,
    Yellow4,
    YellowGreen,
}

impl From<X11Color> for Color {
    fn from(color: X11Color) -> Self {
        Color::X11Color(color)
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
    /* Must specify the record shape with a Label.
     * Record
     * MRecord */
}

#[derive(Default, Debug, Clone)]
pub struct Label {
    text: LabelText,
    id: String,
    style: Option<Style>,
    fillcolor: Option<Color>,
    fontcolor: Option<Color>,
    shape: Option<Shape>,
    sides: Option<u32>,
    peripheries: Option<u32>,
    distortion: Option<f32>,
    skew: Option<f32>,
}

impl Label {
    pub fn from_text(id: String, text: String) -> Label {
        Label::default().text(text).id(id)
    }

    pub fn from_html(id: String, text: String) -> Label {
        Label::default().html(text).id(id)
    }

    pub fn style(mut self, val: Style) -> Self {
        self.style = Some(val);
        self
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

    pub fn id(mut self, id: String) -> Self {
        self.id = id;
        self
    }

    pub fn write_dot_format(&self, writer: &mut dyn Write) {
        writeln!(
            writer,
            "{id} [label={label}{style}{fillcolor}{fontcolor}{shape}\
             {peripheries}{skew}{distortion}{sides}];",
            id = self.id,
            label = match self.text {
                // TODO: escape
                LabelText::Plain(ref text) => dot_string(&text),
                LabelText::Html(ref text) => format!("<{}>", text),
            },
            style = {
                self.style
                    .map(|v| format!(",style={}", v.to_string().to_lowercase()))
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

fn dot_string(string: &str) -> String {
    format!("\"{}\"", string.replace("\"", "'").replace("\n", "\\n"))
}
