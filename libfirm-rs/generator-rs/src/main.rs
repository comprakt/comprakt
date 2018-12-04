#![feature(const_fn)]

use serde_json;
use serde_derive::{Serialize, Deserialize};

use std::error::Error;
use std::fs::File;

#[derive(Serialize, Deserialize)]
struct Node {
    name: String,
    ins: Vec<InOut>,
    outs: Option<Vec<InOut>>,
    attrs: Option<Vec<Attr>>,
    doc: String,
    mode: Option<String>,
}

impl Node {
    fn struct_name(&self) -> String {
        self.name
    }
    fn variant_name(&self) -> String {
        self.name
    }
    fn create_name(&self) -> String {
        format!("create_{}", self.name.to_lowercase())
    }
}

#[derive(Serialize, Deserialize)]
struct InOut {
    comment: String,
    name: String,
}

#[derive(Serialize, Deserialize)]
struct Attr {
    comment: String,
    name: String,
    #[serde(rename="type")]
    ty: String,
}

trait InOutAttr {
    fn getter_name(&self) -> String;
    fn setter_name(&self) -> String;
}

impl InOutAttr for InOut {
    fn getter_name(&self) -> String {
        escape_rust_keywords(&self.name).to_string()
    }

    fn setter_name(&self) -> String {
        format!("set_{}", escape_rust_keywords(&self.name))
    }
}

impl InOutAttr for Attr {
    fn getter_name(&self) -> String {
        escape_rust_keywords(&self.name).to_string()
    }

    fn setter_name(&self) -> String {
        format!("set_{}", escape_rust_keywords(&self.name))
    }
}

const INTERNAL_IR_NODE: &str = "internal_ir_node";

struct TypeDef {
    wrapped: String,
    unwrapped: String,
    rust_in_name: String,
    rust_out_name: String,
}

const NodeType


fn raw_pointer_type(name: &str) -> TypeDef {
    return raw_type(&format!("*mut bindings::{}", name))
}

fn raw_type(name: &str) -> TypeDef {
    TypeDef()
}

fn lookup_type(types: HashMap<&str, TypeDef>, name: &str) -> TypeDef {
    types.get(name).expect("type name not registered")
}

fn escape_rust_keywords(keyword: &str) -> &str {
    match keyord {
        "type" => "ty",
        "true" => "true_",
        "false" => "false_",
        "loop" => "loop_",
        _ => keyword,
    }
}

fn main() -> Result<(), Box<Error>> {
    let file = File::open("data.json")?;
    let data: Vec<Node> = serde_json::from_reader(file)?;

    Ok(())
}
