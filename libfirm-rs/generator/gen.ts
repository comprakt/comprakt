import fs from "fs";
import { CodeMaker } from 'codemaker';

interface NodeIn {
    comment: string,
    name: string,
}

interface Node {
    name: string,
    structName: string,
    variantName: string;
    ins: NodeIn[],
    outs: any[],
    doc: string,
    mode: string,
    arguments: any[],
}
type Data = Node[];

const content = fs.readFileSync("./data.json", "utf8");
const nodes = JSON.parse(content) as Data;

for (const node of nodes) {
    node.structName = node.name + "Node";
    node.variantName = node.name;

}

const w = new CodeMaker();
w.openFile("nodes_gen.rs");

w.line("use libfirm_rs_bindings as bindings;");
w.line("use std::collections::HashMap;");
w.line();


// generate Node enum
{
    w.line("#[derive(Debug)]");
    w.indent("pub enum Node {");
    for (const node of nodes) {
        w.line(`${node.variantName}(${node.structName}),`);
    }
    w.unindent("}");
    w.line();

    w.indent(`impl Into<*const ir_node> for Node {`);
    w.indent(`fn into(self) -> *const ir_node {`);
    w.indent(`match self {`);
    for (const node of nodes) {
        w.line(`${node.variantName}(node) => node.into(),`);
    }
    w.unindent(`}`);
    w.unindent(`}`);
    w.unindent("}");
}

// generate NodeFactory to map ir_node to Node
{
    w.line("struct NodeFactory(HashMap<i32, Fn(*const ir_node) -> Node>);");
    w.indent("impl NodeFactory {");
    w.indent("pub fn new() -> Self {");
    w.line(`let map = HashMap::new();`);
    for (const node of nodes) {
        w.line(`let op = bindings::get_op_${node.name}();`);
        w.line(`let op_code = bindings::get_op_code(op);`);
        w.line(`map.insert(op_code, create_${node.name.toLowerCase()});`);
    }
    w.line(`NodeFactory(map)`);
    w.unindent("}");
    w.line();

    w.indent("pub fn node(*const ir_node) -> Node {");
    w.line(`Self::new().create(ir_node)`);
    w.unindent("}");
    w.line();

    w.indent("pub fn create(&self, *const ir_node) -> Node {");
    w.line(`let op_code = bindings::get_irn_opcode(ir_node);`);
    w.line(`let f = self.0.get(op_code).unwrap();`);
    w.line(`f()`);
    w.unindent("}");
    w.line();

    for (const node of nodes) {
        w.indent(`fn create_${node.name.toLowerCase()}(ir_node: *const ir_node) -> Node {`);
        w.line(`Node::${node.name}(${node.structName}(ir_node))`);
        w.unindent("}");
    }

    w.unindent("}");
    w.line();
}

for (const node of nodes) {
    // Node struct
    if (node.doc) {
        for (const line of node.doc.split("\n")) {
            w.line(`/// ${line.trim()}`);
        }
    }
    w.line("#[derive(Debug)]");
    w.line(`pub struct ${node.structName}(*const ir_node);`);
    w.line();

    w.indent(`impl ${node.structName} {`);
    {
        // getter and setter
        for (const input of node.ins) {
            if (input.comment) { w.line(`/// Gets the ${input.comment}.`); }
            w.indent(`pub fn ${input.name}(&self) -> Node {`);
            w.line(`let ir_node = bindings::get_${node.name}_${input.name}(self.0);`);
            w.line(`NodeFactory::node(ir_node)`);
            w.unindent(`}`);

            if (input.comment) { w.line(`/// Sets the ${input.comment}.`); }
            w.indent(`pub fn set_${input.name}(&self, node: &'_ Node) {`);
            w.line(`bindings::set_${node.name}_${input.name}(self.0, node.0)`);
            w.unindent(`}`);
        }
    }
    w.unindent(`}`);

    // intos
    w.indent(`impl Into<Node> for ${node.structName} {`);
    w.indent(`fn into(self) -> Node {`);
    w.line(`Node::${node.variantName}(self)`);
    w.unindent(`}`);
    w.unindent("}");
    w.line();

    w.indent(`impl Into<*const ir_node> for ${node.structName} {`);
    w.indent(`fn into(self) -> *const ir_node {`);
    w.line(`self.0`);
    w.unindent(`}`);
    w.unindent("}");
    w.line();
}


w.closeFile("nodes_gen.rs");
w.save("../src/");
