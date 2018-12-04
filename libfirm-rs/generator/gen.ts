import fs from "fs";
import { CodeMaker } from 'codemaker';

interface NodeIn {
    comment: string,
    name: string,
}

interface NodeAttr {
    name: string,
    type: string,
    comment: string,
}

interface Node {
    name: string,
    structName: string,
    variantName: string;
    ins: NodeIn[],
    outs: any[],
    attrs?: NodeAttr[];
    doc: string,
    mode: string,
    arguments: any[],
}
type Data = Node[];

class NodeImpl {
    public readonly name: string;
    public readonly doc: string;
    public readonly attrOrInputs: ReadonlyArray<NodeAttrOrInputImpl>;

    public get structName() { return this.name; }
    public get variantName() { return this.name; }
    public get create_name() { return `create_${this.name.toLowerCase()}` }

    constructor(node: Node) {
        this.name = node.name;
        this.doc = node.doc;
        this.attrOrInputs =
            node.ins.map(input => new NodeInputImpl(input))
            .concat(
                node.attrs ? node.attrs.map(a => new NodeAttrImpl(a)) : []
            );
    }
}

function escapeRustKeywords(ident: string) {
    if (ident === "type") return "ty";
    if (ident === "true") return "true_";
    if (ident === "false") return "false_";
    if (ident === "loop") return "loop_";
    if (ident === "false") return "false_";
    return ident;
}

abstract class NodeAttrOrInputImpl {
    public readonly name: string;
    public readonly type: TypeDef;
    public readonly comment: string;

    public get getterName(): string { return escapeRustKeywords(this.name); }
    // for symmetry
    public get setterName(): string { return `set_${escapeRustKeywords(this.name)}`; }

    constructor(name: string, type: TypeDef, comment: string) {
        this.name = name;
        this.type = type;
        this.comment = comment;
    }
}

class NodeAttrImpl extends NodeAttrOrInputImpl {
    constructor(attr: NodeAttr) {
        super(attr.name, lookupType(attr.type), attr.comment);
    }
}

class NodeInputImpl extends NodeAttrOrInputImpl {
    constructor(attr: NodeIn) {
        super(attr.name, nodeType, attr.comment);
    }
}

interface TypeDef {
    wrap(expr: string): string,
    unwrap(expr: string): string,
    rustInName: string,
    rustOutName: string,
}

const internal_ir_node = "internal_ir_node";

const nodeType: TypeDef = ({
    wrap: (expr: string) => `NodeFactory::node(${expr})`,
    unwrap: (expr: string) => `${expr}.${internal_ir_node}()`,
    rustInName: "&'_ Node",
    rustOutName: "Node",
});

function rawPointerType(name: string): TypeDef {
    return rawType(`*mut bindings::${name}`);
}

function rawType(name: string): TypeDef {
    return ({
        rustInName: `${name}`,
        rustOutName: `${name}`,
        unwrap: expr => expr,
        wrap: expr => expr,
    });
}

function lookupType(typeName: string) {
    const types: { [key: string]: TypeDef } = {
        "ir_node *": nodeType,
        "int": ({
            wrap: (expr: string) => `${expr}`,
            unwrap: (expr: string) => expr,
            rustOutName: `i32`,
            rustInName: "i32",
        }),
        "unsigned": ({
            wrap: (expr: string) => `${expr}`,
            unwrap: (expr: string) => expr,
            rustOutName: "::std::os::raw::c_uint",
            rustInName: "::std::os::raw::c_uint",
        }),
        "size_t": {
            wrap: (expr: string) => `${expr}`,
            unwrap: (expr: string) => expr,
            rustOutName: "usize",
            rustInName: "usize",
        },
        "ir_builtin_kind": rawType("bindings::ir_builtin_kind::Type"),
        "ir_relation": rawType("bindings::ir_relation::Type"),
        "cond_jmp_predicate": rawType("bindings::cond_jmp_predicate::Type"),
        "ir_volatility": rawType("bindings::ir_volatility::Type"),
        "ir_cons_flags": rawType("bindings::ir_cons_flags::Type"),
        "ir_align": rawType("bindings::ir_align::Type"),
        "ir_entity*": rawPointerType("ir_entity"),
        "ir_type*": rawPointerType("ir_type"),
        "ir_mode*": rawPointerType("ir_mode"),
        "ir_tarval*": rawPointerType("ir_tarval"),
        "ir_switch_table*": rawPointerType("ir_switch_table"),
        "ir_asm_constraint*": rawPointerType("ir_asm_constraint"),
        "ident*": rawPointerType("ident"),
        "ident**": rawType("*mut *mut bindings::ident"),
    };
    const type = types[typeName];
    if (!type) {
        throw new Error(`Type "${typeName}" does not exist.`);
    }
    return type;
}

const ir_node_type = "*mut bindings::ir_node";
const content = fs.readFileSync("./data.json", "utf8");
const nodesJson = JSON.parse(content) as Data;
const nodes = nodesJson.map(n => {
    if (n.name === "ASM") return null!;
    return new NodeImpl(n);
}).filter(n => n !== null);


const w = new CodeMaker();
w.openFile("nodes_gen.rs");

w.line("use libfirm_rs_bindings as bindings;");
w.line("use std::collections::HashMap;");
w.line("use super::nodes::NodeTrait;");
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

    w.indent(`impl NodeTrait for Node {`);
    w.indent(`fn ${internal_ir_node}(&self) -> ${ir_node_type} {`);
    w.indent(`match self {`);
    for (const node of nodes) {
        w.line(`Node::${node.variantName}(node) => node.${internal_ir_node}(),`);
    }
    w.unindent(`}`);
    w.unindent(`}`);
    w.unindent("}");
    w.line();
}

// generate NodeFactory to map ir_node to Node
{
    w.line("type NodeFactoryFn = fn(*mut bindings::ir_node) -> Node;");
    w.line(`pub struct NodeFactory(HashMap<u32, NodeFactoryFn>);`);
    w.indent("impl NodeFactory {");
    w.indent("pub fn new() -> Self {");
    w.line(`let mut map = HashMap::<u32, NodeFactoryFn>::new();`);
    w.indent(`unsafe {`);
    for (const node of nodes) {
        w.line(`let op = bindings::get_op_${node.name}();`);
        w.line(`let op_code = bindings::get_op_code(op);`);
        w.line(`map.insert(op_code, Self::${node.create_name});`);
    }
    w.unindent(`}`);
    w.line(`NodeFactory(map)`);
    w.unindent("}");
    w.line();

    w.indent(`pub fn node(ir_node: ${ir_node_type}) -> Node {`);
    w.line(`Self::new().create(ir_node)`);
    w.unindent("}");
    w.line();

    w.indent(`pub fn create(&self, ir_node: ${ir_node_type}) -> Node {`);
    w.line(`let op_code = unsafe { bindings::get_irn_opcode(ir_node) };`);
    w.line(`let f = self.0.get(&op_code).unwrap();`);
    w.line(`f(ir_node)`);
    w.unindent("}");
    w.line();

    for (const node of nodes) {
        w.indent(`fn ${node.create_name}(ir_node: ${ir_node_type}) -> Node {`);
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
    w.line(`pub struct ${node.structName}(${ir_node_type});`);
    w.line();

    w.indent(`impl ${node.structName} {`);
    {
        // getter and setter for input nodes and attributes
        for (const input of node.attrOrInputs) {
            if (input.comment) { w.line(`/// Gets ${input.comment}.`); }
            w.indent(`pub fn ${input.getterName}(&self) -> ${input.type.rustOutName} {`);
            w.line(`let unwrapped = unsafe { bindings::get_${node.name}_${input.name}(self.0) };`);
            w.line(input.type.wrap("unwrapped"));
            w.unindent(`}`);
            w.line();

            if (input.comment) { w.line(`/// Sets ${input.comment}.`); }
            w.indent(`pub fn ${input.setterName}(&self, val: ${input.type.rustInName}) {`);
            w.line(`let unwrapped = ${input.type.unwrap("val")};`);
            w.line(`unsafe { bindings::set_${node.name}_${input.name}(self.0, unwrapped); }`);
            w.unindent(`}`);
            w.line();
        }
    }
    w.unindent(`}`);
    w.line();

    // into Node
    w.indent(`impl Into<Node> for ${node.structName} {`);
    w.indent(`fn into(self) -> Node {`);
    w.line(`Node::${node.variantName}(self)`);
    w.unindent(`}`);
    w.unindent("}");
    w.line();

    // node Trait
    w.indent(`impl NodeTrait for ${node.structName} {`);
    w.indent(`fn ${internal_ir_node}(&self) -> ${ir_node_type} {`);
    w.line(`self.0`);
    w.unindent(`}`);
    w.unindent("}");
    w.line();
}


w.closeFile("nodes_gen.rs");
w.save("../src/");
