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

    public get structName() { return this.name + "Node"; }
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


const nodeType: TypeDef = ({
    wrap: (expr: string) => `NodeFactory::node(${expr})`,
    unwrap: (expr: string) => `${expr}.into()`,
    rustInName: "&'_ Node",
    rustOutName: "Node",
});

function rawPointerType(name: string): TypeDef {
    return ({
        rustInName: `const *${name}`,
        rustOutName: `const *${name}`,
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
            rustOutName: "isize",
            rustInName: "isize",
        }),
        "unsigned": ({
            wrap: (expr: string) => `${expr}`,
            unwrap: (expr: string) => expr,
            rustOutName: "isize",
            rustInName: "isize",
        }),
        "size_t": {
            wrap: (expr: string) => `${expr}`,
            unwrap: (expr: string) => expr,
            rustOutName: "usize",
            rustInName: "usize",
        },
        "ir_type*": rawPointerType("ir_type"),
        "ir_builtin_kind": rawPointerType("ir_builtin_kind"),
        "ir_relation": rawPointerType("ir_relation"),
        "cond_jmp_predicate": rawPointerType("cond_jmp_predicate"),
        "ir_tarval*": rawPointerType("ir_tarval"),
        "ir_volatility": rawPointerType("ir_volatility"),
        "ir_switch_table*": rawPointerType("ir_switch_table"),
        "ir_align": rawPointerType("ir_align"),
        "ir_mode*": rawPointerType("ir_mode"),
        "ir_cons_flags": rawPointerType("ir_cons_flags"),
        "ir_entity*": rawPointerType("ir_entity"),
        "ir_asm_constraint*": rawPointerType("ir_asm_constraint"),
        "ident**": rawPointerType("ident"),
        "ident*": rawPointerType("ident"),
    };
    const type = types[typeName];
    if (!type) {
        throw new Error(`Type "${typeName}" does not exist.`);
    }
    return type;
}

const content = fs.readFileSync("./data.json", "utf8");
const nodesJson = JSON.parse(content) as Data;
const nodes = nodesJson.map(n => new NodeImpl(n));

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
        w.line(`map.insert(op_code, ${node.create_name});`);
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
        w.indent(`fn ${node.create_name}(ir_node: *const ir_node) -> Node {`);
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
        // getter and setter for input nodes and attributes
        for (const input of node.attrOrInputs) {
            if (input.comment) { w.line(`/// Gets ${input.comment}.`); }
            w.indent(`pub fn ${input.getterName}(&self) -> ${nodeType.rustOutName} {`);
            w.line(`let ir_node = bindings::get_${node.name}_${input.name}(self.0);`);
            w.line(nodeType.wrap("ir_node"));
            w.unindent(`}`);
            w.line();

            if (input.comment) { w.line(`/// Sets ${input.comment}.`); }
            w.indent(`pub fn ${input.setterName}(&self, node: ${nodeType.rustInName}) {`);
            w.line(`let ir_node = ${nodeType.unwrap("node")}`);
            w.line(`bindings::set_${node.name}_${input.name}(self.0, ir_node)`);
            w.unindent(`}`);
            w.line();
        }
    }
    w.unindent(`}`);
    w.line();

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
