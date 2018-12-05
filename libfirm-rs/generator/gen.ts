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

type NodeArg = NodeAttr;

interface Node {
    name: string,
    structName: string,
    variantName: string;
    ins: NodeIn[],
    outs: any[],
    attrs?: NodeAttr[];
    doc: string,
    mode: string,
    arguments: NodeArg[],
    usesGraph?: boolean,
    // I don't really now what `block` is for
    block: string,
    constructor: boolean,
}
type Data = Node[];

class NodeImpl {
    public readonly name: string;
    public readonly doc: string;
    public readonly attrOrInputs: ReadonlyArray<NodeMemberImpl>;
    public readonly args: ReadonlyArray<NodeArgImpl>;
    public readonly usesGraph: boolean;
    public readonly block: boolean;
    public readonly hasConstructor: boolean;

    public get needsBlock() { return !this.block; }
    public get structName() { return this.name; }
    public get variantName() { return this.name; }
    public get create_name() { return `create_${this.name.toLowerCase()}`; }
    public get new_name() { return `new_${this.name.toLowerCase()}`; }

    constructor(node: Node) {
        this.name = node.name;
        this.doc = node.doc;
        this.attrOrInputs =
            node.ins.map(input => new NodeInputImpl(input))
            .concat(
                node.attrs ? node.attrs.map(a => new NodeAttrImpl(a)) : []
            );
        this.usesGraph = !!node.usesGraph;
        this.args = node.arguments.map(input => new NodeArgImpl(input));
        this.block = !!node.block;
        this.hasConstructor = node.constructor;
    }
}

function escapeRustKeywords(ident: string) {
    if (ident === "type") return "ty";
    if (ident === "true") return "true_";
    if (ident === "false") return "false_";
    if (ident === "loop") return "loop_";
    if (ident === "in") return "in_";
    return ident;
}

abstract class NodeMemberImpl {
    public readonly name: string;
    public readonly type: TypeDef;
    public readonly comment: string;

    public get argName(): string { return escapeRustKeywords(this.name); }
    public get getterName(): string { return escapeRustKeywords(this.name); }
    // for symmetry
    public get setterName(): string { return `set_${escapeRustKeywords(this.name)}`; }

    constructor(name: string, type: TypeDef, comment: string) {
        this.name = name;
        this.type = type;
        this.comment = comment;
    }
}

class NodeAttrImpl extends NodeMemberImpl {
    constructor(attr: NodeAttr) {
        super(attr.name, lookupType(attr.type), attr.comment);
    }
}

class NodeInputImpl extends NodeMemberImpl {
    constructor(attr: NodeIn) {
        super(attr.name, nodeType, attr.comment);
    }
}

class NodeArgImpl extends NodeMemberImpl {
    constructor(attr: NodeArg) {
        super(attr.name, lookupType(attr.type), attr.comment);
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

const notFoundTypes = new Array<string>();

function lookupType(typeName: string) {
    typeName = typeName.replace(/ /g, "");
    const types: { [key: string]: TypeDef } = {
        "ir_node*": nodeType,
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
        "ir_node*const*": rawType("NodeList")
    };
    const type = types[typeName];
    if (!type) {
        notFoundTypes.push(typeName);
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

if (notFoundTypes.length > 0) {
    throw new Error(`Types ${notFoundTypes.map(t => `"${t}"`).join(", ")} do not exist.`);
}

const w = new CodeMaker();
w.openFile("nodes_gen.rs");

w.line("use libfirm_rs_bindings as bindings;");
w.line("use std::collections::HashMap;");
w.line("use super::nodes::NodeTrait;");
w.line("use super::other::Graph;");
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
        // new function
        w.indent(`pub(crate) fn new(ir_node: ${ir_node_type}) -> Self {`);
        {
            w.indent(`if unsafe { bindings::is_${node.name}(ir_node) } == 0 {`);
            w.line(`panic!("given ir_node is not a ${node.name}");`);
            w.unindent(`}`);
            w.line(`${node.structName}(ir_node)`);
            w.unindent(`}`);
        }
        w.line();

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

function generateConstructionFunction(node: NodeImpl, context: "graph"|"block") {
    if (!node.hasConstructor) { return; }
    const params = new Array<string>();
    const args = new Array<string>();
    if (context === "block") {
        if (node.needsBlock) {
            args.push(`self.0`);
        }
        else { return; }
    }
    else if (context === "graph") {
        if (node.needsBlock) {
            params.push(`block: &'_ Block`);
            args.push(nodeType.unwrap(`block`));
        }
        else if (node.usesGraph) {
            args.push(`self.irg`);
        }
    }

    let statements = new Array<string>();
    let nextIsArray = false;
    for (const arg of node.args) {
        if (nextIsArray) {
            params.push(`${arg.argName}: Vec<Node>`);
            statements.push(`let ${arg.argName}: Vec<*mut bindings::ir_node> = ${arg.argName}.iter().map(|v| ${nodeType.unwrap(`v`)}).collect();`);
            args.push(`${arg.argName}.len() as i32`, `${arg.argName}.as_ptr()`);
            nextIsArray = false;
        } else if (arg.name === "arity") {
            nextIsArray = true;
        }
        else {
            params.push(`${arg.argName}: ${arg.type.rustInName}`);
            args.push(arg.type.unwrap(arg.argName));
        }
    }

    w.indent(`pub fn ${node.new_name}(&self, ${params.join(", ")}) -> ${node.structName} {`);
    for (const line of statements) { w.line(line); }
    w.line(`let ir_node = unsafe { bindings::new_r_${node.name}(${args.join(", ")}) };`);
    w.line(`${node.structName}::new(ir_node)`);
    w.unindent(`}`);
}

// graph node construction functions
w.indent(`impl Graph {`);
{
    for (const node of nodes) {
        generateConstructionFunction(node, "graph");
    }
}
w.unindent("}");
w.line();

w.indent(`impl Block {`);
{
    for (const node of nodes) {
        generateConstructionFunction(node, "block");
    }
}
w.unindent("}");
w.line();

w.closeFile("nodes_gen.rs");
w.save("../src/");
