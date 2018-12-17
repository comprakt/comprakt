import fs from "fs";
import { CodeMaker } from 'codemaker';
import { execSync, exec } from 'child_process';

interface NodeIn {
    comment: string,
    name: string,
}

type NodeOut = NodeIn;

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
    outs: NodeOut[],
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
    public readonly outs: ReadonlyArray<NodeOutputImpl>;

    public get isCond() { return this.name === "Cond"; }
    public get isProj() { return this.name === "Proj"; }

    public get needsBlock() { return !this.block; }
    public get structName() { return this.name; }
    public get variantName() { return this.name; }
    public get create_name() { return `create_${this.name.toLowerCase()}`; }
    public get new_name() { return `new_${this.name.toLowerCase()}`; }
    public get is_name() { return `is_${this.name.toLowerCase()}`; }
    public get as_name() { return `as_${this.name.toLowerCase()}`; }

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
        const self = this;
        this.outs = (node.outs || []).map((o, idx) => new NodeOutputImpl(self, o, idx));
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

class NodeOutputImpl extends NodeMemberImpl {
    public readonly idx: number;

    public get out_proj_fnName(): string {
        return `out_proj_${this.name.toLowerCase()}`;
    }

    public get new_proj_fnName(): string {
        return `new_proj_${this.name.toLowerCase()}`;
    }

    public get isCondTrue(): boolean { return this.parent.isCond && this.name === "true"; }
    public get isCondFalse(): boolean { return this.parent.isCond && this.name === "false"; }

    public projKind_variantCtor(nodeExpr: string): string {
        const extra = (this.isCondFalse || this.isCondTrue) ? `${this.isCondTrue}, ` : ``;
        return `${this.projKind_variantName}(${extra}${nodeExpr})`;
    }

    public projKind_variantDecl(): { comment: string, decl: string } | undefined {
        const extra = this.isCondTrue ? `bool, ` : ``;
        const comment = this.isCondTrue ? `control flow if operand is "false" or "true"` : this.comment;
        if (this.isCondFalse) { return undefined; }
        return { comment, decl: `${this.projKind_variantName}(${extra}${this.parent.structName})` };
    }

    private get projKind_variantName(): string {
        if (this.isCondTrue || this.isCondFalse) { return "Cond_Val"; }
        const parts = this.name.split("_").map(p => p[0].toUpperCase() + p.substr(1).toLowerCase());
        const str = parts.join("");
        return `${this.parent.name}_${str}`;
    }

    constructor(public readonly parent: NodeImpl, attr: NodeIn, idx: number) {
        super(attr.name, nodeType, attr.comment);
        this.idx = idx;
    }

    public guessModeType(): string|undefined {
        const projectionModes: { [name: string]: { [projName: string]: string|undefined }|undefined } = {
            "Cond": {
                "false": "X",
                "true": "X",
            }
        };

        const nodeModes = projectionModes[this.parent.name];
        let outMode = nodeModes && nodeModes[this.name];

        if (!outMode) {
            const modes: { [name: string]: string|undefined } = {
                "m": "M",
                "t": "T",
                "x": "X",
            };
            for (const entry of this.name.split(`_`)) {
                outMode = modes[entry.toLowerCase()];
                if (outMode) break;
            }
        }
        if (!outMode) return undefined;

        return `bindings::mode::${outMode}`;
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
    rustInName: "Node",
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

function intoRustType(name: string): TypeDef {
    return {
        rustInName: name,
        rustOutName: name,
        wrap: arg => `${arg}.into()`,
        unwrap: arg => `${arg}.into()`,
    };
}

function lookupType(typeName: string) {
    typeName = typeName.replace(/ /g, "");
    const types: { [key: string]: TypeDef } = {
        "ir_node*": nodeType,
        "ir_node*const*": rawType("NodeList"),
        "ir_entity*": intoRustType("Entity"),
        "ir_tarval*": intoRustType("Tarval"),
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
        "ir_type*": rawPointerType("ir_type"),
        "ir_mode*": rawPointerType("ir_mode"),
        "ir_switch_table*": rawPointerType("ir_switch_table"),
        "ir_asm_constraint*": rawPointerType("ir_asm_constraint"),
        "ident*": rawPointerType("ident"),
        "ident**": rawType("*mut *mut bindings::ident"),
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
generateHeader();

generateNodeEnum();
generateNodeIsAsNodeImpl();
generateNodeDebug();
generateNodeTrait();

generateProjEnum();
generateProjKindImpl();
generateNodeFactory();

for (const node of nodes) {
    generateNodeStruct(node);
    generateNodeImpl(node);
    generateNodeInto(node);
    generateNodeTraitImpl(node);
    generateNodeDebugImpl(node);
}

generateGraphImpl();
generateBlockImpl();

w.closeFile("nodes_gen.rs");
w.save("../src/");

exec("cargo fmt --package libfirm-rs", (err, stdout, stderr) => {
    if (err) { console.error(err); }
    if (stderr) { console.error(stderr); }
    if (stdout) { console.log(stdout); }
});


function generateHeader() {
    w.line("// This file is generated! Do not edit by hand!");
    w.line("// Follow the instructions in the README on how to update this file.");
    w.line("use libfirm_rs_bindings as bindings;");
    w.line("use std::collections::HashMap;");
    w.line("use super::nodes::{NodeTrait, NodeDebug, NodeDebugOpts};");
    w.line("use super::graph::Graph;");
    w.line("use super::tarval::Tarval;");
    w.line("use super::entity::Entity;");
    w.line("use std::fmt;");
}

function generateNodeEnum() {
    w.line("#[derive(Clone, Copy)]");
    w.indent("pub enum Node {");
    for (const node of nodes) {
        if (node.isProj) {
            w.line(`${node.variantName}(${node.structName}, ProjKind),`);
        } else {
            w.line(`${node.variantName}(${node.structName}),`);
        }
    }
    w.unindent("}");
    w.line();
}

function generateNodeIsAsNodeImpl() {
    w.indent(`impl Node {`);
    for (const node of nodes) {
        w.indent(`pub fn ${node.is_name}(node: Node) -> bool {`);
        w.line(`match node { Node::${node.variantName}(_node${node.isProj ? ", _proj_kind" : ""}) => true, _ => false }`);
        w.unindent(`}`);

        w.indent(`pub fn ${node.as_name}(node: Node) -> Option<${node.structName}> {`);
        w.line(`match node { Node::${node.variantName}(node${node.isProj ? ", _proj_kind" : ""}) => Some(node), _ => None }`);
        w.unindent(`}`);
    }
    w.unindent(`}`);
}

function generateNodeDebug() {
    w.indent(`impl NodeDebug for Node {`);
    w.indent(`fn fmt(&self, f: &mut fmt::Formatter, opts: NodeDebugOpts) -> fmt::Result {`);
    w.indent(`match self {`);
    for (const node of nodes) {
        w.indent(`Node::${node.variantName}(node${node.isProj ? ", proj_kind" : ""}) => {`);
        if (node.isProj) {
            w.line(`write!(f, "{}: {:?}", node.debug_fmt().with(opts), proj_kind)`);
        } else {
            w.line(`write!(f, "{}", node.debug_fmt().with(opts))`);
        }
        w.unindent(`},`);
    }
    w.unindent(`}`);
    w.unindent(`}`);
    w.unindent(`}`);

    w.indent(`impl fmt::Debug for Node {`);
    w.indent(`fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {`);
    w.line(`fmt::Display::fmt(&self.debug_fmt(), f)`);
    w.unindent(`}`);
    w.unindent(`}`);
}

function generateNodeTrait() {
    w.indent(`impl NodeTrait for Node {`);
    w.indent(`fn ${internal_ir_node}(&self) -> ${ir_node_type} {`);
    w.indent(`match self {`);
    for (const node of nodes) {
        w.line(`Node::${node.variantName}(node${node.isProj ? ", _" : ""}) => node.${internal_ir_node}(),`);
    }
    w.unindent(`}`);
    w.unindent(`}`);
    w.unindent("}");
    w.line();
}


function generateNodeFactory() {
    w.line("type NodeFactoryFn = fn(*mut bindings::ir_node) -> Node;");
    w.line(`pub struct NodeFactory(HashMap<u32, NodeFactoryFn>);`);
    w.line("#[allow(clippy::new_without_default_derive)]");
    w.indent("impl NodeFactory {");
    w.indent("pub fn new() -> Self {");
    w.line(`let mut map = HashMap::<u32, NodeFactoryFn>::new();`);
    w.indent(`unsafe {`);
    for (const node of nodes) {
        w.line(`let op = bindings::get_op_${node.name}();`);
        w.line(`map.insert(bindings::get_op_code(op), Self::${node.create_name});`);
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
    w.line(`let f = self.0[&op_code];`);
    w.line(`f(ir_node)`);
    w.unindent("}");
    w.line();


    // generate create_XYZ-Node functions
    for (const node of nodes) {
        w.indent(`fn ${node.create_name}(ir_node: ${ir_node_type}) -> Node {`);
        if (node.name === `Proj`) {
            w.line(`let proj = ${node.structName}(ir_node);`);
            w.line(`Node::${node.name}(proj, proj.kind())`);
        }
        else {
            w.line(`Node::${node.name}(${node.structName}(ir_node))`);
        }
        w.unindent("}");
    }

    w.unindent("}");
    w.line();
}

function generateProjEnum() {
    w.line("#[derive(Debug, Clone, Copy, Eq, PartialEq)]");
    w.line("#[allow(non_camel_case_types)]");
    w.indent("pub enum ProjKind {");
    for (const node of nodes) {
        for (const out of node.outs) {
            const decl = out.projKind_variantDecl();
            if (!decl) { continue; }

            w.line(`/// ${decl.comment}`);
            w.line(`${decl.decl},`);
        }
        if (node.name === "Start") {
            w.line(`Start_TArgs_Arg(/* arg_idx */ u32, /* pred_pred */ Start, /* pred */ Proj),`);
        }
        if (node.name === "Call") {
            w.line(`Call_TResult_Arg(/* arg_idx */ u32, /* pred_pred */ Call, /* pred */ Proj),`);
        }
    }
    w.line(`Other,`);
    w.unindent("}");
    w.line();
}

function generateProjKindImpl() {
    w.indent(`impl Proj {`);
    w.indent(`pub fn kind(self) -> ProjKind {`);
    {
        w.line(`let pred = self.pred();`);
        w.indent(`match pred {`);
        for (const predNode of nodes) {
            if (predNode.isProj || predNode.outs.length === 0) continue;
            w.line(`Node::${predNode.variantName}(node) => `);
            w.indentation++;
            w.indent(`match self.num() {`);
            let idx = 0;
            for (const out of predNode.outs) {
                w.line(`${idx} => ProjKind::${out.projKind_variantCtor("node")},`);
                idx++;
            }
            w.line(`_ => ProjKind::Other,`);
            w.unindent(`},`);
            w.indentation--;
        }
        w.line(`Node::Proj(proj, ProjKind::Start_TArgs(start)) => ProjKind::Start_TArgs_Arg(self.num(), start, proj),`);
        w.line(`Node::Proj(proj, ProjKind::Call_TResult(call)) => ProjKind::Call_TResult_Arg(self.num(), call, proj),`);
        w.line(`_ => ProjKind::Other,`);
        w.unindent("}");
    }
    w.unindent(`}`);
    w.unindent(`}`);
    w.line();
}

function generateNodeStruct(node: NodeImpl) {
    if (node.doc) {
        for (const line of node.doc.split("\n")) {
            w.line(`/// ${line.trim()}`);
        }
    }
    w.line("#[derive(Clone, Copy, Eq, PartialEq)]");
    w.line(`pub struct ${node.structName}(${ir_node_type});`);
    w.line();
}

function generateNodeImpl(node: NodeImpl) {
    w.indent(`impl ${node.structName} {`);

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
        w.indent("#[allow(clippy::let_and_return)]");
        w.indent(`pub fn ${input.getterName}(self) -> ${input.type.rustOutName} {`);
        w.line(`let unwrapped = unsafe { bindings::get_${node.name}_${input.name}(self.0) };`);
        w.line(input.type.wrap("unwrapped"));
        w.unindent(`}`);
        w.line();

        if (input.comment) { w.line(`/// Sets ${input.comment}.`); }
        w.indent("#[allow(clippy::let_and_return)]");
        w.indent(`pub fn ${input.setterName}(self, val: ${input.type.rustInName}) {`);
        w.line(`let unwrapped = ${input.type.unwrap("val")};`);
        w.line(`unsafe { bindings::set_${node.name}_${input.name}(self.0, unwrapped); }`);
        w.unindent(`}`);
        w.line();
    }

    // projection functions
    for (const out of node.outs) {
        if (out.comment) { w.line(`/// ${out.comment}.`); }
        const outMode = out.guessModeType();
        let args = outMode ? "" : `, mode: bindings::mode::Type`;
        let modeValue = outMode ? `${outMode}` : `mode`;

        w.indent(`pub fn ${out.new_proj_fnName}(self${args}) -> Proj {`);
        w.line(`Proj::new(unsafe { bindings::new_r_Proj(self.0, ${modeValue}, ${out.idx}) })`);
        w.unindent(`}`);
        w.line();
    }

    // projection reverse function
    for (const out of node.outs) {
        if (out.comment) { w.line(`/// ${out.comment}.`); }

        w.indent(`pub fn ${out.out_proj_fnName}(self) -> Option<Proj> {`);
        w.indent(`for out_node in self.out_nodes() {`);
        w.indent(`if let Node::Proj(proj, ProjKind::${out.projKind_variantCtor("_")}) = out_node {`);
        w.line(`return Some(proj);`);
        w.unindent(`}`);
        w.unindent(`}`);
        w.line(`None`);
        w.unindent(`}`);
        w.line();
    }

    w.unindent(`}`);
    w.line();
}

function generateNodeInto(node: NodeImpl) {
    w.indent(`impl Into<Node> for ${node.structName} {`);
    w.indent(`fn into(self) -> Node {`);
    w.line(`Node::${node.variantName}(self${node.isProj ? ", self.kind()" : ""})`);
    w.unindent(`}`);
    w.unindent("}");
    w.line();
}

function generateNodeTraitImpl(node: NodeImpl) {
    w.indent(`impl NodeTrait for ${node.structName} {`);
    w.indent(`fn ${internal_ir_node}(&self) -> ${ir_node_type} {`);
    w.line(`self.0`);
    w.unindent(`}`);
    w.unindent("}");
    w.line();
}

function generateNodeDebugImpl(node: NodeImpl) {
    if (["Address", "Call"].indexOf(node.name) === -1) {
        w.indent(`impl NodeDebug for ${node.structName} {`);
        w.indent(`fn fmt(&self, f: &mut fmt::Formatter, _opts: NodeDebugOpts) -> fmt::Result {`);
        w.line(`write!(f, "${node.name} {}", self.node_id())`);
        w.unindent(`}`);
        w.unindent(`}`);
    }

    w.indent(`impl fmt::Debug for ${node.structName} {`);
    w.indent(`fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {`);
    w.line(`fmt::Display::fmt(&self.debug_fmt(), f)`);
    w.unindent(`}`);
    w.unindent(`}`);
}

function generateGraphImpl() {
    w.indent(`impl Graph {`);
    {
        for (const node of nodes) {
            generateConstructionFunction(node, "graph");
        }
    }
    w.unindent("}");
    w.line();
}

function generateBlockImpl() {
    w.indent(`impl Block {`);
    {
        for (const node of nodes) {
            generateConstructionFunction(node, "block");
        }
    }
    w.unindent("}");
    w.line();
}

function generateConstructionFunction(node: NodeImpl, context: "graph"|"block") {
    if (!node.hasConstructor) { return; }
    const params = new Array<{ name: string, type: string, doc: string }>();
    const args = new Array<string>();
    if (context === "block") {
        if (node.needsBlock) {
            args.push(`self.0`);
        }
        else { return; }
    }
    else if (context === "graph") {
        if (node.needsBlock) {
            params.push({ name: `block`, type: `Block`, doc: `The block.` });
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
            params.push({ name: arg.argName, type: `Vec<Node>`, doc: arg.comment });
            statements.push(`let ${arg.argName}: Vec<*mut bindings::ir_node> = ${arg.argName}.iter().map(|v| ${nodeType.unwrap(`v`)}).collect();`);
            args.push(`${arg.argName}.len() as i32`, `${arg.argName}.as_ptr()`);
            nextIsArray = false;
        } else if (arg.name === "arity") {
            nextIsArray = true;
        }
        else {
            params.push({ name: arg.argName, type: arg.type.rustInName, doc: arg.comment });
            args.push(arg.type.unwrap(arg.argName));
        }
    }

    w.line(`/// Creates a new ${node.name}-node.`);
    for (const param of params) {
        w.line(`/// * \`${param.name}\` ${param.doc}`);
    }
    const paramsStr = params.map(p => `${p.name}: ${p.type}`).join(", ");
    w.indent("#[allow(clippy::style)]");
    w.indent(`pub fn ${node.new_name}(self, ${paramsStr}) -> ${node.structName} {`);
    for (const line of statements) { w.line(line); }
    w.line(`let ir_node = unsafe { bindings::new_r_${node.name}(${args.join(", ")}) };`);
    w.line(`${node.structName}::new(ir_node)`);
    w.unindent(`}`);
    w.line();
}
