use super::{firm_program::*, runtime::Runtime, MethodBodyGenerator};
use crate::{
    asciifile::Span,
    ast,
    parser::visitor::NodeKind,
    strtab::StringTable,
    type_checking::{
        type_analysis::TypeAnalysis,
        type_system::{Body, ClassMethodBody, TypeSystem},
    },
};
use libfirm_rs::{
    nodes::{Node, NodeTrait, ProjKind},
    Graph,
};
use log;
use std::{cell::RefCell, collections::HashMap, rc::Rc, sync::Mutex};

lazy_static::lazy_static! {
    static ref SPANS: Mutex<Spans> = Mutex::new(Spans::new());
}

/// This struct is only for debugging.
#[derive(Default)]
pub struct Spans {
    spans: RefCell<HashMap<Node, Span<'static>>>,
}

impl Spans {
    pub fn new() -> Spans {
        Spans::default()
    }
    pub fn lookup_span(node: impl NodeTrait) -> Option<Span<'static>> {
        SPANS.lock().unwrap().lookup_span_(node)
    }

    pub fn span_str(node: impl NodeTrait) -> String {
        if let Some(span) = Self::lookup_span(node) {
            format!(" [src:{}]", span)
        } else {
            "".to_string()
        }
    }

    pub fn add_span(node: impl NodeTrait, span: Span<'_>) {
        let mut spans = HashMap::new();
        spans.insert(node.as_node(), span);
        SPANS.lock().unwrap().add_spans_(&spans);
    }

    pub fn copy_span(target_node: impl NodeTrait, source_node: impl NodeTrait) {
        if let Some(span) = Spans::lookup_span(source_node) {
            Spans::add_span(target_node, span);
        }
    }

    pub fn add_spans(spans: &HashMap<Node, Span<'_>>) {
        SPANS.lock().unwrap().add_spans_(spans);
    }

    fn lookup_span_(&self, node: impl NodeTrait) -> Option<Span<'static>> {
        let map = self.spans.borrow();
        let node = match node.as_node() {
            Node::Proj(_proj, ProjKind::Call_TResult_Arg(_idx, call, _)) => call.into(),
            Node::Proj(proj, _) => proj.pred(),
            node => node,
        };
        map.get(&node.as_node()).cloned()
    }

    fn add_spans_(&self, spans: &HashMap<Node, Span<'_>>) {
        let mut map = self.spans.borrow_mut();
        for (key, span) in spans {
            // the spans life long enough - longer than you might have access to nodes.
            let span: Span<'static> = unsafe { std::mem::transmute(*span) };
            map.insert(*key, span);
        }
    }
}

pub struct ProgramGenerator<'src, 'ast> {
    runtime: Rc<Runtime>,
    type_system: &'src TypeSystem<'src, 'ast>,
    type_analysis: &'src TypeAnalysis<'src, 'ast>,
    strtab: &'src StringTable<'src>,
}

impl<'src, 'ast> ProgramGenerator<'src, 'ast> {
    pub fn new(
        runtime: Rc<Runtime>,
        type_system: &'src TypeSystem<'src, 'ast>,
        type_analysis: &'src TypeAnalysis<'src, 'ast>,
        strtab: &'src StringTable<'src>,
    ) -> Self {
        Self {
            runtime,
            type_system,
            type_analysis,
            strtab,
        }
    }

    pub fn generate(mut self) -> FirmProgram<'src, 'ast> {
        let program = FirmProgram::new(self.type_system, Rc::clone(&self.runtime));

        for method in program.methods.values() {
            log::debug!("generate method body for {:?}", method.borrow().def.name);
            let mut graph = None;
            if let ClassMethodBody::AST(body) = method.borrow().body {
                let matured_graph = self.generate_method_body(&method.borrow(), body, &program);
                graph = Some(matured_graph);
            }
            method.borrow_mut().graph = graph;
        }

        program
    }

    fn generate_method_body(
        &mut self,
        method: &FirmMethod<'src, 'ast>,
        body: &'ast Body<'src, 'ast>,
        program: &FirmProgram<'src, 'ast>,
    ) -> Graph {
        assert!(!method.def.is_static || (method.def.is_static && method.def.is_main));

        let local_vars_count = LocalVarDefVisitor::count_local_vars(&NodeKind::from(body));
        let this_param = if method.def.is_main { 0 } else { 1 };
        let slot_count = this_param + method.def.params.len() + local_vars_count;
        let graph = Graph::function_with_entity(method.entity, slot_count);
        let mut method_body_gen = MethodBodyGenerator::new(
            graph,
            program,
            Rc::clone(&method.def),
            &self.type_system,
            &self.type_analysis,
            &self.runtime,
            &self.strtab,
        );
        method_body_gen.gen_method(body);
        Spans::add_spans(&method_body_gen.spans);
        graph.finalize_construction();
        graph
    }
}

struct LocalVarDefVisitor {
    count: usize,
}

impl<'a, 'f> LocalVarDefVisitor {
    fn count_local_vars(node: &NodeKind<'a, 'f>) -> usize {
        let mut visitor = LocalVarDefVisitor::new();
        visitor.visit(node);
        visitor.count
    }

    fn new() -> Self {
        Self { count: 0 }
    }

    fn visit(&mut self, node: &NodeKind<'a, 'f>) {
        use self::NodeKind::*;
        node.for_each_child(&mut |child| {
            if let Stmt(stmt) = child {
                if let ast::Stmt::LocalVariableDeclaration(..) = stmt.data {
                    self.count += 1;
                }
            }

            self.visit(&child)
        });
    }
}
