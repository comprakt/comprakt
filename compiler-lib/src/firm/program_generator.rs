use super::{firm_program::*, MethodBodyGenerator, Runtime};
use crate::{
    ast,
    strtab::StringTable,
    type_checking::{
        type_analysis::TypeAnalysis,
        type_system::{Body, ClassMethodBody, TypeSystem},
    },
    visitor::NodeKind,
};
use libfirm_rs::graph::Graph;
use log;
use std::rc::Rc;

pub struct ProgramGenerator<'src, 'ast> {
    runtime: Runtime,
    type_system: &'src TypeSystem<'src, 'ast>,
    type_analysis: &'src TypeAnalysis<'src, 'ast>,
    strtab: &'src mut StringTable<'src>,
}

impl<'src, 'ast> ProgramGenerator<'src, 'ast> {
    pub fn new(
        type_system: &'src TypeSystem<'src, 'ast>,
        type_analysis: &'src TypeAnalysis<'src, 'ast>,
        strtab: &'src mut StringTable<'src>,
    ) -> Self {
        Self {
            runtime: Runtime::new(),
            type_system,
            type_analysis,
            strtab,
        }
    }

    pub fn generate(mut self) -> FirmProgram<'src, 'ast> {
        let program = FirmProgram::new(self.type_system);

        for method in program.methods.values() {
            log::debug!("generate method body for {:?}", method.borrow().def.name);
            let mut graph = None;
            if let ClassMethodBody::AST(body) = method.borrow().body {
                let matured_graph = self.generate_method_body(&method.borrow(), body, &program);
                // TODO assert matured
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
            &mut self.strtab,
        );
        method_body_gen.gen_method(body);
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
