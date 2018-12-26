use super::{type_translation::*, Class, Field, Method, MethodBodyGenerator, Program, Runtime};
use crate::{
    ast,
    strtab::{StringTable, Symbol},
    type_checking::{
        type_analysis::TypeAnalysis,
        type_system::{Body, ClassMethodBody, TypeSystem},
    },
    visitor::NodeKind,
};
use libfirm_rs::{entity::*, graph::Graph, types::*};
use log;
use std::{cell::RefCell, collections::HashMap, ffi::CString, rc::Rc};

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

    pub fn generate(mut self) -> Program<'src, 'ast> {
        let classes = self.build_entities();

        // TODO glue classes and runtime functions together here!
        for class in classes.values() {
            let class = class.borrow();
            log::debug!("generate methods for class {:?}", class.def.name);
            for method in class.methods.values() {
                log::debug!("generate method body for {:?}", method.borrow().def.name);
                let mut graph = None;
                if let ClassMethodBody::AST(body) = method.borrow().body {
                    let matured_graph =
                        self.generate_method_body(&method.borrow(), body, &class, &classes);
                    // TODO assert matured
                    graph = Some(matured_graph);
                }
                method.borrow_mut().graph = graph;
            }
        }
        Program { classes }
    }

    fn build_entities(&self) -> HashMap<Symbol<'src>, Rc<RefCell<Class<'src, 'ast>>>> {
        let mut classes = HashMap::new();
        // Define classes
        for class in self.type_system.defined_classes.values() {
            log::debug!("gen class {:?}", class.name.as_str());
            let class_type = ClassTy::new(class.name.as_str());
            let class_name = CString::new(class.name.as_str()).unwrap();
            let class_entity = Entity::new_global(&class_name, class_type.into());

            let gclass = Rc::new(RefCell::new(Class {
                def: class,
                name: CString::new(class.name.as_str()).unwrap(),
                entity: class_entity,
                fields: HashMap::new(),
                methods: HashMap::new(),
            }));

            for field in class.iter_fields() {
                log::debug!("\tgen field {:?}", field.name.as_str());
                let field_type = ty_from_checked_type(&field.ty)
                    .expect("field type must be convertible to a Firm type");

                let field_entity = Entity::new_entity(
                    class_type.into(),
                    &format!("{}.F.{}", class.name, field.name),
                    field_type.into(),
                );

                gclass.borrow_mut().fields.insert(
                    field.name,
                    Rc::new(RefCell::new(Field {
                        _class: Rc::downgrade(&gclass),
                        _name: CString::new(field.name.as_str()).unwrap(),
                        def: field,
                        entity: field_entity,
                    })),
                );
            }

            for method in class.iter_methods() {
                if let ClassMethodBody::AST(_) = method.body {
                } else {
                    break;
                }

                log::debug!("\tgen method{:?}", method.name.as_str());
                assert!(!method.is_static || (method.is_static && method.is_main));
                let mut method_ty_builder = MethodTyBuilder::new();

                // add this parameter
                if !method.is_main {
                    method_ty_builder.add_param(PrimitiveTy::ptr().into());
                }

                for param in &method.params {
                    let param_type = ty_from_checked_type(&param.ty)
                        .expect("parameter must be convertible to a Firm type");
                    method_ty_builder.add_param(param_type);
                }
                if let Some(return_ty) = ty_from_checked_type(&method.return_ty) {
                    method_ty_builder.set_res(return_ty);
                }
                let method_type = method_ty_builder.build(!method.is_main);

                let method_name = if method.is_main {
                    "mj_main".to_owned()
                } else {
                    format!("{}.M.{}", class.name, method.name)
                };

                let method_entity =
                    Entity::new_entity(class_type.into(), &method_name, method_type.into());

                gclass.borrow_mut().methods.insert(
                    method.name,
                    Rc::new(RefCell::new(Method {
                        _class: Rc::downgrade(&gclass),
                        _name: CString::new(method_name).unwrap(),
                        def: Rc::clone(&method),
                        entity: method_entity,
                        graph: None,
                        body: method.body,
                    })),
                );
            }

            class_type.default_layout();

            classes.insert(class.name, gclass);
        }

        classes
    }

    fn generate_method_body(
        &mut self,
        method: &Method<'src, 'ast>,
        body: &'ast Body<'src, 'ast>,
        own_class: &Class<'src, 'ast>,
        classes: &HashMap<Symbol<'src>, Rc<RefCell<Class<'src, 'ast>>>>,
    ) -> Graph {
        assert!(!method.def.is_static || (method.def.is_static && method.def.is_main));

        let local_vars_count = LocalVarDefVisitor::count_local_vars(&NodeKind::from(body));
        let this_param = if method.def.is_main { 0 } else { 1 };
        let slot_count = this_param + method.def.params.len() + local_vars_count;
        let graph = Graph::function_with_entity(method.entity, slot_count);
        let mut method_body_gen = MethodBodyGenerator::new(
            libfirm_rs::Graph::new(graph.into()),
            own_class,
            classes,
            Rc::clone(&method.def),
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
