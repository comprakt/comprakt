use crate::{
    ast::{self, Program, AST},
    type_checking::type_system::{CheckedType, TypeSystem},
    visitor::NodeKind,
};
use libfirm_rs::{bindings::*, *};
use std::{
    ffi::{CStr, CString},
    path::PathBuf,
};

#[derive(Debug, Clone)]
pub struct Options {
    pub dump_firm_graph: Option<PathBuf>,
    pub dump_lowered_firm_graph: Option<PathBuf>,
    pub dump_assembler: Option<PathBuf>,
}

pub struct FirmGenerator<'ir, 'src> {
    program: &'ir Program<'src>,
    type_system: &'ir TypeSystem<'src>,
}

impl<'ir, 'src> FirmGenerator<'ir, 'src> {
    fn new(program: &'ir Program<'src>, type_system: &'ir TypeSystem<'src>) -> Self {
        Self {
            program,
            type_system,
        }
    }

    fn init_functions(&self) -> Vec<Graph> {
        let mut fn_graphs = vec![];
        for class in &self.program.classes {
            for member in &class.members {
                if let Some(block) = member.kind.method_body() {
                    let method = self
                        .type_system
                        .defined_classes()
                        .get(&class.name)
                        .expect(
                            "after type checking every class should be defined in the type system",
                        )
                        .method(member.name)
                        .expect("after type checking every method should be deined in it's class");
                    let is_main = method.is_static;
                    let mut ft = FunctionType::new();

                    // Set param and return types. `main` method has neither
                    let method_name = if !is_main {
                        let this_param = ClassType::new_class_type(class.name.as_str());
                        ft.add_param(this_param);

                        for param in &method.params {
                            let ty = get_firm_type(&param.ty);
                            ft.add_param(ty.expect("params never have `void` or `null` type"));
                        }

                        if let Some(ty) = get_firm_type(&method.return_ty) {
                            ft.set_res(ty);
                        }
                        format!("{}.{}", class.name.data, method.name)
                    } else {
                        format!(".{}", method.name)
                    };

                    let method_type = ft.build(is_main);
                    let mut local_var_def_visitor = LocalVarDefVisitor::new();
                    local_var_def_visitor.visit(&NodeKind::from(block));
                    let graph = Graph::function(
                        &method_name,
                        method_type,
                        method.params.len() + local_var_def_visitor.count,
                    );
                    let start_block = graph.start_block();
                    // FIXME: Swap this with body generation
                    let mem = graph.cur_store();
                    let ret = start_block.new_return(mem, None);
                    let end_block = graph.end_block();

                    end_block.add_pred(&ret);

                    unsafe {
                        mature_immBlock(get_irg_start_block(graph.into()));
                        irg_finalize_cons(graph.into());
                    }
                    fn_graphs.push(graph);
                }
            }
        }

        fn_graphs
    }
}

struct LocalVarDefVisitor {
    count: usize,
}

impl<'a, 'f> LocalVarDefVisitor {
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

fn get_firm_type(ty: &CheckedType<'_>) -> Option<Ty> {
    match ty {
        CheckedType::Int => Some(PrimitiveType::i32()),
        CheckedType::Boolean => Some(PrimitiveType::bool()),
        CheckedType::TypeRef(name) => Some(ClassType::new_class_type(name.as_str())),
        CheckedType::Array(checked_type) => Some(
            get_firm_type(checked_type)
                .expect("Arrays are never of type `void` or `null`")
                .pointer(),
        ),
        // Not possible
        CheckedType::Void | CheckedType::Null => None,
    }
}

unsafe fn setup() {
    ir_init_library();

    // this call panics on error
    let triple = ir_get_host_machine_triple();
    ir_target_set_triple(triple);

    // pic=1 means 'generate position independent code'
    ir_target_option(CString::new("pic=1").expect("CString::new failed").as_ptr());

    // The backend can also dump graphs after each step.
    // Note(Reiner): seems undocumented?!
    //ir_target_option(CStr::from_bytes_with_nul(b"dump=all\0").unwrap().as_ptr());
    ir_target_init();

    set_optimize(0);
}

pub unsafe fn build(_opts: &Options, ast: &AST<'_>, type_system: &TypeSystem<'_>) {
    let program = match ast {
        ast::AST::Program(program) => program,
        ast::AST::Empty => unreachable!(),
    };
    let firm_gen = FirmGenerator::new(program, type_system);
    setup();

    let graphs = firm_gen.init_functions();

    for graph in graphs {
        dump_ir_graph(
            graph.into(),
            CString::new("ssa-constructed")
                .expect("CString::new failed")
                .as_ptr(),
        );
    }

    lower_highlevel();
    be_lower_for_target();
    be_main(
        stdout,
        CString::new("<stdout>")
            .expect("CString::new failed")
            .as_ptr(),
    );
    ir_finish();
}

pub unsafe fn _print_machine_triple(triple: *mut ir_machine_triple_t) {
    let cpu = ir_triple_get_cpu_type(triple);
    let manu = ir_triple_get_manufacturer(triple);
    let os = ir_triple_get_operating_system(triple);

    println!("% TARGET TRIPLE");
    println!("% =============\n");

    println!(
        "% CPU:                  {}",
        CStr::from_ptr(cpu).to_string_lossy()
    );
    println!(
        "% Manufacturer:         {}",
        CStr::from_ptr(manu).to_string_lossy()
    );
    println!(
        "% Operating System:     {}",
        CStr::from_ptr(os).to_string_lossy()
    );

    let pointer_size = ir_target_pointer_size();
    println!("% Pointer Size:         {} Byte", pointer_size);
}
