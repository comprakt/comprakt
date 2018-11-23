use crate::{
    asciifile::Spanned,
    ast::{self, Program, AST},
    strtab::Symbol,
    type_checking::type_system::{CheckedType, ClassMethodDef, TypeSystem},
    visitor::NodeKind,
};
use libfirm_rs::{bindings::*, *};
use std::{
    collections::HashMap,
    ffi::{CStr, CString, OsStr},
    path::PathBuf,
};

#[derive(Debug, Clone, Default)]
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
                        ".main".to_string()
                    };

                    let method_type = ft.build(is_main);
                    let mut local_var_def_visitor = LocalVarDefVisitor::new();
                    local_var_def_visitor.visit(&NodeKind::from(block));

                    let graph = Graph::function(
                        &method_name,
                        method_type,
                        // "+1" is THIS-ptr
                        method.params.len() + 1 + local_var_def_visitor.count,
                    );

                    MethodBodyGenerator::new(graph, method).gen_method(&block);

                    unsafe {
                        irg_finalize_cons(graph.into());
                    }
                    fn_graphs.push(graph);
                }
            }
        }

        fn_graphs
    }
}

struct MethodBodyGenerator<'ir, 'src> {
    graph: Graph,
    method_def: &'ir ClassMethodDef<'src>,
    local_vars: HashMap<Symbol<'src>, (usize, mode::Type)>,
    num_vars: usize,
}

impl<'a, 'ir, 'src> MethodBodyGenerator<'ir, 'src> {
    fn new(graph: Graph, method_def: &'ir ClassMethodDef<'src>) -> Self {
        let mut se1f = MethodBodyGenerator {
            graph,
            local_vars: HashMap::new(),
            num_vars: 0,
            method_def,
        };

        let args = graph.args_node();

        if !method_def.is_static {
            // TODO `this`-ptr graph.set_value(0, &args.project(unsafe { mode::P }, 0));
            se1f.num_vars += 1;

            for (i, p) in method_def.params.iter().enumerate() {
                let mode = get_firm_mode(&p.ty).expect("args mustn't have void type");
                graph.set_value(se1f.new_local_var(p.name, mode), &args.project(mode, i + 1));
            }
        }

        // TODO remove
        //unsafe { keep_alive(graph.start_block().into()) };
        //unsafe { keep_alive(graph.end_block().into()) };

        se1f
    }

    /// Generate IR for a method body
    fn gen_method(&mut self, body: &Spanned<'src, ast::Block<'src>>) {
        unsafe { self.graph.set_cur_block(self.graph.start_block()) };
        self.gen_block(body);

        // Void functions have an implicit return in the end
        if self.method_def.return_ty == CheckedType::Void {
            let mem = self.graph.cur_store();
            let ret = self.graph.cur_block().new_return(mem, None);
            self.graph.end_block().add_pred(&ret);
        }

        self.graph.cur_block().mature();
        self.graph.end_block().mature();
    }

    /// Generate IR for a whole block
    fn gen_block(&mut self, block: &Spanned<'src, ast::Block<'src>>) {
        for stmt in &block.statements {
            self.gen_stmt(&stmt);
        }
    }

    /// Generate IR for a single statement
    fn gen_stmt(&mut self, stmt: &Spanned<'src, ast::Stmt<'src>>) {
        use self::ast::Stmt::*;
        match &**stmt {
            Block(block) => self.gen_block(block),
            If(cond, then_arm, else_arm) => {
                let prev_block = self.graph.cur_block();

                // TODO Do we really need an additional header_block? Can't we just put the
                // new_cond in the prev_block?
                let incoming_jmp = prev_block.new_jmp();
                let header_block = self.graph.new_imm_block(&incoming_jmp);

                prev_block.mature(); // This block is done now
                unsafe { self.graph.set_cur_block(header_block) };

                // We evaluate the condition
                let cond = header_block.new_cond(&self.gen_cond_expr(cond));

                // If its true, we take the then_arm
                let then_block = self.graph.new_imm_block(&cond.project_true());
                {
                    unsafe { self.graph.set_cur_block(then_block) };
                    self.gen_stmt(&**then_arm);
                }

                // If its false, we take the else_arm
                // We generate an else_block in any case, when there is no `else`, it's empty
                let else_block = self.graph.new_imm_block(&cond.project_false());
                if let Some(else_arm) = else_arm {
                    unsafe { self.graph.set_cur_block(else_block) };
                    self.gen_stmt(&**else_arm);
                }

                header_block.mature();

                let from_then_jmp = then_block.new_jmp();
                let from_else_jmp = else_block.new_jmp();

                // Now we close the if-diamond
                let next_block = self.graph.new_imm_block(&from_then_jmp);
                next_block.add_pred(&from_else_jmp);
                unsafe { self.graph.set_cur_block(next_block) };

                // Those blocks are finished now
                then_block.mature();
                else_block.mature();
            }

            While(cond, body) => unimplemented!(),

            Expression(expr) => {
                self.gen_expr(expr);
            }

            Return(res_expr) => {
                let mem = self.graph.cur_store();
                let res = res_expr.as_ref().map(|res_expr| self.gen_expr(&*res_expr));

                let ret = self.graph.cur_block().new_return(mem, res);

                self.graph.end_block().add_pred(&ret);
            }

            LocalVariableDeclaration(_ty, name, init_expr) => {
                // TODO here we need hennings type_analysis, because _ty is not a CheckedType.
                // For now, just assume i32
                //let mode = get_firm_mode(_ty).expect(&format!("var '{}' is void", name));
                let mode = unsafe { mode::Is };
                let var_slot = self.new_local_var(**name, mode);
                if let Some(init_expr) = init_expr {
                    self.graph.set_value(var_slot, &self.gen_expr(init_expr));
                }
            }

            Empty => (),
        }
    }

    /// Return a node that evaluates the given expression
    ///
    /// TODO non-raw-ptr abstraction for ret type; Box<dyn ValueNode> might
    /// work, but unnecessary box
    fn gen_expr(&mut self, expr: &Spanned<'src, ast::Expr<'src>>) -> *mut ir_node {
        use self::ast::Expr::*;
        match &**expr {
            Int(lit) => {
                let val = unsafe { new_tarval_from_long(lit.parse().unwrap(), mode::Is) };
                self.graph.new_const(val).as_value_node()
            }
            Var(name) => {
                let (slot, mode) = self.local_var(**name);
                self.graph.value(slot, mode).as_value_node()
            }

            _ => unimplemented!(),
        }
    }

    /// Assume an expression can be evaluated as a boolean and generate a
    /// `Selector` for it
    fn gen_cond_expr(&mut self, expr: &Spanned<'src, ast::Expr<'src>>) -> Selector {
        use self::ast::Expr::*;
        match &**expr {
            Var(name) => {
                let (slot, mode) = self.local_var(**name);
                let val = self.graph.value(slot, mode);

                let zero = self
                    .graph
                    .new_const(unsafe { new_tarval_from_long(0, mode) });

                // cond is true iff. val != 0
                self.graph
                    .cur_block()
                    // TODO ir_relation::NotEqual does not exist. Need ALU-neg?
                    .new_cmp(&val, &zero, ir_relation::Greater)
                    .as_selector()
            }

            _ => unimplemented!(),
        }
    }

    /// Allocate a new local variable in the next free slot
    fn new_local_var(&mut self, name: Symbol<'src>, mode: mode::Type) -> usize {
        let slot = self.num_vars;
        self.num_vars += 1;
        self.local_vars.insert(name, (slot, mode));
        slot
    }

    /// Get name and mode of previously allocated local var
    fn local_var(&mut self, name: Symbol<'src>) -> (usize, mode::Type) {
        self.local_vars
            .get(&name)
            .expect(&format!("undefined variable '{}'", name))
            .clone()
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
        CheckedType::Void | CheckedType::Null | CheckedType::UnknownType(_) => None,
    }
}

fn get_firm_mode(ty: &CheckedType<'_>) -> Option<mode::Type> {
    match ty {
        CheckedType::Int => Some(unsafe { mode::Is }),
        CheckedType::Boolean => Some(unsafe { mode::Bu }),
        CheckedType::TypeRef(_) | CheckedType::Array(_) | CheckedType::Null => {
            Some(unsafe { mode::P })
        }
        // Not possible
        CheckedType::Void => None,
    }
}

unsafe fn setup() {
    ir_init_library();

    // this call panics on error
    let triple = ir_get_host_machine_triple();
    ir_target_set_triple(triple);

    // pic=1 means 'generate position independent code'
    ir_target_option(CString::new("pic=1").expect("CString::new failed").as_ptr());

    ir_target_init();

    set_optimize(0);
}

pub unsafe fn build(opts: &Options, ast: &AST<'_>, type_system: &TypeSystem<'_>) {
    let program = match ast {
        ast::AST::Program(program) => program,
        ast::AST::Empty => unreachable!(),
    };
    let firm_gen = FirmGenerator::new(program, type_system);
    setup();

    // TODO: implement firm dumps in opts
    let _graphs = firm_gen.init_functions();

    lower_highlevel();
    be_lower_for_target();

    if let Some(ref path) = opts.dump_assembler {
        let is_stdout = path == OsStr::new("-");
        let label = CStr::from_bytes_with_nul(b"<stdin>\0").unwrap().as_ptr();

        if is_stdout {
            be_main(stdout, label);
        } else {
            // NOTE: we could also do:
            // - open file with rust API
            // - get a file pointer using as_raw_fd()
            // - use libc::fdopen() to convert the file pointer to a FILE struct
            let mut cpath = path.to_string_lossy().to_string();
            cpath.push('\0');

            let path_cstr = CStr::from_bytes_with_nul(cpath.as_bytes())
                .unwrap()
                .as_ptr();

            let assembly_file = libc::fopen(
                path_cstr,
                CStr::from_bytes_with_nul(b"w\0").unwrap().as_ptr(),
            );

            #[allow(clippy::cast_ptr_alignment)]
            be_main(assembly_file as *mut _IO_FILE, label);

            libc::fclose(assembly_file);
        }
    }

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
