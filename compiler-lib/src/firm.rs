use crate::{
    asciifile::Spanned,
    ast,
    strtab::Symbol,
    type_checking::type_system::{
        BuiltinMethodBody, CheckedType, ClassDef, ClassFieldDef, ClassMethodBody, ClassMethodDef,
        TypeSystem,
    },
    visitor::NodeKind,
};
use libfirm_rs::{bindings::*, *};
use log;
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

pub struct FirmGenerator<'src, 'ast> {
    /// `FirmGenerator::gen()` creates identifiers for Firm entities using
    /// CString, which heap-allocates.
    /// However, firm only contains raw pointers to the CString instances,
    /// hence the CString must be kept around for the lifetime of the graph.
    /// Users of `FirmGenerator::gen()` must thus keep the instance of Self
    /// around until the graph is no longer used.
    _classes: Vec<Rc<RefCell<GeneratorClass<'src, 'ast>>>>,
}

use std::{
    cell::RefCell,
    rc::{Rc, Weak},
};

struct GeneratorClass<'src, 'ast> {
    def: &'src ClassDef<'src, 'ast>,
    entity: Entity,
    /// `entity` stores a ref to `_name_store`, hence keep `_name_store` alive
    /// in this struct
    _name_store: CString,
    fields: Vec<Rc<RefCell<GeneratorField<'src, 'ast>>>>,
    methods: Vec<Rc<RefCell<GeneratorMethod<'src, 'ast>>>>,
}

struct GeneratorField<'src, 'ast> {
    class: Weak<RefCell<GeneratorClass<'src, 'ast>>>,
    def: &'src ClassFieldDef<'src>,
    /// `entity` stores a ref to `_name_store`, hence keep `_name_store` alive
    /// in this struct
    _name_store: CString,
    entity: Entity,
}

struct GeneratorMethod<'src, 'ast> {
    class: Weak<RefCell<GeneratorClass<'src, 'ast>>>,
    body: ClassMethodBody<'src, 'ast>,
    def: &'src ClassMethodDef<'src, 'ast>,
    entity: Entity,
    /// `entity` stores a ref to `_name_store`, hence keep `_name_store` alive
    /// in this struct
    _name_store: CString,
    graph: Option<Graph>,
}

impl<'src, 'ast> FirmGenerator<'src, 'ast> {
    fn gen(type_system: &'src TypeSystem<'src, 'ast>) -> Self {
        let runtime = Runtime::new();
        let classes = Self::build_entities(type_system);
        // TODO glue classes and runtime functions together here!
        for class in &classes {
            let class = class.borrow_mut();
            log::debug!("generate methods for class {:?}", class.def.name);
            for method in &class.methods {
                let mut method = method.borrow_mut();

                log::debug!("generate method body for {:?}", method.def.name);

                let matured_graph = match method.body {
                    ClassMethodBody::Builtin(builtin) => {
                        runtime.graph_from_builtin_method_body(builtin)
                    }
                    ClassMethodBody::AST(body) => {
                        let mut local_var_def_visitor = LocalVarDefVisitor::new();
                        local_var_def_visitor.visit(&NodeKind::from(body));

                        assert!(
                            !method.def.is_static || (method.def.is_static && method.def.is_main)
                        );
                        let this_param = if method.def.is_main { 0 } else { 1 };
                        let param_count =
                            this_param + method.def.params.len() + local_var_def_visitor.count;
                        let graph = Graph::function_with_entity(method.entity, param_count);
                        let mut method_body_gen =
                            MethodBodyGenerator::new(graph, method.def, &runtime);
                        method_body_gen.gen_method(body);
                        unsafe {
                            irg_finalize_cons(graph.into());
                        }
                        graph
                    }
                };
                // TODO assert matured
                method.graph = Some(matured_graph);
            }
        }
        Self { _classes: classes }
    }

    /// `None` indicates that the given type is not convertible, which
    /// is not necessarily an error (e.g. `void`)
    fn ty_from_checked_type(ct: &CheckedType<'src>) -> Option<Ty> {
        let ty = match ct {
            CheckedType::Int => PrimitiveType::i32(),
            CheckedType::Void => return None,
            CheckedType::TypeRef(_) => PrimitiveType::ptr(),
            CheckedType::Array(_) => PrimitiveType::ptr(), // TODO safe array type?
            x => panic!("unimplemented to-libfirm-type conversion for {:?}", x), // TODO
        };
        Some(ty)
    }

    fn build_entities(
        type_system: &'src TypeSystem<'src, 'ast>,
    ) -> Vec<Rc<RefCell<GeneratorClass<'src, 'ast>>>> {
        let mut classes = Vec::new();
        // Define classes
        for (_, class) in &type_system.defined_classes {
            unsafe {
                log::debug!("gen class {:?}", class.name.as_str());
                let class_name_str = class.name.as_str();
                let class_name = CString::new(class_name_str).unwrap();
                let class_name_id = new_id_from_str(class_name.as_ptr());
                let class_type = new_type_class(class_name_id);
                let class_entity = Entity::new_global(&class_name, class_type.into());

                let gclass = Rc::new(RefCell::new(GeneratorClass {
                    def: class,
                    _name_store: class_name,
                    entity: class_entity,
                    fields: Vec::new(),
                    methods: Vec::new(),
                }));

                for field in class.iter_fields() {
                    log::debug!("\tgen field {:?}", field.name.as_str());
                    let field_type = Self::ty_from_checked_type(&field.ty)
                        .expect("field type must be convertible to a Firm type");
                    let field_name =
                        CString::new(format!("{}.F.{}", class_name_str, field.name)).unwrap();
                    let field_entity = new_entity(
                        class_type,
                        field_name.as_ptr() as *mut i8,
                        field_type.into(),
                    );

                    gclass
                        .borrow_mut()
                        .fields
                        .push(Rc::new(RefCell::new(GeneratorField {
                            class: Rc::downgrade(&gclass),
                            _name_store: field_name,
                            def: field,
                            entity: field_entity.into(),
                        })));
                }

                for method in class.iter_methods() {
                    log::debug!("\tgen method{:?}", method.name.as_str());
                    assert!(!method.is_static || (method.is_static && method.is_main));
                    let mut method_type = FunctionType::new();
                    // TODO `this` param?
                    for param in &method.params {
                        let param_type = Self::ty_from_checked_type(&param.ty)
                            .expect("parameter must be convertible to a Firm type");
                        method_type.add_param(param_type);
                    }
                    if let Some(return_ty) = Self::ty_from_checked_type(&method.return_ty) {
                        method_type.set_res(return_ty);
                    }
                    let method_type = method_type.build(!method.is_main);

                    let method_name = if method.is_main {
                        CString::new("mj_main").unwrap()
                    } else {
                        CString::new(format!("{}.M.{}", class_name_str, method.name)).unwrap()
                    };
                    let method_entity = new_entity(
                        class_type,
                        method_name.as_ptr() as *mut i8,
                        method_type.into(),
                    );

                    gclass
                        .borrow_mut()
                        .methods
                        .push(Rc::new(RefCell::new(GeneratorMethod {
                            class: Rc::downgrade(&gclass),
                            _name_store: method_name,
                            def: method,
                            entity: method_entity.into(),
                            graph: None,
                            body: method.body,
                        })));
                }

                classes.push(gclass);
            }
        }

        classes
    }
}

struct Runtime {
    system_out_println: Entity,
    system_out_write: Entity,
    system_out_flush: Entity,
    system_in_read: Entity,
    new: Entity,

    dumpstack: Entity,
    null_usage: Entity,
    array_out_of_bounds: Entity,
    div_by_zero: Entity,
}

impl Runtime {
    fn new() -> Runtime {
        let dumpstack = {
            let t = FunctionType::new().build(false);
            let id = CString::new("mjrt_dumpstack").unwrap();
            Entity::new_global(&id, t)
        };

        let system_out_println = {
            let it = PrimitiveType::i32();
            let mut t = FunctionType::new();
            t.add_param(it);
            let t = t.build(false);
            let id = CString::new("mjrt_system_out_println").unwrap();
            Entity::new_global(&id, t)
        };

        let system_out_write = {
            let it = PrimitiveType::i32();
            let mut t = FunctionType::new();
            t.add_param(it);
            let t = t.build(false);
            let id = CString::new("mjrt_system_out_write").unwrap();
            Entity::new_global(&id, t)
        };

        let system_out_flush = {
            let t = FunctionType::new().build(false);
            let id = CString::new("mjrt_system_out_flush").unwrap();
            Entity::new_global(&id, t)
        };

        let system_in_read = {
            let it = PrimitiveType::i32();
            let mut t = FunctionType::new();
            t.set_res(it);
            let t = t.build(false);
            let id = CString::new("mjrt_system_in_read").unwrap();
            Entity::new_global(&id, t)
        };

        let new = {
            let it = PrimitiveType::i32();
            let mut t = FunctionType::new();
            t.set_res(it);
            let t = t.build(false);
            let id = CString::new("mjrt_new").unwrap();
            Entity::new_global(&id, t)
        };

        let div_by_zero = {
            let t = FunctionType::new().build(false);
            let id = CString::new("mjrt_div_by_zero").unwrap();
            Entity::new_global(&id, t)
        };

        let null_usage = {
            let t = FunctionType::new().build(false);
            let id = CString::new("mjrt_null_usage").unwrap();
            Entity::new_global(&id, t)
        };

        let array_out_of_bounds = {
            let t = FunctionType::new().build(false);
            let id = CString::new("mjrt_array_out_of_bounds").unwrap();
            Entity::new_global(&id, t)
        };

        Runtime {
            system_out_println,
            system_out_write,
            system_out_flush,
            system_in_read,
            new,
            dumpstack,
            div_by_zero,
            null_usage,
            array_out_of_bounds,
        }
    }

    fn graph_from_builtin_method_body(&self, mb: BuiltinMethodBody) -> Graph {
        let (rt_entity, slot_count, has_int_arg, returns_value) = match mb {
            BuiltinMethodBody::SystemOutPrintln => (self.system_out_println, 2, true, false),
            BuiltinMethodBody::SystemOutWrite => (self.system_out_write, 2, true, false),
            BuiltinMethodBody::SystemInRead => (self.system_in_read, 1, false, true),
            BuiltinMethodBody::SystemOutFlush => (self.system_out_flush, 1, false, false),
        };

        // wrap it into a proper function
        let wrapper_function_name = rt_entity.name().to_str().unwrap();
        let wrapper_function_name = format!("$WRAPPER$_{}", wrapper_function_name);

        let graph = Graph::function(&wrapper_function_name, rt_entity.ty(), slot_count);

        let args = if has_int_arg {
            let paramnode = graph.value(1, unsafe { mode::Is }).into();
            vec![paramnode] // slot 1 because 0 is this
        } else {
            vec![]
        };
        let func_addr = graph.new_addr(rt_entity);
        let call = graph
            .cur_block()
            .new_call(graph.cur_store(), func_addr, &args);
        unsafe {
            keep_alive(call.into());
        }
        let call_post_mem = call.project_mem();
        graph.set_store(call_post_mem);

        let mem = graph.cur_store();
        if returns_value {
            let result_tuple = call.project_result_tuple();
            let result_int = result_tuple.project(unsafe { mode::Is }, 0);
            let ret = graph.cur_block().new_return(mem, Some(result_int.into()));
            graph.end_block().add_pred(&ret);
        } else {
            let ret = graph.cur_block().new_return(mem, None);
            graph.end_block().add_pred(&ret);
        }

        graph.cur_block().mature();
        graph.end_block().mature();

        unsafe {
            irg_finalize_cons(graph.into());
        }

        log::debug!("\tgraph done");

        graph
    }
}

struct MethodBodyGenerator<'ir, 'src, 'ast> {
    graph: Graph,
    method_def: &'ir ClassMethodDef<'src, 'ast>,
    local_vars: HashMap<Symbol<'src>, (usize, mode::Type)>,
    num_vars: usize,
    runtime: &'ir Runtime,
}

impl<'a, 'ir, 'src, 'ast> MethodBodyGenerator<'ir, 'src, 'ast> {
    fn new(
        graph: Graph,
        method_def: &'ir ClassMethodDef<'src, 'ast>,
        runtime: &'ir Runtime,
    ) -> Self {
        let mut se1f = MethodBodyGenerator {
            graph,
            local_vars: HashMap::new(),
            num_vars: 0,
            method_def,
            runtime,
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

            While(cond, body) => {
                // TODO DRY beginning nearly the same as If-case
                let prev_block = self.graph.cur_block();

                let incoming_jmp = prev_block.new_jmp();
                let header_block = self.graph.new_imm_block(&incoming_jmp);

                prev_block.mature(); // This block is done now
                unsafe { self.graph.set_cur_block(header_block) };

                // We evaluate the condition
                let cond = header_block.new_cond(&self.gen_cond_expr(cond));

                // Run body if cond is true
                let body_block = self.graph.new_imm_block(&cond.project_true());
                {
                    unsafe { self.graph.set_cur_block(body_block) };
                    self.gen_stmt(&**body);

                    // We jump back to the condition-check
                    header_block.add_pred(&body_block.new_jmp());
                }

                // Leave loop if cond is false
                let next_block = self.graph.new_imm_block(&cond.project_false());
                unsafe { self.graph.set_cur_block(next_block) };

                header_block.mature();
                body_block.mature();
            }

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
        *self.local_vars.get(&name).expect("undefined variable")
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
        CheckedType::Void | CheckedType::UnknownType(_) => None,
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

pub unsafe fn build(opts: &Options, type_system: &TypeSystem<'_, '_>) {
    setup();

    let gen = FirmGenerator::gen(type_system);

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

    // See comment inside FirmGenerator for why this is necessary.
    drop(gen);

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
