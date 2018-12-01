//! Lowers the output of the semantic analysis phase (AST, Type System, Type
//! Analysis), into Firm for code generation.
//!
//!
//! # Generated Labels
//!
//! A dot (`.`) is a valid character in an ASM label, but not in MiniJava. This
//! is why it's used as a separator. As properties and methods live in their
//! own namespaces, fields have the additional segment `.F.`, methods have the
//! additional segment `.M.`.
//!
//! # Unused Struct Properties
//!
//! While building the firm graph, identifiers are created for Firm entities
//! using CString, which heap-allocates. However, firm only contains raw
//! pointers to the CString instances, hence the CString must be kept around
//! for the lifetime of the graph. Otherwise rust would de allocate the CString
//! to early!
pub mod method_body_generator;
pub mod program_generator;
pub mod runtime;

pub use self::{
    method_body_generator::MethodBodyGenerator, program_generator::ProgramGenerator,
    runtime::Runtime,
};

use crate::{
    strtab::{StringTable, Symbol},
    type_checking::{
        type_analysis::TypeAnalysis,
        type_system::{
            CheckedType, ClassDef, ClassFieldDef, ClassMethodBody, ClassMethodDef, TypeSystem,
        },
    },
    OutputSpecification,
};
use libfirm_rs::{bindings::*, *};
use std::{
    cell::RefCell,
    collections::HashMap,
    ffi::{CStr, CString},
    fs,
    path::PathBuf,
    rc::{Rc, Weak},
};

/// Enable or disable behaviour during the lowering phase
#[derive(Debug, Clone, Default)]
pub struct Options {
    pub dump_folder: PathBuf,
    pub dump_firm_graph: bool,
    pub dump_class_layouts: bool,
    pub dump_assembler: Option<OutputSpecification>,
}

pub struct Program<'src, 'ast> {
    classes: HashMap<Symbol<'src>, Rc<RefCell<Class<'src, 'ast>>>>,
}

struct Class<'src, 'ast> {
    name: CString,
    def: &'src ClassDef<'src, 'ast>,
    entity: Entity,
    pub fields: HashMap<Symbol<'src>, Rc<RefCell<Field<'src, 'ast>>>>,
    methods: HashMap<Symbol<'src>, Rc<RefCell<Method<'src, 'ast>>>>,
}

struct Field<'src, 'ast> {
    _name: CString,
    _class: Weak<RefCell<Class<'src, 'ast>>>,
    def: Rc<ClassFieldDef<'src>>,
    entity: Entity,
}

struct Method<'src, 'ast> {
    _name: CString,
    _class: Weak<RefCell<Class<'src, 'ast>>>,
    body: ClassMethodBody<'src, 'ast>,
    def: Rc<ClassMethodDef<'src, 'ast>>,
    entity: Entity,
    graph: Option<Graph>,
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

pub unsafe fn build<'src, 'ast>(
    opts: &Options,
    type_system: &'src TypeSystem<'src, 'ast>,
    type_analysis: &'src TypeAnalysis<'src, 'ast>,
    strtab: &'src mut StringTable<'src>,
) {
    setup();

    let generator = ProgramGenerator::new(type_system, type_analysis, strtab);
    let program = generator.generate();
    if !opts.dump_folder.exists() {
        fs::create_dir_all(&opts.dump_folder).expect("Failed to create output directory");
    }
    ir_set_dump_path(opts.dump_folder.to_str().unwrap().as_ptr() as *const i8);

    if opts.dump_firm_graph {
        let suffix = CString::new("high-level").unwrap();
        dump_all_ir_graphs(suffix.as_ptr());
    }

    if opts.dump_class_layouts {
        for class in program.classes.values() {
            #[allow(clippy::cast_ptr_alignment)]
            dump_type_to_file(
                libc::fopen(
                    opts.dump_folder
                        .join(class.borrow().name.to_str().unwrap())
                        .with_extension("layout")
                        .to_str()
                        .and_then(|s| CString::new(s).ok())
                        .unwrap()
                        .as_ptr() as *mut i8,
                    CStr::from_bytes_with_nul(b"w\0").unwrap().as_ptr() as *mut i8,
                ) as *mut libfirm_rs::bindings::_IO_FILE,
                class.borrow().entity.ty().into(),
            );
        }
    }

    lower_highlevel();
    be_lower_for_target();

    if let Some(ref output_spec) = opts.dump_assembler {
        // TODO: real label
        let label = CStr::from_bytes_with_nul(b"<stdin>\0").unwrap().as_ptr();

        match output_spec {
            OutputSpecification::Stdout => be_main(stdout, label),
            OutputSpecification::File(path) => {
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
    }

    // This is necessary to extend the lifetime of program
    // data beyond their usage within libfirm. See comments
    // in the head of this file.
    drop(program);

    ir_finish();
}

/// `None` indicates that the given type is not convertible, which
/// is not necessarily an error (e.g. `void`)
fn ty_from_checked_type(ct: &CheckedType<'_>) -> Option<Ty> {
    let ty = match ct {
        CheckedType::Int => PrimitiveType::i32(),
        CheckedType::Void => return None,
        CheckedType::TypeRef(_) => PrimitiveType::ptr(),
        CheckedType::Array(checked_type) => ty_from_checked_type(checked_type)
            .expect("Arrays are never of type `void`")
            .array(),
        CheckedType::Boolean => PrimitiveType::bool(),
        CheckedType::Null => unreachable!(),
        CheckedType::UnknownType(_) => unreachable!(),
    };
    Some(ty)
}

fn get_firm_mode(ty: &CheckedType<'_>) -> Option<mode::Type> {
    match ty {
        CheckedType::Int => Some(unsafe { mode::Is }),
        CheckedType::Boolean => Some(unsafe { mode::Bu }),
        CheckedType::TypeRef(_) | CheckedType::Array(_) | CheckedType::Null => {
            Some(unsafe { mode::P })
        }
        CheckedType::Void | CheckedType::UnknownType(_) => None,
    }
}

fn size_of(ty: &CheckedType<'_>) -> Option<u32> {
    get_firm_mode(ty).map(|mode| unsafe { get_mode_size_bytes(mode) })
}
