//! Lowers the output of the semantic analysis phase (AST, Type System, Type
//! Analysis), into Firm for code generation.
//!
//!
//! # Generated Labels
//!
//! A dot (`$`) is a valid character in an ASM label, but not in MiniJava. This
//! is why it's used as a separator. As properties and methods live in their
//! own namespaces, fields have the additional segment `$F$`, methods have the
//! additional segment `$M$`.
//!
//! # Unused Struct Properties
//!
//! While building the firm graph, identifiers are created for Firm entities
//! using CString, which heap-allocates. However, firm only contains raw
//! pointers to the CString instances, hence the CString must be kept around
//! for the lifetime of the graph. Otherwise rust would de allocate the CString
//! to early!
pub mod firm_program;
mod lower;
pub mod method_body_generator;
pub mod program_generator;
pub mod runtime;
mod type_translation;

pub use self::{
    firm_program::*, method_body_generator::MethodBodyGenerator,
    program_generator::ProgramGenerator, runtime::Runtime,
};

use failure::{Error, Fail};

use crate::{
    lowering::{lir::LIR, molki},
    optimization,
    strtab::StringTable,
    type_checking::{type_analysis::TypeAnalysis, type_system::TypeSystem},
    OutputSpecification,
};
use libfirm_rs::{bindings, types::TyTrait};
use std::{
    ffi::{CStr, CString},
    fs,
    path::PathBuf,
};

/// Enable or disable behaviour during the lowering phase
#[derive(Debug, Clone, Default)]
pub struct Options {
    pub dump_folder: PathBuf,
    pub dump_firm_graph: bool,
    pub dump_class_layouts: bool,
    pub dump_assembler: Option<OutputSpecification>,
    pub optimizations: optimization::Level,
}

#[derive(Debug, Fail)]
pub enum FirmError {
    #[fail(display = "failed to write assembly to file {:?}", path)]
    EmitAsmFailure { path: PathBuf },
}

unsafe fn setup() {
    libfirm_rs::init();

    // this call panics on error
    let triple = bindings::ir_get_host_machine_triple();
    bindings::ir_target_set_triple(triple);

    // pic=1 means 'generate position independent code'
    bindings::ir_target_option(CString::new("pic=1").expect("CString::new failed").as_ptr());

    bindings::ir_target_init();

    bindings::set_optimize(0);
}

pub unsafe fn build<'src, 'ast>(
    opts: &Options,
    type_system: &'src TypeSystem<'src, 'ast>,
    type_analysis: &'src TypeAnalysis<'src, 'ast>,
    strtab: &'src mut StringTable<'src>,
) -> Result<(), Error> {
    setup();

    let rt = std::rc::Rc::new(Runtime::new(box runtime::Molki)); // FIXME constant
    let generator = ProgramGenerator::new(rt, type_system, type_analysis, strtab);
    let program = generator.generate();

    // FIXME: This function maybe has to be moved after the optimizations
    lower::lower_ir_nodes(&program);

    if !opts.dump_folder.exists() {
        fs::create_dir_all(&opts.dump_folder).expect("Failed to create output directory");
    }

    let dump_folder_cstr = CString::new(opts.dump_folder.to_string_lossy().as_bytes()).unwrap();
    bindings::ir_set_dump_path(dump_folder_cstr.as_ptr());

    if opts.dump_firm_graph {
        let suffix = CString::new("high-level").unwrap();
        bindings::dump_all_ir_graphs(suffix.as_ptr());
    }

    if opts.dump_class_layouts {
        for class in program.classes.values() {
            #[allow(clippy::cast_ptr_alignment)]
            bindings::dump_type_to_file(
                libc::fopen(
                    opts.dump_folder
                        .join(class.borrow().def.name.as_str())
                        .with_extension("layout")
                        .to_str()
                        .and_then(|s| CString::new(s).ok())
                        .unwrap()
                        .as_ptr() as *mut i8,
                    CStr::from_bytes_with_nul(b"w\0").unwrap().as_ptr() as *mut i8,
                ) as *mut libfirm_rs::bindings::_IO_FILE,
                class.borrow().entity.ty().ir_type(),
            );
        }
    }

    opts.optimizations.run_all(&program);

    // TODO Better seperation of modules
    let lir = LIR::from(&program);
    let molki = molki::Program::from(lir);
    molki.emit_molki(&mut std::io::stdout()).unwrap();

    bindings::lower_highlevel();
    bindings::be_lower_for_target();

    if let Some(ref output_spec) = opts.dump_assembler {
        // TODO: real label
        let label = CStr::from_bytes_with_nul(b"<stdin>\0").unwrap().as_ptr();

        match output_spec {
            OutputSpecification::Stdout => bindings::be_main(bindings::stdout, label),
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

                if assembly_file.is_null() {
                    return Err(FirmError::EmitAsmFailure { path: path.clone() }.into());
                }

                #[allow(clippy::cast_ptr_alignment)]
                bindings::be_main(assembly_file as *mut bindings::_IO_FILE, label);

                libc::fclose(assembly_file);
            }
        }
    }

    // This is necessary to extend the lifetime of program
    // data beyond their usage within libfirm. See comments
    // in the head of this file.
    drop(program);

    bindings::ir_finish();
    Ok(())
}
