use crate::{
    firm::{
        runtime::{self, Runtime},
        safety, FirmProgram, Options, ProgramGenerator,
    },
    strtab::StringTable,
    type_checking::{type_analysis::TypeAnalysis, type_system::TypeSystem},
};

use lazy_static::lazy_static;
use libfirm_rs::{bindings, types::TyTrait};
use optimization;
use std::{
    ffi::{CStr, CString},
    fs,
    path::{Path, PathBuf},
};

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
enum FirmContextState {
    // Each of the following 3 can leapfrog, but not jump back
    Built,
    Dumped,
    Optimized,

    // Terminal 1
    AsmEmitted,
    // Terminal 2
    ExternalBackend,
}

/// `FirmContext` is a singleton that represents the global state of `libFIRM`
/// library configuration and FIRM-graph construction.
pub struct FirmContext<'src, 'ast> {
    state: FirmContextState,

    dump_dir: PathBuf,

    // outputs
    program: FirmProgram<'src, 'ast>,
}

use std::sync::atomic::{AtomicBool, Ordering::SeqCst};

lazy_static! {
    static ref FIRM_CONTEXT_INITIALIZED: AtomicBool = AtomicBool::new(false);
}

impl<'src, 'ast> FirmContext<'src, 'ast> {
    ///
    /// * Initialize libFIRM
    /// * Setup libFIRM target options in case libFirm is used for lowering
    ///   (TODO can we move this
    /// to a libFIRM backend?)
    /// * Build a FIRM graph from the AST stored in `type_system`.
    ///
    /// The lowering / target configuration of libFIRM is stored in global
    /// storage. Hence, this function must be **called at most once**, and
    /// it will panic otherwise.
    pub fn build(
        dump_dir: &Path,
        safety_flags: &'src [safety::Flag],
        type_system: &'src TypeSystem<'src, 'ast>,
        type_analysis: &'src TypeAnalysis<'src, 'ast>,
        strtab: &'src StringTable<'src>,
        rtlib: Box<dyn runtime::RTLib>,
    ) -> FirmContext<'src, 'ast> {
        let dump_dir = dump_dir.to_owned();
        if !dump_dir.exists() {
            fs::create_dir_all(&dump_dir).expect("Failed to create output directory");
        }

        // This block protects against concurrent use of libFIRM, which is necessary
        // because the backend-configuration is stored in globals inside libFIRM.
        let prev = FIRM_CONTEXT_INITIALIZED.compare_and_swap(false, true, SeqCst);
        if prev {
            panic!(
                "libFIRM lowering context initialized, concurrent or \
                 repeated use not supported due to library-internal state"
            );
        } else {
            unsafe {
                libfirm_rs::init();

                // this call panics on error
                let triple = bindings::ir_get_host_machine_triple();
                bindings::ir_target_set_triple(triple);

                // pic=1 means 'generate position independent code'
                bindings::ir_target_option(
                    CString::new("pic=1").expect("CString::new failed").as_ptr(),
                );
                bindings::ir_target_init();
                bindings::set_optimize(0);

                // manually verified that the char* is copied internally,
                // thus it's ok to drop CString right away
                let dump_dir_cstr = CString::new(dump_dir.to_str().unwrap().as_bytes()).unwrap();
                bindings::ir_set_dump_path(dump_dir_cstr.as_ptr());
            }
        }

        let runtime = std::rc::Rc::new(Runtime::new(rtlib));
        let generator =
            ProgramGenerator::new(runtime, safety_flags, type_system, type_analysis, strtab);
        let program = generator.generate();

        FirmContext {
            state: FirmContextState::Built,
            dump_dir,
            program,
        }
    }

    /// May panic or fail silently if dumping fails.
    /// Must only be called once.
    pub fn high_level_dump(&mut self, opts: &Options) {
        use self::FirmContextState::*;
        match self.state {
            Built => self.state = Dumped,
            Dumped | Optimized | AsmEmitted | ExternalBackend => {
                panic!("invalid state {:?}", self.state)
            }
        }

        if opts.dump_firm_graph {
            let suffix = CString::new("high-level").unwrap();
            unsafe { bindings::dump_all_ir_graphs(suffix.as_ptr()) };
        }

        if opts.dump_class_layouts {
            for class in self.program.classes.values() {
                let outpath = self
                    .dump_dir
                    .join(class.borrow().def.name.as_str())
                    .with_extension("layout")
                    .to_str()
                    .and_then(|s| CString::new(s).ok())
                    .unwrap();
                let mode = CStr::from_bytes_with_nul(b"w\0").unwrap().as_ptr() as *mut i8;
                unsafe {
                    let file = libc::fopen(outpath.as_ptr() as *mut i8, mode);
                    #[allow(clippy::cast_ptr_alignment)]
                    bindings::dump_type_to_file(
                        file as *mut libfirm_rs::bindings::_IO_FILE,
                        class.borrow().entity.ty().ir_type(),
                    );
                    libc::fclose(file);
                }
            }
        }
    }

    /// Must only be called once.
    pub fn run_optimizations(&mut self, optimizations: optimization::Level) {
        use self::FirmContextState::*;
        match self.state {
            Built | Dumped => self.state = Optimized,
            Optimized | AsmEmitted | ExternalBackend => panic!("invalid state {:?}", self.state),
        }

        optimizations.run_all(&mut self.program);
    }

    /// Must only be called once.
    pub fn use_external_backend(&mut self) -> &FirmProgram<'src, 'ast> {
        use self::FirmContextState::*;
        match self.state {
            Built | Dumped | Optimized => self.state = ExternalBackend,
            ExternalBackend | AsmEmitted => panic!("invalid state {:?}", self.state),
        }
        &self.program
    }
}

use crate::backend;

impl backend::AsmBackend for FirmContext<'_, '_> {
    /// This implementation of `emit_asm` may only be called once and will panic
    /// on subsequent calls.
    fn emit_asm(&mut self, out: &mut dyn backend::AsmOut) -> std::io::Result<()> {
        use self::FirmContextState::*;
        match self.state {
            Built | Dumped | Optimized => self.state = AsmEmitted,
            AsmEmitted | ExternalBackend => panic!("invalid state {:?}", self.state),
        }

        // this can only happen once, and is protected by the state guard above
        unsafe {
            bindings::lower_highlevel();
            bindings::be_lower_for_target();

            // TODO: real label
            let label = CStr::from_bytes_with_nul(b"<unknown>\0").unwrap().as_ptr();

            // TODO libc and libFIRM error checks
            let fd = out.as_raw_fd();
            let mode = CStr::from_bytes_with_nul(b"w\0").unwrap().as_ptr();
            let assembly_file = libc::fdopen(fd, mode);
            #[allow(clippy::cast_ptr_alignment)]
            bindings::be_main(assembly_file as *mut bindings::_IO_FILE, label);
            // not close, since we used fdopen (will be closed by creator of `out`)
            libc::fflush(assembly_file);
        }

        Ok(())
    }
}
