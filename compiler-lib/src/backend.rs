pub trait AsmOut: std::io::Write + std::os::unix::io::AsRawFd {}

impl AsmOut for std::fs::File {}

pub trait AsmBackend {
    fn emit_asm(&mut self, out: &mut dyn AsmOut) -> std::io::Result<()>;
}

pub mod amd64 {

    use crate::firm_context::FirmContext;
    use lowering::{
        amd64,
        lir::{self, LIR},
    };

    pub use lowering::amd64::CallingConv;

    pub struct Options {
        pub cconv: CallingConv,
    }

    pub struct Backend<'src, 'ast> {
        // member lir holds raw pointers to data stored in firm_ctx
        pub firm_ctx: FirmContext<'src, 'ast>,
        pub opts: Options,
    }

    use super::{AsmBackend, AsmOut};

    impl AsmBackend for Backend<'_, '_> {
        fn emit_asm(&mut self, out: &mut dyn AsmOut) -> std::io::Result<()> {
            compiler_shared::timed_scope!("amd64");

            let lir = {
                compiler_shared::timed_scope!("amd64::lir_construction");
                LIR::from(self.firm_ctx.use_external_backend())
            };
            crate::debugging::breakpoint!("LIR representation", lir, &|block: &lir::BasicBlock| {
                lowering::lir_debugging::default_lir_label(block)
            });

            let mut p = {
                compiler_shared::timed_scope!("amd64::program_new");
                amd64::Program::new(&lir, self.opts.cconv)
            };

            compiler_shared::timed_scope!("amd64::emit_asm");
            p.emit_asm(&mut box out) // the heck I know why we need to re-box this here...
        }
    }

}

pub mod planb {

    use crate::firm_context::FirmContext;

    pub struct Backend<'src, 'ast> {
        // member lir holds raw pointers to data stored in firm_ctx
        pub firm_ctx: FirmContext<'src, 'ast>,
    }

    use super::{AsmBackend, AsmOut};

    impl AsmBackend for Backend<'_, '_> {
        fn emit_asm(&mut self, out: &mut dyn AsmOut) -> std::io::Result<()> {
            let firm_program = self.firm_ctx.use_external_backend();
            compiler_shared::timed_scope!("planb");
            backend_planb::emit_asm(&mut box out, firm_program)
        }
    }

}

pub mod molki {
    // TODO: MolkiBackend
}
