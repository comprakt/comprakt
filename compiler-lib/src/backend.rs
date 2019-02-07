pub trait AsmOut: std::io::Write + std::os::unix::io::AsRawFd {}

impl AsmOut for std::fs::File {}

pub trait AsmBackend {
    fn emit_asm(&mut self, out: &mut dyn AsmOut) -> std::io::Result<()>;
}

pub mod amd64 {

    use crate::firm_context::FirmContext;
    use lowering;

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
            compiler_shared::timed_scope!("backend");
            let firm_program = self.firm_ctx.use_external_backend();
            lowering::run_backend(firm_program, &mut box out)
        }
    }

}
