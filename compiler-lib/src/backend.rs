pub trait AsmOut: std::io::Write + std::os::unix::io::AsRawFd {}

impl AsmOut for std::fs::File {}

pub trait AsmBackend {
    fn emit_asm(&mut self, out: &mut dyn AsmOut) -> std::io::Result<()>;
}

pub mod amd64 {

    use crate::{
        breakpoint, dot,
        firm::FirmContext,
        lowering::{
            amd64,
            lir::{self, LIR},
        },
    };

    pub use crate::lowering::amd64::CallingConv;

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
            let lir = LIR::from(self.firm_ctx.use_external_backend());
            breakpoint!("LIR representation", lir, &|block: &lir::BasicBlock| {
                dot::default_lir_label(block)
            });

            let mut p = amd64::Program::new(&lir, self.opts.cconv);
            p.emit_asm(&mut box out) // the heck I know why we need to re-box this here...
        }
    }

}

pub mod molki {
    // TODO: MolkiBackend
}
