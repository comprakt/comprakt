pub trait AsmOut: std::io::Write + std::os::unix::io::AsRawFd {}

impl AsmOut for std::fs::File {}

pub trait AsmBackend {
    fn emit_asm(&mut self, out: &mut dyn AsmOut) -> std::io::Result<()>;
}

pub mod amd64 {
    // TODO: AMD64 backend
}

pub mod molki {
    // TODO: MolkiBackend
}
