use super::asm::Program;
use std::io;

impl Program {
    pub fn emit_asm64(&self, out: &mut impl io::Write) -> io::Result<()> {
        //for f in &self.functions {
        //f.emit_asm64(out)?;
        //}
        Ok(())
    }
}
