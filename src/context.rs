//! All state shared by the lexer, parser and semantic analysis phases.
use crate::{asciifile::AsciiFile, diagnostics::Diagnostics, strtab::StringTable};
use termcolor::{ColorChoice, StandardStream, WriteColor};

pub struct Context<'ctx> {
    pub file: AsciiFile<'ctx>,
    pub strtab: StringTable,
    pub diagnostics: Diagnostics,
}

impl<'ctx> Context<'ctx> {
    pub fn new(file: AsciiFile<'ctx>, writer: Box<dyn WriteColor>) -> Self {
        Self {
            file,
            strtab: StringTable::new(),
            diagnostics: Diagnostics::new(writer),
        }
    }

    pub fn default(file: AsciiFile<'ctx>) -> Self {
        let stderr = StandardStream::stderr(ColorChoice::Always);
        Self::new(file, Box::new(stderr))
    }
}
