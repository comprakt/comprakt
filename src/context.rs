//! All state shared by the lexer, parser and semantic analysis phases.
use crate::{asciifile::AsciiFile, diagnostics::Diagnostics, lexer::Spanned, strtab::StringTable};
use failure::AsFail;
use termcolor::WriteColor;

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

    pub fn warning(&self, spanned: Spanned<Box<dyn AsFail>>) {
        self.diagnostics
            .warning_with_source_snippet(spanned, &self.file);
    }

    pub fn error(&self, spanned: Spanned<Box<dyn AsFail>>) {
        self.diagnostics
            .error_with_source_snippet(spanned, &self.file);
    }
}
