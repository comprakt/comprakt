//! All state shared by the lexer, parser and semantic analysis phases.
use asciifile::{AsciiFile, Spanned};
use crate::diagnostics::{Diagnostics, MaybeSpanned::WithSpan};
use std::fmt::Display;
use termcolor::WriteColor;

pub struct Context<'m> {
    pub file: &'m AsciiFile<'m>,
    pub diagnostics: Diagnostics,
}

impl<'m> Context<'m> {
    pub fn new(file: &'m AsciiFile<'m>, writer: Box<dyn WriteColor>) -> Self {
        Self {
            file,
            diagnostics: Diagnostics::new(writer),
        }
    }

    #[cfg(test)]
    pub fn dummy(file: &'m AsciiFile<'m>) -> Self {
        Self::new(file, box dummy_writer())
    }

    #[allow(dead_code)]
    pub fn warning(&self, spanned: Spanned<'m, &dyn Display>) {
        self.diagnostics.warning(WithSpan(spanned));
    }

    #[allow(dead_code)]
    pub fn error(&self, spanned: Spanned<'m, &dyn Display>) {
        self.diagnostics.error(WithSpan(spanned));
    }

    #[allow(dead_code)]
    pub fn info(&self, spanned: Spanned<'m, &dyn Display>) {
        self.diagnostics.info(WithSpan(spanned));
    }
}

// dummy_writer returns a WriteColor meant for use in tests.
#[cfg(test)]
pub fn dummy_writer() -> impl termcolor::WriteColor {
    use termcolor::Buffer;
    // FIXME: actually have something that discards the output
    Buffer::no_color()
}
