//! All state shared by the lexer, parser and semantic analysis phases.
use asciifile::{AsciiFile, Spanned};
use crate::diagnostics::{Diagnostics, MaybeSpanned::WithSpan};
use failure::AsFail;
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

    pub fn warning(&self, spanned: Spanned<'m, Box<dyn AsFail>>) {
        // TODO: add some kind of autoconversion and make this method obsolete
        self.diagnostics.warning(WithSpan(spanned));
    }

    pub fn error(&self, spanned: Spanned<'m, Box<dyn AsFail>>) {
        self.diagnostics.error(WithSpan(spanned));
    }
}

// dummy_writer returns a WriteColor meant for use in tests.
#[cfg(test)]
pub fn dummy_writer() -> impl termcolor::WriteColor {
    use termcolor::Buffer;
    // FIXME: actually have something that discards the output
    Buffer::no_color()
}
