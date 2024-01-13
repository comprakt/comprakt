//! All state shared by the lexer, parser and semantic analysis phases.
use asciifile::AsciiFile;
use diagnostics::Diagnostics;
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

    pub fn dummy(file: &'m AsciiFile<'m>) -> Self {
        Self::new(file, box dummy_writer())
    }
}

// dummy_writer returns a WriteColor meant for use in tests.
pub fn dummy_writer() -> impl termcolor::WriteColor {
    use termcolor::Buffer;
    // FIXME: actually have something that discards the output
    Buffer::no_color()
}
