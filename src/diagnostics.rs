//! This is a list of semantic and lexical errors and warnings the compiler
//! emits.
//!
//! Error numbers could be generated automatically, _however_ error numbers
//! should be consistent for all versions of the compiler, even if errors or
//! warnings are retired.
//!
//! This implementation is NOT thread-safe.

// TODO: import spanned and span into this module?
use crate::{
    asciifile::AsciiFile,
    lexer::{Span, Spanned},
};
use failure::{AsFail, Fail};
use std::cell::RefCell;
use termcolor::{Color, ColorSpec, WriteColor};

// Error catalog.
//#[derive(Eq, PartialEq, Fail, Debug)]
//pub enum ErrorKind {
//}

//impl ErrorKind {
//fn get_message(&self) -> String {
//match self {
//ErrorKind::NonAsciiCharacter => {
//"encountered character outside of ASCII range, which is not
//"encountered allowed.".to_string()
//}
//ErrorKind::CommentSeparatorInsideComment => {
//"confusing usage of comment separator inside a comment.".to_string()
//}

//pub fn get_id(&self) -> String {
////format!("M{:03}", *self as u8)
//"E001".to_string()
//}

/// Tagging Interface marking failures as warnings.
/// Avoids accidental calls of error methods with warnings.
pub trait Warning: Fail {}

/// Tagging Interface marking failures as warnings.
/// Avoids accidental calls of error methods with warnings.
pub trait CompileError: Fail {}

/// Instead of writing errors, warnings and lints generated in the different
/// compiler stages directly to stdout, they are collected in this object.
///
/// This has several advantages:
/// - the output level can be adapted by users.
/// - we have a single source responsible for formatting compiler messages.
/// - unit tests can run the compiler and just assert the diagnostics object
///   instead of stdout/stderr of another process.
pub struct Diagnostics {
    // TODO: there is no reason to collect the messages except
    // for debugging purposes. So, maybe remove...
    messages: RefCell<Vec<Message>>,
    writer: RefCell<Box<dyn WriteColor>>,
}

impl Diagnostics {
    pub fn new(writer: Box<dyn WriteColor>) -> Self {
        Self {
            writer: RefCell::new(writer),
            messages: RefCell::new(Vec::new()),
        }
    }

    /// True when an error message was emitted, false
    /// if only warnings were emitted.
    pub fn errored(&self) -> bool {
        self.messages
            .borrow()
            .iter()
            .any(|msg| msg.level == MessageLevel::Error)
    }

    pub fn count(&self, level: MessageLevel) -> usize {
        self.messages
            .borrow()
            .iter()
            .filter(|msg| msg.level == level)
            .count()
    }

    fn write_statistics(&self) {
        let mut writer = self.writer.borrow_mut();

        if self.errored() {
            writer
                .set_color(ColorSpec::new().set_fg(MessageLevel::Error.color()))
                .ok();
            writeln!(
                writer,
                "Compilation aborted due to {}",
                match self.count(MessageLevel::Error) {
                    1 => "an error".to_string(),
                    n => format!("{} errors", n),
                }
            );
        } else {
            writer
                .set_color(ColorSpec::new().set_fg(Some(Color::Green)))
                .ok();
            writeln!(
                writer,
                "Compilation finished successfully {}",
                match self.count(MessageLevel::Warning) {
                    0 => "without warnings".to_string(),
                    1 => "with a warning".to_string(),
                    n => format!("with {} warnings", n),
                }
            );
        }
        writer.set_color(ColorSpec::new().set_fg(None)).ok();
    }

    // TODO: as we do not use warnings here. the warning trait is redundant!
    pub fn warning(&self, kind: Box<dyn AsFail>) {
        let msg = Message {
            level: MessageLevel::Warning,
            kind,
        };

        let mut writer = self.writer.borrow_mut();
        msg.write_colored(&mut **writer);
        &self.messages.borrow_mut().push(msg);
    }

    pub fn warning_with_source_snippet<'ctx>(
        &self,
        spanned: Spanned<Box<dyn AsFail>>,
        file: &AsciiFile<'ctx>,
    ) {
        let msg = Message {
            level: MessageLevel::Warning,
            kind: spanned.data,
        };

        let mut writer = self.writer.borrow_mut();
        msg.write_colored_with_code(&mut **writer, spanned.span, file);
        // TODO: store span
        &self.messages.borrow_mut().push(msg);
    }
}

#[derive(Copy, Clone, Eq, PartialEq)]
pub enum MessageLevel {
    Error,
    Warning,
}

impl MessageLevel {
    fn color(&self) -> Option<Color> {
        // Don't be confused by return type. `None` means default color!
        match self {
            MessageLevel::Error => Some(Color::Red),
            MessageLevel::Warning => Some(Color::Yellow),
        }
    }

    fn name(&self) -> &str {
        match self {
            MessageLevel::Error => "error",
            MessageLevel::Warning => "warning",
        }
    }
}

pub struct Message {
    pub level: MessageLevel,
    pub kind: Box<dyn AsFail>,
    /* TODO: draw code segment with error highlighted
     * pub span: Span,
     * TODO: maybe add suggestions for fixes
     * pub suggestions: Vec<Message>
     * TODO: filename seems unnecessary as we only compile a single file
     * pub filename: Path */
}

impl Message {
    fn write_colored(&self, writer: &mut dyn WriteColor) {
        // ignore coloring failures using ok()
        writer
            .set_color(ColorSpec::new().set_fg(self.level.color()))
            .ok();

        //write!(writer, "{} [{}]: ", text, self.kind.get_id());
        write!(writer, "{}: ", self.level.name());

        // ignore coloring failures using ok()
        writer.set_color(ColorSpec::new().set_fg(None)).ok();

        writeln!(writer, "{}\n", self.kind.as_fail());
    }

    fn write_colored_with_code<'ctx>(
        &self,
        writer: &mut dyn WriteColor,
        span: Span,
        file: &AsciiFile<'ctx>,
    ) {
        self.write_colored(writer);
        writer
            .set_color(ColorSpec::new().set_fg(Some(Color::Cyan)))
            .ok();
        // TODO: pad with whitespace, right align
        let line_marker_len = "XXXX | ".len();
        write!(writer, "{:4} | ", span.start.row + 1);

        if span.is_multiline() {
            writer
                .set_color(ColorSpec::new().set_fg(self.level.color()))
                .ok();
            write!(writer, ">");
        }

        writer.set_color(ColorSpec::new().set_fg(None)).ok();
        writeln!(writer, "{}", span.start.get_line(file));

        // TODO: print multiline spans correctly!
        if !span.is_multiline() {
            // add positional indicators.
            let indicator = format!(
                "{spaces}{markers}",
                spaces = " ".repeat(line_marker_len + span.start.col + 1),
                markers = "^".repeat(span.end.col - span.start.col)
            );
            writer
                .set_color(ColorSpec::new().set_fg(self.level.color()))
                .ok();
            writeln!(writer, "{}", indicator);
        }

        writer.set_color(ColorSpec::new().set_fg(None)).ok();
    }
}

/// Print a statistic at the end of compilation
impl Drop for Diagnostics {
    fn drop(&mut self) {
        self.write_statistics();
    }
}
