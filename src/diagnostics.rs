//! This is a list of semantic and lexical errors and warnings the compiler
//! emits.
//!
//! NOT thread-safe.

use std::cell::RefCell;
use termcolor::{Color, ColorSpec, WriteColor};

/// Error catalog.
#[derive(Copy, Clone, Eq, PartialEq)]
pub enum ErrorKind {
    NonAsciiCharacter,
    CommentSeparatorInsideComment,
}

impl ErrorKind {
    fn get_message(&self) -> String {
        match self {
            ErrorKind::NonAsciiCharacter => {
                "encountered character outside of ASCII range, which is not allowed.".to_string()
            }
            ErrorKind::CommentSeparatorInsideComment => {
                "confusing usage of comment separator inside a comment.".to_string()
            }
        }
    }

    pub fn get_id(&self) -> String {
        format!("M{:03}", *self as u8)
    }
}

/// Instead of writing errors, warnings and lints generated in the different
/// compiler stages directly to stdout, they are collected in this object.
///
/// This has several advantages:
/// - the output level can be adapted by users.
/// - we have a single source responsible for formatting compiler messages.
/// - unit tests can run the compiler and just assert the diagnostics object
///   instead of stdout/stderr of another process.
pub struct Diagnostics<T> {
    // TODO: there is no reason to collect the messages except
    // for debugging purposes. So, maybe remove...
    messages: RefCell<Vec<Message>>,
    writer: RefCell<Box<T>>,
}

impl<T: WriteColor> Diagnostics<T> {
    pub fn new(writer: Box<T>) -> Self {
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
            .filter(|msg| msg.level != level)
            .count()
    }

    pub fn warning(&self, kind: ErrorKind) {
        let msg = Message {
            level: MessageLevel::Warning,
            kind: kind,
        };

        let mut writer = self.writer.borrow_mut();
        msg.write_colored(&mut **writer);
        &self.messages.borrow_mut().push(msg);
    }
}

#[derive(Copy, Clone, Eq, PartialEq)]
pub enum MessageLevel {
    Error,
    Warning,
}

pub struct Message {
    pub level: MessageLevel,
    pub kind: ErrorKind,
    /* TODO: draw code segment with error highlighted
     * pub span: Span,
     * TODO: maybe add suggestions for fixes
     * pub suggestions: Vec<Message>
     * TODO: filename seems unnecessary as we only compile a single file
     * pub filename: Path */
}

impl Message {
    fn write_colored<T: WriteColor>(&self, writer: &mut T) {
        let (color, text) = match self.level {
            MessageLevel::Error => (Color::Red, "Error"),
            MessageLevel::Warning => (Color::Yellow, "Warning"),
        };

        // ignore coloring failures using ok()
        writer.set_color(ColorSpec::new().set_fg(Some(color))).ok();

        write!(writer, "{} [{}]: ", text, self.kind.get_id());

        // ignore coloring failures using ok()
        writer.set_color(ColorSpec::new().set_fg(None)).ok();

        writeln!(writer, "{}\n", self.kind.get_message());
    }
}
