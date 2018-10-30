//! The diagnostics object controls the output of warnings and errors generated
//! by the compiler during the lexing, parsing and semantic analysis phases.
//! The diagnostics object is tracks the number of warnings and errors
//! generated.
//!
//! This implementation is NOT thread-safe.

// TODO: import spanned and span into this module?
use crate::{
    asciifile::{AsciiFile, LineContext},
    lexer::{Span, Spanned},
};
use failure::AsFail;
use std::cell::RefCell;
use termcolor::{Color, ColorSpec, WriteColor};

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

    pub fn write_statistics(&self) {
        let mut writer = self.writer.borrow_mut();
        let mut output = ColorOutput::new(&mut **writer);

        output.set_bold(true);

        if self.errored() {
            output.set_color(MessageLevel::Error.color());
            writeln!(
                output.writer(),
                "Compilation aborted due to {}",
                match self.count(MessageLevel::Error) {
                    1 => "an error".to_string(),
                    n => format!("{} errors", n),
                }
            );
        } else {
            output.set_color(Some(Color::Green));
            writeln!(
                output.writer(),
                "Compilation finished successfully {}",
                match self.count(MessageLevel::Warning) {
                    0 => "without warnings".to_string(),
                    1 => "with a warning".to_string(),
                    n => format!("with {} warnings", n),
                }
            );
        }
    }

    /// Generate an error or a warning that is printed to the
    /// writer given in the `new` constructor. Most of the time
    /// this will be stderr.
    pub fn emit(&self, level: MessageLevel, kind: Box<dyn AsFail>) {
        let msg = Message { level, kind };

        let mut writer = self.writer.borrow_mut();
        msg.write_colored(&mut **writer);
        self.messages.borrow_mut().push(msg);
    }

    #[allow(dead_code)]
    pub fn warning(&self, kind: Box<dyn AsFail>) {
        self.emit(MessageLevel::Warning, kind)
    }

    #[allow(dead_code)]
    pub fn error(&self, kind: Box<dyn AsFail>) {
        self.emit(MessageLevel::Error, kind)
    }

    pub fn warning_with_source_snippet<'ctx>(
        &self,
        spanned: Spanned<Box<dyn AsFail>>,
        file: &AsciiFile<'ctx>,
    ) {
        self.emit_with_source_snippet(MessageLevel::Warning, spanned, file)
    }

    pub fn error_with_source_snippet<'ctx>(
        &self,
        spanned: Spanned<Box<dyn AsFail>>,
        file: &AsciiFile<'ctx>,
    ) {
        self.emit_with_source_snippet(MessageLevel::Error, spanned, file)
    }

    pub fn emit_with_source_snippet<'ctx>(
        &self,
        level: MessageLevel,
        spanned: Spanned<Box<dyn AsFail>>,
        file: &AsciiFile<'ctx>,
    ) {
        let msg = Message {
            level,
            kind: spanned.data,
        };

        let mut writer = self.writer.borrow_mut();
        msg.write_colored_with_code(&mut **writer, &spanned.span, file);
        // TODO: store span
        self.messages.borrow_mut().push(msg);
    }
}

#[derive(Copy, Clone, Eq, PartialEq)]
pub enum MessageLevel {
    Error,
    Warning,
}

impl MessageLevel {
    fn color(self) -> Option<Color> {
        // Don't be confused by the return type. `None` means default color!
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
}

struct ColorOutput<'a> {
    writer: &'a mut dyn WriteColor,
    spec: ColorSpec,
}
impl<'a> ColorOutput<'a> {
    fn new(writer: &'a mut dyn WriteColor) -> Self {
        Self {
            writer,
            spec: ColorSpec::new(),
        }
    }

    fn set_color(&mut self, color: Option<Color>) {
        // ignore coloring failures using ok()
        self.spec.set_fg(color);
        self.writer.set_color(&self.spec).ok();
    }

    fn set_bold(&mut self, yes: bool) {
        // ignore coloring failures using ok()
        self.spec.set_bold(yes);
        self.writer.set_color(&self.spec).ok();
    }

    fn writer(&mut self) -> &mut dyn WriteColor {
        self.writer
    }
}

/// reset to no color by default. Otherwise code that
/// is not color aware will print everything in the
/// color last used.
impl<'a> Drop for ColorOutput<'a> {
    fn drop(&mut self) {
        // ignore coloring failures using ok()
        self.writer.reset().ok();
    }
}

const MAX_CONTEXT_LENGTH: usize = 80;
const MAX_CONTEXT_LENGTH_MULTILINE: usize = 160;

impl Message {
    fn write_colored(&self, writer: &mut dyn WriteColor) {
        self.write_colored_header(writer);
        writeln!(writer);
    }

    fn write_colored_header(&self, writer: &mut dyn WriteColor) {
        let mut output = ColorOutput::new(writer);
        output.set_color(self.level.color());
        output.set_bold(true);
        write!(output.writer(), "{}: ", self.level.name());

        output.set_color(None);
        writeln!(output.writer(), "{}", self.kind.as_fail());
    }

    fn write_colored_with_code<'ctx>(
        &self,
        writer: &mut dyn WriteColor,
        span: &Span,
        file: &AsciiFile<'ctx>,
    ) {
        self.write_colored_header(writer);

        let mut output = ColorOutput::new(writer);

        let line_number_width = (span.end.row + 1).to_string().len();

        // NOTE: this has to be the same width as the line_marker with
        // line numbers, otherwise the indicators for single line warnings/errors
        // are misaligned.
        let empty_line_marker = format!(" {} | ", " ".repeat(line_number_width));
        let truncation_str = "...";

        // add padding line above
        output.set_color(Some(Color::Cyan));
        output.set_bold(true);

        writeln!(output.writer(), "{}", empty_line_marker);

        // Add marker below the line if the error is on a single line. Add the
        // marker at the start of the line for multiline errors.
        if !span.is_multiline() {
            // add line number
            write!(
                output.writer(),
                " {padded_linenumber} | ",
                padded_linenumber = pad_left(&(span.start.row + 1).to_string(), line_number_width)
            );

            // add source code line
            let (truncation_before, src_line, truncation_after) =
                span.start
                    .get_line(file, MAX_CONTEXT_LENGTH, MAX_CONTEXT_LENGTH);

            if truncation_before == LineContext::Truncated {
                output.set_color(Some(Color::Cyan));
                output.set_bold(true);
                write!(output.writer(), "{}", truncation_str);
            }

            output.set_color(None);
            output.set_bold(false);
            write!(output.writer(), "{}", src_line);

            if truncation_after == LineContext::Truncated {
                output.set_color(Some(Color::Cyan));
                output.set_bold(true);
                write!(output.writer(), "{}", truncation_str);
            }

            writeln!(output.writer());

            // add positional indicators.
            output.set_color(Some(Color::Cyan));
            output.set_bold(true);
            write!(output.writer(), "{}", empty_line_marker);

            let indicator = format!(
                "{spaces}{markers}",
                spaces = " ".repeat(if truncation_before == LineContext::Truncated {
                    MAX_CONTEXT_LENGTH + truncation_str.len()
                } else {
                    span.start.col
                }),
                markers = "^".repeat(1 + span.end.col - span.start.col)
            );

            output.set_bold(true);
            output.set_color(self.level.color());
            writeln!(output.writer(), "{}", indicator);
        } else {
            // move position to first element in line as we always want to
            // render the first N characters if a line is way to long for
            // an error message.
            // TODO: would be nicer to always show the column of a multiline
            // error. But I am not sure how the line columns should be aligned
            // if lines are truncated differently.
            let mut line_first_char = span.start.to_line_start(file);

            for _line_num in span.start.row..=span.end.row {
                // add line number
                output.set_color(Some(Color::Cyan));
                output.set_bold(true);
                write!(
                    output.writer(),
                    " {padded_linenumber} |",
                    padded_linenumber =
                        pad_left(&(line_first_char.row + 1).to_string(), line_number_width)
                );

                // Add marker at the beginning of the line, if it spans
                // multiple lines
                output.set_color(self.level.color());
                write!(output.writer(), "> ");

                // add source code line
                output.set_color(None);
                output.set_bold(false);

                let (truncation_before, line, truncation_after) =
                    line_first_char.get_line(file, 1, MAX_CONTEXT_LENGTH_MULTILINE);
                debug_assert!(truncation_before == LineContext::NotTruncated);

                write!(output.writer(), "{}", line);

                if truncation_after == LineContext::Truncated {
                    output.set_color(Some(Color::Cyan));
                    output.set_bold(true);
                    write!(output.writer(), "{}", truncation_str);
                }

                writeln!(output.writer());

                line_first_char = match line_first_char.next_line(file) {
                    Ok(pos) => pos,
                    Err(_) /* EOF */ => { break; }
                }
            }

            // add padding line below
            output.set_color(Some(Color::Cyan));
            output.set_bold(true);
            writeln!(output.writer(), "{}", empty_line_marker);
        }

        writeln!(output.writer());
    }
}

pub fn pad_left(s: &str, pad: usize) -> String {
    pad_left_with_char(s, pad, ' ')
}

pub fn pad_left_with_char(s: &str, pad: usize, chr: char) -> String {
    format!(
        "{padding}{string}",
        padding = chr
            .to_string()
            .repeat(pad.checked_sub(s.len()).unwrap_or(0)),
        string = s
    )
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_pad_left() {
        let tests = vec![("a", "    a"), ("", "          "), ("a", "a"), ("", "")];

        for (input, expected) in tests {
            println!("Testing: {:?} => {:?}", input, expected);
            assert_eq!(expected, pad_left(input, expected.len()));
        }

        // not enough padding does not truncate string
        assert_eq!("a", pad_left("a", 0));
    }
}
