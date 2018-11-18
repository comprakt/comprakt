//! The diagnostics object controls the output of warnings and errors generated
//! by the compiler during the lexing, parsing and semantic analysis phases.
//! It also tracks the number of warnings and errors generated for flow control.
//!
//! This implementation is NOT thread-safe. Messages from different threads may
//! be interleaved.
use crate::{
    asciifile::{MaybeSpanned, Span, Spanned},
    color::ColorOutput,
};
use failure::Error;
use std::{ascii::escape_default, cell::RefCell, collections::HashMap, fmt::Display};
use termcolor::{Color, WriteColor};

pub fn u8_to_printable_representation(byte: u8) -> String {
    let bytes = escape_default(byte).collect::<Vec<u8>>();
    let rep = unsafe { std::str::from_utf8_unchecked(&bytes) };
    rep.to_owned()
}

/// This abstraction allows us to call the diagnostics API with pretty
/// much everything.
///
/// The following examples are all equivalent and will print a warning
/// without a source code snippet below the message:
///
/// ```rust,ignore
/// context.diagnostics.warning(&"Something went wrong");
/// context
///     .diagnostics
///     .warning(&WithoutSpan("Something went wrong"));
/// ```
///
/// The following examples will print a message with a source code
/// snippet. Note that all errors generated by the compiler are
/// a `Spanned<_, Fail>` and can therefore be directly passed to
/// the diagnostics API.
///
/// ```rust,ignore
/// // `lexer_error` is the `Err` returned by `Lexer::next`
/// context.diagnostics.error(&lexer_error);
/// // `span` is some `asciifile::Span`
/// context.diagnostics.error(&Spanned {
///     span: span,
///     data: "something went wrong"
/// });
///
/// context.diagnostics.error(Printable {
///     message: "expected an identifier, found 'int'",
///     annotations: vec![
///         Spanned(""), // mark something without a message
///         WithoutSpan("'int' is a keyword and cannot be used as a class name"), // add a hint
/// });
/// context.diagnostics.error(Printable {
///     message: "expected 'String', found 'int'",
///     annotations: vec![
///         WithSpan("expected 'String'"), // mark exact problem
///         WithSpan("what looks like the declaration of the main function"), // add context span
/// });
/// ```
///
/// Here is a more elaborate example:
///
/// ```
/// use compiler_lib::{
///     asciifile::{AsciiFile, MaybeSpanned::*, Position, Span, Spanned},
///     diagnostics::{Diagnostics, Printable},
/// };
/// use std::cell::UnsafeCell;
/// use termcolor::Ansi;
///
/// // TODO: remove use of unsafe cell for a mut and immutable borrow concurrently
/// let mut output: UnsafeCell<Vec<u8>> = UnsafeCell::new(Vec::new());
/// let stderr = Ansi::new(unsafe { &mut *output.get() });
/// let diagnostics = Diagnostics::new(Box::new(stderr));
/// let input = "banana\nanother banana\nbananarama\na\n\n\n\nmuch more banana\n";
/// let file = AsciiFile::new(input.as_bytes()).unwrap();
/// let initial_pos = Position::at_file_start(&file).unwrap();
/// let a_line = initial_pos.clone().iter().nth(33);
/// assert_eq!(a_line.unwrap().chr(), 'a');
/// let banana_end = initial_pos.clone().iter().nth(5).unwrap();
/// let banana_end_newline = initial_pos.clone().iter().nth(6).unwrap();
/// let banana_end_multinewline = initial_pos.clone().iter().nth(36).unwrap();
/// let last_pos = initial_pos.clone().iter().last().unwrap();
///
/// assert_eq!(Span::new(initial_pos, banana_end).as_str(), "banana");
/// assert_eq!(
///     Span::new(initial_pos, banana_end_newline).as_str(),
///     "banana\n"
/// );
///
/// diagnostics.error(&"banana");
/// diagnostics.error(&Spanned {
///     data: "this is a banana",
///     span: Span::new(initial_pos, banana_end),
/// });
/// diagnostics.warning(&WithSpan(Spanned {
///     data: "this is another banana",
///     span: Span::new(initial_pos, banana_end),
/// }));
/// diagnostics.info(&WithoutSpan("there is a banana"));
/// diagnostics.warning(&Printable {
///     message: &"let me show you!",
///     annotations: vec![
///         Spanned {
///             span: Span::new(initial_pos, initial_pos),
///             data: "the banana starts here",
///         },
///         Spanned {
///             span: Span::new(banana_end, banana_end),
///             data: "this is the end",
///         },
///         Spanned {
///             span: a_line.unwrap().get_line(),
///             data: "this line only has an A",
///         },
///     ],
/// });
/// diagnostics.error(&Printable {
///     message: &"a bag of bananas",
///     annotations: vec![
///         Spanned {
///             span: Span::new(initial_pos, banana_end),
///             data: "this is the first banana",
///         },
///         Spanned {
///             span: Span::new(initial_pos, banana_end_newline),
///             data: "multiline banana",
///         },
///         Spanned {
///             span: Span::new(initial_pos, banana_end_multinewline),
///             data: "multiple newline banana",
///         },
///         Spanned {
///             span: Span::new(initial_pos, initial_pos),
///             data: "the first banana starts here",
///         },
///         Spanned {
///             span: Span::new(initial_pos, last_pos),
///             data: "this is the bag of bananas",
///         },
///     ],
/// });
/// diagnostics.error(&Printable {
///     message: &"redefinition of method 'banana'",
///     annotations: vec![
///         Spanned {
///             span: Span::new(initial_pos, banana_end),
///             data: "'banana' is first defined here",
///         },
///         Spanned {
///             span: last_pos.get_line(),
///             data: "this redefinition is not allowed",
///         },
///     ],
/// });
///
/// println!("{}", String::from_utf8_lossy(unsafe { &mut *output.get() }));
///
/// assert_eq!(
///     r#"error: banana
///
/// error: this is a banana
///    |
///  1 | banana
///    | ^^^^^^
///
/// warning: this is another banana
///    |
///  1 | banana
///    | ^^^^^^
///
/// info: there is a banana
///
/// info: let me show you!
///    |
///  1 | banana
///    | ^ the banana starts here
///    |      ^ this is the end
///
/// error: a bag of bananas
///    |
///  1 | banana
///    | ^^^^^^ this is the first banana
///    | ^ the first banana starts here
///    | ^^^^^^
///  2 | another banana
///    | ^^^^^^^^^^^^^^ this is the bag of bananas
///
/// "#,
///     &String::from_utf8_lossy(unsafe { &mut *output.get() })
/// );
/// ```
#[derive(Clone)]
pub struct Printable<'ctx, 'caller> {
    pub message: &'caller dyn Display,
    pub annotations: Vec<Spanned<'ctx, &'caller str>>,
}

pub trait AsPrintable<'a, 'b> {
    fn as_printable(&'b self) -> Printable<'a, 'b>;
}

// TODO: implementing on `str` (which is what you would like to do, to
// support calls with warning("aa") instead of warning(&"aa").
impl<'a, 'b> AsPrintable<'a, 'b> for &'b str {
    fn as_printable(&'b self) -> Printable<'a, 'b> {
        Printable {
            message: self,
            annotations: vec![],
        }
    }
}

impl<'a, 'b> AsPrintable<'a, 'b> for Printable<'a, 'b> {
    fn as_printable(&'b self) -> Printable<'a, 'b> {
        Printable {
            message: self.message,
            annotations: self.annotations.iter().cloned().collect(),
        }
    }
}

/// This signature is satisfied by all error messages generated by each
/// subsystem of the compiler (lexer, parser, semantic analysis, ...)
/// and therefore allows error objects to be rendered directly without
/// conversion.
impl<'a, 'b, T: Display + 'b> AsPrintable<'a, 'b> for Spanned<'a, T> {
    fn as_printable(&'b self) -> Printable<'a, 'b> {
        Printable {
            message: &self.data,
            annotations: vec![Spanned {
                span: self.span.clone(),
                data: "",
            }],
        }
    }
}

impl<'a, 'b, T: Display + 'b> AsPrintable<'a, 'b> for MaybeSpanned<'a, T> {
    fn as_printable(&'b self) -> Printable<'a, 'b> {
        match self {
            MaybeSpanned::WithSpan(ref spanned) => Printable {
                message: &spanned.data,
                annotations: vec![Spanned {
                    span: spanned.span.clone(),
                    data: "",
                }],
            },
            MaybeSpanned::WithoutSpan(ref data) => Printable {
                message: data,
                annotations: vec![],
            },
        }
    }
}

/// Width of tabs in error and warning messages
const TAB_WIDTH: usize = 4;

/// When rendering an error with multiple spans, replace lines that are not
/// part of any span with an ellipsis if there is a block of at least
/// `ELIDE_LINE_GAPS_LARGER_THAN` continous lines.
///
/// MUST be greater than 1.
const ELIDE_LINE_GAPS_LARGER_THAN: usize = 3;

// All RENDERING_* characters MUST have a output width of
// of 1 when rendered on the console.
const RENDERING_SINGLE_CHAR_SPAN: char = '⮤';
const RENDERING_SINGLE_CHAR_SPAN_NO_ANNOTATION: char = '^';
const RENDERING_SPAN_START: char = '╰';
const RENDERING_SPAN_MIDDLE: char = '─';
const RENDERING_SPAN_END: char = '╯';
const RENDERING_SPAN_CONTINUATION: char = '┈';
const RENDERING_LINE_NUMBER_SEPARATOR: char = '│';
const RENDERING_LINE_NUMBER_ELLIPSIS: char = '┆';
const RENDERING_LINE_NUMBER_ELLIPSIS_NUM: usize = 2;

// ASCII variant
//const RENDERING_SINGLE_CHAR_SPAN: char = '^';
//const RENDERING_SPAN_START: char = '\\';
//const RENDERING_SPAN_MIDDLE: char = '_';
//const RENDERING_SPAN_END: char = '/';
//const RENDERING_LINE_NUMBER_SEPARATOR: char = '|';
//const RENDERING_LINE_NUMBER_ELLIPSIS: &str = "...";

// Fat variant of Unicode mode
//const RENDERING_SPAN_START: char = '┗';
//const RENDERING_SPAN_MIDDLE: char = '━';
//const RENDERING_SPAN_END: char = '┛';
//const RENDERING_SPAN_CONTINUATION: char = '┅';

/// Color used for rendering escape sequences and other special
/// output...
const HIGHLIGHT_COLOR: Option<Color> = Some(Color::Cyan);
/// Color used for line numbers next to source code output
/// in messages
const LINE_NUMBER_COLOR: Option<Color> = Some(Color::Rgb(0x99, 0x99, 0x99));

// TODO reimplement line truncation

/// Instead of writing errors, warnings and lints generated in the different
/// compiler stages directly to stdout, they are collected in this object.
///
/// This has several advantages:
/// - the output level can be adapted by users.
/// - we have a single source responsible for formatting compiler messages.
pub struct Diagnostics {
    message_count: RefCell<HashMap<MessageLevel, usize>>,
    writer: RefCell<Box<dyn WriteColor>>,
}

impl Diagnostics {
    pub fn new(writer: Box<dyn WriteColor>) -> Self {
        Self {
            writer: RefCell::new(writer),
            message_count: RefCell::new(HashMap::new()),
        }
    }

    /// True when an error message was emitted, false
    /// if only warnings were emitted.
    pub fn errored(&self) -> bool {
        self.message_count
            .borrow()
            .get(&MessageLevel::Error)
            .is_some()
    }

    pub fn count(&self, level: MessageLevel) -> usize {
        self.message_count
            .borrow()
            .get(&level)
            .cloned()
            .unwrap_or(0)
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
    pub fn emit(&self, level: MessageLevel, kind: Printable<'_, '_>) {
        self.increment_level_count(level);
        let mut writer = self.writer.borrow_mut();
        let msg = Message { level, kind };

        // `ok()` surpresses io error
        msg.write(&mut **writer).ok();
    }

    #[allow(dead_code)]
    pub fn warning<'a, 'b, T: AsPrintable<'a, 'b> + ?Sized>(&self, kind: &'b T) {
        self.emit(MessageLevel::Warning, kind.as_printable())
    }

    #[allow(dead_code)]
    pub fn error<'a, 'b, T: AsPrintable<'a, 'b> + ?Sized>(&self, kind: &'b T) {
        self.emit(MessageLevel::Error, kind.as_printable())
    }

    #[allow(dead_code)]
    pub fn info<'a, 'b, T: AsPrintable<'a, 'b> + ?Sized>(&self, kind: &'b T) {
        self.emit(MessageLevel::Info, kind.as_printable())
    }

    fn increment_level_count(&self, level: MessageLevel) {
        let mut message_count = self.message_count.borrow_mut();
        let counter = message_count.entry(level).or_insert(0);
        *counter += 1;
    }
}

#[derive(Copy, Clone, Eq, PartialEq, Hash)]
pub enum MessageLevel {
    Error,
    Warning,
    Info,
}

impl MessageLevel {
    fn color(self) -> Option<Color> {
        // Don't be confused by the return type.
        // `None` means default color in the colorterm
        // crate!
        self.color_variant(0)
    }

    fn color_variant(self, variant: usize) -> Option<Color> {
        (match self {
            MessageLevel::Error => [
                Color::Rgb(0xFF, 0x00, 0x00),
                Color::Rgb(0xFF, 0x00, 0x80),
                Color::Rgb(0xFF, 0x00, 0xB0),
                Color::Rgb(0xFF, 0x00, 0xF0),
            ],
            MessageLevel::Warning => [
                Color::Rgb(0xFF, 0xFF, 0x00),
                Color::Rgb(0xFF, 0xBF, 0x00),
                Color::Rgb(0xFF, 0x8F, 0x00),
                Color::Rgb(0xFF, 0x4F, 0x00),
            ],
            MessageLevel::Info => [
                Color::Rgb(0x00, 0xFF, 0xFF),
                Color::Rgb(0x00, 0xBF, 0xFF),
                Color::Rgb(0x00, 0x8F, 0xFF),
                Color::Rgb(0x00, 0x4F, 0xFF),
            ],
        })
        .iter()
        .map(|color| Some(color.clone()))
        .cycle()
        .nth(variant)
        .unwrap()
    }

    fn name(&self) -> &str {
        match self {
            MessageLevel::Error => "error",
            MessageLevel::Warning => "warning",
            MessageLevel::Info => "info",
        }
    }
}

pub struct Message<'file, 'msg> {
    level: MessageLevel,
    kind: Printable<'file, 'msg>,
}

impl<'file, 'msg> Message<'file, 'msg> {
    pub fn new(level: MessageLevel, kind: Printable<'file, 'msg>) -> Self {
        // sort annotations in reading order using their start position
        let mut sorted_annotations = kind.annotations.clone();
        sorted_annotations
            .sort_by_key(|spanned_str| spanned_str.span.start_position().byte_offset());

        Self {
            level,
            kind: Printable {
                message: kind.message,
                annotations: sorted_annotations,
            },
        }
    }
    pub fn write(&self, writer: &mut dyn WriteColor) -> Result<(), Error> {
        self.write_description(writer)?;
        self.write_code(writer)?;
        writeln!(writer)?;
        Ok(())
    }

    fn write_description(&self, writer: &mut dyn WriteColor) -> Result<(), Error> {
        let mut output = ColorOutput::new(writer);
        output.set_color(self.level.color());
        output.set_bold(true);
        write!(output.writer(), "{}: ", self.level.name())?;

        output.set_color(None);
        writeln!(output.writer(), "{}", self.kind.message)?;

        Ok(())
    }

    /// Get the smallest span that contains all spans of this message
    fn overall_span(&self) -> Option<Span<'file>> {
        let iter = self.kind.annotations.iter();

        let mut overall = match self.kind.annotations.iter().next() {
            None => return None,
            Some(ref spanned) => spanned.span.clone(),
        };

        for spanned in iter {
            overall = Span::combine(&overall, &spanned.span);
        }

        Some(overall)
    }

    // TODO: must take multiple spans
    fn write_code(&self, writer: &mut dyn WriteColor) -> Result<(), Error> {
        // groups spans into blocks that are separated by at most 5 lines. then prints
        // each block separated by dots
        let span = match self.overall_span() {
            None => {
                // no code blocks, return
                return Ok(());
            }
            Some(span) => span,
        };

        let mut output = ColorOutput::new(writer);

        let num_fmt = LineNumberFormatter::new(&span);

        num_fmt.empty_line(output.writer())?;

        let mut currently_active_spans = 0;
        let mut starting_span = self.kind.annotations.iter().peekable();
        let mut skipping = false;

        for (line_number, line) in span.lines().numbered() {
            if let Some(next) = starting_span.peek() {
                if next.span.start_position().line_number() < line_number {
                    starting_span.next();
                }
            }

            if currently_active_spans == 0 {
                if let Some(next) = starting_span.peek() {
                    match next.span.start_position().line_number() {
                        l if skipping && l == line_number => {
                            num_fmt.ellipsis(output.writer())?;
                            skipping = false;
                        }
                        l if l > line_number + ELIDE_LINE_GAPS_LARGER_THAN => {
                            skipping = true;
                            continue;
                        }
                        _ if skipping => {
                            continue;
                        }
                        _ => {}
                    }
                }
            }

            let line_fmt = LineFormatter::new(&line);

            num_fmt.number(output.writer(), line_number)?;
            write!(output.writer(), " ");
            line_fmt.render(output.writer())?;

            for (annotation_index, annotation) in self.kind.annotations.iter().enumerate() {
                if let Some(faulty_part_of_line) = Span::intersect(&annotation.span, &line) {
                    // TODO: implement this without the following 3 assumptions:
                    // - start_pos - end_pos >= 0, guranteed by data structure invariant of Span
                    // - start_term_pos - end_term_pos >= 0, guranteed by monotony of columns
                    //   (a Position.char() can only be rendered to 0 or more terminal characters)
                    // - unwrap(.): both positions are guranteed to exist in the line since we just
                    //   got them from the faulty line, which is a subset of the whole error line
                    let (start_term_pos, end_term_pos) =
                        line_fmt.get_actual_columns(&faulty_part_of_line).unwrap();

                    let term_width = end_term_pos - start_term_pos;

                    num_fmt.spaces(output.writer())?;

                    {
                        let mut output = ColorOutput::new(output.writer());
                        // TODO: we can reuse colors by using
                        // `currently_active_spans` instead of `annotation_index`
                        output.set_color(self.level.color_variant(annotation_index));
                        output.set_bold(true);

                        let (starts_here, front_margin_char) =
                            if annotation.span.start_position().line_number() == line_number {
                                (true, ' ')
                            } else {
                                // if it did not start on this line, it started on
                                // a previous line
                                (false, RENDERING_SPAN_CONTINUATION)
                            };

                        if starts_here {
                            currently_active_spans += 1;
                        }

                        let ends_here = annotation.span.end_position().line_number() == line_number;

                        if ends_here {
                            currently_active_spans -= 1;
                        }

                        if annotation.span.is_single_char() {
                            let has_annotation = annotation.data != "";
                            writeln!(
                                output.writer(),
                                "{margin}{spaces}{marker}{msg}",
                                margin = front_margin_char,
                                spaces = " ".repeat(start_term_pos),
                                marker = (if has_annotation {
                                    RENDERING_SINGLE_CHAR_SPAN
                                } else {
                                    RENDERING_SINGLE_CHAR_SPAN_NO_ANNOTATION
                                }),
                                msg = (if has_annotation {
                                    format!(" {}", annotation.data)
                                } else {
                                    "".to_string()
                                })
                            )?;
                        } else {
                            // NOTE: at this point we know that the WHOLE span is not a single_char,
                            // meaning it has at least length 2. This does however not mean, that
                            // the span of the current line has length > 1.
                            if faulty_part_of_line.is_single_char() {
                                writeln!(
                                    output.writer(),
                                    "{margin}{spaces}{underline}{msg}",
                                    margin = front_margin_char,
                                    spaces = " ".repeat(start_term_pos),
                                    underline = (if ends_here {
                                        format!("{}", RENDERING_SPAN_END)
                                    } else if starts_here {
                                        format!(
                                            "{}{}",
                                            RENDERING_SPAN_START, RENDERING_SPAN_CONTINUATION
                                        )
                                    } else {
                                        format!("{}", RENDERING_SPAN_CONTINUATION)
                                    }),
                                    msg = (if !ends_here || annotation.data == "" {
                                        "".to_string()
                                    } else {
                                        format!(" {}", annotation.data)
                                    })
                                );
                            } else {
                                let tail_offset = if ends_here
                                    && line_fmt
                                        .render_char(faulty_part_of_line.end_position().chr())
                                        .0
                                        .len()
                                        > 0
                                {
                                    1
                                } else {
                                    0
                                };

                                let head_offset = if starts_here
                                    && line_fmt
                                        .render_char(faulty_part_of_line.start_position().chr())
                                        .0
                                        .len()
                                        > 0
                                {
                                    1
                                } else {
                                    0
                                };

                                writeln!(
                                    output.writer(),
                                    "{margin}{spaces}{underline_start}{middle}{end}{msg}",
                                    margin = front_margin_char,
                                    spaces = " ".repeat(start_term_pos),
                                    underline_start = if starts_here {
                                        RENDERING_SPAN_START.to_string()
                                    } else {
                                        "".to_string()
                                    },
                                    // substract once for underline_start and optionally 1 for
                                    // underline_end
                                    middle = RENDERING_SPAN_MIDDLE
                                        .to_string()
                                        .repeat(term_width - tail_offset - head_offset),
                                    end = if ends_here {
                                        RENDERING_SPAN_END
                                    } else {
                                        RENDERING_SPAN_CONTINUATION
                                    },
                                    msg = (if !ends_here || annotation.data == "" {
                                        "".to_string()
                                    } else {
                                        format!(" {}", annotation.data)
                                    })
                                )?;
                            }
                        }
                    }
                }
            }
        }
        Ok(())
    }
}

/// Helper that prints a range of numbers with the correct
/// amount of padding
struct LineNumberFormatter {
    width: usize,
}

impl LineNumberFormatter {
    pub fn new(span: &Span<'_>) -> Self {
        Self {
            width: span.end_position().line_number().to_string().len(),
        }
    }

    pub fn empty_line(&self, writer: &mut dyn WriteColor) -> Result<(), Error> {
        let mut output = ColorOutput::new(writer);
        output.set_color(LINE_NUMBER_COLOR);
        output.set_bold(true);
        writeln!(
            output.writer(),
            " {} {}",
            " ".repeat(self.width),
            RENDERING_LINE_NUMBER_SEPARATOR
        )?;
        Ok(())
    }

    pub fn spaces(&self, writer: &mut dyn WriteColor) -> Result<(), Error> {
        let mut output = ColorOutput::new(writer);
        output.set_color(LINE_NUMBER_COLOR);
        output.set_bold(true);
        write!(
            output.writer(),
            " {} {}",
            " ".repeat(self.width),
            RENDERING_LINE_NUMBER_SEPARATOR
        )?;
        Ok(())
    }

    pub fn ellipsis(&self, writer: &mut dyn WriteColor) -> Result<(), Error> {
        let mut output = ColorOutput::new(writer);
        output.set_color(LINE_NUMBER_COLOR);
        output.set_bold(true);

        for _ in 0..RENDERING_LINE_NUMBER_ELLIPSIS_NUM {
            writeln!(
                output.writer(),
                " {} {}",
                " ".repeat(self.width),
                RENDERING_LINE_NUMBER_ELLIPSIS
            )?;
        }

        Ok(())
    }

    pub fn number(&self, writer: &mut dyn WriteColor, line_number: usize) -> Result<(), Error> {
        let mut output = ColorOutput::new(writer);
        output.set_color(LINE_NUMBER_COLOR);
        output.set_bold(true);
        let padded_number = pad_left(&line_number.to_string(), self.width);
        write!(
            output.writer(),
            " {} {}",
            padded_number,
            RENDERING_LINE_NUMBER_SEPARATOR
        )?;
        Ok(())
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

/// Writes a user-supplied input line in a safe manner by replacing
/// control-characters with escape sequences.
struct LineFormatter<'span, 'file> {
    line: &'span Span<'file>,
}

impl<'span, 'file> LineFormatter<'span, 'file> {
    fn new(line: &'span Span<'file>) -> Self {
        Self { line }
    }

    fn render(&self, writer: &mut dyn WriteColor) -> Result<(), Error> {
        let mut output = ColorOutput::new(writer);

        // TODO: implement an iterator
        let chars = self.line.start_position().iter();

        for position in chars {
            let (text, color) = self.render_char(position.chr());
            output.set_color(color);
            write!(output.writer(), "{}", text)?;

            if position == self.line.end_position() {
                break;
            }
        }

        writeln!(output.writer())?;

        Ok(())
    }

    /// Map terminal columns to `Position` columns. Returns a inclusive
    /// lower bound, and an exclusive upper bound.
    ///
    /// Each printed character does not actually take up monospace grid cell.
    /// For example a TAB character may be represented by 4 spaces. This
    /// function will return the actual number of 'monospace grid cells'
    /// rendered before the given
    /// position.
    ///
    /// Returns `None` if the column is out of bounds.
    fn get_actual_columns(&self, span: &Span<'_>) -> Option<(usize, usize)> {
        let lower = self.len_printed_before(span.start_position().column());
        let upper = self.len_printed_before(span.end_position().column());

        match (lower, upper) {
            (Some(lower), Some(upper)) => {
                let last_char_width = self.render_char(span.end_position().chr()).0.len();
                Some((lower, upper + last_char_width))
            }
            _ => None,
        }
    }

    fn len_printed_before(&self, col: usize) -> Option<usize> {
        // TODO: get rid of this nonsense
        // NOTE: it would actually be nice to condition the Position on the Line
        // instead of AsciiFile. Thinking of this, we could actually just do
        // `AsciiFile::new((span.as_str().as_bytes()))`. Meaning AsciiFile is
        // not a file, but a View
        // that restricts the
        // linked lists in Positions and Spans to a subset of the file.
        // TODO: implement an iterator on span, or
        // span.to_view().iter()/.to_ascii_file().iter() this method is
        // inherintly unsafe
        // because we do not have
        // a way to restrict
        // positions in a type safe manner.
        if self.line.len() < col {
            return None;
        }

        let chars = self.line.start_position().iter();

        let mut actual_column = 0;

        for position in chars {
            if position.column() == col {
                break;
            }

            actual_column += self.render_char(position.chr()).0.len();
        }

        Some(actual_column)
    }

    fn render_char(&self, chr: char) -> (String, Option<Color>) {
        match chr {
            '\t' => (" ".repeat(TAB_WIDTH), None),
            '\r' | '\n' => ("".to_string(), None),
            chr if chr.is_control() => (
                format!("{{{}}}", u8_to_printable_representation(chr as u8)),
                HIGHLIGHT_COLOR,
            ),
            _ => (chr.to_string(), None),
        }
    }
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
