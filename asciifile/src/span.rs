//! # Input File Ranges
//!
//! A `Span` is equivalent to a slice of the input file annotated with
//! line and column of start and end indices.
//!
//! A `Span` is immutable by design. Mutations will always return a new
//! instance of a Span.
//!
//! A `Span` is a guranteed valid text range of the input file. This means
//! actions like `as_str()` cannot fail with out of bounds indices on run-time.
//!
//! The range defined by the start and end position of a `Span` is inclusive on
//! the both sides (lower and upper bound). As a result, `as_str()` will contain
//! the character pointed at by the start position and the character pointed to
//! by the end position. Beware that this is not equivalent to rusts behaviour
//! on slices, which will not contain the upper bound if `low..high` is used.
//! By including the upper bound in the `Span`, we do not have to generate a
//! fake `EOF`-Position/character beyond the real end of input to create a
//! `Span` that contains the last character of the input file.
//! This also means that you cannot create an empty `Span`.
//!
//! Analog to [`Position`](struct.Position.html), rows and columns are zero
//! indexed. This means that the first character of a file is positioned at
//! column 0 in row 0.
use crate::Position;
use std::{
    cmp::{max, min},
    fmt,
};

///
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Span<'f> {
    /// position of the first char in the span.
    start: Position<'f>,
    /// position of the last char in the span.
    end: Position<'f>,
}

impl<'f> Span<'f> {
    pub fn new(a: Position<'f>, b: Position<'f>) -> Self {
        //debug_assert!(a.file == b.file);

        Self {
            start: min(a, b),
            end: max(a, b),
        }
    }

    /// Creates a span containing only the given position
    ///
    /// ```
    /// use asciifile::{AsciiFile, Position, Span};
    ///
    /// let file = AsciiFile::new(b"ABCD").unwrap();
    /// let position = file.iter().nth(2).unwrap();
    /// let span = Span::from_single_position(position);
    /// assert_eq!("C", span.as_str());
    /// ```
    pub fn from_single_position(position: Position<'f>) -> Self {
        Span {
            start: position,
            end: position,
        }
    }

    /// Creates a span containing only the given position
    ///
    /// ```
    /// use asciifile::{AsciiFile, Position, Span};
    ///
    /// let file = AsciiFile::new(b"abcdfeghAAA").unwrap();
    ///
    /// let positions = file
    ///     .iter()
    ///     .take_while(|position| position.chr().is_lowercase())
    ///     .collect::<Vec<_>>();
    ///
    /// let span = Span::from_positions(&positions).unwrap();
    /// assert_eq!("abcdfegh", span.as_str());
    /// ```
    pub fn as_str(&'f self) -> &'f str {
        // the range is inclusive on both sides!
        &self.start.file()[self.start.byte_offset()..=self.end.byte_offset()]
    }

    pub fn is_empty(&self) -> bool {
        self.end.byte_offset() == self.start.byte_offset()
    }

    pub fn is_single_char(&self) -> bool {
        self.end.byte_offset() - self.start.byte_offset() == 1
    }

    pub fn start_position(&self) -> Position<'f> {
        self.start
    }

    pub fn end_position(&self) -> Position<'f> {
        self.end
    }

    /// Check if a span extends over multiple lines
    ///
    /// This will consider spans that contain a single trailing
    /// whitespace as multiline.
    ///
    /// ```
    /// use asciifile::{AsciiFile, Span};
    ///
    /// let file = AsciiFile::new("a\n".as_bytes()).unwrap();
    /// let first = file.iter().next().unwrap();
    /// let last = file.iter().last().unwrap();
    /// let span = Span::new(first, last);
    /// assert!(span.is_multiline());
    /// ```
    ///
    /// A span only containing a newline will not be considered
    /// multiline.
    ///
    /// ```
    /// use asciifile::{AsciiFile, Span};
    ///
    /// let file = AsciiFile::new("\n".as_bytes()).unwrap();
    /// let newline = file.iter().next().unwrap();
    /// let span = Span::new(newline, newline);
    /// assert!(!span.is_multiline());
    /// ```
    pub fn is_multiline(&self) -> bool {
        self.start.row() != self.end.row()
    }

    /// An iterator over the lines of a span.
    pub fn lines<'span>(&'span self) -> LineIterator<'span, 'f> {
        LineIterator::new(self)
    }

    /// extends the span to include the given position
    pub fn extend_to_position(self, position: &Position<'f>) -> Span<'f> {
        Span::combine(&position.to_single_char_span(), &self)
    }

    /// test if a span contains another span
    pub fn has_subset(self, span: &Span<'f>) -> bool {
        Span::combine(span, &self) == self
    }

    pub fn combine(a: &Span<'f>, b: &Span<'f>) -> Span<'f> {
        Span {
            start: min(a.start, b.start),
            end: max(a.end, b.end),
        }
    }

    /// Get the overlapping part of two spans.
    /// Returns `None` if the spans are disjunct (do not overlap).
    pub fn intersect(a: &Span<'f>, b: &Span<'f>) -> Option<Span<'f>> {
        if b.start > a.end || a.start > b.end {
            None
        } else {
            Some(Span {
                start: max(a.start, b.start),
                end: min(a.end, b.end),
            })
        }
    }

    /// Get the number of characters in a span
    pub fn len(&self) -> usize {
        self.as_str().len()
    }

    pub fn from_positions(positions: &[Position<'f>]) -> Option<Self> {
        match positions {
            [] => None,
            [single] => Some(Span::from_single_position(*single)),
            [head, tail..] => {
                let mut span = Span::from_single_position(*head);
                for position in tail {
                    span = span.extend_to_position(position)
                }
                Some(span)
            }
        }
    }
}

impl fmt::Display for Span<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if self.is_single_char() {
            write!(f, "{}", self.start)
        } else {
            write!(f, "{}-{}", self.start, self.end)
        }
    }
}

/// An Iterator over the lines of a `Span`.
///
/// This will always emit the whole lines. So for example,
/// if your span contains the sequence "d\ne" in the file
/// "abcd\nefgh\nijkl" the iterator will return the following
/// two lines: ["abcd\n", "efgh\n"]
///
/// Analog to `Position::get_line` this function will append
/// newlines to the end of each line. This means that `is_mutiline(.)`
/// will be true, since "\n" is positioned at the next line.
///
/// ```
/// use asciifile::{AsciiFile, Position, Span};
///
/// let file = AsciiFile::new(b"abcd\nefgh\nijkl").unwrap();
/// let start = file.iter().nth(2).unwrap();
/// assert_eq!(start.chr(), 'c');
/// let end = file.iter().nth(9).unwrap();
/// assert_eq!(end.chr(), '\n');
/// let span = Span::new(start, end);
/// assert_eq!(span.as_str(), "cd\nefgh\n");
///
/// let lines = span
///     .lines()
///     .map(|span| span.as_str().to_string())
///     .collect::<Vec<_>>();
///
/// assert_eq!(lines, vec!["abcd\n".to_string(), "efgh\n".to_string(),]);
/// ```
pub struct LineIterator<'span, 'file> {
    span: &'span Span<'file>,
    line_to_emit: Option<Span<'file>>,
}

impl<'span, 'file> LineIterator<'span, 'file> {
    pub fn new(span: &'span Span<'file>) -> Self {
        Self {
            span,
            line_to_emit: Some(span.start_position().get_line()),
        }
    }

    pub fn numbered(self) -> LineNumberIterator<'span, 'file> {
        LineNumberIterator::new(self)
    }
}

impl<'span, 'file> Iterator for LineIterator<'span, 'file> {
    type Item = Span<'file>;

    fn next(&mut self) -> Option<Self::Item> {
        match self.line_to_emit {
            None => None,
            Some(ref line) => {
                let line_to_emit = line.clone();

                self.line_to_emit = if self.span.end_position() > line.end_position() {
                    line.end_position()
                        .next()
                        .map(|first_char| first_char.get_line())
                } else {
                    None
                };

                Some(line_to_emit)
            }
        }
    }
}

pub struct LineNumberIterator<'span, 'file> {
    lines: LineIterator<'span, 'file>,
}

impl<'span, 'file> LineNumberIterator<'span, 'file> {
    pub fn new(lines: LineIterator<'span, 'file>) -> Self {
        Self { lines }
    }
}

impl<'span, 'file> Iterator for LineNumberIterator<'span, 'file> {
    type Item = (usize, Span<'file>);

    fn next(&mut self) -> Option<Self::Item> {
        self.lines
            .next()
            .map(|span| (span.start_position().line_number(), span))
    }
}
