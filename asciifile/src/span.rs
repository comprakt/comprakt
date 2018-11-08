use crate::Position;
use std::{
    cmp::{max, min},
    fmt,
};

/// A guranteed valid text range of the input file.
///
/// A span is defined by a start and an end position. The range is inclusive on
/// the lower bound, meaning it does contain the character at the start
/// position, and *exclusive* on the upper bound, meaning the end position is
/// **not** part of the span.
///
/// Analog to [`Position`](struct.Position.html), rows and columns are zero
/// indexed. This means that the first character of a file is positioned at
/// column 0 in row 0.
#[derive(Debug, Clone)]
pub struct Span<'f> {
    // TODO: make private
    /// position of the first char in the span.
    pub start: Position<'f>,
    /// position after the last char in the span.
    pub end: Position<'f>,
}

impl<'f> Span<'f> {
    pub fn new(a: Position<'f>, b: Position<'f>) -> Self {
        Self {
            start: min(a, b),
            end: max(a, b),
        }
    }

    pub fn is_empty(&self) -> bool {
        self.end.byte_offset() == self.start.byte_offset()
    }

    pub fn is_single_char(&self) -> bool {
        self.end.byte_offset() - self.start.byte_offset() == 1
    }

    pub fn start_position(&self) -> Position<'_> {
        self.start
    }

    pub fn end_position(&self) -> Position<'_> {
        self.end
    }

    /// Check if a span extends over multiple lines
    ///
    /// This will consider spans that contain a single trailing
    /// whitespace as multiline.
    ///
    /// ```
    /// use std::io;
    /// # fn main() -> Result<(), ()> {
    /// let file = AsciiFile::new("\n".as_bytes()).unwrap().iter().collect();
    /// let first = file.next().unwrap();
    /// let last = file.last().unwrap();
    /// let span = Span::new(first, last);
    /// assert!(span.is_multiline())
    /// # Ok(())
    /// # }
    /// ```
    pub fn is_multiline(&self) -> bool {
        self.start.row != self.end.row
    }

    pub fn combine(a: &Span<'f>, b: &Span<'f>) -> Span<'f> {
        Span {
            start: min(a.start, b.start),
            end: max(a.end, b.end),
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
