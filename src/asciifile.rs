use core::cmp::Ordering;
use failure::Fail;
use std::cmp::{max, min};

#[derive(Debug)]
pub struct AsciiFile<'m> {
    mapping: &'m [u8],
}

pub type AsciiFileIterator<'t> = PositionedChars<'t, std::str::Chars<'t>>;

#[derive(Debug, Fail)]
pub enum EncodingError {
    #[fail(
        display = "input contains non-ascii character at byte offset {}: {}<?>",
        position,
        prev
    )]
    NotAscii { position: usize, prev: String },
}

const ENCODING_ERROR_MAX_CONTEXT_LENGTH: usize = 80;

#[derive(Debug, Eq, PartialEq, Copy, Clone)]
pub enum LineTruncation {
    Truncated,
    NotTruncated,
}

impl<'m> AsciiFile<'m> {
    /// return the start index of the line without the newline character (\n).
    /// This means you can make slices that do NOT include the newline
    /// character by simply using the return value of this function as
    /// lower bound X in the range X..Y.
    ///
    /// If the cursor/byte offset is on a newline character, the newline
    /// character will belong to the previous line, meaning the return
    /// value will be the given byte offset and
    /// not the byte offset of the next new line.
    fn get_line_end_idx(
        &'m self,
        byte_offset: usize,
        max_context_length: usize,
    ) -> (LineTruncation, usize) {
        AsciiFile::get_line_end_idx_byteslice(self.mapping, byte_offset, max_context_length)
    }

    fn get_line_end_idx_byteslice(
        mapping: &[u8],
        byte_offset: usize,
        max_context_length: usize,
    ) -> (LineTruncation, usize) {
        debug_assert!(byte_offset <= mapping.len());

        let region_max_end = byte_offset + max_context_length;
        let (truncation, region_end) = if mapping.len() > region_max_end {
            (LineTruncation::Truncated, region_max_end)
        } else {
            (LineTruncation::NotTruncated, mapping.len())
        };

        let region = &mapping[byte_offset..region_end];

        let newline_position = region
            .iter()
            .position(|&chr| chr as char == '\n')
            .map(|pos| pos + byte_offset);

        match newline_position {
            Some(position) => (LineTruncation::NotTruncated, position),
            None => (truncation, region_end),
        }
    }

    /// return the end index of the line includes the newline character (\n).
    /// This means you can make slices that do NOT include the newline
    /// character by simply using the return value of this function as
    /// upper bound Y in the range X..Y.
    ///
    /// If the cursor/byte offset is on a newline character, the newline
    /// character will belong to the previous line, meaning the return
    /// value will not be the given by the byte
    /// offset, but the byte offset of the previous new line.
    fn get_line_start_idx(
        &'m self,
        byte_offset: usize,
        max_context_length: usize,
    ) -> (LineTruncation, usize) {
        AsciiFile::get_line_start_idx_byteslice(self.mapping, byte_offset, max_context_length)
    }

    fn get_line_start_idx_byteslice(
        mapping: &[u8],
        byte_offset: usize,
        max_context_length: usize,
    ) -> (LineTruncation, usize) {
        debug_assert!(byte_offset <= mapping.len());

        let (truncation, region_start) = byte_offset
            .checked_sub(max_context_length)
            .map(|start| (LineTruncation::Truncated, start))
            .unwrap_or((LineTruncation::NotTruncated, 0));

        let region = &mapping[region_start..byte_offset];

        region
            .iter()
            .rposition(|&chr| chr as char == '\n')
            .map(|pos| (LineTruncation::NotTruncated, pos + region_start + 1))
            .unwrap_or((truncation, region_start))
    }

    // cost: O(fileLen) since we need to check if all chars are ASCII
    pub fn new(mapping: &'m [u8]) -> Result<AsciiFile<'m>, EncodingError> {
        if let Some(position) = mapping.iter().position(|c| !c.is_ascii()) {
            let (truncation, start_idx) = AsciiFile::get_line_start_idx_byteslice(
                mapping,
                position,
                ENCODING_ERROR_MAX_CONTEXT_LENGTH,
            );
            // We know everything until now has been ASCII
            let prev: &str =
                unsafe { std::str::from_utf8_unchecked(&mapping[start_idx..position]) };
            let prev = format!(
                "{dots}{context}",
                context = prev,
                dots = if truncation == LineTruncation::Truncated {
                    "..."
                } else {
                    ""
                }
            );
            return Err(EncodingError::NotAscii { position, prev });
        }

        Ok(AsciiFile { mapping })
    }

    pub fn iter(&self) -> AsciiFileIterator<'_> {
        let s: &str = self;
        PositionedChars {
            curpos: Position {
                row: 0,
                col: 0,
                byte_offset: 0,
                file: self,
            },
            ascii_file: s.chars(),
            peeked: String::new(),
        }
    }
}

#[derive(Debug, Clone)]
pub struct Span<'f> {
    pub start: Position<'f>,
    pub end: Position<'f>,
}

impl Span<'_> {
    pub fn is_single_char(&self) -> bool {
        if self.start.row != self.end.row {
            return false;
        }
        // ignore inconsisent end before start
        self.end
            .col
            .checked_sub(self.start.col)
            .map(|d| d <= 1)
            .unwrap_or(false)
    }

    /// Check if a span extends over multiple lines
    ///
    /// This will consider spans that contain a single trailing
    /// whitespace, e.g. "a\n" as multiline.
    pub fn is_multiline(&self) -> bool {
        self.start.row != self.end.row
    }

    pub fn combine<'f>(a: &Span<'f>, b: &Span<'f>) -> Span<'f> {
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

#[derive(Debug, Clone)]
pub struct Spanned<'f, T> {
    pub span: Span<'f>,
    pub data: T,
}

impl<T> fmt::Display for Spanned<'_, T>
where
    T: fmt::Display,
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{} at {}", self.data, self.span)
    }
}

impl<'f, T> Spanned<'f, T> {
    pub fn new(start: Position<'f>, end: Position<'f>, value: T) -> Self {
        Spanned {
            span: Span { start, end },
            data: value,
        }
    }

    pub fn map<U, F>(&self, f: F) -> Spanned<'f, U>
    where
        F: FnOnce(&T) -> U,
    {
        Spanned {
            span: self.span.clone(),
            data: f(&self.data),
        }
    }
}

use std::ops::Deref;

#[derive(Copy, Clone)]
pub struct Position<'t> {
    pub row: usize,
    pub col: usize,
    byte_offset: usize,
    file: &'t AsciiFile<'t>,
}

#[cfg(test)]
impl<'t> Position<'t> {
    pub fn dummy() -> Self {
        Position {
            row: 0,
            col: 0,
            byte_offset: 0,
            file: Box::leak(box AsciiFile::new(&[]).unwrap()),
        }
    }
}

impl PartialOrd for Position<'_> {
    fn partial_cmp(&self, other: &Position<'_>) -> Option<Ordering> {
        // TODO: the typesystem does not gurantee that both positions come from the
        // same file
        Some(self.byte_offset.cmp(&other.byte_offset))
    }
}

impl Ord for Position<'_> {
    fn cmp(&self, other: &Position<'_>) -> Ordering {
        self.byte_offset.cmp(&other.byte_offset)
    }
}

impl<'t> Position<'t> {
    pub fn to_single_char_span(&self) -> Span<'t> {
        Span {
            start: *self,
            end: self.consume("\n"),
        }
    }

    pub fn char(&self) -> char {
        self.file.mapping[self.byte_offset] as char
    }

    pub fn byte(&self) -> u8 {
        self.file.mapping[self.byte_offset]
    }

    pub fn get_line(
        &self,
        max_context_length_before: usize,
        max_context_length_after: usize,
    ) -> (LineTruncation, &'t str, LineTruncation) {
        let (truncated_before, start) = self
            .file
            .get_line_start_idx(self.byte_offset, max_context_length_before);
        let (truncated_after, end) = self
            .file
            .get_line_end_idx(self.byte_offset, max_context_length_after);
        let line = &self.file.mapping[start..end];

        (
            truncated_before,
            unsafe { std::str::from_utf8_unchecked(line) },
            truncated_after,
        )
    }

    /// Advance the position by examining a row of input characters. Will
    /// update the row count for each new line encountered, will update the
    /// column count for all other characters.
    pub fn consume(&self, upcoming: &str) -> Self {
        let matches: Vec<_> = upcoming.match_indices('\n').collect();
        let count = matches.len();

        match matches.last() {
            None => Self {
                // single line
                col: self.col + upcoming.len(),
                row: self.row,
                byte_offset: self.byte_offset + upcoming.len(),
                file: self.file,
            },
            Some((start_idx, _)) => Self {
                // multi line
                col: 0,
                row: self.row + count,
                byte_offset: self.byte_offset + upcoming.len() - start_idx,
                file: self.file,
            },
        }
    }

    pub fn to_line_start(&self) -> Self {
        let (_start_truncated, start_idx) = self
            .file
            .get_line_start_idx(self.byte_offset, self.file.mapping.len());

        Self {
            col: 0,
            row: self.row,
            byte_offset: start_idx,
            file: self.file,
        }
    }

    pub fn next_line(&self) -> Result<Self, ()> {
        let (_end_truncated, end_idx) = self
            .file
            .get_line_end_idx(self.byte_offset, self.file.mapping.len());
        let is_eof = end_idx == self.file.mapping.len() - 1;

        if is_eof {
            return Err(());
        }

        Ok(Self {
            col: 0,
            row: self.row + 1,
            byte_offset: end_idx + 1,
            file: self.file,
        })
    }
}

use std::fmt::{self, Debug, Display};

impl Display for Position<'_> {
    fn fmt(&self, fmt: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(fmt, "{}:{}", self.row + 1, self.col + 1)
    }
}

impl Debug for Position<'_> {
    fn fmt(&self, fmt: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            fmt,
            "Position{{row: {:?}, col: {:?}, byte_offset: {:?}, file: {:?}}}",
            self.row, self.col, self.byte_offset, self.file as *const _
        )
    }
}

impl PartialEq for Position<'_> {
    fn eq(&self, rhs: &Position<'_>) -> bool {
        self.byte_offset == rhs.byte_offset && self.file as *const _ == (rhs.file as *const _)
    }
}

impl Eq for Position<'_> {}

#[derive(Debug, PartialEq)]
pub struct PositionedChar<'t>(pub Position<'t>, pub char);

pub struct PositionedChars<'t, I>
where
    I: Iterator<Item = char>,
{
    curpos: Position<'t>,
    ascii_file: I,
    peeked: String,
}

impl<'t, I> Iterator for PositionedChars<'t, I>
where
    I: Iterator<Item = char>,
{
    type Item = PositionedChar<'t>;
    fn next(&mut self) -> Option<PositionedChar<'t>> {
        let c = self.peek()?;
        self.peeked.remove(0);

        let retpos = self.curpos;
        self.curpos.byte_offset += 1;
        self.curpos.col += 1;
        if c == '\n' {
            self.curpos.row += 1;
            self.curpos.col = 0;
        }

        Some(PositionedChar(retpos, c))
    }
}

impl<'t, I> PositionedChars<'t, I>
where
    I: Iterator<Item = char>,
{
    pub fn try_peek_multiple(&mut self, n: usize) -> Option<&str> {
        let peeked = self.peek_multiple(n);
        if peeked.len() < n {
            None
        } else {
            Some(peeked)
        }
    }

    /// Peek the next n characters, without advancing the iteratior (it is
    /// actually advanced, of course, but this is hidden by a buffer).
    /// If there are less than n charcters left, the returned slice is shorter
    /// than that
    pub fn peek_multiple(&mut self, n: usize) -> &str {
        // We already have .len() characters "in stock", so we get the remaining n-len,
        // if there are then many
        for _ in self.peeked.len()..n {
            match self.ascii_file.next() {
                Some(next) => self.peeked.push(next),
                None => break,
            }
        }

        &self.peeked[0..self.peeked.len().min(n)]
    }

    /// Can't use peekable, because we don't care about position when peeking
    pub fn peek(&mut self) -> Option<char> {
        self.peek_multiple(1).chars().next()
    }

    pub fn eof_reached(&mut self) -> bool {
        self.peek().is_none()
    }

    pub fn current_position(&mut self) -> Position<'t> {
        self.curpos
    }
}

impl<'m> Deref for AsciiFile<'m> {
    type Target = str;
    fn deref(&self) -> &Self::Target {
        unsafe { std::str::from_utf8_unchecked(&self.mapping) }
    }
}

impl<'m, 'a> Into<&'m str> for &'a AsciiFile<'m> {
    fn into(self) -> &'m str {
        unsafe { std::str::from_utf8_unchecked(&self.mapping) }
    }
}

#[cfg(test)]
mod tests {

    use super::*;

    fn testfile(s: &str) -> &[u8] {
        s.as_bytes()
    }

    #[test]
    fn works_with_ascii() {
        let instr = "ABCDEFG\n\t";
        let f = testfile(instr);
        let af = AsciiFile::new(f).unwrap();
        let s: &str = &af;
        assert!(instr == s);
    }

    #[test]
    fn returns_err_on_non_ascii() {
        let f = testfile("one💩two");
        let mm = AsciiFile::new(f);
        assert!(mm.is_err());
        let e = mm.err().unwrap();
        println!("{:?}", e);
        let EncodingError::NotAscii { position, prev } = e;
        assert_eq!(position, 3);
        assert_eq!(prev, "one");
    }

    #[test]
    fn returns_err_on_non_ascii_non_utf8() {
        let input: Vec<u16> = "ä".encode_utf16().collect();
        let input: &[u8] =
            unsafe { std::slice::from_raw_parts(input.as_ptr() as *const u8, 2 * input.len()) };
        assert_eq!(input.len(), 2);
        match std::str::from_utf8(input) {
            Ok(s) => panic!("test implementation error {:?}", s),
            Err(x) => println!("ä in utf16 is not valid utf8, but let's be sure {:?}", x),
        }
        let r = AsciiFile::new(&input);
        assert!(r.is_err());
        let EncodingError::NotAscii { position, prev } = r.err().unwrap();
        assert_eq!(position, 0);
        assert_eq!(prev, "");
    }

    #[test]
    fn err_on_ascii_context_only_current_line() {
        let f = testfile("0\n12345💩");
        let mm = AsciiFile::new(f);
        assert!(mm.is_err());
        let e = mm.err().unwrap();
        println!("{:?}", e);
        let EncodingError::NotAscii { prev, .. } = e;
        assert_eq!(prev, "12345");
    }

    #[test]
    fn err_on_ascii_context_bounded() {
        let long_thing = "9876543210".repeat(ENCODING_ERROR_MAX_CONTEXT_LENGTH / 10);
        let instr = format!("abcd{}💩", long_thing);
        let f = testfile(&instr);
        let mm = AsciiFile::new(f);

        assert!(mm.is_err());

        let e = mm.err().unwrap();
        let EncodingError::NotAscii { prev, .. } = e;

        assert_eq!(prev, format!("...{}", long_thing));
    }

    #[test]
    fn test_indices() {
        let max_ctx = 80;
        {
            let single_line = b"|123456789";
            let (end_truncation, line_end) =
                AsciiFile::get_line_end_idx_byteslice(single_line, 5, max_ctx);
            println!("{:?} {:?}", end_truncation, line_end);
            assert_eq!(LineTruncation::NotTruncated, end_truncation);
            assert_eq!(single_line, &single_line[..line_end]);
        }

        {
            let single_line = b"|123456789";
            let (start_truncation, line_start) =
                AsciiFile::get_line_start_idx_byteslice(single_line, 5, max_ctx);

            assert_eq!(LineTruncation::NotTruncated, start_truncation);
            assert_eq!(single_line, &single_line[line_start..]);
        }

        {
            let multi_line = b"|123456789\nabcdefghijklmno";
            let (end_truncation, line_end) =
                AsciiFile::get_line_end_idx_byteslice(multi_line, 15, max_ctx);
            assert_eq!(LineTruncation::NotTruncated, end_truncation);
            assert_eq!(multi_line, &multi_line[..line_end]);
        }

        {
            let multi_line = b"|123456789\nabcdefghijklmno";
            let (end_truncation, line_end) =
                AsciiFile::get_line_end_idx_byteslice(multi_line, 5, max_ctx);
            assert_eq!(LineTruncation::NotTruncated, end_truncation);
            assert_eq!(b"|123456789", &multi_line[..line_end]);
        }

        {
            let multi_line = b"|123456789\nabcdefghijklmno";
            let (start_truncation, line_start) =
                AsciiFile::get_line_start_idx_byteslice(multi_line, 15, max_ctx);
            assert_eq!(LineTruncation::NotTruncated, start_truncation);
            assert_eq!(b"abcdefghijklmno", &multi_line[line_start..]);
        }

        {
            let multi_line = b"|123456789\nabcdefghijklmno";
            let (start_truncation, line_start) =
                AsciiFile::get_line_start_idx_byteslice(multi_line, 5, max_ctx);
            assert_eq!(LineTruncation::NotTruncated, start_truncation);
            assert_eq!(multi_line, &multi_line[line_start..]);
        }

        {
            let long_line = b"|123456789\n".repeat(ENCODING_ERROR_MAX_CONTEXT_LENGTH);
            let (start_truncation, line_start) =
                AsciiFile::get_line_start_idx_byteslice(&long_line, 20, max_ctx);
            let (end_truncation, line_end) =
                AsciiFile::get_line_end_idx_byteslice(&long_line, 20, max_ctx);

            assert_eq!(LineTruncation::NotTruncated, start_truncation);
            assert_eq!(LineTruncation::NotTruncated, end_truncation);
            assert_eq!(b"|123456789", &long_line[line_start..line_end]);
        }

        {
            let empty_line = b"\n\n\n".repeat(ENCODING_ERROR_MAX_CONTEXT_LENGTH);
            let (start_truncation, line_start) =
                AsciiFile::get_line_start_idx_byteslice(&empty_line, 1, max_ctx);
            let (end_truncation, line_end) =
                AsciiFile::get_line_end_idx_byteslice(&empty_line, 1, max_ctx);

            assert_eq!(LineTruncation::NotTruncated, start_truncation);
            assert_eq!(LineTruncation::NotTruncated, end_truncation);
            assert_eq!(b"", &empty_line[line_start..line_end]);
        }

        {
            let new_line = b"a\nb\nc\nd\n";

            assert_eq!(
                b"a\nb",
                &new_line[..AsciiFile::get_line_end_idx_byteslice(new_line, 3, max_ctx).1]
            );

            assert_eq!(
                b"b",
                &new_line[AsciiFile::get_line_start_idx_byteslice(new_line, 3, max_ctx).1
                    ..AsciiFile::get_line_end_idx_byteslice(new_line, 3, max_ctx).1]
            );
        }
    }

    #[test]
    fn iterator_works() {
        let f = testfile("one\ntwo three\nfour\n\n");
        let af = AsciiFile::new(f).unwrap();
        let res: Vec<PositionedChar<'_>> = af.iter().collect();

        #[rustfmt::skip]
        let exp = vec![
            PositionedChar(Position { byte_offset:  0, row: 0, col: 0, file:  &af }, 'o'),
            PositionedChar(Position { byte_offset:  1, row: 0, col: 1 , file: &af }, 'n'),
            PositionedChar(Position { byte_offset:  2, row: 0, col: 2 , file: &af }, 'e'),
            PositionedChar(Position { byte_offset:  3, row: 0, col: 3 , file: &af }, '\n'),
            PositionedChar(Position { byte_offset:  4, row: 1, col: 0 , file: &af }, 't'),
            PositionedChar(Position { byte_offset:  5, row: 1, col: 1 , file: &af }, 'w'),
            PositionedChar(Position { byte_offset:  6, row: 1, col: 2 , file: &af }, 'o'),
            PositionedChar(Position { byte_offset:  7, row: 1, col: 3 , file: &af }, ' '),
            PositionedChar(Position { byte_offset:  8, row: 1, col: 4 , file: &af }, 't'),
            PositionedChar(Position { byte_offset:  9, row: 1, col: 5 , file: &af }, 'h'),
            PositionedChar(Position { byte_offset: 10, row: 1, col: 6 , file: &af }, 'r'),
            PositionedChar(Position { byte_offset: 11, row: 1, col: 7 , file: &af }, 'e'),
            PositionedChar(Position { byte_offset: 12, row: 1, col: 8 , file: &af }, 'e'),
            PositionedChar(Position { byte_offset: 13, row: 1, col: 9 , file: &af }, '\n'),
            PositionedChar(Position { byte_offset: 14, row: 2, col: 0 , file: &af }, 'f'),
            PositionedChar(Position { byte_offset: 15, row: 2, col: 1 , file: &af }, 'o'),
            PositionedChar(Position { byte_offset: 16, row: 2, col: 2 , file: &af }, 'u'),
            PositionedChar(Position { byte_offset: 17, row: 2, col: 3 , file: &af }, 'r'),
            PositionedChar(Position { byte_offset: 18, row: 2, col: 4 , file: &af }, '\n'),
            PositionedChar(Position { byte_offset: 19, row: 3, col: 0 , file: &af }, '\n'),
        ];
        assert_eq!(exp, res);
    }

    #[test]
    fn peeking_works() {
        let f = testfile("one\ntwo three\nfour\n");
        let af = AsciiFile::new(f).unwrap();
        let mut i = af.iter();

        let mut peeked = i.peek();
        while let Some(PositionedChar(_, c)) = i.next() {
            assert_eq!(c, peeked.unwrap());
            peeked = i.peek();
        }
    }

    #[test]
    fn multi_peeking_works() {
        let s = "one\ntwo three\nfour\n\n";
        let f = testfile(s);
        let af = AsciiFile::new(f).unwrap();
        let mut i = af.iter();

        let peeked = i.peek_multiple(s.len());

        assert_eq!(s, peeked);

        #[rustfmt::skip]
        let exp = vec![
            PositionedChar(Position { byte_offset:  0, row: 0, col: 0, file: &af}, 'o'),
            PositionedChar(Position { byte_offset:  1, row: 0, col: 1, file: &af}, 'n'),
            PositionedChar(Position { byte_offset:  2, row: 0, col: 2, file: &af}, 'e'),
            PositionedChar(Position { byte_offset:  3, row: 0, col: 3, file: &af}, '\n'),
            PositionedChar(Position { byte_offset:  4, row: 1, col: 0, file: &af}, 't'),
            PositionedChar(Position { byte_offset:  5, row: 1, col: 1, file: &af}, 'w'),
            PositionedChar(Position { byte_offset:  6, row: 1, col: 2, file: &af}, 'o'),
            PositionedChar(Position { byte_offset:  7, row: 1, col: 3, file: &af}, ' '),
            PositionedChar(Position { byte_offset:  8, row: 1, col: 4, file: &af}, 't'),
            PositionedChar(Position { byte_offset:  9, row: 1, col: 5, file: &af}, 'h'),
            PositionedChar(Position { byte_offset: 10, row: 1, col: 6, file: &af}, 'r'),
            PositionedChar(Position { byte_offset: 11, row: 1, col: 7, file: &af}, 'e'),
            PositionedChar(Position { byte_offset: 12, row: 1, col: 8, file: &af}, 'e'),
            PositionedChar(Position { byte_offset: 13, row: 1, col: 9, file: &af}, '\n'),
            PositionedChar(Position { byte_offset: 14, row: 2, col: 0, file: &af}, 'f'),
            PositionedChar(Position { byte_offset: 15, row: 2, col: 1, file: &af}, 'o'),
            PositionedChar(Position { byte_offset: 16, row: 2, col: 2, file: &af}, 'u'),
            PositionedChar(Position { byte_offset: 17, row: 2, col: 3, file: &af}, 'r'),
            PositionedChar(Position { byte_offset: 18, row: 2, col: 4, file: &af}, '\n'),
            PositionedChar(Position { byte_offset: 19, row: 3, col: 0, file: &af}, '\n'),
        ];

        let res: Vec<PositionedChar<'_>> = i.collect();

        assert_eq!(exp, res);
    }

    #[test]
    fn next_returns_correct_position() {
        let s = "one\ntwo three\nfour\n\n";
        let f = testfile(s);
        let af = AsciiFile::new(f).unwrap();
        let mut chars_seen = 0;

        for PositionedChar(pos, c) in af.iter() {
            chars_seen += 1;
            assert_eq!(c, s.chars().nth(pos.byte_offset).unwrap());
        }

        assert_eq!(chars_seen, s.len());
    }

    #[test]
    fn test_position_get_line() {
        let s = "bkajsdlkajsdlk jalksj dlkajs lkdjd

/* reiner mag /* in kommentaren nicht */
/* some people like to see a /* */
Reiner mag Kuchen!!! \\] and green bananas.";
        let f = testfile(s);
        let af = AsciiFile::new(f).unwrap();

        let pos = Position {
            row: 4,
            col: 21,
            byte_offset: 133,
            file: &af,
        };

        let max_ctx = 80;

        assert_eq!(
            pos.get_line(max_ctx, max_ctx),
            (
                LineTruncation::NotTruncated,
                "Reiner mag Kuchen!!! \\] and green bananas.",
                LineTruncation::NotTruncated,
            )
        );
    }
}
