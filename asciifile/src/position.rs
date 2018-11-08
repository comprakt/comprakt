//! abstraction over a character and its position within a file.
//!
//! # Doubly Linked List
//!
//! Positon implements a doubly linked list on the positions and therefore
//! characters of a file it was extraced from.
//!
//! # End of File (EOF) Position
//!
//! A EOF character is automatically inserted at the end of the file (which
//! is not emitted by the rust APIs by default.) The EOF character is positioned
//! on the same line as the last real character advanced by a single byte.
//! This token exists because rust
use crate::{file::LineTruncation, AsciiFile, Span};
use std::cmp::Ordering;

#[derive(Copy, Clone)]
pub struct Position<'t> {
    // TODO: should all be private
    row: usize,
    col: usize,
    byte_offset: usize,
    file: &'t AsciiFile<'t>,
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
    #[cfg(test)]
    pub fn dummy() -> Self {
        Position {
            row: 0,
            col: 0,
            byte_offset: 0,
            file: Box::leak(Box::new(AsciiFile::new(&[]).unwrap())),
        }
    }

    /// Create a new Position object pointing at the first character
    /// of a file. Returns `None` for empty files.
    pub fn at_file_start(file: &'t AsciiFile<'t>) -> Option<Self> {
        if file.mapping.len() == 0 {
            return None;
        }

        Some(Self {
            row: 0,
            col: 0,
            byte_offset: 0,
            file,
        })
    }

    pub fn to_single_char_span(self) -> Span<'t> {
        Span::from_single_position(self)
    }

    pub fn byte_offset(&self) -> usize {
        self.byte_offset
    }

    pub fn file(&self) -> &AsciiFile {
        self.file
    }

    /// Get the character at this position
    ///
    /// Guranteed to be within the ASCII range of UTF-8 that does not use the
    /// upper half of the bytes. Use `byte()` if you need a single
    /// byte representation instead.
    pub fn chr(&self) -> char {
        self.file.mapping[self.byte_offset] as char
    }

    /// Get the byte at this position
    ///
    /// For matching on the character or comparisons, you probably want `chr()`
    /// instead.
    pub fn byte(&self) -> u8 {
        self.file.mapping[self.byte_offset]
    }

    /// Return the row of the characters position within the file.
    ///
    /// The row is zero based, meaning characters on the first line
    /// of the file are in row `0`. To get the line number, add `1` to
    /// the return value.
    // TODO: why not use the natural representation of 1-based by default.
    pub fn row(&self) -> usize {
        self.row
    }

    /// Return the column of the characters position within the file.
    ///
    /// The column is zero based, meaning the first characters of
    /// a line/row is positioned at column `0`.
    pub fn column(&self) -> usize {
        self.col
    }

    /// Get the position immediatly following this position or `None` if
    /// this is the last position in the file.
    pub fn next(&'t self) -> Option<Position<'t>> {
        let pos = self.clone();
        pos.next_mut().ok()
    }

    /// The same as `next()` but in-place.
    ///
    /// Fails if there is no next position and returns the unchanged
    /// position.
    pub fn next_mut(mut self) -> Result<Self, Self> {
        let next_byte = self.file.mapping.get(self.byte_offset + 1);
        match next_byte {
            None => Err(self),
            Some(&byte) => {
                self.consume_byte_mut(byte);
                Ok(self)
            }
        }
    }

    /// Check if we are at the end of the file (the last valid position/the
    /// last character of the file).
    pub fn is_last(&self) -> bool {
        self.next().is_none()
    }

    /// Get the position immediatly following this position or `None` if
    /// this is the last position in the file.
    pub fn prev(&'t self) -> Option<Position<'t>> {
        let pos = self.clone();
        pos.prev_mut().ok()
    }

    /// The same as `prev()` but in-place.
    ///
    /// Fails if there is no prev position and returns the unchanged
    /// position.
    fn prev_mut(mut self) -> Result<Self, Self> {
        if self.byte_offset == 0 {
            return Err(self);
        }

        match self.chr() {
            '\n' => {
                // we have to reconstruct the column of the
                // last character by finding its distance
                // to the previous newline character
                let consumed = &self.file[..self.byte_offset];
                let previous_newline_index: usize = consumed.rfind('\n').unwrap_or(0);
                println!("newline_idx: {}", previous_newline_index);
                // minus one transposes 1-indexed columns to 0-indexed columns
                self.col = self.byte_offset - previous_newline_index - 1;

                self.byte_offset -= 1;
                self.row -= 1;
                Ok(self)
            }
            _ => {
                self.col -= 1;
                self.byte_offset -= 1;
                Ok(self)
            }
        }
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

    fn consume_byte(&self, byte: u8) -> Self {
        let mut pos = self.clone();
        pos.consume_byte_mut(byte);
        pos
    }

    fn consume_byte_mut(&mut self, byte: u8) {
        self.consume_char_mut(byte as char);
    }

    fn consume_char(&self, byte: char) -> Self {
        let mut pos = self.clone();
        pos.consume_char_mut(byte);
        pos
    }

    fn consume_char_mut(&mut self, chr: char) {
        match chr {
            '\n' => {
                self.col = 0;
                self.row += 1;
                self.byte_offset += 1;
            }
            _ => {
                self.col += 1;
                self.byte_offset += 1;
            }
        };
    }

    fn consume_mut(&mut self, upcoming: &str) {
        for chr in upcoming.chars() {
            self.consume_char_mut(chr);
        }
    }

    /// Advance the position by examining a row of input characters. Will
    /// update the row count for each new line encountered, will update the
    /// column count for all other characters.
    // TODO: should be private! allows to build invalid chars
    pub fn consume(&mut self, upcoming: &str) -> Self {
        let mut pos = self.clone();
        pos.consume_mut(upcoming);
        pos
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

#[cfg(test)]
mod tests {
    use super::*;

    fn pair_with_char<'f>(positions: Vec<Position<'f>>) -> Vec<(Position<'f>, char)> {
        positions
            .into_iter()
            .map(|position| (position, position.chr()))
            .collect()
    }

    #[test]
    fn peeking_works() {
        let input = b"one\ntwo three\nfour\n\n";
        let file = AsciiFile::new(input).unwrap();

        let mut i = file.iter();

        while i.next().is_some() {
            let peeked = i.peek();
            let consumed = i.next();
            assert_eq!(peeked, consumed);
        }

        assert_eq!(i.peek(), None);
    }

    #[test]
    fn peeking_multiple_times_works() {
        let input = b"abc";
        let file = AsciiFile::new(input).unwrap();
        let mut iter = file.iter();
        let peeked_once = iter.peek();
        let peeked_twice = iter.peek();
        assert_eq!(peeked_once, peeked_twice);
        let consumed = iter.next();
        assert_eq!(peeked_once, consumed);
    }

    #[test]
    fn peeking_multiple_chars_at_once_works() {
        let input = "one\ntwo three\nfour\n\n";
        let file = AsciiFile::new(input.as_bytes()).unwrap();
        let iter = file.iter();

        let peeked_once = iter.peek_exactly(input.len()).unwrap();

        assert_eq!(input, peeked_once.as_str());

        // peek a second time, state should not be forwarded
        let peeked_twice = iter.peek_exactly(2).unwrap();

        assert_eq!(&input[..2], peeked_twice.as_str());
    }

    #[test]
    fn peeking_multiple_chars_with_at_most_works() {
        let input = "one\ntwo three\nfour\n\n";
        let file = AsciiFile::new(input.as_bytes()).unwrap();
        let iter = file.iter();

        let peeked_once = iter.peek_at_most(input.len() + 1).unwrap();

        assert_eq!(input, peeked_once.as_str());

        let peeked_twice = iter.peek_at_most(2).unwrap();

        assert_eq!(&input[..2], peeked_twice.as_str());
    }

    #[test]
    fn iterator_works() {
        let input = b"one\ntwo three\nfour\n\n";
        let file = AsciiFile::new(input).unwrap();
        let actual = pair_with_char(file.iter().collect());

        #[rustfmt::skip]
        let expected = vec![
            (Position { byte_offset:  0, row: 0, col: 0, file:  &file }, 'o'),
            (Position { byte_offset:  1, row: 0, col: 1 , file: &file }, 'n'),
            (Position { byte_offset:  2, row: 0, col: 2 , file: &file }, 'e'),
            (Position { byte_offset:  3, row: 0, col: 3 , file: &file }, '\n'),
            (Position { byte_offset:  4, row: 1, col: 0 , file: &file }, 't'),
            (Position { byte_offset:  5, row: 1, col: 1 , file: &file }, 'w'),
            (Position { byte_offset:  6, row: 1, col: 2 , file: &file }, 'o'),
            (Position { byte_offset:  7, row: 1, col: 3 , file: &file }, ' '),
            (Position { byte_offset:  8, row: 1, col: 4 , file: &file }, 't'),
            (Position { byte_offset:  9, row: 1, col: 5 , file: &file }, 'h'),
            (Position { byte_offset: 10, row: 1, col: 6 , file: &file }, 'r'),
            (Position { byte_offset: 11, row: 1, col: 7 , file: &file }, 'e'),
            (Position { byte_offset: 12, row: 1, col: 8 , file: &file }, 'e'),
            (Position { byte_offset: 13, row: 1, col: 9 , file: &file }, '\n'),
            (Position { byte_offset: 14, row: 2, col: 0 , file: &file }, 'f'),
            (Position { byte_offset: 15, row: 2, col: 1 , file: &file }, 'o'),
            (Position { byte_offset: 16, row: 2, col: 2 , file: &file }, 'u'),
            (Position { byte_offset: 17, row: 2, col: 3 , file: &file }, 'r'),
            (Position { byte_offset: 18, row: 2, col: 4 , file: &file }, '\n'),
            (Position { byte_offset: 19, row: 3, col: 0 , file: &file }, '\n'),
        ];
        assert_eq!(expected, actual);
    }

    #[test]
    fn all_indexing_works() {
        file_index_roundtrip(b"one\ntwo three\nfour\n\n");
        file_index_roundtrip(b"a");
        file_index_roundtrip(b"\n");
        file_index_roundtrip(b"\n\n\n\n");
        file_index_roundtrip(b"aksjdaklsd");
        file_index_roundtrip(b"aksjd\naklsd");
        file_index_roundtrip(b"aksjdaklsd\n");
        file_index_roundtrip(b"");
    }

    fn file_index_roundtrip(input: &[u8]) {
        // the only functions playing with indices are `consume_char_mut()` and
        // `prev_mut()`. We test these here by invoking them via the doubly
        // linked list `prev()`/`next()`. The correctness of all other
        // functions mutating Positions and
        // Spans should follow from testing these.
        println!("INPUT=```{}```", String::from_utf8_lossy(input));
        let file = AsciiFile::new(input).unwrap();

        let front_to_back = {
            println!("FRONT TO BACK");
            // we iterate and collect by hand, the iterator might hide bugs
            // through custom EOF logic.
            let mut vec = Vec::new();
            let mut pos = Position::at_file_start(&file);

            while let Some(next) = pos {
                println!("{:?}: '{}'", next, next.chr());
                vec.push(next);
                pos = next.clone().next_mut().ok();
            }

            vec
        };

        let back_to_front = {
            println!("BACK TO FRONT");
            let mut vec = Vec::new();
            let mut pos = file.iter().last();

            while let Some(prev) = pos {
                println!("{:?}: '{}'", prev, prev.chr());
                vec.push(prev);
                pos = prev.clone().prev_mut().ok();
            }

            vec.reverse();
            vec
        };

        assert_eq!(back_to_front.len(), input.len());
        assert_eq!(pair_with_char(back_to_front), pair_with_char(front_to_back));
    }
}
