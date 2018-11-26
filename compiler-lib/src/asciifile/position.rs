//! abstraction over a character and its position within a file.
//!
//! # Doubly Linked List
//!
//! Positon implements a doubly linked list on the positions and therefore
//! characters of a file it was extraced from.
//!
//! # End of File (EOF) Position
//!
//! There is no end of file character. The end of file is modeled using
//! the `None` case of `Option`.
use super::{AsciiFile, PositionIterator, ReversePositionIterator, Span};
use std::cmp::Ordering;

#[derive(Copy, Clone)]
pub struct Position<'t> {
    byte_offset: usize,
    file: &'t AsciiFile<'t>,
}

impl PartialOrd for Position<'_> {
    fn partial_cmp(&self, other: &Position<'_>) -> Option<Ordering> {
        if !std::ptr::eq(self.file, other.file) {
            return None;
        }
        Some(self.byte_offset.cmp(&other.byte_offset))
    }
}

impl Ord for Position<'_> {
    fn cmp(&self, other: &Position<'_>) -> Ordering {
        if !std::ptr::eq(self.file, other.file) {
            panic!(
                "comparing positions of different files: {:?} {:?}",
                self.file as *const _, other.file as *const _
            )
        }
        self.byte_offset.cmp(&other.byte_offset)
    }
}

// TODO: given that we implement Copy, differencing between *_mut and
// non-mutable versions of functions does not really make sense.
impl<'t> Position<'t> {
    /// Create a new Position object pointing at the first character
    /// of a file. Returns `None` for empty files.
    pub fn at_file_start(file: &'t AsciiFile<'t>) -> Option<Self> {
        match file.mapping.get(0).map(|byte| *byte as char) {
            None => {
                // empty file
                None
            }
            Some(_) => Some(Self {
                byte_offset: 0,
                file,
            }),
        }
    }

    pub fn to_single_char_span(self) -> Span<'t> {
        Span::from_single_position(self)
    }

    pub fn byte_offset(&self) -> usize {
        self.byte_offset
    }

    pub fn file(&self) -> &AsciiFile<'t> {
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

    /// Return the row of the character's position within the file.
    ///
    /// The row is zero based, meaning characters on the first line
    /// of the file are in row `0`. To get the line number, add `1` to
    /// the return value.
    pub fn row(&self) -> usize {
        self.file.lookup_cache().row(self.byte_offset)
    }

    /// Return the character's line number.
    /// Identical to `row() + 1`
    pub fn line_number(&self) -> usize {
        self.file.lookup_cache().line_number(self.byte_offset)
    }

    /// Return the column of the characters position within the file.
    ///
    /// The column is zero based, meaning the first characters of
    /// a line/row is positioned at column `0`.
    pub fn column(&self) -> usize {
        self.file.lookup_cache().column(self.byte_offset)
    }

    /// Return the column of the characters position within the file.
    ///
    /// The column is zero based, meaning the first characters of
    /// a line/row is positioned at column `0`.
    pub fn row_and_column(&self) -> (usize, usize) {
        self.file.lookup_cache().row_and_column(self.byte_offset)
    }

    /// Get the position immediatly following this position or `None` if
    /// this is the last position in the file.
    pub fn next(&self) -> Option<Position<'t>> {
        self.next_mut().ok()
    }

    /// The same as `next()` but in-place.
    ///
    /// Fails if there is no next position and returns the unchanged
    /// position.
    pub fn next_mut(mut self) -> Result<Self, Self> {
        let next_byte = self.file.mapping.get(self.byte_offset + 1);
        match next_byte {
            None => Err(self),
            Some(_) => {
                self.byte_offset += 1;
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
    pub fn prev(&self) -> Option<Position<'t>> {
        self.prev_mut().ok()
    }

    /// The same as `prev()` but in-place.
    ///
    /// Fails if there is no prev position and returns the unchanged
    /// position.
    pub fn prev_mut(mut self) -> Result<Self, Self> {
        if self.byte_offset == 0 {
            return Err(self);
        }

        self.byte_offset -= 1;
        Ok(self)
    }

    /// Get source code line containing the position.
    ///
    /// The returned `Span` will include the trailing newline character
    /// of the line.
    ///
    /// ```
    /// use compiler_lib::asciifile::{AsciiFile, Position, Span};
    ///
    /// let file = AsciiFile::new(b"banana\napple\n\nkiwi").unwrap();
    ///
    /// let lines = file
    ///     .iter()
    ///     .map(|position| {
    ///         let line = position.line();
    ///         (position.chr(), line.as_str().to_string())
    ///     })
    ///     .collect::<Vec<_>>();
    ///
    /// let expected: Vec<(char, String)> = vec![
    ///     ('b', "banana\n"),
    ///     ('a', "banana\n"),
    ///     ('n', "banana\n"),
    ///     ('a', "banana\n"),
    ///     ('n', "banana\n"),
    ///     ('a', "banana\n"),
    ///     ('\n', "banana\n"),
    ///     ('a', "apple\n"),
    ///     ('p', "apple\n"),
    ///     ('p', "apple\n"),
    ///     ('l', "apple\n"),
    ///     ('e', "apple\n"),
    ///     ('\n', "apple\n"),
    ///     ('\n', "\n"),
    ///     ('k', "kiwi"),
    ///     ('i', "kiwi"),
    ///     ('w', "kiwi"),
    ///     ('i', "kiwi"),
    /// ]
    /// .into_iter()
    /// .map(|(c, s)| (c, s.to_string()))
    /// .collect();
    ///
    /// assert_eq!(expected, lines);
    /// ```
    ///
    /// Windows style line endings are not considered. This means `'\r'`
    /// will be evaluated as a normal character without any special meaning.
    pub fn line(&self) -> Span<'t> {
        // TODO: this is way more complicated than it has to be, because we are
        // converting our 'NEWLINE in front-position at column 0' convention to
        // a trailing newline convention. But having the newline at the front
        // is really weird. commit 0431f178ec33af0d2f3f14732bcdad0f3b2f56a3 contains
        // an implementation without front-to-back conversion.
        let start = self
            .reverse_iter()
            .find(|position| match position.prev() {
                Some(next) if next.chr() == '\n' => true,
                None => true,
                _ => false,
            })
            // the find(.) call above looks one character ahead
            .unwrap();

        let end = self
            .iter()
            .find(|position| position.chr() == '\n')
            .unwrap_or_else(|| self.iter().last().unwrap());

        Span::new(start, end)
    }

    pub fn iter(&self) -> PositionIterator<'t> {
        PositionIterator::new(Some(*self))
    }

    pub fn reverse_iter(&self) -> ReversePositionIterator<'t> {
        ReversePositionIterator::new(Some(*self))
    }
}

use std::fmt::{self, Debug, Display};

impl Display for Position<'_> {
    fn fmt(&self, fmt: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(fmt, "{}:{}", self.line_number(), self.column())
    }
}

impl Debug for Position<'_> {
    fn fmt(&self, fmt: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            fmt,
            "Position{{row: {:?}, col: {:?}, byte_offset: {:?}, file: {:?}}}",
            self.row(),
            self.column(),
            self.byte_offset,
            self.file as *const _
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
#[allow(clippy::print_stdout)]
mod tests {
    use super::*;

    fn positions_to_tuples<'f>(
        positions: Vec<Position<'f>>,
    ) -> Vec<((usize, char), (usize, usize))> {
        positions
            .into_iter()
            .map(|position| {
                (
                    (position.byte_offset(), position.chr()),
                    position.row_and_column(),
                )
            })
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
        let actual = positions_to_tuples(file.iter().collect());

        #[rustfmt::skip]
        let expected = vec![
            // (byte_offset, file) (row, col) char
            (( 0, 'o'), (0, 0), ),
            (( 1, 'n'), (0, 1), ),
            (( 2, 'e'), (0, 2), ),
            (( 3, '\n'), (0, 3),),
            (( 4, 't'), (1, 0), ),
            (( 5, 'w'), (1, 1), ),
            (( 6, 'o'), (1, 2), ),
            (( 7, ' '), (1, 3), ),
            (( 8, 't'), (1, 4), ),
            (( 9, 'h'), (1, 5), ),
            ((10, 'r'), (1, 6), ),
            ((11, 'e'), (1, 7), ),
            ((12, 'e'), (1, 8), ),
            ((13, '\n'), (1, 9),),
            ((14, 'f'), (2, 0), ),
            ((15, 'o'), (2, 1), ),
            ((16, 'u'), (2, 2), ),
            ((17, 'r'), (2, 3), ),
            ((18, '\n'), (2, 4),),
            ((19, '\n'), (3, 0),),
        ];

        assert_eq!(expected, actual);
    }

    #[test]
    fn file_starting_with_newline() {
        let input = b"\none\ntwo three\nfour\n\n";
        let file = AsciiFile::new(input).unwrap();
        let start = Position::at_file_start(&file).unwrap();
        let end = file.iter().last().unwrap();
        let span = Span::new(start, end);

        let actual = span
            .lines()
            .enumerate()
            .map(|(real_row, line)| {
                (
                    real_row + 1,
                    line.start_position().line_number(),
                    line.as_str().to_string(),
                )
            })
            .collect::<Vec<_>>();

        let expected: Vec<(usize, usize, String)> = vec![
            (1, 1, "\n".to_string()),
            (2, 2, "one\n".to_string()),
            (3, 3, "two three\n".to_string()),
            (4, 4, "four\n".to_string()),
            (5, 5, "\n".to_string()),
        ];

        assert_eq!(expected, actual);
    }

    #[test]
    fn file_starting_with_two_newlines() {
        let input = b"\n\none\ntwo three\nfour\n\n";
        let file = AsciiFile::new(input).unwrap();
        let start = Position::at_file_start(&file).unwrap();
        let end = file.iter().last().unwrap();
        let span = Span::new(start, end);

        let actual = span
            .lines()
            .enumerate()
            .map(|(real_row, line)| {
                (
                    real_row + 1,
                    line.start_position().line_number(),
                    line.as_str().to_string(),
                )
            })
            .collect::<Vec<_>>();

        let expected: Vec<(usize, usize, String)> = vec![
            (1, 1, "\n".to_string()),
            (2, 2, "\n".to_string()),
            (3, 3, "one\n".to_string()),
            (4, 4, "two three\n".to_string()),
            (5, 5, "four\n".to_string()),
            (6, 6, "\n".to_string()),
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
                pos = next.next();
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
                pos = prev.prev();
            }

            vec.reverse();
            vec
        };

        assert_eq!(back_to_front.len(), input.len());
        assert_eq!(
            positions_to_tuples(back_to_front),
            positions_to_tuples(front_to_back)
        );
    }

    macro_rules! same_position_different_files {
        ($input:expr, $f1pos:ident, $f2pos:ident) => {
            let input1 = $input.as_ref().clone();
            let file1 = AsciiFile::new(input1).unwrap();
            let input2 = $input.as_ref().clone();
            let file2 = AsciiFile::new(input2).unwrap();
            let $f1pos: Position<'_> = file1.iter().next().unwrap();
            let $f2pos: Position<'_> = file2.iter().next().unwrap();
        };
    }
    #[test]
    fn position_partial_cmp_from_different_files_no_ordering() {
        same_position_different_files!(b"samestring", p1, p2);
        let res = p1.partial_cmp(&p2);
        assert_eq!(res, None);
    }
    #[test]
    #[should_panic]
    fn position_cmp_of_different_files_panics() {
        same_position_different_files!(b"samestring", p1, p2);
        p1.cmp(&p2);
    }

}
