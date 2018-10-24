use failure::Fail;
use memmap::Mmap;

pub struct AsciiFile {
    mapping: Mmap,
}

#[derive(Debug, Fail)]
pub enum EncodingError {
    #[fail(
        display = "input contained non-ascii character '{}' at byte offset {}",
        character,
        position
    )]
    NotAscii { position: usize, character: char },
}

impl<'a> AsciiFile {
    // cost: O(fileLen) since we need to check if all chars are ASCII
    pub fn new(mapping: Mmap) -> Result<AsciiFile, EncodingError> {
        if let Some(position) = mapping.iter().position(|c| !c.is_ascii()) {
            return Err(EncodingError::NotAscii {
                position,
                character: mapping[position].into(),
            });
        }

        Ok(AsciiFile { mapping })
    }

    pub fn iter(&self) -> PositionedChars<std::str::Chars<'_>> {
        let s: &str = self;
        PositionedChars {
            curpos: Position { row: 0, col: 0 },
            af: s.chars(),
            peeked: String::new(),
        }
    }
}

use std::ops::Deref;

#[derive(Debug, Copy, Clone, PartialEq)]
pub struct Position {
    row: usize,
    col: usize,
}

use std::fmt::{self, Display};

impl Display for Position {
    fn fmt(&self, fmt: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(fmt, "{}:{}", self.row, self.col)
    }
}

#[derive(Debug, PartialEq)]
pub struct PositionedChar(pub Position, pub char);

pub struct PositionedChars<I>
where
    I: Iterator<Item = char>,
{
    curpos: Position,
    af: I, // form ascii file
    peeked: String,
}

impl<I> Iterator for PositionedChars<I>
where
    I: Iterator<Item = char>,
{
    type Item = PositionedChar;
    fn next(&mut self) -> Option<PositionedChar> {
        let c = self.peek()?;
        self.peeked.remove(0);

        let retpos = self.curpos;
        self.curpos.col += 1;
        if c == '\n' {
            self.curpos.row += 1;
            self.curpos.col = 0;
        }

        Some(PositionedChar(retpos, c))
    }
}

impl<I> PositionedChars<I>
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
            match self.af.next() {
                Some(next) => self.peeked.push(next),
                None => break,
            }
        }

        &self.peeked[0..self.peeked.len().min(n)]
    }

    /// Can't use peekable, pecause we don't care about position when peeking
    pub fn peek(&mut self) -> Option<char> {
        self.peek_multiple(1).chars().next()
    }

    pub fn eof_reached(&mut self) -> bool {
        self.peek().is_some()
    }

    pub fn eof_position(&mut self) -> Position {
        if self.next().is_some() {
            panic!("Must only be called at EOF")
        }

        let mut pos = self.curpos;
        pos.col += 1;

        pos
    }
}

impl Deref for AsciiFile {
    type Target = str;
    fn deref(&self) -> &Self::Target {
        unsafe { std::str::from_utf8_unchecked(&self.mapping) }
    }
}

impl<'a> Into<&'a str> for &'a AsciiFile {
    fn into(self) -> &'a str {
        self
    }
}

#[cfg(test)]
mod tests {

    use super::*;

    fn testfile(s: &str) -> Mmap {
        use std::io::{Seek, SeekFrom, Write};
        use tempfile::tempfile;
        let mut f = tempfile().unwrap();
        f.write_all(s.as_bytes()).unwrap();
        f.seek(SeekFrom::Start(0)).unwrap();
        unsafe { Mmap::map(&f).unwrap() }
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
        match e {
            EncodingError::NotAscii { position: 3, .. } => (),
            EncodingError::NotAscii { position: x, .. } => {
                panic!("detected not ascii, but wrong line {}", x)
            }
        }
    }

    #[test]
    fn iterator_works() {
        let f = testfile("one\ntwo three\nfour\n\n");
        let af = AsciiFile::new(f).unwrap();
        let res: Vec<PositionedChar> = af.iter().collect();

        let exp = vec![
            PositionedChar(Position { row: 0, col: 0 }, 'o'),
            PositionedChar(Position { row: 0, col: 1 }, 'n'),
            PositionedChar(Position { row: 0, col: 2 }, 'e'),
            PositionedChar(Position { row: 0, col: 3 }, '\n'),
            PositionedChar(Position { row: 1, col: 0 }, 't'),
            PositionedChar(Position { row: 1, col: 1 }, 'w'),
            PositionedChar(Position { row: 1, col: 2 }, 'o'),
            PositionedChar(Position { row: 1, col: 3 }, ' '),
            PositionedChar(Position { row: 1, col: 4 }, 't'),
            PositionedChar(Position { row: 1, col: 5 }, 'h'),
            PositionedChar(Position { row: 1, col: 6 }, 'r'),
            PositionedChar(Position { row: 1, col: 7 }, 'e'),
            PositionedChar(Position { row: 1, col: 8 }, 'e'),
            PositionedChar(Position { row: 1, col: 9 }, '\n'),
            PositionedChar(Position { row: 2, col: 0 }, 'f'),
            PositionedChar(Position { row: 2, col: 1 }, 'o'),
            PositionedChar(Position { row: 2, col: 2 }, 'u'),
            PositionedChar(Position { row: 2, col: 3 }, 'r'),
            PositionedChar(Position { row: 2, col: 4 }, '\n'),
            PositionedChar(Position { row: 3, col: 0 }, '\n'),
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

        let exp = vec![
            PositionedChar(Position { row: 0, col: 0 }, 'o'),
            PositionedChar(Position { row: 0, col: 1 }, 'n'),
            PositionedChar(Position { row: 0, col: 2 }, 'e'),
            PositionedChar(Position { row: 0, col: 3 }, '\n'),
            PositionedChar(Position { row: 1, col: 0 }, 't'),
            PositionedChar(Position { row: 1, col: 1 }, 'w'),
            PositionedChar(Position { row: 1, col: 2 }, 'o'),
            PositionedChar(Position { row: 1, col: 3 }, ' '),
            PositionedChar(Position { row: 1, col: 4 }, 't'),
            PositionedChar(Position { row: 1, col: 5 }, 'h'),
            PositionedChar(Position { row: 1, col: 6 }, 'r'),
            PositionedChar(Position { row: 1, col: 7 }, 'e'),
            PositionedChar(Position { row: 1, col: 8 }, 'e'),
            PositionedChar(Position { row: 1, col: 9 }, '\n'),
            PositionedChar(Position { row: 2, col: 0 }, 'f'),
            PositionedChar(Position { row: 2, col: 1 }, 'o'),
            PositionedChar(Position { row: 2, col: 2 }, 'u'),
            PositionedChar(Position { row: 2, col: 3 }, 'r'),
            PositionedChar(Position { row: 2, col: 4 }, '\n'),
            PositionedChar(Position { row: 3, col: 0 }, '\n'),
        ];

        let res: Vec<PositionedChar> = i.collect();

        assert_eq!(exp, res);
    }
}
