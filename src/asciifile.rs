use failure::Fail;
use memmap::Mmap;

use std::ops::Deref;

use std::fmt::{self, Display};

pub struct AsciiFile {
    mapping: Mmap,
}

#[derive(Debug, Fail)]
pub enum EncodingError {
    #[fail(
        display = "input contains non-ascii character at byte offset {}: ...{}<?>",
        position,
        prev
    )]
    NotAscii { position: usize, prev: String },
}

const ENCODING_ERROR_MAX_CONTEXT_LEN: usize = 180;

impl AsciiFile {
    // cost: O(fileLen) since we need to check if all chars are ASCII
    pub fn new(mapping: Mmap) -> Result<AsciiFile, EncodingError> {
        if let Some(position) = mapping.iter().position(|c| !c.is_ascii()) {
            let end_idx = position;
            let min_start_idx = position - ENCODING_ERROR_MAX_CONTEXT_LEN.min(position);
            let mut start_idx = min_start_idx;
            for x in (min_start_idx..end_idx).rev() {
                const NEWLINE: u8 = b'\n';
                if mapping[x] == NEWLINE {
                    start_idx = x + 1;
                    break;
                }
            }
            assert!(position >= start_idx);

            // We know everything until now has been ASCII
            let prev: &str =
                unsafe { std::str::from_utf8_unchecked(&mapping[start_idx..position]) };
            let prev = prev.to_owned();
            return Err(EncodingError::NotAscii { position, prev });
        }

        Ok(AsciiFile { mapping })
    }

    pub fn scanner(&self) -> StrFrame<'_> {
        let s: &str = self;
        StrFrame::new(s)
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

#[derive(Debug, Copy, Clone, PartialEq)]
pub struct Position {
    row: usize,
    col: usize,
}

impl Display for Position {
    fn fmt(&self, fmt: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(fmt, "{}:{}", self.row, self.col)
    }
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct Span {
    pub start: Position,
    pub end: Position,
}

impl fmt::Display for Span {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}-{}", self.start, self.end)
    }
}

#[derive(Debug)]
pub struct StrFrame<'s> {
    span: Span,
    len: usize,
    remainder: &'s str,
}

#[must_use]
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum ExtensionError {
    NoMatch,
    EOF,
}

type ExtendResult<'s> = Result<&'s str, ExtensionError>;

impl ExtensionError {
    pub fn eof_reached(self) -> bool {
        self == ExtensionError::EOF
    }
}

impl<'s> StrFrame<'s> {
    fn new(slice: &'s str) -> Self {
        let pos = Position { row: 0, col: 0 };
        StrFrame {
            span: Span {
                start: pos,
                end: pos,
            },
            len: 0,
            remainder: slice,
        }
    }

    /// If EOF within next `n` chars, return `false` and leave frame unchanged
    pub fn extend_by(&mut self, n: usize) -> ExtendResult<'s> {
        if self.remainder.len() >= self.len + n {
            // Assumes ASCII
            for c in self.remainder[self.len..(self.len + n)].chars() {
                self.span.end.col += 1;
                if c == '\n' {
                    self.span.end.row += 1;
                    self.span.end.col = 0;
                }
            }

            self.len += n;

            Ok(&self.remainder[..self.len])
        } else {
            Err(ExtensionError::EOF)
        }
    }

    pub fn extend_to(&mut self, n: usize) -> ExtendResult<'s> {
        n.checked_sub(self.len)
            .ok_or(ExtensionError::NoMatch)
            .and_then(|diff| self.extend_by(diff))
    }

    pub fn extend_to_eof(&mut self) {
        let res = self.extend_by(self.remainder.len() - self.len);
        assert!(res.is_ok());
        assert!(self.remainder.len() == self.len);
    }

    /// If char stream doesn't contain `want` at begining of this frame, return
    /// `NoMatch` and leave frame unchanged
    pub fn extend_so_that_eq(&mut self, want: &str) -> ExtendResult<'s> {
        // FIXME This never returns EOF?
        let n = want.len();
        self.remainder
            .get(..n)
            .ok_or(ExtensionError::EOF)
            .and_then(|have| {
                if have == want {
                    self.extend_to(n)
                } else {
                    Err(ExtensionError::NoMatch)
                }
            })
    }

    pub fn extend_while<P>(&mut self, lookahead: usize, predicate: P) -> ExtendResult<'s>
    where
        P: Fn(&str) -> bool,
    {
        assert!(self.len <= self.remainder.len());

        for i in self.len..self.remainder.len() {
            if !self
                .remainder
                .get(..(i + lookahead))
                .map_or(false, &predicate)
            {
                return Ok(&self.remainder[..i]);
            } else {
                let res = self.extend_by(1);
                assert!(res.is_ok());
            }
        }

        Err(ExtensionError::EOF)
    }

    /// Remove leading `n` chars from frame. If frame is shorter than `n`,
    /// return `false` and leave it unchanged
    pub fn trim_head(&mut self, n: usize) -> Result<&str, ()> {
        assert!(self.len <= self.remainder.len());

        if self.len >= n {
            let discarded = &self.remainder[..n];
            self.remainder = &self.remainder[n..];
            Ok(discarded)
        } else {
            Err(())
        }
    }

    /*
    /// Remove trailing `n` chars from frame. If frame is shorter than `n`,
    /// return `false` and leave it unchanged. The trailing characters are not
    /// re-added to the unprocessed chars, and thus will be lost to safe code
    pub fn trim_tail(&mut self, n: usize) -> bool {
        unimplemented!()
    }
     */

    /// Finalize this frame by return the span and contents, and reinitializing
    /// it as an empty frame looking at the beginning of the remaining
    /// unprocessed chars
    #[allow(clippy::almost_swapped)]
    pub fn finish(&mut self) -> (Span, &'s str) {
        assert!(self.len <= self.remainder.len());
        let result = (self.span, &self.remainder[..self.len]);

        self.remainder = &self.remainder[self.len..];

        self.span.start = self.span.end;
        self.span.end = self.span.start;
        self.len = 0;

        result
    }

    /// Same as `finish`, but exclude the last `n` chars, from the result,
    /// without re-addeing them to the unprocessed chars. Returns `None` if
    /// current frame is shorter than `n`.
    pub fn finish_with_trimmed_tail(&mut self, n: usize) -> Option<(Span, &'s str)> {
        if self.len >= n {
            let (c, s) = self.finish();
            // Assumes ASCII
            Some((c, &s[..s.len() - n]))
        } else {
            None
        }
    }

    pub fn first(&self) -> Option<char> {
        self.head(1).and_then(|s| s.chars().next())
    }

    pub fn head(&self, n: usize) -> Option<&'s str> {
        if self.len >= n {
            // Assumes ASCII
            Some(&self.remainder[..n])
        } else {
            None
        }
    }

    pub fn is_empty(&self) -> bool {
        self.len == 0
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
        use std::io::{Seek, SeekFrom, Write};
        use tempfile::tempfile;
        let mut f = tempfile().unwrap();
        f.write_all(input).unwrap();
        f.seek(SeekFrom::Start(0)).unwrap();
        let mm = unsafe { Mmap::map(&f).unwrap() };
        let r = AsciiFile::new(mm);
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
        use std::fmt::Write;
        let mut s = String::new();
        for i in (0..ENCODING_ERROR_MAX_CONTEXT_LEN).rev() {
            write!(s, "{}", i % 10);
        }
        let instr = format!("{}💩", s);
        let f = testfile(&instr);
        let mm = AsciiFile::new(f);
        assert!(mm.is_err());
        let e = mm.err().unwrap();
        println!("{:?}", e);
        let EncodingError::NotAscii { prev, .. } = e;
        println!("{:?}", prev);
        let l = s.len();
        assert_eq!(l, ENCODING_ERROR_MAX_CONTEXT_LEN);
        let exp = &s[(l - ENCODING_ERROR_MAX_CONTEXT_LEN)..l];
        assert_eq!(prev, exp);
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
