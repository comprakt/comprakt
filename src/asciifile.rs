use memmap::{Mmap, MmapOptions};
use std::fs::File;

pub struct AsciiFile {
    #[allow(unused)] // mapping must not outlive File
    file: File,
    mapping: Mmap,
}

use std::io;

quick_error!{
    #[derive(Debug)]
    pub enum Error {
        IO(err: io::Error) {
            cause(err)
            from()
        }
        NotAscii(byte_pos: usize) {}
    }
}

impl<'a> AsciiFile {
    // cost: O(fileLen) since we need to check if all chars are ASCII
    pub fn new(file: File) -> Result<AsciiFile, Error> {
        let mapping = unsafe { MmapOptions::new().map(&file)? };

        let (err_idx, is_ascii) = mapping
            .iter()
            .enumerate()
            .fold((0, true), |(i, a), (j, b)| {
                if !a {
                    return (i, a);
                }
                if !b.is_ascii() {
                    return (j, false);
                }
                (j, true)
            });

        if !is_ascii {
            return Err(Error::NotAscii(err_idx));
        }

        Ok(AsciiFile { file, mapping })
    }

    fn iter(&'a self) -> impl Iterator<Item = Char> + 'a {
        let s: &str = self;
        Chars {
            curpos: Position { row: 0, col: 0 },
            af: s.chars(),
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
pub struct Char(Position, char);

pub struct Chars<I>
where
    I: Iterator<Item = char>,
{
    curpos: Position,
    af: I, // form ascii file
}

impl<I> Iterator for Chars<I>
where
    I: Iterator<Item = char>,
{
    type Item = Char;
    fn next(&mut self) -> Option<Char> {
        let c = self.af.next()?;
        let retpos = self.curpos;
        self.curpos.col += 1;
        if c == '\n' {
            self.curpos.row += 1;
            self.curpos.col = 0;
        }
        Some(Char(retpos, c))
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

    fn testfile(s: &str) -> File {
        use std::io::{Seek, SeekFrom, Write};
        use tempfile::tempfile;
        let mut f = tempfile().unwrap();
        f.write_all(s.as_bytes()).unwrap();
        f.seek(SeekFrom::Start(0)).unwrap();
        f
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
            Error::NotAscii(3) => (),
            Error::NotAscii(x) => panic!("detected not ascii, but wrong line {}", x),
            x => {
                panic!("wrong error type: {:?}", x);
            }
        }
    }

    #[test]
    fn iterator_works() {
        let f = testfile("one\ntwo three\nfour\n\n");
        let af = AsciiFile::new(f).unwrap();
        let res: Vec<Char> = af.iter().collect();

        let exp = vec![
            Char(Position { row: 0, col: 0 }, 'o'),
            Char(Position { row: 0, col: 1 }, 'n'),
            Char(Position { row: 0, col: 2 }, 'e'),
            Char(Position { row: 0, col: 3 }, '\n'),
            Char(Position { row: 1, col: 0 }, 't'),
            Char(Position { row: 1, col: 1 }, 'w'),
            Char(Position { row: 1, col: 2 }, 'o'),
            Char(Position { row: 1, col: 3 }, ' '),
            Char(Position { row: 1, col: 4 }, 't'),
            Char(Position { row: 1, col: 5 }, 'h'),
            Char(Position { row: 1, col: 6 }, 'r'),
            Char(Position { row: 1, col: 7 }, 'e'),
            Char(Position { row: 1, col: 8 }, 'e'),
            Char(Position { row: 1, col: 9 }, '\n'),
            Char(Position { row: 2, col: 0 }, 'f'),
            Char(Position { row: 2, col: 1 }, 'o'),
            Char(Position { row: 2, col: 2 }, 'u'),
            Char(Position { row: 2, col: 3 }, 'r'),
            Char(Position { row: 2, col: 4 }, '\n'),
            Char(Position { row: 3, col: 0 }, '\n'),
        ];

        assert_eq!(exp, res);
    }

}
