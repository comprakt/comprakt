///! An input file that my only contain ASCII characters.
///!
///! # Positions and Text Ranges
///!
pub mod file;
pub mod position;
pub mod span;
pub mod spanned;

// TODO: remove LinenTruncation. Will reduce performance but keep my sanity
pub use self::{
    file::{AsciiFile, AsciiFileIterator, LineTruncation},
    position::Position,
    span::Span,
    spanned::Spanned,
};
use std::marker::PhantomData;

// TODO: Position has evolved to a struct that contains identical
// information to PositionedChar. We may remove PositionedChar
#[derive(Debug, PartialEq)]
pub struct PositionedChar<'t>(pub Position<'t>, pub char);

#[derive(Copy, Clone)]
pub struct PositionedChars<'t, I: Clone> {
    current_position: Option<Position<'t>>,
    // TODO: phantom is here to not change the api until the refactor
    _phantom: PhantomData<I>,
}

impl<'t, I: Clone> Iterator for PositionedChars<'t, I> {
    type Item = PositionedChar<'t>;
    fn next(&mut self) -> Option<PositionedChar<'t>> {
        if self.current_position.is_none() {
            return None;
        }

        match self.current_position {
            Some(position) => {
                let next_position: Option<Position<'t>> = position.clone().next_mut().ok();

                self.current_position = next_position;
                let chr: char = position.chr();
                Some(PositionedChar(position, chr))
            }
            None => None,
        }
    }
}

impl<'t, I: Clone> PositionedChars<'t, I>
where
    I: Iterator<Item = char> + Clone,
{
    pub fn new(position: Position<'t>) -> Self {
        Self {
            current_position: Some(position),
            _phantom: PhantomData,
        }
    }

    pub fn try_peek_multiple(&mut self, n: usize) -> Option<String> {
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
    pub fn peek_multiple(&mut self, n: usize) -> String {
        let chars: String = self.clone().take(n).map(|pos| pos.0.chr()).collect();
        chars
    }

    /// Can't use peekable, because we don't care about position when peeking
    pub fn peek(&mut self) -> Option<char> {
        self.peek_multiple(1).chars().next()
    }

    pub fn eof_reached(&mut self) -> bool {
        self.peek().is_none()
    }

    pub fn current_position(&mut self) -> Option<Position<'t>> {
        // TODO: we need a special EOF position!
        self.current_position
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
