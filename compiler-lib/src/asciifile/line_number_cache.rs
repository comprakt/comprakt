//! Caches information about linebreaks that allows us to reconstruct the line
//! number and character column in O(log N). This makes sense as we only
//! request line numbers in a few cases (e.g. on error).
//!
//! ```
//! use compiler_lib::asciifile::LineNumberCache;
//!
//! let input = "asd\nqwer\nZU\nfgfh";
//! let positions = input
//!     .chars()
//!     .enumerate()
//!     .filter_map(
//!         |(offset, chr)| {
//!             if chr == '\n' {
//!                 Some(offset)
//!             } else {
//!                 None
//!             }
//!         },
//!     )
//!     .collect();
//! let cache = LineNumberCache::new(positions);
//!
//! let actual: Vec<_> = input
//!     .chars()
//!     .enumerate()
//!     .map(|(offset, chr)| (chr, cache.row_and_column(offset)))
//!     .collect();
//!
//! let expected = vec![
//!     ('a', (0, 0)),
//!     ('s', (0, 1)),
//!     ('d', (0, 2)),
//!     ('\n', (0, 3)),
//!     ('q', (1, 0)),
//!     ('w', (1, 1)),
//!     ('e', (1, 2)),
//!     ('r', (1, 3)),
//!     ('\n', (1, 4)),
//!     ('Z', (2, 0)),
//!     ('U', (2, 1)),
//!     ('\n', (2, 2)),
//!     ('f', (3, 0)),
//!     ('g', (3, 1)),
//!     ('f', (3, 2)),
//!     ('h', (3, 3)),
//! ];
//!
//! assert_eq!(actual, expected);
//! ```
#[derive(Debug)]
pub struct LineNumberCache {
    /// byte index of linebreaks (\n) in the file. The
    /// index of the newline within the sorted list is
    /// the row, the line number is the row plus 1.
    linebreaks: Vec<usize>,
}

impl LineNumberCache {
    pub fn new(linebreaks: Vec<usize>) -> Self {
        Self { linebreaks }
    }

    /// Return the line number at `byte_offset`.
    /// Identical to `row() + 1`.
    ///
    /// This does not detect errors caused by `byte_offset`s that
    /// are out of range. Prefer the safe API of the `Position` struct
    /// instead.
    pub fn line_number(&self, byte_offset: usize) -> usize {
        self.row(byte_offset) + 1
    }

    /// Return the column of the characters position within the file.
    ///
    /// The column is zero based, meaning the first characters of
    /// a line/row is positioned at column `0`.
    ///
    /// This does not detect errors caused by `byte_offset`s that
    /// are out of range. Prefer the safe API of the `Position` struct
    /// instead.
    pub fn column(&self, byte_offset: usize) -> usize {
        self.row_and_column(byte_offset).1
    }

    /// Return the row of the character's position within the file.
    ///
    /// This does not detect errors caused by `byte_offset`s that
    /// are out of range. Prefer the safe API of the `Position` struct
    /// instead.
    pub fn row(&self, byte_offset: usize) -> usize {
        // - binary_search always gives the index of the smaller number
        //   if the fiven offset lies between two newlines.
        // - binary_search returns len() if the number is bigger than
        //   everything seen
        // => the returned index is always the row-number.
        match self.linebreaks.binary_search(&byte_offset) {
            Err(row) => row,
            Ok(row) => {
                // we position newlines at the end of the line. Meaning
                // the `line break` is performed after the `newline`
                // character
                row
            }
        }
    }

    /// Get row and column at once. Equivalent to calling
    /// `row(&self, byte_offset: usize)` and `column(&self, byte_offset:
    /// usize)`.
    ///
    /// This method exists because most of the time you want both.
    /// And it's more efficient to look up both at once.
    ///
    /// This does not detect errors caused by `byte_offset`s that
    /// are out of range. Prefer the safe API of the `Position` struct
    /// instead.
    pub fn row_and_column(&self, byte_offset: usize) -> (usize, usize) {
        let row = self.row(byte_offset);
        let col = if row == 0 {
            // no linebreak yet => first line
            byte_offset
        } else {
            // line that is last line or line in-between

            // we have to `-1` since we want to look up the
            // position of the line break before our current line.
            //
            // we have to `+1` since we want to look up
            // the position of the first character of the
            // line, which is one character after the newline.
            let line_start_offset = self.linebreaks[row - 1] + 1;
            byte_offset - line_start_offset
        };

        (row, col)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn empty_linebreaks() {
        let linebreaks = vec![];
        let c = LineNumberCache::new(linebreaks);
        assert_eq!((0, 23), c.row_and_column(23));
        assert_eq!(0, c.row(23));
        assert_eq!(23, c.column(23));
        assert_eq!(1, c.line_number(23));
    }
}
