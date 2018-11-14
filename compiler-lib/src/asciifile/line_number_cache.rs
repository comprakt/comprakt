//! Caches information about linebreaks that allows us to
//! reconstruct the line number and character column in
//! O(log N). This makes sense as we only request line
//! numbers in a few cases (e.g. on error).
//!
//! ```
//! use compiler_lib::asciifile::LineNumberCache;
//!
//! let input = "asd\nqwer\nZU\nfgfh";
//! let positions: Vec<_> = input
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
//! let cache = LineNumberCache::new(&positions);
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
pub struct LineNumberCache<'slice> {
    /// byte index of linebreaks (\n) in the file. The
    /// index of the newline within the sorted list is
    /// the row, the line number is the row plus 1.
    linebreaks: &'slice [usize],
}

impl<'slice> LineNumberCache<'slice> {
    pub fn new(linebreaks: &'slice [usize]) -> Self {
        Self { linebreaks }
    }

    pub fn line_number(&self, byte_offset: usize) -> usize {
        self.row(byte_offset) + 1
    }

    pub fn column(&self, byte_offset: usize) -> usize {
        self.row_and_column(byte_offset).1
    }

    /// does not detect/error on byte_offsets out of range
    pub fn row(&self, byte_offset: usize) -> usize {
        // returns `Err` if the number as not found.
        // => given character was not a linebreak. error contains the insertion
        // position, which is the line number returns `Ok` if the number is
        // found. => given charater is a line break. `Ok` contains the row
        // number. column is 0
        // binary_search always gives index of the smaller number
        // binary_search returns len if the number is bigger than everything seen
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

    /// Get row and column at once.
    ///
    /// This method exists because most of the time you want both.
    /// And its more efficient to look up both at once.
    pub fn row_and_column(&self, byte_offset: usize) -> (usize, usize) {
        let row = self.row(byte_offset);
        let col = if row == 0 {
            // no linebreak yet
            byte_offset
        } else {
            // line that is last line or line in-between

            // we have to add one since we want to look one
            // character after the newline!
            let line_start_offset = self.linebreaks[row - 1] + 1;
            byte_offset - line_start_offset
        };

        (row, col)
    }
}
