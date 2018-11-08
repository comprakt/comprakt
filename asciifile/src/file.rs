use crate::{Position, PositionIterator};
use failure::Fail;
use std::ops::Deref;

#[derive(Debug)]
pub struct AsciiFile<'m> {
    // TODO: mapping should be private
    pub mapping: &'m [u8],
}

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
    // TODO: remove these index based methods
    pub fn get_line_end_idx(
        &'m self,
        byte_offset: usize,
        max_context_length: usize,
    ) -> (LineTruncation, usize) {
        AsciiFile::get_line_end_idx_byteslice(self.mapping, byte_offset, max_context_length)
    }

    pub fn get_line_end_idx_byteslice(
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
    pub fn get_line_start_idx(
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

    pub fn iter(&self) -> PositionIterator<'_> {
        PositionIterator::new(Position::at_file_start(self))
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

    #[test]
    fn works_with_ascii() {
        let string = "ABCDEFG\n\t";
        let file = AsciiFile::new(string.as_bytes()).unwrap();
        let contents: &str = &file;
        assert!(string == contents);
    }

    #[test]
    fn returns_err_on_non_ascii() {
        let input = "one💩two";
        let file = AsciiFile::new(input.as_bytes());
        assert!(file.is_err());
        let e = file.err().unwrap();
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

        // check if we generated the invalid string correctly
        assert_eq!(input.len(), 2);
        assert!(std::str::from_utf8(input).is_err());

        // check if it is rejected
        let file = AsciiFile::new(&input);
        assert!(file.is_err());

        let EncodingError::NotAscii { position, prev } = file.err().unwrap();
        assert_eq!(position, 0);
        assert_eq!(prev, "");
    }
}
