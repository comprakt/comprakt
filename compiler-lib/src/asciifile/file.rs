use super::{Position, PositionIterator};
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
        display = "input contains non-ascii character at byte offset {}.",
        position
    )]
    NotAscii { position: usize },
}

impl<'m> AsciiFile<'m> {
    // cost: O(fileLen) since we need to check if all chars are ASCII
    pub fn new(mapping: &'m [u8]) -> Result<AsciiFile<'m>, EncodingError> {
        // TODO: move validation out of constructor
        // TODO: add nice error back
        if let Some(position) = mapping.iter().position(|c| !c.is_ascii()) {
            return Err(EncodingError::NotAscii { position });
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
        let EncodingError::NotAscii { position } = e;
        assert_eq!(position, 3);
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

        let EncodingError::NotAscii { position } = file.err().unwrap();
        assert_eq!(position, 0);
    }
}
