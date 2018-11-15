use super::{Position, PositionIterator};
use failure::{Error, Fail, ResultExt};
use memmap::Mmap;
use std::{
    fs::File,
    io,
    ops::Deref,
    path::{Path, PathBuf},
};

#[derive(Debug, Clone)]
pub struct AsciiFile<'m> {
    // TODO: mapping should be private
    pub mapping: &'m [u8],
}

const EMPTY_FILE: [u8; 0] = [0; 0];

#[derive(Debug, Fail)]
enum AsciiFileError {
    #[fail(display = "cannot open input file {:?}", path)]
    OpenInput { path: PathBuf },
    #[fail(display = "cannot mmap input file {:?}", path)]
    Mmap { path: PathBuf },
    #[fail(
        display = "input contains non-ascii character at byte offset {}.",
        position
    )]
    NotAscii { position: usize },
}

impl<'m> AsciiFile<'m> {
    // cost: O(fileLen) since we need to check if all chars are ASCII
    pub fn new(mapping: &'m [u8]) -> Result<AsciiFile<'m>, Error> {
        // TODO: move validation out of constructor
        // TODO: add nice error back
        if let Some(position) = mapping.iter().position(|c| !c.is_ascii()) {
            return Err(AsciiFileError::NotAscii { position }.into());
        }

        Ok(AsciiFile { mapping })
    }

    pub fn mmap<P: AsRef<Path>>(path_ref: P) -> Result<Box<AsRef<[u8]>>, Error> {
        let path = path_ref.as_ref().to_path_buf();
        let file = File::open(&path).context(AsciiFileError::OpenInput { path: path.clone() })?;

        let mmres = unsafe { Mmap::map(&file) };

        match mmres {
            Ok(mmap) => return Ok(box mmap),
            Err(e) if e.kind() == io::ErrorKind::InvalidInput => {
                // Linux returns EINVAL on file size 0, but let's be sure
                let file_size = file
                    .metadata()
                    .map(|m| m.len())
                    .context("could not get file metadata while interpreting mmap error")?;

                if file_size == 0 {
                    return Ok(box EMPTY_FILE);
                } else {
                    return Err(e
                        .context(AsciiFileError::Mmap { path: path.clone() })
                        .into());
                }
            }
            Err(e) => {
                return Err(e
                    .context(AsciiFileError::Mmap { path: path.clone() })
                    .into())
            }
        };
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
