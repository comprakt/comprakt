#![feature(try_from)]

extern crate serde_json;
#[macro_use]
extern crate serde_derive;
#[macro_use]
extern crate quote;
#[macro_use]
extern crate failure;
extern crate strum;
#[macro_use]
extern crate strum_macros;

use failure::{Error, Fail, ResultExt};
use quote::TokenStreamExt;
use std::path::PathBuf;

fn mjtests_path() -> PathBuf {
    PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("./tests")
}

#[derive(Serialize, Deserialize, Debug)]
pub enum SyntaxAndSemanticFilePath {
    Valid(PathBuf),   // absolute path to test file
    Invalid(PathBuf), // absolute path to test file
}

#[derive(Display, Debug, Fail)]
enum SyntaxAndSemanticFilePathError {
    GetFileType,
    NotAFile,
    FilenameNotUTF8,
    UnexpectedFilename(String),
}

impl SyntaxAndSemanticFilePath {
    fn all<TestCaseType>(p: &PathBuf) -> Result<Vec<TestCaseType>, Error>
    where
        TestCaseType: From<SyntaxAndSemanticFilePath>,
    {
        let files = std::fs::read_dir(&p).context(format_err!(
            "cannot read syntax test case directory {:?}",
            p
        ))?;
        let mut cases: Vec<TestCaseType> = Vec::new();
        for f in files {
            let f = f.expect("could not unwrap dir entry");
            if let Some(fp) = SyntaxAndSemanticFilePath::from_dir_entry(&f)? {
                cases.push(TestCaseType::from(fp));
            }
        }
        Ok(cases)
    }

    fn from_dir_entry(f: &std::fs::DirEntry) -> Result<Option<SyntaxAndSemanticFilePath>, Error> {
        let ft = f
            .file_type()
            .context(SyntaxAndSemanticFilePathError::GetFileType)?;
        if !ft.is_file() {
            return Err(SyntaxAndSemanticFilePathError::NotAFile)?;
        }

        let filename = f.file_name();
        let filename = filename
            .to_str()
            .ok_or_else(|| SyntaxAndSemanticFilePathError::FilenameNotUTF8)?;

        match filename {
            n if n.ends_with(".invalid.mj") || n.ends_with(".invalid.java") => {
                Ok(Some(SyntaxAndSemanticFilePath::Invalid(f.path())))
            }
            n if n.ends_with(".mj")
                || n.ends_with(".java")
                || n.ends_with(".valid.mj")
                || n.ends_with(".valid.java") =>
            {
                Ok(Some(SyntaxAndSemanticFilePath::Valid(f.path())))
            }
            n if n.starts_with('.') => Ok(None), // hidden files
            n if n.ends_with('~') => Ok(None),   // vim + emacs backup file
            _ => Err(SyntaxAndSemanticFilePathError::UnexpectedFilename(
                filename.to_string(),
            ))?,
        }
    }

    pub fn path(&self) -> &PathBuf {
        match self {
            SyntaxAndSemanticFilePath::Valid(ref p) => p,
            SyntaxAndSemanticFilePath::Invalid(ref p) => p,
        }
    }

    pub fn file_name(&self) -> &str {
        let p = self.path();
        p.file_name()
            .unwrap_or_else(|| panic!("test case path must point to file: {:?}", p))
            .to_str()
            .unwrap_or_else(|| panic!("test file name not utf-8: {:?}", p))
    }
}

#[derive(Debug, Serialize, Deserialize)]
pub enum SyntaxTestCase {
    Valid(SyntaxAndSemanticFilePath),   // absolute path to test file
    Invalid(SyntaxAndSemanticFilePath), // absolute path to test file
}

impl From<SyntaxAndSemanticFilePath> for SyntaxTestCase {
    fn from(fp: SyntaxAndSemanticFilePath) -> Self {
        match fp {
            SyntaxAndSemanticFilePath::Valid(_) => SyntaxTestCase::Valid(fp),
            SyntaxAndSemanticFilePath::Invalid(_) => SyntaxTestCase::Invalid(fp),
        }
    }
}

use std::ops::Deref;

impl Deref for SyntaxTestCase {
    type Target = SyntaxAndSemanticFilePath;
    fn deref(&self) -> &Self::Target {
        match self {
            SyntaxTestCase::Valid(p) => p,
            SyntaxTestCase::Invalid(p) => p,
        }
    }
}

impl SyntaxTestCase {
    pub fn all() -> Result<Vec<SyntaxTestCase>, Error> {
        let p = mjtests_path().join("./syntax");
        SyntaxAndSemanticFilePath::all(&p)
    }
}

#[derive(Debug, Serialize, Deserialize)]
pub enum SemanticTestCase {
    Valid(SyntaxAndSemanticFilePath),
    Invalid(SyntaxAndSemanticFilePath),
}

impl From<SyntaxAndSemanticFilePath> for SemanticTestCase {
    fn from(fp: SyntaxAndSemanticFilePath) -> Self {
        match fp {
            SyntaxAndSemanticFilePath::Valid(_) => SemanticTestCase::Valid(fp),
            SyntaxAndSemanticFilePath::Invalid(_) => SemanticTestCase::Invalid(fp),
        }
    }
}

impl Deref for SemanticTestCase {
    type Target = SyntaxAndSemanticFilePath;
    fn deref(&self) -> &Self::Target {
        match self {
            SemanticTestCase::Valid(p) => p,
            SemanticTestCase::Invalid(p) => p,
        }
    }
}

impl SemanticTestCase {
    pub fn all() -> Result<Vec<SemanticTestCase>, Error> {
        let p = mjtests_path().join("./semantic");
        SyntaxAndSemanticFilePath::all(&p)
    }
}

// QuotingWormhole is a gadget to encapsulate the use of serde_json
// for transfer of test case definitions from this procedural macro
// to the generated test cases.
//
// It must be public because the quote!() in impl quote::ToTokens uses it,
// but it should never be used by external users of this crate.
pub trait QuotingWormhole<'a>: serde::Deserialize<'a> + serde::Serialize {
    fn to_json(&self) -> String {
        serde_json::to_string_pretty(self).expect("could not serialize syntax test case")
    }
    fn from_json(json: &'a str) -> Self {
        serde_json::from_str(json).expect("could not deserialize syntax test case")
    }
}

impl<'a> QuotingWormhole<'a> for SyntaxTestCase {}
impl<'a> QuotingWormhole<'a> for SemanticTestCase {}

macro_rules! derive_to_tokens_for_wormwhole {
    ($wormwholetype:ty) => {
        impl quote::ToTokens for $wormwholetype {
            fn to_tokens(&self, tokens: &mut proc_macro2::TokenStream) {
                let x = self.to_json();
                let code = quote!({
                    use mjtest::QuotingWormhole;
                    let y: $wormwholetype = QuotingWormhole::from_json(#x);
                    y
                });
                tokens.append_all(code);
            }
        }
    };
}

derive_to_tokens_for_wormwhole!(SyntaxTestCase);
derive_to_tokens_for_wormwhole!(SemanticTestCase);
