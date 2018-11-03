extern crate serde_json;
#[macro_use]
extern crate serde_derive;
#[macro_use]
extern crate quote;

use quote::TokenStreamExt;
use std::path::PathBuf;

fn mjtests_path() -> PathBuf {
    PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("./tests")
}

#[derive(Debug, Serialize, Deserialize)]
pub enum SyntaxTestCase {
    Valid(PathBuf),   // absolute path to test file
    Invalid(PathBuf), // absolute path to test file
}

impl SyntaxTestCase {
    pub fn all() -> Vec<SyntaxTestCase> {
        let p = mjtests_path().join("./syntax");
        let files = std::fs::read_dir(&p)
            .expect(&format!("cannot read syntax test case directory {:?}", p));

        let mut cases: Vec<SyntaxTestCase> = Vec::new();
        for f in files {
            let f = f.expect("could not unwrap dir entry");
            let ft = f
                .file_type()
                .expect(&format!("could not get filetype of {:?}", f.path()));
            if !ft.is_file() {
                continue;
            }

            let filename = f.file_name();
            let filename = filename
                .to_str()
                .expect(&format!("test file name not utf-8: {:?}", filename));

            match filename {
                n if n.ends_with(".invalid.mj") || n.ends_with(".invalid.java") => {
                    cases.push(SyntaxTestCase::Invalid(f.path()))
                }
                n if n.ends_with(".mj")
                    || n.ends_with(".java")
                    || n.ends_with(".valid.mj")
                    || n.ends_with(".valid.java") =>
                {
                    cases.push(SyntaxTestCase::Valid(f.path()))
                }
                n if n == ".mjtest_correct_testcases_syntax" => (),
                _ => panic!("unexpected file {:?}", filename),
            }
        }

        cases
    }

    pub fn path(&self) -> &PathBuf {
        match self {
            SyntaxTestCase::Valid(ref p) => p,
            SyntaxTestCase::Invalid(ref p) => p,
        }
    }

    pub fn file_name(&self) -> &str {
        let p = self.path();
        let filename = p
            .file_name()
            .expect(&format!("test case path must point to file: {:?}", p));
        let filename = filename
            .to_str()
            .expect(&format!("test file name not utf-8: {:?}", filename));
        filename
    }

    pub fn test_name(&self) -> String {
        use SyntaxTestCase::*;
        let prefix = match self {
            Valid(_) => "valid",
            Invalid(_) => "invalid",
        };

        let escaped_name = self.file_name().replace(".", "__");
        format!("mjtest_syntax_{}_{}", prefix, escaped_name)
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

impl quote::ToTokens for SyntaxTestCase {
    fn to_tokens(&self, tokens: &mut proc_macro2::TokenStream) {
        let x = self.to_json();
        let code = quote!({
            use mjtest::QuotingWormhole;
            let y: SyntaxTestCase = QuotingWormhole::from_json(#x);
            y
        });
        tokens.append_all(code);
    }
}
