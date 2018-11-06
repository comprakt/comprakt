//! A macro that generates a test case for each test available during
//! compilation. We do this to make them show up as single tests in the output
//! of `cargo test`.
#![feature(proc_macro_hygiene)]

extern crate proc_macro;
extern crate proc_macro2;
extern crate quote;
extern crate syn;

use proc_macro::TokenStream;
use proc_macro2::{Ident, Span};
use quote::quote;
use std::{ffi::OsStr, fs, path::PathBuf};

const ROOT_DIR: &str = env!("CARGO_MANIFEST_DIR");
const INTEGRATION_TEST_DIR: &str = "../integration-tests";

#[proc_macro]
pub fn gen_integration_tests(_: TokenStream) -> TokenStream {
    let mut out = String::new();

    let test_dir: PathBuf = [ROOT_DIR, INTEGRATION_TEST_DIR].iter().collect();

    let cases = fs::read_dir(&test_dir)
        .expect(&format!("test directory {:?} does not exist.", test_dir))
        .map(|entry| fs::canonicalize(entry.unwrap().path()).unwrap())
        .filter(|path| {
            if !path.is_file() {
                return false;
            }

            match path.extension().and_then(OsStr::to_str) {
                Some("mj") | Some("java") | Some(".invalid.mj") | Some(".invalid.java") => true,
                None | Some(_) => false,
            }
        });

    for (id, case) in cases.enumerate() {
        let ascii_casename = case
            .file_name()
            .unwrap_or(OsStr::new(""))
            .to_string_lossy()
            .replace(|c: char| !c.is_ascii_alphanumeric(), "_");

        let function_name =
            Ident::new(&format!("cli_{}_{}", id, ascii_casename), Span::call_site());

        let path = case.to_str();

        let tokens = quote! {
            #[test]
            fn #function_name() {
                assert_parser_failure(#path);
            }
        };

        out.push_str(&tokens.to_string());
    }

    out.parse().unwrap()
}
