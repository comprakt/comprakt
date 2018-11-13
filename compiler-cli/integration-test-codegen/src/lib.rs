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

// TODO: expose gen_integration_tests instead and parse phase and subfolder as
// path
#[allow(clippy::needless_pass_by_value)] // rust-clippy/issues/3067
#[proc_macro]
pub fn gen_lexer_integration_tests(_args: TokenStream) -> TokenStream {
    gen_integration_tests(
        &quote! { CompilerPhase::Lexer },
        "lexer",
        |v| quote! { #v },
        "",
        true,
    )
}

#[allow(clippy::needless_pass_by_value)] // rust-clippy/issues/3067
#[proc_macro]
pub fn gen_parser_integration_tests(_args: TokenStream) -> TokenStream {
    gen_integration_tests(
        &quote! { CompilerPhase::Parser },
        "parser",
        |v| quote! { #v },
        "",
        true,
    )
}

#[allow(clippy::needless_pass_by_value)] // rust-clippy/issues/3067
#[proc_macro]
pub fn gen_ast_reference_integration_tests(_args: TokenStream) -> TokenStream {
    gen_integration_tests(
        &quote! { CompilerPhase::Ast },
        "ast",
        |v| quote! { #v },
        "",
        true,
    )
}

#[allow(clippy::needless_pass_by_value)] // rust-clippy/issues/3067
#[proc_macro]
pub fn gen_ast_idempotence_integration_tests(_args: TokenStream) -> TokenStream {
    gen_integration_tests(
        &quote! { CompilerPhase::Ast },
        "ast",
        |v| {
            quote! { with_extension(&#v, ".stdout") }
        },
        "_idempotence",
        false,
    )
}

fn gen_integration_tests<F>(
    phase: &proc_macro2::TokenStream,
    subfolder: &str,
    input_adaptor: F,
    test_name_suffix: &str,
    should_generate: bool,
) -> TokenStream
where
    F: Fn(proc_macro2::TokenStream) -> proc_macro2::TokenStream,
{
    let mut out = String::new();

    let test_dir: PathBuf = [ROOT_DIR, INTEGRATION_TEST_DIR, subfolder].iter().collect();

    let ascii_test_dir = subfolder.replace(|c: char| !c.is_ascii_alphanumeric(), "_");

    let cases = fs::read_dir(&test_dir)
        .unwrap_or_else(|_| panic!("test directory {:?} does not exist.", test_dir))
        .map(|entry| fs::canonicalize(entry.unwrap().path()).unwrap())
        .filter(|path| {
            if !path.is_file() {
                return false;
            }

            match path.extension().and_then(OsStr::to_str) {
                Some("mj") | Some("java") => true,
                None | Some(_) => false,
            }
        });

    for (id, case) in cases.enumerate() {
        let ascii_casename = case
            .file_name()
            .unwrap_or_else(|| OsStr::new(""))
            .to_string_lossy()
            .replace(|c: char| !c.is_ascii_alphanumeric(), "_");

        let function_name = Ident::new(
            &format!(
                "cli_{}_{}_{}{}",
                ascii_test_dir, id, ascii_casename, test_name_suffix
            ),
            Span::call_site(),
        );

        let path_str = case.to_str().unwrap();
        let path_input_tokenstream = input_adaptor(quote! { PathBuf::from(#path_str) });
        let tokens = quote! {
            #[test]
            fn #function_name() {
                let path = PathBuf::from(#path_str);
                let input = #path_input_tokenstream;

                assert_compiler_phase(#phase, &TestFiles {
                    input,
                    stderr: with_extension(&path, ".stderr"),
                    stdout: with_extension(&path, ".stdout"),
                    exitcode: with_extension(&path, ".exitcode"),
                    generate_tentatives: #should_generate
                });
            }
        };

        out.push_str(&tokens.to_string());
    }

    out.parse().unwrap()
}
