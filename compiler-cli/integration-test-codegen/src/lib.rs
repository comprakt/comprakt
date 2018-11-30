//! A macro that generates a test case for each test available during
//! compilation. We do this to make them show up as single tests in the output
//! of `cargo test`.
#![feature(proc_macro_hygiene)]
#![recursion_limit = "128"]

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
    gen_integration_tests("lexer", "", |test_name, mj_file| {
        default_test_generator(
            &quote! { CompilerCall::RawCompiler(CompilerPhase::Lexer) },
            test_name,
            mj_file,
        )
    })
}

#[allow(clippy::needless_pass_by_value)] // rust-clippy/issues/3067
#[proc_macro]
pub fn gen_parser_integration_tests(_args: TokenStream) -> TokenStream {
    gen_integration_tests("parser", "", |test_name, mj_file| {
        default_test_generator(
            &quote! { CompilerCall::RawCompiler(CompilerPhase::Parser) },
            test_name,
            mj_file,
        )
    })
}

#[allow(clippy::needless_pass_by_value)] // rust-clippy/issues/3067
#[proc_macro]
pub fn gen_assembly_integration_tests(_args: TokenStream) -> TokenStream {
    gen_integration_tests("assembly", "", |test_name, mj_file| {
        default_test_generator(
            &quote! { CompilerCall::RawCompiler(CompilerPhase::Assembly) },
            test_name,
            mj_file,
        )
    })
}

#[allow(clippy::needless_pass_by_value)] // rust-clippy/issues/3067
#[proc_macro]
pub fn gen_binary_integration_tests(_args: TokenStream) -> TokenStream {
    gen_integration_tests("binary", "", |test_name, mj_file| {
        let function_name = Ident::new(&test_name, Span::call_site());
        let path_str = mj_file.to_str().unwrap();

        quote! {
            #[test]
            fn #function_name() {
                let input = PathBuf::from(#path_str);
                let mut binary_path = input.with_extension("out");

                assert_compiler_phase(CompilerCall::RawCompiler(CompilerPhase::Binary {
                    output: binary_path.clone()
                }), &TestFiles {
                    stderr: with_extension(&input, ".stderr"),
                    stdout: with_extension(&input, ".stdout"),
                    exitcode: with_extension(&input, ".exitcode"),
                    input: input,
                    generate_tentatives: true
                });

                // reaching this line means the compiler assertions were correct
                let mut cmd = std::process::Command::new(&binary_path);
                let output = cmd.output().expect("failed to invoke generated binary");

                assert_output(&output, &TestFiles {
                    stderr: with_extension(&binary_path, ".stderr"),
                    stdout: with_extension(&binary_path, ".stdout"),
                    exitcode: with_extension(&binary_path, ".exitcode"),
                    input: binary_path.clone(),
                    generate_tentatives: true
                });
            }
        }
    })
}

#[allow(clippy::needless_pass_by_value)] // rust-clippy/issues/3067
#[proc_macro]
pub fn gen_ast_reference_integration_tests(_args: TokenStream) -> TokenStream {
    gen_integration_tests("ast", "", |test_name, mj_file| {
        default_test_generator(
            &quote! { CompilerCall::RawCompiler(CompilerPhase::Ast) },
            test_name,
            mj_file,
        )
    })
}

#[allow(clippy::needless_pass_by_value)] // rust-clippy/issues/3067
#[proc_macro]
pub fn gen_ast_idempotence_integration_tests(_args: TokenStream) -> TokenStream {
    gen_integration_tests("ast", "_idempotence", |test_name, mj_file| {
        let function_name = Ident::new(&test_name, Span::call_site());
        let path_str = mj_file.to_str().unwrap();

        quote! {
            #[test]
            fn #function_name() {
                let input = PathBuf::from(#path_str);

                assert_compiler_phase(CompilerCall::RawCompiler(CompilerPhase::Ast), &TestFiles {
                    stderr: with_extension(&input, ".stderr"),
                    stdout: with_extension(&input, ".stdout"),
                    exitcode: with_extension(&input, ".exitcode"),
                    input: with_extension(&input, ".stdout"),
                    generate_tentatives: false
                });
            }
        }
    })
}

#[allow(clippy::needless_pass_by_value)] // rust-clippy/issues/3067
#[proc_macro]
pub fn gen_semantic_integration_tests(_args: TokenStream) -> TokenStream {
    gen_integration_tests("semantic", "", |test_name, mj_file| {
        default_test_generator(
            &quote! { CompilerCall::RawCompiler(CompilerPhase::Semantic) },
            test_name,
            mj_file,
        )
    })
}

#[allow(clippy::needless_pass_by_value)] // rust-clippy/issues/3067
#[proc_macro]
pub fn gen_ast_inspector_tests(_args: TokenStream) -> TokenStream {
    gen_integration_tests("spans", "", |test_name, mj_file| {
        default_test_generator(&quote! { CompilerCall::AstInspector }, test_name, mj_file)
    })
}

fn default_test_generator(
    phase: &proc_macro2::TokenStream,
    test_name: &str,
    mj_file: &PathBuf,
) -> proc_macro2::TokenStream {
    let function_name = Ident::new(&test_name, Span::call_site());
    let path_str = mj_file.to_str().unwrap();

    quote! {
        #[test]
        fn #function_name() {
            let input = PathBuf::from(#path_str);

            assert_compiler_phase(#phase, &TestFiles {
                stderr: with_extension(&input, ".stderr"),
                stdout: with_extension(&input, ".stdout"),
                exitcode: with_extension(&input, ".exitcode"),
                input: input,
                generate_tentatives: true
            });
        }
    }
}

fn gen_integration_tests<F>(
    subfolder: &str,
    test_name_suffix: &str,
    test_generator: F,
) -> TokenStream
where
    F: Fn(&str, &PathBuf) -> proc_macro2::TokenStream,
{
    let test_dir: PathBuf = [ROOT_DIR, INTEGRATION_TEST_DIR, subfolder].iter().collect();

    let ascii_test_dir = subfolder.replace(|c: char| !c.is_ascii_alphanumeric(), "_");

    let mut out = format!(
        "mod {}{} {{ use super::*;",
        ascii_test_dir, test_name_suffix
    );

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

    for case in cases {
        let ascii_casename = case
            .file_name()
            .expect("failed to get file name of test case")
            .to_string_lossy()
            .replace(|c: char| !c.is_ascii_alphanumeric(), "_");

        let tokens = test_generator(&ascii_casename, &case);

        out.push_str(&tokens.to_string());
    }

    out.push('}'); // close test module

    out.parse().unwrap()
}
