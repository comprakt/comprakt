//! Checks for regresssions in the CLI interface code
//!
//! To skip unit tests, and only run integration tests, execute:
//!
//! ```sh
//! cargo test --test integration
//! ```
//!
//! By default, the main binary is used. But you can specify another
//! binary by setting the `COMPILER_BINARY` environment flag. For
//! example:
//!
//! ```sh
//! COMPILER_BINARY="./run" cargo test --test integration
//! ```

use assert_cmd::prelude::*;
use difference::Changeset;
use integration_test_codegen::*;
use predicates::prelude::*;
use std::{
    ffi::OsStr,
    fs::File,
    io::{Read, Write},
    path::PathBuf,
    process::Command,
};

#[derive(Debug, Copy, Clone)]
#[allow(dead_code)]
enum CompilerPhase {
    Lexer,
    Parser,
    Ast,
    Semantic,
}

#[derive(Debug, Copy, Clone)]
#[allow(dead_code)]
enum CompilerCall {
    RawCompiler(CompilerPhase),
    AstInspector,
}

const ROOT_DIR: &str = env!("CARGO_MANIFEST_DIR");

fn compiler_flag(phase: CompilerPhase) -> &'static str {
    match phase {
        CompilerPhase::Lexer => "--lextest",
        CompilerPhase::Parser => "--parsetest",
        CompilerPhase::Ast => "--print-ast",
        CompilerPhase::Semantic => "--check",
    }
}

fn compiler_call(compiler_call: CompilerCall, filepath: &PathBuf) -> Command {
    match compiler_call {
        CompilerCall::RawCompiler(phase) => {
            let mut cmd = std::env::var("COMPILER_BINARY")
                .map(|path| {
                    println!("Test run using alternate compiler binary at {}", path);
                    Command::new(path)
                })
                .unwrap_or_else(|_| {
                    println!("Test run using the default compiler binary");
                    Command::main_binary().unwrap()
                });
            cmd.env("TERM", "dumb"); // disable color output
            cmd.args(&[OsStr::new(compiler_flag(phase)), filepath.as_os_str()]);
            cmd
        }
        CompilerCall::AstInspector => {
            // TODO: do not invoke through cargo to speed up tests
            let mut cmd = Command::new("cargo");
            cmd.env("TERM", "dumb"); // disable color output
            cmd.args(&[OsStr::new("-q"), OsStr::new("run")]);

            if !cfg!(debug_assertions) {
                cmd.arg(OsStr::new("--release"));
            }

            cmd.args(&[
                OsStr::new("-p"),
                OsStr::new("inspect-ast"),
                OsStr::new("--"),
                OsStr::new(filepath.as_os_str()),
            ]);
            cmd
        }
    }
}

fn normalize_stderr(stderr: &str) -> String {
    stderr.replace(ROOT_DIR, "{ROOT}")
}

fn with_extension(path: &PathBuf, extension: &str) -> PathBuf {
    let mut filepath = path.clone();

    let original_extension = filepath
        .extension()
        .unwrap_or_else(|| OsStr::new(""))
        .to_os_string();

    filepath.set_extension({
        let mut ext = original_extension.clone();
        ext.push(OsStr::new(extension));
        ext
    });

    filepath
}

struct TestFiles {
    input: PathBuf,
    stderr: PathBuf,
    stdout: PathBuf,
    exitcode: PathBuf,
    generate_tentatives: bool,
}

#[allow(dead_code)]
fn assert_compiler_phase(phase: CompilerCall, file: &TestFiles) {
    if !file.stderr.is_file() || !file.stdout.is_file() || !file.exitcode.is_file() {
        // if any of the files is missing. Fail the test. Regenerate all files
        // with an additional ".tenative" extension. The programmer can then
        // verify the generated tentative new reference results and remove
        // the ".tentative" suffix.
        if !file.generate_tentatives {
            panic!("Cannot find required reference output files.");
        }

        let file_stderr_tentative = with_extension(&file.stderr, ".tentative");
        let file_stdout_tentative = with_extension(&file.stdout, ".tentative");
        let file_exitcode_tentative = with_extension(&file.exitcode, ".tentative");

        match compiler_call(phase, &file.input)
            .stdout(File::create(&file_stdout_tentative).expect("write stdout file failed"))
            .stderr(File::create(&file_stderr_tentative).expect("write stderr file failed"))
            .status()
        {
            Ok(status) => {
                // output exitcode to file
                File::create(&file_exitcode_tentative)
                    .and_then(|mut file| {
                        if let Some(exit_code) = status.code() {
                            file.write_all(exit_code.to_string().as_bytes())
                        } else {
                            Ok(())
                        }
                    })
                    .ok();

                // fail the incomplete test
                panic!(
                    "Cannot find required reference output files. \
                     The current output was written to {:?}, {:?}
                     and {:?}. \
                     Verify them and remove the `.tentative` suffix.",
                    file_stderr_tentative, file_stdout_tentative, file_exitcode_tentative
                );
            }
            Err(_msg) => panic!("Cannot find required reference output files.",),
        }
    }

    let assertion = compiler_call(phase, &file.input).assert();

    // stderr
    let stderr_expected = read_file(&file.stderr);
    let stderr_changeset = Changeset::new(
        &stderr_expected,
        &normalize_stderr(&String::from_utf8_lossy(&assertion.get_output().stderr)),
        " ",
    );
    let stderr_predicate = predicate::function(|actual: &[u8]| {
        normalize_stderr(&String::from_utf8_lossy(actual)) == stderr_expected
    });

    // stdout
    let stdout_expected = read_file(&file.stdout);
    let stdout_changeset = Changeset::new(
        &stdout_expected,
        &String::from_utf8_lossy(&assertion.get_output().stdout),
        " ",
    );
    let stdout_predicate =
        predicate::function(|actual: &[u8]| actual == stdout_expected.as_bytes());

    // exitcode
    let exit_code_expected_str = read_file(&file.exitcode);
    let exit_code_expected_str = exit_code_expected_str.trim();

    if let Ok(exit_code_expected) = exit_code_expected_str.parse::<i32>() {
        // all reference files available, check if the outputs of the current
        // version are identical to the reference outputs.
        assertion
            .append_context("changeset stderr", format!("\n{}", stderr_changeset))
            .append_context("changeset stdout", format!("\n{}", stdout_changeset))
            .stderr(stderr_predicate)
            .stdout(stdout_predicate)
            .code(exit_code_expected);
    } else {
        panic!("failed to parse reference exit code. Failing test.");
    }
}

fn read_file(filename: &PathBuf) -> String {
    let mut f = File::open(filename).unwrap_or_else(|_| panic!("file not found: {:?}", filename));
    let mut contents = String::new();
    f.read_to_string(&mut contents)
        .unwrap_or_else(|_| panic!("could not read file: {:?}", filename));
    contents
}

gen_lexer_integration_tests!();
gen_parser_integration_tests!();
gen_ast_reference_integration_tests!();
gen_ast_idempotence_integration_tests!();
gen_semantic_integration_tests!();
gen_ast_inspector_tests!();
