//! Checks for regresssions in the CLI interface code

use assert_cmd::prelude::*;
use difference::Changeset;
use integration_test_codegen::{gen_lexer_integration_tests, gen_parser_integration_tests};
use predicates::prelude::*;
use std::{ffi::OsStr, fs::File, io::Read, path::PathBuf, process::Command};

#[derive(Debug, Copy, Clone)]
#[allow(dead_code)]
enum CompilerPhase {
    Lexer,
    Parser,
}

const ROOT_DIR: &str = env!("CARGO_MANIFEST_DIR");

fn compiler_flag(phase: CompilerPhase) -> &'static str {
    match phase {
        CompilerPhase::Lexer => "--lextest",
        CompilerPhase::Parser => "--parsetest",
    }
}

fn compiler_call(phase: CompilerPhase, filepath: &PathBuf) -> Command {
    let mut cmd = Command::main_binary().unwrap();
    cmd.env("TERM", "dumb");
    cmd.args(&[OsStr::new(compiler_flag(phase)), filepath.as_os_str()]);
    cmd
}

fn normalize_stderr(stderr: &str) -> String {
    stderr.replace(ROOT_DIR, "{ROOT}")
}

#[allow(dead_code)]
fn assert_compiler_phase_failure(phase: CompilerPhase, filename: &str) {
    let filepath = PathBuf::from(filename);
    let extension = filepath
        .extension()
        .unwrap_or_else(|| OsStr::new(""))
        .to_os_string();

    let mut filepath_stderr = filepath.clone();

    filepath_stderr.set_extension({
        let mut ext = extension.clone();
        ext.push(OsStr::new(".stderr"));
        ext
    });

    if !filepath_stderr.is_file() {
        let mut filepath_stderr_tentative = filepath.clone();
        filepath_stderr_tentative.set_extension({
            let mut ext = extension.clone();
            ext.push(OsStr::new(".stderr.tentative"));
            ext
        });

        let mut filepath_stdout_tentative = filepath.clone();
        filepath_stdout_tentative.set_extension({
            let mut ext = extension.clone();
            ext.push(OsStr::new(".stdout.tentative"));
            ext
        });

        // TODO: normalize stderr
        match compiler_call(phase, &filepath)
            .stdout(File::create(&filepath_stdout_tentative).expect("write stdout file failed"))
            .stderr(File::create(&filepath_stderr_tentative).expect("write stderr file failed"))
            .spawn()
        {
            Ok(_) => {
                panic!(
                    "Cannot find required reference output file {:?}. \
                     The current output was written to {:?}. \
                     Verify it and remove the `.tentative` suffix.",
                    filepath_stderr, filepath_stderr_tentative
                );
            }
            Err(_msg) => panic!(
                "Cannot find required reference output file {:?}.",
                filepath_stderr
            ),
        }
    }

    let assertion = compiler_call(phase, &filepath).assert();

    let stderr_expected = read_file(&filepath_stderr);
    let changeset = Changeset::new(
        &stderr_expected,
        &normalize_stderr(&String::from_utf8_lossy(&assertion.get_output().stderr)),
        "\n",
    );
    let stderr_predicate = predicate::function(|actual: &[u8]| {
        normalize_stderr(&String::from_utf8_lossy(actual)) == stderr_expected
    });

    assertion
        .append_context("changeset stderr", format!("\n{}", changeset))
        .stderr(stderr_predicate)
        .failure();
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
