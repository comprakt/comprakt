//! Checks for regresssions in the CLI interface code

use assert_cmd::prelude::*;
use integration_test_codegen::gen_integration_tests;
use predicates::prelude::*;
use std::{ffi::OsStr, fs::File, path::PathBuf, process::Command};

fn parsetest_cmd(filepath: &PathBuf) -> Command {
    let mut cmd = Command::main_binary().unwrap();
    cmd.env("TERM", "dumb");
    cmd.args(&[OsStr::new("--parsetest"), filepath.as_os_str()]);
    cmd
}

fn assert_parser_failure(filename: &str) {
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

        match parsetest_cmd(&filepath)
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

    let stderr_predicate = predicate::path::eq_file(filepath_stderr);

    parsetest_cmd(&filepath)
        .assert()
        .stderr(stderr_predicate)
        .failure();
}

gen_integration_tests!();
