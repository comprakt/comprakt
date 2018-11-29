#![feature(nll)]
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

use difference::Changeset;
use integration_test_codegen::*;
use std::{
    collections::HashMap,
    env,
    ffi::{OsStr, OsString},
    fs::File,
    io::{Read, Write},
    path::PathBuf,
    process::{Command, Output},
    sync::Mutex,
};

#[derive(Debug, Clone)]
#[allow(dead_code)]
enum CompilerPhase {
    Lexer,
    Parser,
    Ast,
    Semantic,
    Assembly,
    Binary { output: PathBuf },
}

#[derive(Debug, Clone)]
#[allow(dead_code)]
enum CompilerCall {
    RawCompiler(CompilerPhase),
    AstInspector,
}

const ROOT_DIR: &str = env!("CARGO_MANIFEST_DIR");

/// Arguments that should be given to the compiler under test
fn compiler_args(phase: CompilerPhase) -> Vec<OsString> {
    let args: &[&str] = match phase {
        CompilerPhase::Lexer => &["--lextest"],
        CompilerPhase::Parser => &["--parsetest"],
        CompilerPhase::Ast => &["--print-ast"],
        CompilerPhase::Semantic => &["--check"],
        CompilerPhase::Assembly => &["--emit-asm"],
        CompilerPhase::Binary { output } => {
            return vec![
                OsString::from("--compile-firm"),
                OsString::from("-o"),
                output.as_os_str().to_os_string(),
            ]
        }
    };

    args.iter().map(OsString::from).collect::<Vec<_>>()
}

fn compiler_call(compiler_call: CompilerCall, filepath: &PathBuf) -> Command {
    match compiler_call {
        CompilerCall::RawCompiler(phase) => {
            let mut cmd = env::var("COMPILER_BINARY")
                .map(|path| {
                    println!("Test run using alternate compiler binary at {}", path);
                    Command::new(path)
                })
                .unwrap_or_else(|_| {
                    let binary = main_binary();
                    println!("Test run using the default compiler binary at {:?}", binary);
                    Command::new(binary)
                });

            cmd.env("TERM", "dumb"); // disable color output

            cmd.args(compiler_args(phase));
            cmd.arg(filepath.as_os_str());

            cmd
        }
        CompilerCall::AstInspector => {
            let ast_inspector_path = project_binary(Some("inspect-ast"));
            println!(
                "Test run using the ast inspector binary at {:?}",
                ast_inspector_path
            );

            let mut cmd = Command::new(ast_inspector_path);

            cmd.env("TERM", "dumb").arg(filepath.as_os_str());

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

fn tentative_file_path(reference: &PathBuf) -> PathBuf {
    let update_references = env::var("UPDATE_REFERENCES");

    if update_references.is_ok() {
        reference.clone()
    } else {
        with_extension(reference, ".tentative")
    }
}

fn load_reference(
    generate_tentatives: bool,
    path: &PathBuf,
    actual: &str,
) -> Result<String, Option<PathBuf>> {
    if !path.is_file() {
        Err(generate_tentative_reference(
            generate_tentatives,
            path,
            actual,
        ))
    } else {
        Ok(read_file(path))
    }
}

fn generate_tentative_reference(
    generate_tentatives: bool,
    path: &PathBuf,
    actual: &str,
) -> Option<PathBuf> {
    if !generate_tentatives {
        return None;
    }

    let file_tentative = tentative_file_path(&path);

    File::create(&file_tentative)
        .and_then(|mut file| file.write_all(actual.as_bytes()))
        .ok();

    Some(file_tentative)
}

fn assert_changeset(
    generate_tentatives: bool,
    label: &str,
    path: &PathBuf,
    reference: &str,
    actual: &str,
) -> Result<(), Option<PathBuf>> {
    if reference != actual {
        let diff_style = env::var("DIFF_USING").unwrap_or_else(|_| {
            let num_different = (reference.len() as isize - actual.len() as isize).abs() as usize
                + reference
                    .chars()
                    .zip(actual.chars())
                    .filter(|(a, b)| a != b)
                    .count();
            if num_different > 10 {
                "\n".to_string()
            } else {
                " ".to_string()
            }
        });

        let changeset = Changeset::new(reference, actual, &diff_style);

        eprintln!("diff for '{}' >>>\n{}<<< end of diff", label, changeset);

        Err(generate_tentative_reference(
            generate_tentatives,
            &path,
            actual,
        ))
    } else {
        Ok(())
    }
}

fn assert_output(output: &Output, file: &TestFiles) {
    let stderr = normalize_stderr(&String::from_utf8_lossy(&output.stderr));
    let stderr_result =
        load_reference(file.generate_tentatives, &file.stderr, &stderr).and_then(|reference| {
            assert_changeset(
                file.generate_tentatives,
                "stderr",
                &file.stderr,
                &reference,
                &stderr,
            )
        });

    let stdout = &String::from_utf8_lossy(&output.stdout);
    let stdout_result =
        load_reference(file.generate_tentatives, &file.stdout, &stdout).and_then(|reference| {
            assert_changeset(
                file.generate_tentatives,
                "stdout",
                &file.stdout,
                &reference,
                &stdout,
            )
        });

    let exitcode = output
        .status
        .code()
        .map(|code| code.to_string())
        .unwrap_or_else(|| "terminated by signal (or crashed)".to_string());

    let exitcode_result = load_reference(file.generate_tentatives, &file.exitcode, &exitcode)
        .and_then(|reference| {
            assert_changeset(
                file.generate_tentatives,
                "exitcode",
                &file.exitcode,
                &reference.trim(),
                &exitcode,
            )
        });

    match (stderr_result, stdout_result, exitcode_result) {
        (Ok(_), Ok(_), Ok(_)) => (),
        (err, out, code) => panic!(
            "Regression test failed:\nstderr = {},\nstdout = {},\nexit code = {}\n",
            result_to_string(err),
            result_to_string(out),
            result_to_string(code)
        ),
    }
}

#[allow(dead_code)]
fn assert_compiler_phase(phase: CompilerCall, file: &TestFiles) {
    let output = compiler_call(phase, &file.input)
        .output()
        .expect("failed to call compiler under test");

    assert_output(&output, &file);
}

fn result_to_string(res: Result<(), Option<PathBuf>>) -> String {
    match res {
        Ok(()) => "Ok!".to_string(),
        Err(None) => "Wrong!".to_string(),
        Err(Some(path)) => format!("Wrong! Actual output written to {:?}", path),
    }
}

#[derive(Debug, serde_derive::Deserialize)]
struct CompilerTarget {
    kind: Vec<String>,
    crate_types: Vec<String>,
    name: String,
}

#[derive(Debug, serde_derive::Deserialize)]
struct CompilerArtifact {
    reason: String,
    target: CompilerTarget,
    filenames: Vec<PathBuf>,
}

lazy_static::lazy_static! {
    static ref BIN_PATH_CACHE: Mutex<HashMap<Option<&'static str>, PathBuf>> =
            { Mutex::new(HashMap::new()) };
}

fn main_binary() -> PathBuf {
    project_binary(None)
}

/// Build and get the main binary
fn project_binary(subproject: Option<&'static str>) -> PathBuf {
    let mut cache = BIN_PATH_CACHE.lock().unwrap();

    if let Some(path) = cache.get(&subproject) {
        return path.clone();
    }

    let mut cmd = Command::new(env::var("CARGO").unwrap());

    cmd.arg("build").arg("--message-format=json");

    if !cfg!(debug_assertions) {
        cmd.arg("--release");
    }

    if let Some(workspace_crate) = subproject {
        cmd.arg("-p");
        cmd.arg(workspace_crate);
    }

    let output = cmd.output().expect("failed to invoke cargo");

    if !output.status.success() {
        panic!(
            "cargo failed to build project: {:?}",
            String::from_utf8_lossy(&output.stderr)
        );
    }

    let artifacts: Vec<CompilerArtifact> = String::from_utf8_lossy(&output.stdout)
        .lines()
        .filter_map(|line| serde_json::from_str(line).ok())
        .collect();

    let possibly_invoked_binaries = artifacts
        .iter()
        .filter(|artifact| {
            artifact.reason == "compiler-artifact"
                && artifact.target.crate_types == ["bin".to_string()]
                && artifact.target.kind == ["bin".to_string()]
        })
        .collect::<Vec<_>>();

    if possibly_invoked_binaries.len() != 1 {
        panic!(
            "could not determine invoked binary. Candidates: {:?}",
            possibly_invoked_binaries
                .iter()
                .map(|candidate| candidate.filenames.clone())
                .collect::<Vec<_>>()
        );
    }

    let invoked_binary = possibly_invoked_binaries[0].filenames[0].clone();

    cache.insert(subproject, invoked_binary);
    cache.get(&subproject).unwrap().clone()
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
gen_assembly_integration_tests!();
gen_binary_integration_tests!();
