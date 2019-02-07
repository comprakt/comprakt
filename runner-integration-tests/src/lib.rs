#![feature(nll)]

pub mod lookup;
pub mod serde_humantime;
mod testkind;
pub mod yaml;
pub use self::{lookup::*, testkind::*};
use difference::Changeset;
use failure::Fail;
use optimization::{compile_time_assertions, Level};
use serde::de::DeserializeOwned;
use serde_derive::Deserialize;
use std::{
    collections::HashMap,
    env,
    ffi::OsString,
    fs::File,
    io::Write,
    path::PathBuf,
    process::{Command, Output},
    sync::Mutex,
    time::Duration,
};

#[derive(Debug, Clone)]
#[allow(dead_code)]
pub enum CompilerPhase {
    Lexer,
    Parser,
    Ast,
    Semantic,
    Binary {
        backend: Backend,
        output: PathBuf,
        assembly: Option<PathBuf>,
        optimizations: Level,
    },
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Backend {
    Own,
    Libfirm,
}

impl Backend {
    pub fn to_ascii_label(self) -> &'static str {
        match self {
            Backend::Own => "own",
            Backend::Libfirm => "libfirm",
        }
    }
}

#[derive(Debug, Clone)]
#[allow(dead_code)]
pub enum CompilerCall {
    RawCompiler(CompilerPhase),
    AstInspector {
        content: Option<String>,
        kind: Option<String>,
        typeinfo: Option<String>,
    },
}

/// Arguments that should be given to the compiler under test
fn compiler_args(phase: CompilerPhase) -> Vec<OsString> {
    let args: &[&str] = match phase {
        CompilerPhase::Lexer => &["--lextest"],
        CompilerPhase::Parser => &["--parsetest"],
        CompilerPhase::Ast => &["--print-ast"],
        CompilerPhase::Semantic => &["--check"],
        CompilerPhase::Binary {
            backend,
            output,
            assembly,
            optimizations,
        } => {
            let cmd_flag = match backend {
                Backend::Libfirm => "--compile-firm",
                Backend::Own => "--compile",
            };

            let mut flags = vec![
                OsString::from(cmd_flag),
                OsString::from("-o"),
                output.as_os_str().to_os_string(),
            ];

            if let Some(path) = assembly {
                flags.push(OsString::from("--emit-asm"));
                flags.push(path.as_os_str().to_os_string());
            }

            let optizimation_arg = match optimizations {
                Level::Custom(passes) => {
                    let vals: String = passes
                        .into_iter()
                        .map(|opt| opt.kind.to_string())
                        .collect::<Vec<_>>()
                        .join(",");

                    OsString::from(format!("custom:{}", vals))
                }
                Level::Aggressive => OsString::from("aggressive"),
                Level::Moderate => OsString::from("moderate"),
                Level::None => OsString::from("none"),
            };

            flags.push(OsString::from("-O"));
            flags.push(optizimation_arg);

            return flags;
        }
    };

    args.iter().map(OsString::from).collect::<Vec<_>>()
}

pub fn compiler_call(compiler_call: CompilerCall, filepath: &PathBuf) -> Command {
    match compiler_call {
        CompilerCall::RawCompiler(phase) => {
            let mut cmd = env::var("COMPILER_BINARY")
                .map(|path| {
                    log::debug!("Test run using alternate compiler binary at {}", path);
                    Command::new(path)
                })
                .unwrap_or_else(|_| {
                    let binary = project_binary(Some("compiler-cli"));
                    log::debug!("Test run using the default compiler binary at {:?}", binary);
                    Command::new(binary)
                });

            cmd.env("TERM", "dumb"); // disable color output
            cmd.env(compile_time_assertions::ENV_VAR_NAME, "enabled");

            cmd.args(compiler_args(phase));
            cmd.arg(filepath.as_os_str());

            cmd
        }
        CompilerCall::AstInspector {
            content,
            kind,
            typeinfo,
        } => {
            let ast_inspector_path = project_binary(Some("inspect-ast"));
            println!(
                "Test run using the ast inspector binary at {:?}",
                ast_inspector_path
            );

            let mut cmd = Command::new(ast_inspector_path);

            if let Some(filter) = content {
                cmd.args(&["-c", &filter]);
            }

            if let Some(filter) = kind {
                cmd.args(&["-k", &filter]);
            }

            if let Some(filter) = typeinfo {
                cmd.args(&["-t", &filter]);
            }

            cmd.env("TERM", "dumb").arg(filepath.as_os_str());

            cmd
        }
    }
}

fn normalize_stderr(stderr: &str) -> String {
    stderr.replace(&*ROOT_DIR, "{ROOT}")
}

#[derive(Debug, Clone)]
pub struct TestSpec {
    pub input: PathBuf,
    pub references: PathBuf,
    pub generate_tentatives: bool,
}

fn tentative_file_path(reference: &PathBuf) -> PathBuf {
    let update_references = env::var("UPDATE_REFERENCES");

    if update_references.is_ok() {
        reference.clone()
    } else {
        add_extension(reference, "actual")
    }
}

#[derive(Debug, Fail)]
enum TestFailure {
    #[fail(
        display = "not found! was expected at {:?}. wrote reference to {:?}",
        tried, wrote
    )]
    NotFoundWroteReference { tried: PathBuf, wrote: PathBuf },
    #[fail(display = "Not found! was expected at {:?}", tried)]
    NotFound { tried: PathBuf },
    #[fail(display = "Wrong! does not match reference output")]
    DiffMismatch,
    #[fail(
        display = "Wrong! does not match reference output. Wrote actual output to {:?}",
        wrote
    )]
    DiffMismatchWroteReference { wrote: PathBuf },
}

fn reference_to_absolute_path(setup: &TestSpec, rel_path: &PathBuf) -> PathBuf {
    let mut path = setup.references.clone();
    path.pop(); // remove filename, so we get the base directory
    path.push(rel_path);
    path
}

fn load_reference(
    setup: &TestSpec,
    actual: &str,
    expected: ExpectedData,
    label: &str,
) -> Result<Option<String>, TestFailure> {
    match expected {
        ExpectedData::Inline(data) => Ok(Some(data)),
        ExpectedData::Ignore => Ok(None),
        ExpectedData::InFile(rel_path) => {
            let path = reference_to_absolute_path(setup, &rel_path);

            if !path.is_file() {
                Err(match generate_tentative_reference(setup, actual, label) {
                    None => TestFailure::NotFound { tried: path },
                    Some(wrote) => TestFailure::NotFoundWroteReference { tried: path, wrote },
                })
            } else {
                Ok(Some(lookup::read(&Some(path.clone())).unwrap_or_else(
                    |msg| {
                        panic!(
                            "failed to read reference file {:?}, because: {:?}",
                            path, msg
                        )
                    },
                )))
            }
        }
    }
}

fn generate_tentative_reference(setup: &TestSpec, actual: &str, label: &str) -> Option<PathBuf> {
    if !setup.generate_tentatives {
        return None;
    }

    let file_tentative = tentative_file_path(&add_extension(&setup.references, label));

    File::create(&file_tentative)
        .and_then(|mut file| file.write_all(actual.as_bytes()))
        .ok();

    Some(file_tentative)
}

fn assert_changeset(
    setup: &TestSpec,
    label: &str,
    reference: &str,
    actual: &str,
) -> Result<(), TestFailure> {
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

        Err(match generate_tentative_reference(setup, actual, label) {
            None => TestFailure::DiffMismatch,
            Some(wrote) => TestFailure::DiffMismatchWroteReference { wrote },
        })
    } else {
        Ok(())
    }
}

fn assert_output(actual: &Output, expected: ReferenceData, setup: &TestSpec) {
    let stdout_result = {
        let stdout = &String::from_utf8_lossy(&actual.stdout);
        load_reference(setup, &stdout, expected.stdout, "stdout").and_then(|expected| {
            if let Some(reference) = expected {
                assert_changeset(setup, "stdout", &reference, &stdout)
            } else {
                Ok(())
            }
        })
    };

    let stderr_result = {
        let stderr = normalize_stderr(&String::from_utf8_lossy(&actual.stderr));
        load_reference(setup, &stderr, expected.stderr, "stderr").and_then(|expected| {
            if let Some(reference) = expected {
                assert_changeset(setup, "stderr", &reference, &stderr)
            } else {
                Ok(())
            }
        })
    };

    let exitcode_result = {
        let exitcode = actual
            .status
            .code()
            .map(|code| code.to_string())
            .unwrap_or_else(|| "terminated by signal or crashed.".to_string());

        load_reference(setup, &exitcode, expected.exitcode, "exitcode").and_then(|expected| {
            if let Some(reference) = expected {
                assert_changeset(setup, "exitcode", &reference.trim(), &exitcode)
            } else {
                Ok(())
            }
        })
    };

    match (stderr_result, stdout_result, exitcode_result) {
        (Ok(_), Ok(_), Ok(_)) => (),
        (err, out, code) => panic!(
            "TEST FAILED:\ntest case = {},\nstderr    = {},\nstdout    = {},\nexit code = {}\n",
            setup.input.display(),
            result_to_string(err),
            result_to_string(out),
            result_to_string(code)
        ),
    }
}

fn load_test_data<
    TestMetadata: IntoReferenceData + FromReferencesPath<TestMetadata> + DeserializeOwned + Clone,
>(
    spec: &TestSpec,
) -> (PathBuf, TestData<TestMetadata>) {
    let test_data = lookup::get_files::<TestMetadata>(&spec.input, &spec.references)
        .unwrap_or_else(|msg| {
            panic!(
                "Failed to load test case {:?}, because {:?}",
                spec.input, msg
            )
        });

    let input_without_yaml_path = match test_data.input {
        InputData::WasStripped(ref mj_str) => {
            let path = add_extension(&spec.input, "stripped");
            lookup::write(&Some(path.clone()), mj_str).expect(
                "Failed to write yaml front matter stripped \
                 file to disk (required for input to the compiler under test)",
            );
            path
        }
        InputData::NotStripped(ref mj_path, ref _mj_str) => mj_path.clone(),
        InputData::NotLoaded(ref mj_path) => mj_path.clone(),
    };

    (input_without_yaml_path, test_data)
}

#[allow(dead_code)]
pub fn assert_compiler_phase<
    TestMetadata: IntoReferenceData + FromReferencesPath<TestMetadata> + DeserializeOwned + Clone,
>(
    phase: CompilerCall,
    spec: &TestSpec,
) -> TestData<TestMetadata> {
    let (input_without_yaml_path, test_data) = load_test_data::<TestMetadata>(spec);
    let mut call = compiler_call(phase, &input_without_yaml_path);
    println!("Executing: {:?}", call);
    let output = call.output().expect("failed to call compiler under test");

    assert_output(
        &output,
        test_data
            .reference
            .clone()
            .into_reference_data(&spec.references),
        &spec,
    );

    test_data
}

fn result_to_string(res: Result<(), TestFailure>) -> String {
    match res {
        Ok(()) => "Ok!".to_string(),
        Err(msg) => msg.to_string(),
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
    static ref ROOT_DIR: String = {
        let mut dir = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
        // walk from the `runner-integration-tests` crate to the
        // virtual workspace one directory above
        dir.pop();
        dir.to_str().expect("test root dir path not valid utf-8").to_owned()
    };
}

/// Build and get a binary project of a workspace crate
///
/// Passing `None` will return the main binary of the method invoking
/// crate. **Not** the main binary of the virtual workspace.
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
