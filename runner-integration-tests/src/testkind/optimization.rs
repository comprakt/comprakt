use crate::*;
use optimization::{self, Optimization};
use serde_derive::Deserialize;
use std::{
    fs::File,
    io::{self, Write},
    path::PathBuf,
    process::Stdio,
};

#[derive(Debug, Deserialize, Clone)]
#[serde(rename_all = "kebab-case")]
pub enum AsmComparisonOutcome {
    /// Assert that the assembly is **NOT** changed by
    /// the optimizations.
    Unchanged,
    /// Assert that the assembly is changed by
    /// the optimizations
    Change,
    /// The mini java code in this tuple should generate
    /// assembly identical to the mini java code currently
    /// tested
    IdenticalTo(ExpectedData),
}

#[derive(Debug, Deserialize, Clone)]
pub struct OptimizationTestData {
    // compiler output during compilation and optimization of the mini java file under test.
    pub compiler_optimized_stderr: Option<ExpectedData>,
    pub compiler_optimized_stdout: Option<ExpectedData>,
    pub compiler_optimized_exitcode: Option<ExpectedData>,
    // compiler output during compilation of the reference
    // binary
    pub compiler_reference_stderr: Option<ExpectedData>,
    pub compiler_reference_stdout: Option<ExpectedData>,
    pub compiler_reference_exitcode: Option<ExpectedData>,
    // stderr, stdout, stdin and exitcode of the
    // 1.) unoptimized reference binary
    // 2.) the optimized binary of the input
    //     of the test spec
    pub stderr: Option<ExpectedData>,
    pub stdout: Option<ExpectedData>,
    pub exitcode: Option<ExpectedData>,
    pub stdin: Option<ExpectedData>,

    /// optimizations that should be applied
    pub optimizations: Vec<optimization::Kind>,
    /// expected outcome of a comparison between
    /// the unoptimized and the optimized asm of
    /// the binary.
    pub expect: AsmComparisonOutcome,
}

impl FromReferencesPath<OptimizationTestData> for OptimizationTestData {
    fn from_reference_path(_base: &PathBuf) -> Self {
        Self {
            compiler_optimized_stderr: None,
            compiler_optimized_stdout: None,
            compiler_optimized_exitcode: None,
            compiler_reference_stderr: None,
            compiler_reference_stdout: None,
            compiler_reference_exitcode: None,
            stderr: None,
            stdout: None,
            exitcode: None,
            stdin: None,
            optimizations: vec![],
            expect: AsmComparisonOutcome::Change,
        }
    }
}

impl IntoReferenceData for OptimizationTestData {
    fn into_reference_data(self, base: &PathBuf) -> ReferenceData {
        self.into_optimizing_compiler_reference_data(base)
    }
}

impl OptimizationTestData {
    fn into_optimizing_compiler_reference_data(self, _base: &PathBuf) -> ReferenceData {
        ReferenceData {
            stderr: self
                .compiler_optimized_stderr
                .unwrap_or_else(|| ExpectedData::Inline("".to_owned())),
            stdout: self
                .compiler_optimized_stdout
                .unwrap_or_else(|| ExpectedData::Inline("".to_owned())),
            exitcode: self
                .compiler_optimized_exitcode
                .unwrap_or_else(|| ExpectedData::Inline("0".to_owned())),
        }
    }

    fn into_reference_compiler_reference_data(self, _base: &PathBuf) -> ReferenceData {
        ReferenceData {
            stderr: self
                .compiler_reference_stderr
                .unwrap_or_else(|| ExpectedData::Inline("".to_owned())),
            stdout: self
                .compiler_reference_stdout
                .unwrap_or_else(|| ExpectedData::Inline("".to_owned())),
            exitcode: self
                .compiler_reference_exitcode
                .unwrap_or_else(|| ExpectedData::Inline("0".to_owned())),
        }
    }

    fn into_binary_reference_data(self, _compiler_base: &PathBuf) -> ReferenceData {
        ReferenceData {
            stderr: self
                .stderr
                .unwrap_or_else(|| ExpectedData::Inline("".to_owned())),
            stdout: self
                .stdout
                .unwrap_or_else(|| ExpectedData::Inline("".to_owned())),
            exitcode: self
                .exitcode
                .unwrap_or_else(|| ExpectedData::Inline("0".to_owned())),
        }
    }
}

pub fn exec_optimization_test(input: PathBuf, backend: Backend) {
    // 1.) compile asm and binary for the unoptimized reference binary
    //     this is either
    //     - the file specified by 'expect: IsIdenticalTo: path"
    //     - or the same file as the optimized file, but without optimizations if
    //       'expect: Change/Unchanged'.
    // 2.) compile asm and binary of the input file with the given optimizations
    // 3.) Assert
    //     - that stdout/stderr/exitcode are the same for both binaries
    //     - if 'expect: Change', that the asm is different
    //     - if 'expect: Unchanged', that the asm is the same
    //     - if 'expect: IsIdenticalTo', that the asm is the same
    let path_binary_optimized = input.with_extension("optimized.out");
    let path_binary_reference = input.with_extension("reference.out");
    let path_asm_optimized = input.with_extension("optimized.S");
    let path_asm_reference = input.with_extension("reference.S");

    let setup = TestSpec {
        references: input.clone(),
        input,
        generate_tentatives: true,
    };

    let (input_without_yaml_path, test_data) = load_test_data::<OptimizationTestData>(&setup);

    if test_data.reference.optimizations.is_empty() {
        panic!("you MUST at least specify one optimization. none given.");
    }

    let callinfo_actual = CompilerCall::RawCompiler(CompilerPhase::Binary {
        backend,
        output: path_binary_optimized.clone(),
        assembly: Some(path_asm_optimized.clone()),
        optimizations: optimization::Level::Custom(
            test_data
                .reference
                .optimizations
                .clone()
                .iter()
                .map(|kind| Optimization {
                    kind: *kind,
                    flags: vec![],
                })
                .collect(),
        ),
    });

    let mut cmd_actual = compiler_call(callinfo_actual, &input_without_yaml_path);
    println!("Executing: {:?}", cmd_actual);
    let output_actual = cmd_actual
        .output()
        .expect("failed to call compiler under test for actual input");

    assert_output(
        &output_actual,
        test_data
            .reference
            .clone()
            .into_optimizing_compiler_reference_data(&path_binary_optimized),
        &TestSpec {
            input: path_binary_optimized.clone(),
            references: path_binary_optimized.clone(),
            generate_tentatives: true,
        },
    );

    let reference_input = match test_data.reference.expect {
        AsmComparisonOutcome::Change
        | AsmComparisonOutcome::Unchanged
        | AsmComparisonOutcome::IdenticalTo(ExpectedData::Ignore) => {
            // compare to the same file unoptimized
            input_without_yaml_path.clone()
        }
        AsmComparisonOutcome::IdenticalTo(ExpectedData::Inline(ref mj_str)) => {
            // reference mini java given as string in yaml front matter
            let path = add_extension(&setup.input, "reference");
            write(&Some(path.clone()), mj_str).expect(
                "Failed to write reference mini java \
                 file to disk (required for input to the compiler under test)",
            );
            path
        }
        AsmComparisonOutcome::IdenticalTo(ExpectedData::InFile(ref mj_rel_path)) => {
            // reference mini java given as file path in yaml front matter
            reference_to_absolute_path(&setup, mj_rel_path)
        }
    };

    let callinfo_reference = CompilerCall::RawCompiler(CompilerPhase::Binary {
        backend,
        output: path_binary_reference.clone(),
        assembly: Some(path_asm_reference.clone()),
        optimizations: optimization::Level::None,
    });

    let mut cmd_reference = compiler_call(callinfo_reference, &reference_input);
    println!("Executing: {:?}", cmd_reference);
    let output_reference = cmd_reference
        .output()
        .expect("failed to call compiler under test for reference input");

    assert_output(
        &output_reference,
        test_data
            .reference
            .clone()
            .into_reference_compiler_reference_data(&path_binary_reference),
        &TestSpec {
            input: path_binary_reference.clone(),
            references: path_binary_reference.clone(),
            generate_tentatives: true,
        },
    );

    // reaching this line means the compiler assertions were correct
    assert_binary(
        &path_binary_optimized,
        &test_data.reference.stdin,
        &setup,
        test_data
            .reference
            .clone()
            .into_binary_reference_data(&path_binary_optimized),
    );
    assert_binary(
        &path_binary_reference,
        &test_data.reference.stdin,
        &setup,
        test_data
            .reference
            .clone()
            .into_binary_reference_data(&path_binary_reference),
    );

    let asm_optimized = read(&Some(path_asm_optimized.clone())).unwrap();
    let asm_reference = read(&Some(path_asm_reference.clone())).unwrap();
    let normalized_optimized_asm = normalize_asm(&asm_optimized);
    let normalized_reference_asm = normalize_asm(&asm_reference);

    write(
        &Some(add_extension(&path_asm_optimized, "normalized")),
        &normalized_optimized_asm,
    )
    .unwrap();
    write(
        &Some(add_extension(&path_asm_reference, "normalized")),
        &normalized_reference_asm,
    )
    .unwrap();

    match test_data.reference.expect {
        AsmComparisonOutcome::Change => {
            if normalized_reference_asm == normalized_optimized_asm {
                panic!(
                    "asserted assembly to NOT be identical to the reference. \
                     But they are the same."
                );
            }
        }
        AsmComparisonOutcome::IdenticalTo(ExpectedData::Ignore) => {}
        AsmComparisonOutcome::Unchanged | AsmComparisonOutcome::IdenticalTo(_) => assert_changeset(
            &TestSpec {
                input: path_asm_optimized,
                references: path_asm_reference,
                generate_tentatives: false,
            },
            "asm",
            &normalized_reference_asm,
            &normalized_optimized_asm,
        )
        .unwrap_or_else(|msg| match test_data.reference.expect {
            AsmComparisonOutcome::Unchanged => {
                panic!("{}. expected asm to be unchanged.", msg.to_string())
            }
            _ => panic!(
                "{}. expected asm to be identical to reference.",
                msg.to_string()
            ),
        }),
    };
}

fn strip_comments(s: &str) -> String {
    let regex = regex::Regex::new("/\\*.*?\\*/").unwrap();
    regex.replace_all(s, "").to_string()
}

// TODO: not sure if this is actually necessary
fn sort_blocks(s: &str) -> String {
    let mut blocks = s
        .split("# -- Begin  ")
        .map(|block| block.trim())
        .collect::<Vec<&str>>();
    blocks.sort();

    format!("# -- Begin {}", blocks.join("\n# -- Begin  "))
}

// TODO: this could also be done in strip_comments
fn remove_trailing_whitespace(s: &str) -> String {
    let mut lines: Vec<&str> = vec![];
    for line in s.lines() {
        let trimmed = line.trim_end();
        if !trimmed.is_empty() {
            lines.push(trimmed);
        }
    }

    lines.join("\n")
}

fn normalize_asm(asm: &str) -> String {
    let no_comments = strip_comments(asm);
    let no_trailing = remove_trailing_whitespace(&no_comments);
    sort_blocks(&no_trailing)
}

fn assert_binary(
    binary_path: &PathBuf,
    stdin: &Option<ExpectedData>,
    setup: &TestSpec,
    references: ReferenceData,
) {
    let output = run_binary(binary_path, stdin, &setup);

    assert_output(
        &output,
        references,
        &TestSpec {
            input: binary_path.clone(),
            references: binary_path.clone(),
            generate_tentatives: true,
        },
    );
}

fn run_binary(binary_path: &PathBuf, stdin: &Option<ExpectedData>, setup: &TestSpec) -> Output {
    let mut cmd = std::process::Command::new(&binary_path);

    let mut child = cmd
        .stdin(Stdio::piped())
        .stdout(Stdio::piped())
        .stderr(Stdio::piped())
        .spawn()
        .expect("failed to invoke generated binary");

    if let Some(ref stdin_data) = stdin {
        match stdin_data {
            ExpectedData::Ignore => {}
            ExpectedData::Inline(stdin_str) => {
                let stdin = child.stdin.as_mut().expect("Failed to open stdin");
                stdin
                    .write_all(stdin_str.as_bytes())
                    .expect("Failed to write to stdin of generated binary");
            }
            ExpectedData::InFile(rel_path) => {
                let stdin = child.stdin.as_mut().expect("Failed to open stdin");
                let stdin_path = reference_to_absolute_path(&setup, &rel_path);
                let mut stdin_reader = File::open(&stdin_path).expect("failed to open stdin file");
                io::copy(&mut stdin_reader, stdin)
                    .expect("failed to write to stdin of generated binary");
            }
        }
    }

    child
        .wait_with_output()
        .expect("failed to invoke generated binary")
}
