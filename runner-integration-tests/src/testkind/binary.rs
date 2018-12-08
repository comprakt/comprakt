use crate::*;
use serde_derive::{Deserialize, Serialize};
use std::{
    fs::File,
    io::{self, Write},
    path::PathBuf,
    process::Stdio,
};

#[derive(Debug, Deserialize, Serialize, Clone)]
pub struct BinaryTestData {
    pub compiler_stderr: Option<ExpectedData>,
    pub compiler_stdout: Option<ExpectedData>,
    pub compiler_exitcode: Option<ExpectedData>,
    pub stderr: Option<ExpectedData>,
    pub stdout: Option<ExpectedData>,
    pub exitcode: Option<ExpectedData>,
    pub stdin: Option<ExpectedData>,
}

impl FromReferencesPath<BinaryTestData> for BinaryTestData {
    fn from_reference_path(_base: &PathBuf) -> Self {
        Self {
            compiler_stderr: None,
            compiler_stdout: None,
            compiler_exitcode: None,
            stderr: None,
            stdout: None,
            exitcode: None,
            stdin: None,
        }
    }
}

impl IntoReferenceData for BinaryTestData {
    fn into_reference_data(self, base: &PathBuf) -> ReferenceData {
        self.into_compiler_reference_data(base)
    }
}

impl BinaryTestData {
    fn into_compiler_reference_data(self, _base: &PathBuf) -> ReferenceData {
        ReferenceData {
            stderr: self
                .compiler_stderr
                .unwrap_or_else(|| ExpectedData::Inline("".to_owned())),
            stdout: self
                .compiler_stdout
                .unwrap_or_else(|| ExpectedData::Inline("".to_owned())),
            exitcode: self
                .compiler_exitcode
                .unwrap_or_else(|| ExpectedData::Inline("0".to_owned())),
        }
    }

    fn into_binary_reference_data(self, compiler_base: &PathBuf) -> ReferenceData {
        let base = compiler_base.with_extension("out");

        ReferenceData {
            stderr: self
                .stderr
                .unwrap_or_else(|| default_reference_stderr(&base)),
            stdout: self
                .stdout
                .unwrap_or_else(|| default_reference_stdout(&base)),
            exitcode: self
                .exitcode
                .unwrap_or_else(|| default_reference_exitcode(&base)),
        }
    }
}

pub fn exec_binary_test(input: PathBuf) {
    let binary_path = input.with_extension("out");
    let setup = TestSpec {
        references: input.clone(),
        input,
        generate_tentatives: true,
    };

    // TODO: instead of panicing when there are no references
    // continue and generate references for the binary.
    let metadata = assert_compiler_phase::<BinaryTestData>(
        CompilerCall::RawCompiler(CompilerPhase::Binary {
            output: binary_path.clone(),
            optimizations: vec![],
            assembly: None,
        }),
        &setup,
    );

    // reaching this line means the compiler assertions were correct
    let mut cmd = std::process::Command::new(&binary_path);

    let mut child = cmd
        .stdin(Stdio::piped())
        .stdout(Stdio::piped())
        .stderr(Stdio::piped())
        .spawn()
        .expect("failed to invoke generated binary");

    if let Some(ref stdin_data) = metadata.reference.stdin {
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

    let output = child
        .wait_with_output()
        .expect("failed to invoke generated binary");

    assert_output(
        &output,
        metadata.reference.into_binary_reference_data(&binary_path),
        &TestSpec {
            input: binary_path.clone(),
            references: binary_path,
            generate_tentatives: true,
        },
    );
}
