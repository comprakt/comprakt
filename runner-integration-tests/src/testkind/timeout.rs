use compiler_lib::optimization;
use crate::*;

pub const DEFAULT_TIMEOUT_SECONDS: u64 = 3;

#[derive(Debug, Deserialize, Clone)]
pub struct Data {
    pub stderr: Option<ExpectedData>,
    pub stdout: Option<ExpectedData>,
    pub exitcode: Option<ExpectedData>,
    pub stdin: Option<ExpectedData>,
    pub timeout: Option<HumanDuration>,
    pub optimizations: Vec<optimization::Kind>,
}

#[derive(Debug, Deserialize, Clone)]
pub struct HumanDuration(#[serde(with = "serde_humantime")] Duration);

impl FromReferencesPath<Data> for Data {
    fn from_reference_path(_base: &PathBuf) -> Self {
        Self {
            stderr: None,
            stdout: None,
            stdin: None,
            exitcode: None,
            timeout: None,
            optimizations: vec![],
        }
    }
}

impl IntoReferenceData for Data {
    fn into_reference_data(self, _base: &PathBuf) -> ReferenceData {
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

pub fn exec_timeout_test(input: PathBuf) {
    use wait_timeout::ChildExt;
    let binary_path = input.with_extension("out");

    let test_data = assert_compiler_phase::<Data>(
        CompilerCall::RawCompiler(CompilerPhase::Binary {
            output: binary_path.clone(),
            optimizations: vec![],
            assembly: None,
        }),
        &TestSpec {
            references: input.clone(),
            input,
            generate_tentatives: true,
        },
    );

    // reaching this line means the compiler assertions were correct
    let mut cmd = std::process::Command::new(&binary_path);

    let mut child = cmd.spawn().expect("failed to invoke generated binary");

    let timeout = test_data
        .reference
        .timeout
        .map(|duration| duration.0)
        .unwrap_or_else(|| Duration::from_secs(DEFAULT_TIMEOUT_SECONDS));

    let _status_code = match child.wait_timeout(timeout).unwrap() {
        Some(status) => {
            panic!(
                "Expected test to timeout, but it returned \
                 early with exit code: {:?}",
                status.code()
            );
        }
        None => {
            // child hasn't exited yet, kill it
            child.kill().unwrap();
            child.wait().unwrap().code()
        }
    };
}
