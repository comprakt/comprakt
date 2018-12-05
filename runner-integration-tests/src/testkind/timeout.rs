use crate::*;

pub const DEFAULT_TIMEOUT_SECONDS: u64 = 3;

// TODO: create own reference data type implementing IntoReferenceData
// and FromReferencesPath
pub fn exec_timeout_test(input: PathBuf) {
    use wait_timeout::ChildExt;
    let binary_path = input.with_extension("out");

    assert_compiler_phase::<OptionalReferenceData>(
        CompilerCall::RawCompiler(CompilerPhase::Binary {
            output: binary_path.clone(),
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

    let timeout = Duration::from_secs(DEFAULT_TIMEOUT_SECONDS);
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
