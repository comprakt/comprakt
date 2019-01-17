//! Executes all mjtests in the /exec/big folder.
use compiler_lib::timing::{AsciiDisp, Benchmark, CompilerMeasurements};
use runner_integration_tests::{compiler_call, CompilerCall, CompilerPhase};
use std::{
    ffi::OsStr,
    fs::{self, File},
    io::BufReader,
    path::PathBuf,
};

fn test_folder() -> PathBuf {
    PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("../mjtest-rs/tests")
}

#[derive(Debug, Clone)]
struct BigTest {
    minijava: PathBuf,
    stdin: Option<PathBuf>,
}

fn big_tests() -> Vec<BigTest> {
    let dirpath = test_folder().join("exec/big");
    log::info!("test directory is {}", dirpath.display());
    let dirlisting = fs::read_dir(dirpath).unwrap();

    let mut big_tests = vec![];

    for entry in dirlisting {
        let path = entry.unwrap().path();
        if path.extension() == Some(OsStr::new("java")) {
            let test = BigTest {
                stdin: {
                    let mut stdin = path.clone();
                    let stem = path.file_stem().unwrap();
                    // remove extension
                    stdin.pop();
                    stdin.push(stem);
                    // set new extension
                    stdin.set_extension("inputc");
                    log::debug!("looking for stdin file at {}", stdin.display());
                    if stdin.is_file() {
                        Some(stdin)
                    } else {
                        None
                    }
                },
                minijava: path,
            };

            log::debug!("Found test: {:?}", test);

            big_tests.push(test);
        }
    }

    big_tests
}

fn profile_compiler(test: &BigTest) -> CompilerMeasurements {
    let mut cmd = compiler_call(
        CompilerCall::RawCompiler(CompilerPhase::Binary {
            // TODO: use temp dir, don't trash
            output: test.minijava.with_extension("out"),
            assembly: None,
            optimizations: compiler_lib::optimization::Level::Aggressive,
        }),
        &test.minijava,
    );

    let measurement_path = "measurement.json";

    cmd.env("MEASURE_JSON", measurement_path);

    // TODO: run and benchmark the binary
    //if let Some(stdin_path) = test.stdin {
    //cmd.stdin(Stdio::piped());
    //let mut stdin_reader = File::open(&stdin_path).expect("failed to open stdin
    // file"); io::copy(&mut stdin_reader, stdin)
    //.expect("failed to write to stdin of binary");
    //}

    log::debug!("calling compiler as: {:?}", cmd);
    cmd.status().expect("failed to compile test");

    let stats_file = File::open(measurement_path).unwrap();
    let stats_reader = BufReader::new(stats_file);
    let profile = serde_json::from_reader(stats_reader).unwrap();
    log::debug!("Stats:\n{}", AsciiDisp(&profile));
    profile
}

fn main() {
    env_logger::init();

    let times = 2;

    for big_test in &big_tests() {
        let mut bench = Benchmark::new();
        for _ in 0..times {
            let timings = profile_compiler(big_test);
            bench.add(&timings);
        }

        let title = format!(
            "BENCHMARK {}",
            big_test.minijava.file_stem().unwrap().to_string_lossy()
        );

        println!("{}\n{}\n", title, "=".repeat(title.len()));
        println!("{}\n", bench);
    }
}
