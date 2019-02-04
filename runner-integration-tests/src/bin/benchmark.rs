#![feature(duration_as_u128)]
//! Executes all mjtests in the /exec/big folder.
use compiler_cli::optimization_arg;
use compiler_shared::timing::{AsciiDisp, CompilerMeasurements, SingleMeasurement};
use humantime::format_duration;
use optimization;
use regex::Regex;
use runner_integration_tests::{compiler_call, Backend, CompilerCall, CompilerPhase};
use stats::OnlineStats;
use std::{
    collections::HashMap,
    ffi::OsStr,
    fmt,
    fs::{self, File, OpenOptions},
    io::{self, BufReader},
    path::PathBuf,
    process::Command,
    time::{Duration, SystemTime},
};
use structopt::StructOpt;

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
                    // TODO: support multiple input files
                    stdin.set_extension("0.inputc");
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

fn profile_compiler(
    test: &BigTest,
    optimizations: optimization::Level,
) -> Option<(PathBuf, CompilerMeasurements)> {
    let outpath = test.minijava.with_extension("benchmark.out");
    let mut cmd = compiler_call(
        CompilerCall::RawCompiler(CompilerPhase::Binary {
            // TODO: use temp dir, don't trash
            output: outpath.clone(),
            assembly: None,
            backend: Backend::Libfirm,
            optimizations,
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

    match cmd.status() {
        Ok(status) if status.success() => (),
        Ok(status) => {
            log::error!("compiler failed with non-zero exit code: {:?}", status);
            return None;
        }
        Err(msg) => {
            log::error!("compiler crash {:?}", msg);
            return None;
        }
    }

    let stats_file = File::open(measurement_path).unwrap();
    let stats_reader = BufReader::new(stats_file);
    let profile = serde_json::from_reader(stats_reader).unwrap();
    log::debug!("Stats:\n{}", AsciiDisp(&profile));
    Some((outpath, profile))
}

#[derive(StructOpt)]
#[structopt(name = "benchmark")]
/// Small utility to benchmark each step of the compiler pipeline
pub struct Opts {
    /// Number of invokations per test file
    #[structopt(short = "s", long = "samples", default_value = "2")]
    samples: usize,
    /// Only test filenames matching the given regex are benchmarked
    #[structopt(short = "o", long = "only", default_value = "")]
    filter: Regex,
    /// Optimization level that should be applied
    #[structopt(long = "--optimization", short = "-O", default_value = "aggressive")]
    opt_level: optimization_arg::Arg,
}

#[derive(serde_derive::Serialize, serde_derive::Deserialize)]
struct ReferenceBenchmark {
    mean: f64,
    timestamp: SystemTime,
}

#[derive(serde_derive::Serialize, serde_derive::Deserialize)]
struct ReferenceFormat {
    // TODO: in contrast to the other code in this file this does
    // not support multiple benchmarks with identical names
    measurements: HashMap<String, ReferenceBenchmark>,
}

impl ReferenceFormat {
    fn new() -> Self {
        Self {
            measurements: HashMap::new(),
        }
    }
}

fn main() {
    env_logger::init();
    let opts = Opts::from_args();

    for big_test in &big_tests() {
        if !opts.filter.is_match(&big_test.minijava.to_string_lossy()) {
            log::info!("skipping {}", big_test.minijava.display());
            continue;
        }

        let mut bench = Benchmark::new(big_test.minijava.clone());
        let mut out = None;

        for _ in 0..opts.samples {
            if let Some((outbinary, timings)) =
                profile_compiler(big_test, opts.opt_level.clone().into())
            {
                bench.add(&timings);
                out = Some(outbinary);
            }
        }

        let title = format!(
            "BENCHMARK {}",
            big_test.minijava.file_stem().unwrap().to_string_lossy()
        );

        bench.load_reference_from_disk();

        println!("{}\n{}\n", title, "=".repeat(title.len()));
        println!("{}\n", bench);

        bench.write_to_disk();

        if let (Ok(cmd_str), Some(binary_path)) = (
            if big_test.stdin.is_some() {
                std::env::var("COMPILED_PROGRAM_BENCHMARK_WITH_STDIN")
            } else {
                std::env::var("COMPILED_PROGRAM_BENCHMARK")
            },
            out,
        ) {
            let cmd_str = cmd_str.replace("BINARY_PATH", binary_path.as_path().to_str().unwrap());

            let cmd_str = if let Some(stdin_file) = &big_test.stdin {
                cmd_str.replace(
                    "INPUT_PATH",
                    &stdin_file.as_path().to_str().unwrap().to_owned(),
                )
            } else {
                cmd_str
            };

            let pieces = shell_words::split(&cmd_str).expect("invalid program benchmark command");
            let (prog, args) = pieces.split_at(1);

            let mut cmd = Command::new(&prog[0]);
            cmd.args(args);

            log::debug!("Benchmarking generated binary using: {:?}", cmd);

            match cmd.status() {
                Ok(status) if status.success() => {}
                Ok(status) => {
                    log::error!(
                        "binary benchmark failed with non-zero exit code: {:?}",
                        status
                    );
                }
                Err(msg) => {
                    log::error!("binary benchmark crash {:?}", msg);
                }
            }
        }
    }
}

#[derive(Debug, Clone)]
pub struct BenchmarkEntry {
    label: String,
    indent: usize,
    stats: OnlineStats,
}

pub struct Benchmark {
    file: PathBuf,
    measurements: Vec<BenchmarkEntry>,
    reference: Option<ReferenceFormat>,
}

impl Benchmark {
    pub fn new(file: PathBuf) -> Self {
        Self {
            measurements: Vec::new(),
            reference: None,
            file,
        }
    }

    pub fn add(&mut self, measurements: &[SingleMeasurement]) {
        if self.measurements.is_empty() {
            for measurement in measurements {
                self.measurements.push(BenchmarkEntry {
                    label: measurement.label.clone(),
                    indent: measurement.indent,
                    stats: OnlineStats::from_slice(&[measurement.duration.as_millis()]),
                });
            }
            return;
        }

        if !self.is_compatible(measurements) {
            panic!("measurements incomplete");
        }

        for (i, item) in measurements.iter().enumerate() {
            self.measurements[i].stats.add(item.duration.as_millis());
        }
    }

    fn is_compatible(&self, measurements: &[SingleMeasurement]) -> bool {
        if measurements.len() != self.measurements.len() {
            return false;
        }

        for (this, other) in self.measurements.iter().zip(measurements.iter()) {
            if this.label != other.label || this.indent != other.indent {
                return false;
            }
        }

        true
    }

    fn filename(&self) -> PathBuf {
        self.file.with_extension("benchmark.json")
    }

    fn write_to_disk(&self) {
        let mut diskformat = ReferenceFormat::new();
        let now = SystemTime::now();

        for measurement in self.measurements.iter() {
            diskformat.measurements.insert(
                measurement.label.clone(),
                ReferenceBenchmark {
                    mean: measurement.stats.mean(),
                    timestamp: now,
                },
            );
        }

        match OpenOptions::new()
            .write(true)
            .truncate(true)
            .create(true)
            .open(self.filename())
        {
            Ok(outfile) => {
                if let Err(msg) = serde_json::ser::to_writer(&outfile, &diskformat) {
                    log::debug!(
                        "could not write file for reference benchmark of {}: {:?}",
                        self.file.display(),
                        msg
                    );
                }
            }
            Err(msg) => {
                log::debug!(
                    "could not open file for reference benchmark of {}: {:?}",
                    self.file.display(),
                    msg
                );
            }
        }
    }
    fn load_reference_from_disk(&mut self) {
        if let Ok(res) = self._load_reference() {
            self.reference = Some(res);
        } else {
            log::debug!(
                "could not find reference benchmark for {}",
                self.file.display()
            );
        }
    }

    fn _load_reference(&mut self) -> io::Result<ReferenceFormat> {
        let file = File::open(self.filename())?;
        let reader = BufReader::new(file);

        // Read the JSON contents of the file as an instance of `User`.
        let u = serde_json::from_reader(reader)?;
        Ok(u)
    }
}

impl fmt::Display for Benchmark {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let min_label_width = 50;
        let now = SystemTime::now();

        for timing in &self.measurements {
            let indent = "  ".repeat(timing.indent);

            let (change, timestamp) = if let Some(previous_invocation) = &self.reference {
                if let Some(previous_result) = previous_invocation.measurements.get(&timing.label) {
                    let change = (timing.stats.mean() / previous_result.mean * 100.0) - 100.0;
                    let reference_date = now.duration_since(previous_result.timestamp).unwrap();
                    // remove some precession
                    let reference_date = Duration::from_secs(reference_date.as_secs());

                    (
                        format!("{: >+8.3}%", change),
                        format_duration(reference_date).to_string(),
                    )
                } else {
                    ("n/a".to_string(), "".to_string())
                }
            } else {
                ("".to_string(), "".to_string())
            };

            writeln!(
                f,
                "{nesting}{label: <label_width$}    \
                 {ms: >ms_width$.5} +/- {stddev: >stddev_width$.5}ms    \
                 {samples} samples    \
                 {change}     {timestamp} ago",
                label = timing.label,
                ms = timing.stats.mean(),
                stddev = timing.stats.stddev(),
                samples = timing.stats.len(),
                nesting = indent,
                label_width = min_label_width - indent.len(),
                ms_width = 20,
                stddev_width = 10,
                change = change,
                timestamp = timestamp
            )?;
        }

        Ok(())
    }
}
