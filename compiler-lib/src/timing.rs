//! Measures time taken by single phases of the compiler.
//!
//! This is especially useful to detect problems in optimization
//! routines.
//!
//! This is NOT an utility that should be used for benchmarking!
//! Benchmarking involves running a program multiple times with
//! warm up phases and median/avg/stddev of measurements.

use std::{
    fmt,
    fs::File,
    sync::Mutex,
    time::{Duration, Instant},
}; // TODO: more precise clock

use stats::OnlineStats;

lazy_static::lazy_static! {
    static ref TIMINGS: Mutex<Timings> = { Mutex::new(Timings { measurements: Vec::new()}) };
}

#[derive(Debug, Clone)]
pub struct Measurement {
    start: Instant,
    label: String,
}

impl Measurement {
    pub fn start(label: &str) -> Measurement {
        Self {
            start: Instant::now(),
            label: label.to_string(),
        }
    }

    pub fn stop(self) {
        let measurement = CompletedMeasurement {
            label: self.label,
            start: self.start,
            stop: Instant::now(),
        };
        TIMINGS.lock().unwrap().measurements.push(measurement);
    }
}

#[derive(Debug, Clone)]
struct CompletedMeasurement {
    start: Instant,
    stop: Instant,
    label: String,
}

impl CompletedMeasurement {
    fn duration(&self) -> Duration {
        self.stop.duration_since(self.start)
    }
}

#[derive(Debug, Clone)]
struct Timings {
    measurements: Vec<CompletedMeasurement>,
}

impl fmt::Display for Timings {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        AsciiDisp(&CompilerMeasurements::from(self.clone())).fmt(f)
    }
}

impl<'a> fmt::Display for AsciiDisp<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let min_label_width = 50;

        for timing in self.0 {
            let indent = "  ".repeat(timing.indent);

            writeln!(
                f,
                "{nesting}{: <label_width$}    {: >ms_width$}ms",
                timing.label,
                timing.duration.as_millis(),
                nesting = indent,
                label_width = min_label_width - indent.len(),
                ms_width = 6
            )?;
        }

        Ok(())
    }
}

pub fn print() {
    if std::env::var("MEASURE_STDERR").is_ok() {
        eprintln!("Performance Analysis");
        eprintln!("====================\n");

        if cfg!(feature = "debugger_gui") {
            eprintln!("Measurements not available with enabled breakpoints");
        } else {
            eprintln!("{}", TIMINGS.lock().unwrap());
        }
    }

    if let Ok(path) = std::env::var("MEASURE_JSON") {
        let file = File::create(path).unwrap();
        serde_json::to_writer(
            file,
            &CompilerMeasurements::from(TIMINGS.lock().unwrap().clone()),
        )
        .unwrap();
    }
}

// Frozen and completed measurements that can be serialized
pub type CompilerMeasurements = Vec<SingleMeasurement>;
pub struct AsciiDisp<'a>(pub &'a CompilerMeasurements);

impl From<Timings> for CompilerMeasurements {
    fn from(measurements: Timings) -> Self {
        let mut frozen = vec![];
        let mut active = vec![];

        let mut listing = measurements.measurements.clone();
        listing.sort_by(|a, b| a.start.cmp(&b.start));

        for timing in listing.into_iter() {
            active.retain(|measurement: &CompletedMeasurement| measurement.stop > timing.start);

            frozen.push(SingleMeasurement {
                label: timing.label.clone(),
                indent: active.len(),
                duration: timing.duration(),
            });

            active.push(timing);
        }

        frozen
    }
}

#[derive(Debug, Clone, serde_derive::Serialize, serde_derive::Deserialize)]
pub struct SingleMeasurement {
    label: String,
    indent: usize,
    duration: Duration,
}

//
#[derive(Debug, Clone)]
pub struct BenchmarkEntry {
    label: String,
    indent: usize,
    stats: OnlineStats,
}

pub struct Benchmark {
    measurements: Vec<BenchmarkEntry>,
}

impl Benchmark {
    pub fn new() -> Self {
        Self {
            measurements: Vec::new(),
        }
    }

    pub fn add(&mut self, measurements: &[SingleMeasurement]) {
        if self.measurements.len() == 0 {
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
}

impl fmt::Display for Benchmark {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let min_label_width = 50;

        for timing in &self.measurements {
            let indent = "  ".repeat(timing.indent);
            writeln!(
                f,
                "{nesting}{label: <label_width$}    {ms: >ms_width$.5} +/- {stddev: >stddev_width$.5}ms    {samples} samples",
                label  = timing.label,
                ms = timing.stats.mean(),
                stddev = timing.stats.stddev(),
                samples = timing.stats.len(),
                nesting = indent,
                label_width = min_label_width - indent.len(),
                ms_width = 20,
                stddev_width = 10
            )?;
        }

        Ok(())
    }
}
