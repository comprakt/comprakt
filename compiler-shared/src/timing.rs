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

    pub fn stop(&self) {
        let measurement = CompletedMeasurement {
            label: self.label.clone(),
            start: self.start,
            stop: Instant::now(),
        };
        TIMINGS.lock().unwrap().measurements.push(measurement);
    }

    pub fn guard(label: &str) -> MeasurementGuard {
        MeasurementGuard(Measurement::start(label))
    }
}

pub struct MeasurementGuard(Measurement);

impl Drop for MeasurementGuard {
    fn drop(&mut self) {
        self.0.stop();
    }
}

#[macro_export]
macro_rules! timed_scope {
    ($label:expr) => {
        let measurement = ::compiler_shared::timing::Measurement::guard($label);
    };
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
    pub label: String,
    pub indent: usize,
    pub duration: Duration,
}
