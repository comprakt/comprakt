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
struct Timings {
    measurements: Vec<CompletedMeasurement>,
}

impl fmt::Display for Timings {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        writeln!(f, "Performance Analysis")?;
        writeln!(f, "====================\n")?;

        if cfg!(feature = "debugger_gui") {
            writeln!(f, "Measurements not available with enabled breakpoints")?;
        } else {
            let min_label_width = 50;

            let mut active = vec![];

            let mut listing = self.measurements.clone();
            listing.sort_by(|a, b| a.start.cmp(&b.start));

            for timing in listing.into_iter() {
                active.retain(|measurement: &CompletedMeasurement| measurement.stop > timing.start);

                let indent = "  ".repeat(active.len());
                writeln!(
                    f,
                    "{nesting}{: <label_width$}    {: >ms_width$}ms",
                    timing.label,
                    timing.duration().as_millis(),
                    nesting = indent,
                    label_width = min_label_width - indent.len(),
                    ms_width = 6
                )?;

                active.push(timing);
            }
        }

        Ok(())
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

pub fn print() {
    eprintln!("{}", TIMINGS.lock().unwrap());
}
