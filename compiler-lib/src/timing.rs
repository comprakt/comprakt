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

            for timing in &self.measurements {
                writeln!(
                    f,
                    "{: <label_width$}    {: >ms_width$}ms",
                    timing.label,
                    timing.duration().as_millis(),
                    label_width = min_label_width,
                    ms_width = 6
                )?;
            }
        }

        Ok(())
    }
}

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
