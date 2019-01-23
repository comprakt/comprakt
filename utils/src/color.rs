//! Helper for writing colors and font styles to the terminal.
//!
//! It primarily ensures that you do not forget to reset the terminal colors
//! and styles after writing through a drop(.) guard/destructor.
//!
//! Calls to functions should pass the raw writer, each function should create
//! its own ColorOutput object that is dropped on return.
//!
//! This may result in unnecessary/suboptimal color controls in the output.
//!
//! NOT thread-safe.
use termcolor::{Color, ColorSpec, WriteColor};

pub struct ColorOutput<'a> {
    writer: &'a mut dyn WriteColor,
    spec: ColorSpec,
}

impl<'a> ColorOutput<'a> {
    pub fn new(writer: &'a mut dyn WriteColor) -> Self {
        writer.reset().ok();

        Self {
            writer,
            spec: ColorSpec::new(),
        }
    }

    pub fn set_color(&mut self, color: Option<Color>) {
        // ignore coloring failures using ok()
        self.spec.set_fg(color);
        self.writer.set_color(&self.spec).ok();
    }

    pub fn set_bold(&mut self, yes: bool) {
        // ignore coloring failures using ok()
        self.spec.set_bold(yes);
        self.writer.set_color(&self.spec).ok();
    }

    pub fn writer(&mut self) -> &mut dyn WriteColor {
        self.writer
    }
}

impl<'a> Drop for ColorOutput<'a> {
    fn drop(&mut self) {
        // ignore coloring failures using ok()
        self.writer.reset().ok();
    }
}
