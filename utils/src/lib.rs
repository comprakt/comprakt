pub mod color;

use std::path::PathBuf;

#[macro_export]
macro_rules! matches {
    ($expression: expr, $( $pattern: pat )|*) => {{
        match $expression {
            $( $pattern )|* => true,
            _ => false,
        }
    }};
}

#[macro_export]
macro_rules! assert_matches {
    ($expression: expr, $( $pattern: pat )|*) => {{
        if cfg!(debug_assertions) {
            match $expression {
                $( $pattern )|* => (),
                expression => panic!(
                    r#"assertion failed: `(if let pattern = expression), {}:{}:{}`
pattern: `{}`,
expression: `{:?}`"#,
                    file!(),
                    line!(),
                    column!(),
                    stringify!($( $pattern )|*),
                    expression
                ),
            }
        }
    }};
}

#[derive(Debug, Clone)]
pub enum OutputSpecification {
    Stdout,
    File(PathBuf),
}

/// Like [`std::iter::Peekable`], but can peek further ahead (needed e.g. for
/// `SLL(k)` where `k > 1`. TODO Some of this is reimplemented directly in the
/// [`crate::asciifile::PositionedChars`], maybe that
/// code should use this instead.
pub struct MultiPeekable<I>
where
    I: Iterator,
{
    iter: I,
    buffer: Vec<I::Item>,
}

impl<I> MultiPeekable<I>
where
    I: Iterator,
{
    pub fn new(iter: I) -> Self {
        MultiPeekable {
            iter,
            buffer: Vec::new(),
        }
    }

    pub fn peek(&mut self) -> Option<&I::Item> {
        self.peek_multiple(1).get(0)
    }

    /// Peek the next `n` items. Returned slice might be shorter than `n`,
    /// if iterator ended.
    pub fn peek_multiple(&mut self, n: usize) -> &[I::Item] {
        for _ in self.buffer.len()..n {
            match self.iter.next() {
                Some(item) => self.buffer.push(item),
                None => break,
            }
        }

        &self.buffer[0..self.buffer.len().min(n)]
    }
}

impl<I> Iterator for MultiPeekable<I>
where
    I: Iterator,
{
    type Item = I::Item;

    fn next(&mut self) -> Option<Self::Item> {
        self.peek()?;
        Some(self.buffer.remove(0))
    }
}
