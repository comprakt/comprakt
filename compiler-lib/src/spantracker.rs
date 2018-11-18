use crate::{asciifile::Span, lexer::Token, utils::MultiPeekable};

pub struct SpanTracker<'f, I>
where
    I: Iterator<Item = Token<'f>>,
{
    lexer: MultiPeekable<I>,
    prev_span: Option<Span<'f>>,
}

#[derive(Debug)]
pub struct EOF;
type EOFResult<T> = Result<T, EOF>;

impl<'f, I> SpanTracker<'f, I>
where
    I: Iterator<Item = Token<'f>>,
{
    pub fn new(lexer: I) -> Self {
        Self {
            lexer: MultiPeekable::new(lexer),
            prev_span: None,
        }
    }

    pub fn next(&mut self) -> EOFResult<Token<'f>> {
        self.peek()?;
        let next = self.lexer.next().ok_or(EOF)?;

        self.prev_span = Some(next.span);
        Ok(next)
    }

    pub fn peek(&mut self) -> EOFResult<&Token<'f>> {
        self.peek_nth(0)
    }

    pub fn peek_nth(&mut self, n: usize) -> EOFResult<&Token<'f>> {
        let v = self.lexer.peek_multiple(n + 1);

        v.get(n).ok_or(EOF)
    }

    pub fn eof(&mut self) -> bool {
        self.peek().is_err()
    }

    pub fn prev_span(&self) -> Option<Span<'f>> {
        self.prev_span
    }

    pub fn peek_span(&mut self) -> EOFResult<Span<'f>> {
        self.peek().map(|token| token.span)
    }
}
