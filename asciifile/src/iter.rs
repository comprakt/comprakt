use crate::{Position, Span};

#[derive(Copy, Clone, Debug)]
pub struct PositionIterator<'t> {
    /// current iterator position. Always points to the next character/position
    /// to emit
    position_to_emit: Option<Position<'t>>,
}

impl<'t> Iterator for PositionIterator<'t> {
    type Item = Position<'t>;
    fn next(&mut self) -> Option<Position<'t>> {
        match self.position_to_emit {
            None => None,
            Some(position) => {
                // this is equivalent to `position.next()`, but lifetime analysis
                // fails if we use it directly since we are not allowed to assert
                // `&'t mut self` because of trait constraints.
                let next = position.clone().next_mut();

                self.position_to_emit = match next {
                    Ok(next_position) => Some(next_position),
                    Err(_unchanged_position) => None,
                };

                Some(position)
            }
        }
    }
}

impl<'t> PositionIterator<'t> {
    pub fn new(position: Option<Position<'t>>) -> Self {
        Self {
            position_to_emit: position,
        }
    }

    /// Look at the next `n` characters without advancing the iterator.
    /// If there are less than `n` characters, return `None`.
    pub fn peek_exactly(&self, n: usize) -> Option<Span> {
        match self.peek_at_most(n) {
            None => None,
            Some(ref span) if span.as_str().len() < n => None,
            span => span,
        }
    }

    /// Look at the next `n` characters without advancing the iterator.
    ///
    /// If there are less than `n` charcters left, the returned Span will be
    /// shortened to the maximal possible length. If there are no
    /// characters left, a span cannot be built, because a span must contain
    /// at least one character.
    pub fn peek_at_most(&self, n: usize) -> Option<Span<'t>> {
        debug_assert!(n >= 1);
        // spans use inclusive ranges, so we have to built one
        // from the next char and `n` characters ahead
        match self.position_to_emit {
            None => None,
            Some(span_start) => {
                // TODO: remove this unwrap()
                // unwrap is save, since we are in a branch asserting
                // that the iterator is not finished => has at least character
                // `span_start` remaining
                let span_end = self.clone().take(n).last().unwrap();
                Some(Span::new(span_start, span_end))
            }
        }
    }

    pub fn peek(&self) -> Option<Position<'t>> {
        self.position_to_emit
    }

    pub fn eof_reached(&self) -> bool {
        self.peek().is_none()
    }
}
