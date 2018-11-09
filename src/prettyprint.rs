use crate::{ast, context};
use failure::Error;

pub fn prettyprint<'f, 'c>(
    _program: &ast::Program<'f>,
    _context: &context::Context<'c>,
) -> Result<(), Error> {
    unimplemented!()
}
