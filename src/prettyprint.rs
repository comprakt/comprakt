use crate::{ast, context};
use failure::Error;

pub fn prettyprint<'f>(
    program: &ast::AST<'f>,
    _context: &context::Context<'f>,
) -> Result<(), Error> {
    println!("way to go:\n{:#?}", program);
    Ok(())
}
