//! The driver provides an easy interface to the compiler and
//! let's you customize its behaviour.
//!
//! For example, to generate a binary for a file:
//! ```
//! use compiler_lib::Driver;
//!
//! let empty_main = "class A{public static void main(String args[]){}}";
//! assert!(Driver::default().compile_code(empty_main).is_ok());
//! ```
//! A compiler that just counts the number of tokens in a file
//!
//! ```
//! use compiler_lib::Driver;
//!
//! let path = "../compiler-cli/integration-tests/parser/array_access.invalid.mj";
//! let mut count = 0;
//!
//! let Driver::default()
//!         .stop_after(CompilerPhase::Lexer)
//!         .lexer_probe(|_driver, token| {
//!             count += 1;
//!         })
//!         .compile(path)
//!         .is_ok()
//!
//! assert_eq!(count, 20);
//! ```
use crate::{
    asciifile::file::AsciiFile,
    context::Context,
    lexer::{Lexer, Token, TokenKind, TokenResult},
    parser::Parser,
    strtab::StringTable,
};
use failure::Error;
use std::{io, path::Path, process::exit};
use termcolor::{ColorChoice, StandardStream, WriteColor};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum CompilerPhase {
    Lexer,
    Parser,
    Semantics,
}

pub type CompilerResult = dyn Fn(Result<i32, (i32, Error)>) -> ();

///
pub struct Driver<'d, 'ctx, 'refs> {
    // data
    /// error output. Defaults to stderr.
    writer_err: Box<dyn WriteColor>,
    /// output. Defaults to stdout.
    writer_out: Box<dyn WriteColor>,
    /// Stops compilation after the given phase. Following
    /// phases will not be executed, `on_compilation_done`
    /// is called instead.
    stop_after: CompilerPhase,
    /// the default adaptor will
    /// - filter whitespace and comment tokens
    /// - halt the compiler on the first incorrect token
    ///   and invoke `on_compilation_done`
    lexer_adaptor: LexerAdaptor<'d, 'ctx, 'refs>,
    /// can examine the lexer output after the `lexer_adaptor`
    /// was applied. It cannot modify the token stream.
    lexer_probe: LexerProbe<'d, 'ctx, 'refs>,
    ///// if true, the driver will take over the process.
    ///// This means on_compilation_done will terminate
    ///// the process with the correct exit code.
    ////take_over_process: bool,
    /// This is called at the end of compilation, indepent of
    /// success or failure.
    on_compilation_done: CompilationDoneHandler<'ctx>,
}

impl<'d, 'ctx, 'refs> Default for Driver<'d, 'ctx, 'refs> {
    fn default() -> Self {
        Self {
            writer_err: box StandardStream::stderr(ColorChoice::Auto),
            writer_out: box StandardStream::stdout(ColorChoice::Auto),
            stop_after: CompilerPhase::Semantics,
            lexer_probe: box default_lexer_probe,
            lexer_adaptor: box default_lexer_adaptor,
            on_compilation_done: box default_on_compilation_done_handler,
        }
    }
}

pub const EXIT_CODE_INTERNAL_ERROR: i32 = 2;
pub const EXIT_CODE_INPUT_ERROR: i32 = 1;
pub const EXIT_CODE_SUCCESS: i32 = 0;

pub type LexerAdaptor<'d, 'ctx, 'refs> = Box<
    dyn Fn(&'refs mut CompilerTask<'d, 'ctx, 'refs>, &'refs Context<'ctx>, TokenResult<'ctx>)
        -> Option<Token<'ctx>>,
>;
pub type LexerProbe<'d, 'ctx, 'refs> =
    Box<dyn Fn(&'refs mut CompilerTask<'d, 'ctx, 'refs>, &'refs Context<'ctx>, Token<'ctx>) -> ()>;
pub type CompilationDoneHandler<'refs> = Box<dyn Fn(&Context<'refs>) -> ()>;

// driver is just used as a builder for compiler instance. This is necessary
// to extend the lifetime of the file and therefore all compiler output.
impl<'d, 'ctx, 'refs> Driver<'d, 'ctx, 'refs> {
    pub fn set_writer_err(mut self, writer: Box<dyn WriteColor>) -> Self {
        self.writer_err = writer;
        self
    }

    pub fn set_writer_out(mut self, writer: Box<dyn WriteColor>) -> Self {
        self.writer_out = writer;
        self
    }

    pub fn writer_err(&mut self) -> &mut dyn WriteColor {
        &mut self.writer_err
    }

    pub fn writer_out(&mut self) -> &mut dyn WriteColor {
        &mut self.writer_out
    }

    pub fn stop_after(mut self, phase: CompilerPhase) -> Self {
        self.stop_after = phase;
        self
    }

    pub fn lexer_adaptor(mut self, adaptor: LexerAdaptor<'d, 'ctx, 'refs>) -> Self {
        self.lexer_adaptor = adaptor;
        self
    }

    pub fn lexer_probe(mut self, probe: LexerProbe<'d, 'ctx, 'refs>) -> Self {
        self.lexer_probe = probe;
        self
    }

    //pub fn file<P: AsRef<Path>>(self, path: P) -> Result<CompilerTask<'d, 'ctx,
    // 'refs>, Error> { let mmap = AsciiFile::mmap(path)?;
    //let file = AsciiFile::new((*mmap).as_ref())?;

    //Ok(CompilerTask {
    //writer_err: self.writer_err,
    //writer_out: self.writer_out,
    //stop_after: self.stop_after,
    //lexer_adaptor: self.lexer_adaptor,
    //lexer_probe: self.lexer_probe,
    //on_compilation_done: self.on_compilation_done,
    //file,
    //})
    //}

    pub fn code<C: AsRef<[u8]> + 'ctx>(
        self,
        code: C,
    ) -> Result<CompilerTask<'d, 'ctx, 'refs>, Error> {
        let file = AsciiFile::new(code.as_ref())?;

        Ok(CompilerTask {
            writer_err: self.writer_err,
            writer_out: self.writer_out,
            stop_after: self.stop_after,
            lexer_adaptor: self.lexer_adaptor,
            lexer_probe: self.lexer_probe,
            on_compilation_done: self.on_compilation_done,
            file,
        })
    }
}

pub struct CompilerTask<'d, 'ctx, 'refs> {
    writer_err: Box<dyn WriteColor>,
    writer_out: Box<dyn WriteColor>,
    stop_after: CompilerPhase,
    lexer_adaptor: LexerAdaptor<'d, 'ctx, 'refs>,
    lexer_probe: LexerProbe<'d, 'ctx, 'refs>,
    on_compilation_done: CompilationDoneHandler<'ctx>,
    file: AsciiFile<'ctx>,
}

impl<'d, 'ctx, 'refs> CompilerTask<'d, 'ctx, 'refs> {
    ///
    /// Returns the exit code and maybe an internal error object.
    fn run(&mut self) -> Result<i32, i32> {
        let context = Context::new(&self.file, self.writer_out);
        let strtab = StringTable::new();
        let raw_lexer = Lexer::new(&strtab, &context);
        let mut lexer_error = None;
        let lexer = raw_lexer
            .filter_map(|token| {
                let maybe_repaired = (self.lexer_adaptor)(&mut self, &context, token);
                if let Some(msg) = maybe_repaired {
                    lexer_error = Some(msg);
                    None
                } else {
                    maybe_repaired
                }
            })
            .map(|token| {
                (self.lexer_probe)(&mut self, &context, token);
                token
            });

        if self.stop_after == CompilerPhase::Lexer {
            // pull the lexer, since the parser won't
            for _token in lexer {
                if let Some(msg) = lexer_error {
                    context.diagnostics.error(&msg);
                    (self.on_compilation_done)(&context);
                    return Err(EXIT_CODE_INPUT_ERROR);
                }
                //if let Err(lexical_error) = token {
                //(self.on_compilation_done)(&context);
                //(callback)(Err((EXIT_CODE_INPUT_ERROR, lexical_error)));
                //return;
                //}
            }

            (self.on_compilation_done)(&context);
            return Ok(EXIT_CODE_SUCCESS);
        }

        let mut parser: Parser<'_, _> = Parser::new(lexer);

        let program = match parser.parse() {
            Ok(p) => p,
            Err(parser_error) => {
                // note since the parser pulls the lexer
                // this might also be a forwarded lexer_error
                context.diagnostics.error(&parser_error);
                (self.on_compilation_done)(&context);
                return Err(EXIT_CODE_INPUT_ERROR);
            }
        };

        if self.stop_after == CompilerPhase::Parser {
            (self.on_compilation_done)(&context);
            return Ok(EXIT_CODE_SUCCESS);
        }

        (self.on_compilation_done)(&context);
        Ok(EXIT_CODE_SUCCESS)
    }
}

/// The default lexer adaptor filters whitespace and comments.
/// Aborts compilation on the first error encountered.
pub fn default_lexer_adaptor<'d, 'ctx, 'refs>(
    driver: &mut CompilerTask<'d, 'ctx, 'refs>,
    context: &Context<'ctx>,
    result: TokenResult<'ctx>,
) -> Option<Token<'ctx>> {
    match result {
        Ok(token) => match token.data {
            TokenKind::Whitespace | TokenKind::Comment(_) => None,
            _ => Some(token),
        },
        Err(lexical_error) => {
            //Some(Err(lexical_error)),
            context.diagnostics.error(&lexical_error);
            (driver.on_compilation_done)(&context);
            exit(EXIT_CODE_INPUT_ERROR);
        }
    }
}

// write out a message and exit the process with the correct
// error message.
pub fn default_on_compilation_done_handler(context: &Context<'_>) /* -> i32 */
{
    context.diagnostics.write_statistics();
    //if context.diagnostics.errored() {
    //1
    //} else {
    //0
    //}
}

pub fn default_lexer_probe<'d, 'ctx, 'refs>(
    _driver: &'refs mut CompilerTask<'d, 'ctx, 'refs>,
    _context: &'refs Context<'ctx>,
    _token: Token<'ctx>,
) {
}

/// Print an error in a format intended for end users and terminate
/// the program.
pub fn exit_process_with_error(err: &Error) -> ! {
    let mut stderr = io::stderr();
    print_error(&mut stderr, err).expect("unable to print error");
    exit(1);
}

/// Print error objects in a format intended for end users
pub fn print_error(writer: &mut dyn io::Write, err: &Error) -> Result<(), Error> {
    writeln!(writer, "error: {}", err.as_fail())?;
    for cause in err.iter_causes() {
        writeln!(writer, "caused by: {}", cause)?;
    }
    Ok(())
}
