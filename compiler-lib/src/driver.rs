use crate::{
    asciifile::file::AsciiFile,
    context::Context,
    lexer::{Token, TokenKind, TokenResult},
};
use failure::Error;
use memmap::Mmap;
use std::{
    fs::File,
    io,
    path::{Path, PathBuf},
    process::exit,
};
use termcolor::{ColorChoice, StandardStream, WriteColor};

#[derive(Debug, Clone, Copy)]
pub enum CompilerPhase {
    Lexer,
    Parser,
    Semantics,
}

///
pub struct Driver<'t> {
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
    lexer_adaptor: LexerAdaptor<'t>,
    /// can examine the lexer output after the `lexer_adaptor`
    /// was applied. It cannot modify the token stream.
    lexer_probe: LexerProbe<'t>,
    ///// if true, the driver will take over the process.
    ///// This means on_compilation_done will terminate
    ///// the process with the correct exit code.
    ////take_over_process: bool,
    /// This is called at the end of compilation, indepent of
    /// success or failure.
    on_compilation_done: CompilationDoneHandler<'t>,
}

impl<'t> Default for Driver<'t> {
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

pub type LexerAdaptor<'t> = Box<dyn Fn(Driver<'t>, TokenResult<'t>) -> Option<TokenResult<'t>>>;
pub type LexerProbe<'t> = Box<dyn Fn(Driver<'t>, Token<'t>) -> ()>;
pub type CompilationDoneHandler<'t> = Box<dyn Fn(&Context<'t>) -> ()>;

impl<'t> Driver<'t> {
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

    pub fn lexer_adaptor(mut self, adaptor: LexerAdaptor<'t>) -> Self {
        self.lexer_adaptor = adaptor;
        self
    }

    pub fn lexer_probe(mut self, probe: LexerProbe<'t>) -> Self {
        self.lexer_probe = probe;
        self
    }

    pub fn compile_code<C: AsRef<[u8]>>(&mut self, code: C) -> Result<(), Error> {
        let file = AsciiFile::new(code.as_ref())?;
        Ok(())
    }

    pub fn compile<P: AsRef<Path>>(&mut self, path: P) -> Result<(), Error> {
        let mmap = AsciiFile::mmap(path)?;
        let file = AsciiFile::new((*mmap).as_ref())?;
        Ok(())
    }
}

//trait CompilerInput<'f, A: AsRef<[u8]>> {
//fn load(&self) -> Result<A, Error>;
//}

///// Allows us to pass inline source code to `Driver::compile`
//impl<'f> CompilerInput<'f> for &'f str {
//fn load(self) -> Result<AsciiFile<'f>, Error> {
//let file = AsciiFile::new(self.as_bytes())?;
//Ok(file)
//}

// Allows us to pass a Path or PathBuf to `Driver::compile`
//impl<'f, P> CompilerInput<'f, Box<dyn AsRef<[u8]>>> for P
//where
//P: AsRef<Path>,
//{
//fn load(&self) -> Result<Box<dyn AsRef<[u8]>>, Error> {
//AsciiFile::mmap(self.as_ref()).map(|a| a.as_ref())
//}

// this would lead to ascii checks being performed twice
//impl<'f> CompilerInput<'f, ()> for AsciiFile<'f> {
//fn load(&self) -> Result<((), AsciiFile<'f>), Error> {
//Ok(((), self.clone()))
//}

/// The default lexer adaptor filters whitespace and comments.
/// Aborts compilation on the first error encountered.
pub fn default_lexer_adaptor<'t>(
    _driver: Driver<'t>,
    result: TokenResult<'t>,
) -> Option<TokenResult<'t>> {
    match result {
        Ok(token) => match token.data {
            TokenKind::Whitespace | TokenKind::Comment(_) => None,
            _ => Some(Ok(token)),
        },
        Err(lexical_error) => Some(Err(lexical_error)),
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

pub fn default_lexer_probe<'t>(_driver: Driver<'t>, _token: Token<'t>) {}

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
