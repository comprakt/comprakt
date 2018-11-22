//! A command-line interface for the [`compiler_lib`]. If you are using
//! this crate programatically, you probably want to call it as follows:
//!
//! ```no_run
//! use compiler_cli::*;
//!
//! let cmd = CliCommand::from_args();
//!
//! if let Err(msg) = run_compiler(&cmd) {
//!     exit_with_error(&msg);
//! }
//! ```
#![warn(rust_2018_idioms)]
#![feature(try_from)]
#![feature(if_while_or_patterns)]
#![feature(bind_by_move_pattern_guards)]
#![feature(const_str_as_bytes)]
#![feature(box_syntax)]
#![feature(repeat_generic_slice)]
#![feature(never_type)]
#![feature(nll)]
#![feature(core_intrinsics)]
#![feature(custom_attribute)]

use compiler_lib::{
    asciifile, ast,
    context::Context,
    firm,
    lexer::{Lexer, TokenKind},
    parser::Parser,
    print::{self, lextest},
    semantics,
    strtab::StringTable,
};
use failure::{Error, Fail, ResultExt};
use memmap::Mmap;
use std::{
    fs::File,
    io::{self, Write},
    path::PathBuf,
    process::{exit, Command},
};
use structopt::StructOpt;
use tempfile::tempdir;
use termcolor::{ColorChoice, StandardStream};

/// An error generated by the cli interface of the compiler
///
/// **NOTE:** this kind of error represents an exception inside the compiler. It
/// does NOT represent lexical or semantic issues of the MiniJava source code
/// given by the user via a command line call. See
/// [`compiler_lib::diagnostics`] for errors and warnings regarding MiniJava
/// file contents.
#[derive(Debug, Fail)]
pub enum CliError {
    #[fail(display = "cannot open input file {:?}", path)]
    OpenInput { path: PathBuf },
    #[fail(display = "cannot mmap input file {:?}", path)]
    Mmap { path: PathBuf },
    // TODO: this should not be a compiler error but a diagnostics error
    #[fail(display = "cannot decode input file {:?}", path)]
    Ascii { path: PathBuf },
    #[fail(display = "cannot copy input file {:?} to stdout", input)]
    Echo { input: PathBuf },
}

/// An in memory representation of command-line flags passed to the
/// executable
#[derive(StructOpt)]
#[structopt(name = "comprakt")]
pub enum CliCommand {
    #[structopt(name = "--echo")]
    /// Writes the input file to stdout without modification
    Echo {
        #[structopt(name = "FILE", parse(from_os_str))]
        path: PathBuf,
    },
    #[structopt(name = "--lextest")]
    /// Only run the lexer stage on the input file, write
    /// recognized tokens to stdout separated by newlines
    LexerTest {
        #[structopt(name = "FILE", parse(from_os_str))]
        path: PathBuf,
    },
    /// Only run the lexer and parser stages on the input file.
    #[structopt(name = "--parsetest")]
    ParserTest {
        #[structopt(name = "FILE", parse(from_os_str))]
        path: PathBuf,
    },
    /// Output the AST as normalized MiniJava.
    #[structopt(name = "--print-ast")]
    PrintAst {
        #[structopt(name = "FILE", parse(from_os_str))]
        path: PathBuf,
    },
    /// Dump the AST in a format close to the actual internal data structure
    #[structopt(name = "--debug-dumpast")]
    DebugDumpAst {
        #[structopt(name = "FILE", parse(from_os_str))]
        path: PathBuf,
    },
    /// Analyze the input file and report errors, but don't build object files
    #[structopt(name = "--check")]
    Check {
        #[structopt(name = "FILE", parse(from_os_str))]
        path: PathBuf,
    },
    /// Output x86-assembler or the firm graph in various stages
    #[structopt(name = "--lower")]
    Lower(LoweringOptions),

    /// Output an executable
    #[structopt(name = "--compile")]
    Compile {
        /// the location of the generated executable file
        #[structopt(name = "FILE", parse(from_os_str))]
        path: PathBuf,
    },
}

/// Command-line options for the [`CliCommand::Lower`] (`--lower`) call
#[derive(StructOpt, Debug, Clone, Default)]
pub struct LoweringOptions {
    /// Output the matured unlowered firm graph as VCG file
    #[structopt(long = "--firm-graph", short = "-g", parse(from_os_str))]
    pub dump_firm_graph: Option<PathBuf>,
    /// Output the matured lowered firm graph as VCG file
    #[structopt(
        long = "--lowered-firm-graph",
        short = "-l",
        parse(from_os_str)
    )]
    pub dump_lowered_firm_graph: Option<PathBuf>,
    /// Write generated assembler code to the given file.
    #[structopt(long = "--assembler", short = "-a", parse(from_os_str))]
    pub dump_assembler: Option<PathBuf>,
}

impl Into<firm::Options> for LoweringOptions {
    fn into(self) -> firm::Options {
        firm::Options {
            dump_assembler: self.dump_assembler,
            dump_lowered_firm_graph: self.dump_lowered_firm_graph,
            dump_firm_graph: self.dump_firm_graph,
        }
    }
}

fn main() {
    let cmd = CliCommand::from_args();

    if let Err(msg) = run_compiler(&cmd) {
        exit_with_error(&msg);
    }
}

/// Execute the command specified by [`CliCommand`]. This takes over the
/// process, including stdout and stderr, and might not return.
pub fn run_compiler(cmd: &CliCommand) -> Result<(), Error> {
    match cmd {
        CliCommand::Echo { path } => cmd_echo(path),
        CliCommand::LexerTest { path } => cmd_lextest(path),
        CliCommand::ParserTest { path } => cmd_parsetest(path),
        CliCommand::PrintAst { path } => cmd_printast(path, &print::pretty::print),
        CliCommand::DebugDumpAst { path } => cmd_printast(path, &print::structure::print),
        CliCommand::Check { path } => cmd_check(path),
        CliCommand::Lower(options) => cmd_lower(&options.clone().into()),
        CliCommand::Compile { path } => cmd_compile(path),
    }
}

/// Print an error in a format intended for end users and terminate
/// the program.
pub fn exit_with_error(err: &Error) -> ! {
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

fn cmd_compile(path: &PathBuf) -> Result<(), Error> {
    let temp_dir = tempdir()?;
    let out_dir = temp_dir.path().to_path_buf();
    let runtime = runtime_rs::SOURCE_CODE;
    let runtime_path = out_dir.join("runtime.rs");
    let out_name = path.file_stem().unwrap();

    std::fs::copy("running_sum.S", out_dir.join("a.s"))?;

    {
        let mut file = File::create(&runtime_path)?;
        file.write_all(runtime)?;
    }

    Command::new("as")
        .arg("-o")
        .arg(&out_dir.join("a.o"))
        .arg(&out_dir.join("a.s"))
        .status()
        .context("assembling using 'as' failed")?;

    Command::new("ar")
        .arg("-crus")
        .arg(&(out_dir.join("liba.a")))
        .arg(&(out_dir.join("a.o")))
        .status()
        .context("packing using 'ar' failed")?;

    Command::new("rustc")
        .arg("-L")
        .arg(format!("{}", out_dir.display()))
        .arg("-l")
        .arg("static=a")
        .arg(&runtime_path)
        .arg("-o")
        .arg(&out_name)
        .status()?;

    //let opts = LoweringOptions::default().into();
    //unsafe { firm::build(&opts) };
    //cc::Build::new().file("running_sum.S").compile(path);
    Ok(())
}

fn cmd_lower(opts: &firm::Options) -> Result<(), Error> {
    unsafe { firm::build(opts) };
    Ok(())
}

fn cmd_echo(path: &PathBuf) -> Result<(), Error> {
    let mut f = File::open(&path).context(CliError::OpenInput { path: path.clone() })?;

    let mut stdout = io::stdout();
    io::copy(&mut f, &mut stdout).context(CliError::Echo {
        input: path.clone(),
    })?;

    Ok(())
}

macro_rules! setup_io {
    (let $context:ident = $path:expr) => {
        let path: &PathBuf = $path;
        let file = File::open(&path).context(CliError::OpenInput { path: path.clone() })?;
        let mmres = unsafe { Mmap::map(&file) };
        const EMPTY: [u8; 0] = [0; 0];
        let bytes: &[u8] = match mmres {
            Ok(ref m) => m,
            Err(e) if e.kind() == io::ErrorKind::InvalidInput => {
                // Linux returns EINVAL on file size 0, but let's be sure
                let file_size = file
                    .metadata()
                    .map(|m| m.len())
                    .context("could not get file metadata while interpreting mmap error")?;
                if file_size == 0 {
                    &EMPTY
                } else {
                    Err(e.context(CliError::Mmap { path: path.clone() }))?
                }
            }
            Err(e) => Err(e.context(CliError::Mmap { path: path.clone() }))?,
        };

        let ascii_file =
            asciifile::AsciiFile::new(&bytes).context(CliError::Ascii { path: path.clone() })?;

        let stderr = StandardStream::stderr(ColorChoice::Auto);
        let $context = Context::new(&ascii_file, box stderr);
    };
}

fn cmd_check(path: &PathBuf) -> Result<(), Error> {
    setup_io!(let context = path);
    let mut strtab = StringTable::new();
    let lexer = Lexer::new(&mut strtab, &context);

    // adapt lexer to fail on first error
    // filter whitespace and comments
    let unforgiving_lexer = lexer.filter_map(|result| match result {
        Ok(token) => match token.data {
            TokenKind::Whitespace | TokenKind::Comment(_) => None,
            _ => Some(token),
        },
        Err(lexical_error) => {
            context.diagnostics.error(&lexical_error);
            context.diagnostics.write_statistics();
            exit(1);
        }
    });

    let mut parser = Parser::new(unforgiving_lexer);

    let ast = match parser.parse() {
        Ok(p) => p,
        Err(parser_error) => {
            context.diagnostics.error(&parser_error);
            context.diagnostics.write_statistics();
            exit(1);
        }
    };

    let check_res = crate::semantics::check(&mut strtab, &ast, &context);
    if check_res.is_err() {
        context.diagnostics.write_statistics();
        exit(1)
    }

    Ok(())
}

fn cmd_printast<P>(path: &PathBuf, printer: &P) -> Result<(), Error>
where
    P: Fn(&ast::AST<'_>, &mut dyn std::io::Write) -> Result<(), Error>,
{
    setup_io!(let context = path);
    let mut strtab = StringTable::new();
    let lexer = Lexer::new(&mut strtab, &context);

    // adapt lexer to fail on first error
    // filter whitespace and comments
    let unforgiving_lexer = lexer.filter_map(|result| match result {
        Ok(token) => match token.data {
            TokenKind::Whitespace | TokenKind::Comment(_) => None,
            _ => Some(token),
        },
        Err(lexical_error) => {
            context.diagnostics.error(&lexical_error);
            context.diagnostics.write_statistics();
            exit(1);
        }
    });

    let mut parser = Parser::new(unforgiving_lexer);

    let program = match parser.parse() {
        Ok(p) => p,
        Err(parser_error) => {
            context.diagnostics.error(&parser_error);
            context.diagnostics.write_statistics();
            exit(1);
        }
    };

    printer(&program, &mut std::io::stdout())
}

fn cmd_parsetest(path: &PathBuf) -> Result<(), Error> {
    setup_io!(let context = path);
    let mut strtab = StringTable::new();
    let lexer = Lexer::new(&mut strtab, &context);

    // adapt lexer to fail on first error
    // filter whitespace and comments
    let unforgiving_lexer = lexer.filter_map(|result| match result {
        Ok(token) => match token.data {
            TokenKind::Whitespace | TokenKind::Comment(_) => None,
            _ => Some(token),
        },
        Err(lexical_error) => {
            context.diagnostics.error(&lexical_error);
            context.diagnostics.write_statistics();
            exit(1);
        }
    });

    let mut parser = Parser::new(unforgiving_lexer);

    match parser.parse() {
        Ok(_) => Ok(()),
        Err(parser_error) => {
            context.diagnostics.error(&parser_error);
            context.diagnostics.write_statistics();
            exit(1);
        }
    }
}

fn cmd_lextest(path: &PathBuf) -> Result<(), Error> {
    setup_io!(let context = path);
    let mut strtab = StringTable::new();
    let lexer = Lexer::new(&mut strtab, &context);

    let mut stdout = io::stdout();

    for token in lexer {
        match token {
            Err(lexical_error) => context.diagnostics.error(&lexical_error),
            Ok(token) => write_token(&mut stdout, &token.data)?,
        }

        // stop compilation on first error during lexing phase
        if context.diagnostics.errored() {
            context.diagnostics.write_statistics();
            exit(1);
        }
    }

    write_eof_token(&mut stdout)?;

    Ok(())
}

fn write_token<O: io::Write>(out: &mut O, token: &TokenKind<'_>) -> Result<(), Error> {
    match token {
        TokenKind::Whitespace | TokenKind::Comment(_) => Ok(()),
        _ => {
            writeln!(out, "{}", lextest::Output::new(&token))?;
            Ok(())
        }
    }
}

fn write_eof_token<O: io::Write>(out: &mut O) -> Result<(), Error> {
    writeln!(out, "EOF")?;
    Ok(())
}
