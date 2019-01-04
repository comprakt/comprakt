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
#![warn(clippy::print_stdout)]
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
    OutputSpecification,
};
use env_logger;
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

mod optimization_arg;

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
    // TODO: this whole linking shenanigans should be part of compiler-lib
    #[fail(display = "failed to link runtime using `cc`")]
    LinkingFailed,
}

/// Cli interface for the comprakt MiniJava compiler library
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
    /// Output x86-assembler and optionally the firm graph in various stages.
    /// Defaults to writing to standard out.
    /// If environment variable EMIT_ASM_MOLKI is set, molki assembly
    /// is emitted instead.
    #[structopt(name = "--emit-asm")]
    EmitAsm(AsmLoweringOptions),

    /// Output an executable. Defaults to 'a.out' in the current working
    /// directory.
    /// Environment variable EMIT_ASM_MOLKI must not be set.
    #[structopt(name = "--compile-firm")]
    CompileFirm(BinaryLoweringOptions),
}

/// Command-line options for the [`CliCommand::EmitAsm`]  (`--emit-asm`) call.
#[derive(StructOpt, Debug, Clone, Default)]
pub struct AsmLoweringOptions {
    /// Folder to dump graphs to
    #[structopt(long = "--emit-to", default_value = ".", parse(from_os_str))]
    pub dump_folder: PathBuf,

    /// Output the matured unlowered firm graphs as VCG files to the dump_folder
    #[structopt(long = "--emit-firm-graph", short = "-g")]
    pub dump_firm_graph: bool,

    /// Dump class layouts in text form to dump_folder
    #[structopt(long = "--emit-class-layouts", short = "-c")]
    pub dump_class_layouts: bool,

    /// Write primary output artifact to the given file or directory
    #[structopt(long = "--output", short = "-o", parse(from_os_str))]
    pub output: Option<PathBuf>,

    /// Optimization level that should be applied
    #[structopt(long = "--optimization", short = "-O", default_value = "Moderate")]
    pub optimizations: optimization_arg::Arg,

    /// A MiniJava input file
    #[structopt(name = "FILE", parse(from_os_str))]
    pub path: PathBuf,
}

impl Into<firm::Options> for AsmLoweringOptions {
    fn into(self) -> firm::Options {
        firm::Options {
            dump_assembler: Some(match self.output {
                None => OutputSpecification::Stdout,
                Some(path) => OutputSpecification::File(path),
            }),
            dump_folder: self.dump_folder,
            dump_firm_graph: self.dump_firm_graph,
            dump_class_layouts: self.dump_class_layouts,
            optimizations: self.optimizations.into(),
        }
    }
}

/// Command-line options for the [`CliCommand::CompileFirm`] (`--compile-firm`)
/// call.
#[derive(StructOpt, Debug, Clone, Default)]
pub struct BinaryLoweringOptions {
    /// Folder to dump graphs to
    #[structopt(long = "--emit-to", default_value = ".", parse(from_os_str))]
    pub dump_folder: PathBuf,

    /// Output the matured unlowered firm graphs as VCG files to the dump_folder
    #[structopt(long = "--emit-firm-graph", short = "-g")]
    pub dump_firm_graph: bool,

    /// Dump class layouts in text form to dump_folder
    #[structopt(long = "--emit-class-layouts", short = "-c")]
    pub dump_class_layouts: bool,

    /// Write assembly for user code
    #[structopt(long = "--emit-asm", short = "-a", parse(from_os_str))]
    pub dump_assembly: Option<PathBuf>,

    /// Write binary to the given file or directory
    #[structopt(long = "--output", short = "-o", parse(from_os_str))]
    pub output: Option<PathBuf>,

    /// Optimization level that should be applied
    #[structopt(long = "--optimization", short = "-O", default_value = "Moderate")]
    pub optimizations: optimization_arg::Arg,

    /// A MiniJava input file
    #[structopt(name = "FILE", parse(from_os_str))]
    pub path: PathBuf,
}

impl Into<firm::Options> for BinaryLoweringOptions {
    fn into(self) -> firm::Options {
        firm::Options {
            dump_assembler: self.dump_assembly.map(OutputSpecification::File),
            dump_folder: self.dump_folder,
            dump_firm_graph: self.dump_firm_graph,
            dump_class_layouts: self.dump_class_layouts,
            optimizations: self.optimizations.into(),
        }
    }
}

fn main() {
    build_logger().init();

    let cmd = CliCommand::from_args();

    if let Err(msg) = run_compiler(&cmd) {
        exit_with_error(&msg);
    }
}

use env_logger::{
    fmt::{Color, Formatter},
    Builder,
};
fn build_logger() -> Builder {
    let mut builder = Builder::from_default_env();
    builder.format(
        |buf: &mut Formatter, record: &log::Record<'_>| -> std::io::Result<()> {
            let mut line_style = buf.style();
            line_style.set_color(Color::Black).set_intense(true);

            writeln!(
                buf,
                "[{:<5}]\t\t{}\t\t{}",
                buf.default_styled_level(record.level()),
                record.args(),
                line_style.value(format!(
                    "({}:{})",
                    record.file().unwrap_or(&"unknown"),
                    record.line().unwrap_or(0)
                ))
            )
        },
    );
    builder
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
        CliCommand::EmitAsm(options) => cmd_emit_asm(options),
        CliCommand::CompileFirm(options) => cmd_compile_firm(options),
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

const DEFAULT_BINARY_FILENAME: &str = "a.out";

fn cmd_compile_firm(options: &BinaryLoweringOptions) -> Result<(), Error> {
    let input = &options.path;
    setup_io!(let context = input);
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

    let (type_system, type_analysis) = crate::semantics::check(&mut strtab, &ast, &context)
        .unwrap_or_else(|()| {
            context.diagnostics.write_statistics();
            exit(1);
        });

    let temp_dir = tempdir()?;
    let compilation_dir = temp_dir.path().to_path_buf();

    let user_assembly = if let Some(ref path) = options.dump_assembly {
        path.clone()
    } else {
        compilation_dir.join("a.s")
    };

    let mut lowering_options: compiler_lib::firm::Options = (options.clone()).into();
    lowering_options.dump_assembler = Some(OutputSpecification::File(user_assembly.clone()));

    unsafe { firm::build(&lowering_options, &type_system, &type_analysis, &mut strtab)? };

    // get runtime library
    let runtime_path = compilation_dir.join("mjrt.a");
    {
        let mut file = File::create(&runtime_path)?;
        file.write_all(mjrt::STATIC_LIB)?;
    }

    let binary_path = match &options.output {
        Some(dir) if dir.is_dir() => dir.join(DEFAULT_BINARY_FILENAME),
        Some(file) => file.clone(),
        None => PathBuf::from(DEFAULT_BINARY_FILENAME),
    };

    // link runtime static library with user's code
    let linker_status = Command::new("cc")
        .arg("-o")
        .arg(&binary_path)
        .args(mjrt::LINKER_FLAGS)
        .arg(&user_assembly)
        .arg(&runtime_path)
        .args(mjrt::LINKER_LIBS)
        .status()
        .context(CliError::LinkingFailed)?;

    if std::env::var("COMPRAKT_LINKER_FAILURE_KEEP_TMP").is_ok() {
        // `into_path(.)` has the side effect "Persist the temporary directory to disk"
        let path = temp_dir.into_path();
        eprintln!(
            "Temporary compilation directory was persisted to {:?}",
            path
        );
    }

    if !linker_status.success() {
        Err(CliError::LinkingFailed.into())
    } else {
        Ok(())
    }
}

fn cmd_emit_asm(opts: &AsmLoweringOptions) -> Result<(), Error> {
    let input = &opts.path;
    setup_io!(let context = input);
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

    let (type_system, type_analysis) = crate::semantics::check(&mut strtab, &ast, &context)
        .unwrap_or_else(|()| {
            context.diagnostics.write_statistics();
            exit(1);
        });

    let firm_options = opts.clone().into();
    unsafe { firm::build(&firm_options, &type_system, &type_analysis, &mut strtab)? };
    Ok(())
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
