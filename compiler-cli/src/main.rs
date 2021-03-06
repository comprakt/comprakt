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
#![feature(bind_by_move_pattern_guards)]
#![feature(const_str_as_bytes)]
#![feature(box_syntax)]
#![feature(repeat_generic_slice)]
#![feature(never_type)]
#![feature(nll)]
#![feature(core_intrinsics)]
#![feature(custom_attribute)]

use compiler_lib::{
    asciifile, ast, backend,
    context::Context,
    firm::{
        self,
        runtime::{self, RTLib},
    },
    lexer::{lextest, Lexer, TokenKind},
    print,
    strtab::StringTable,
    type_checking::semantics,
    OutputSpecification,
};
use env_logger;
use failure::{format_err, Error, Fail, ResultExt};
use memmap::Mmap;
use parser::Parser;
use std::{
    fs::File,
    io::{self, Write},
    path::{Path, PathBuf},
    process::{exit, Command},
};
use structopt::StructOpt;
use tempfile::{tempdir, TempDir};
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
        #[structopt(short = "-l", long = "--lint")]
        lint: bool,
        #[structopt(name = "FILE", parse(from_os_str))]
        path: PathBuf,
    },

    /// Compile to an ELF executable using libFIRM's backend.
    /// Defaults to 'a.out' in the current working directory.
    #[structopt(name = "--compile-firm")]
    CompileFirm(CompileFirmOptions),

    /// Compile to an ELF executable using the comprakt backend.
    /// Defaults to 'a.out' in the current working directory.
    #[structopt(name = "--compile")]
    Compile(CompileOptions),
}

use std::str::FromStr;

/// Command-line options for the [`CliCommand::CompileFirm`] (`--compile-firm`)
/// call.
#[derive(StructOpt, Debug, Clone)]
pub struct CompileFirmOptions {
    // backend is (implicitly) FIRM
    // gotta do that because of exercise sheet's CLI interface requirements
    #[structopt(flatten)]
    pre_backend_options: PreBackendOptions,
    #[structopt(flatten)]
    backend_options: BackendOptions,
}

/// Command-line options for the [`CliCommand::Compile`] (`--compile`) call.
#[derive(StructOpt, Debug, Clone)]
pub struct CompileOptions {
    #[structopt(long = "no-peep")]
    no_peep: bool,
    #[structopt(flatten)]
    pre_backend_options: PreBackendOptions,
    #[structopt(flatten)]
    backend_options: BackendOptions,
}

/// Common options for compiler-phases pre backend.
#[derive(StructOpt, Debug, Clone)]
pub struct PreBackendOptions {
    /// Folder to dump graphs to
    #[structopt(long = "--emit-to", default_value = ".", parse(from_os_str))]
    pub dump_folder: PathBuf,

    /// Output the matured but unoptimized firm graphs as VCG files to the
    /// dump_folder. The file name will contain the word 'high-level'.
    #[structopt(long = "--emit-firm-graph", short = "-g")]
    pub dump_firm_graph: bool,

    /// Dump class layouts in text form to dump_folder
    #[structopt(long = "--emit-class-layouts", short = "-c")]
    pub dump_class_layouts: bool,

    /// Optimization level that should be applied
    #[structopt(long = "--optimization", short = "-O", default_value = "aggressive")]
    pub opt_level: optimization_arg::Arg,

    /// A MiniJava input file
    #[structopt(name = "FILE", parse(from_os_str))]
    pub input: PathBuf,
}

impl PreBackendOptions {
    fn default_with_input(input: PathBuf) -> Self {
        PreBackendOptions {
            dump_folder: PathBuf::default(),
            dump_firm_graph: bool::default(),
            dump_class_layouts: bool::default(),
            opt_level: optimization_arg::Arg::from_str("aggressive").unwrap(), // checked in test
            input,
        }
    }
}

impl Into<firm::Options> for PreBackendOptions {
    fn into(self) -> firm::Options {
        firm::Options {
            dump_firm_graph: self.dump_firm_graph,
            dump_class_layouts: self.dump_class_layouts,
        }
    }
}

/// Common options for the backend ("lowering").
#[derive(StructOpt, Debug, Clone, Default)]
pub struct BackendOptions {
    /// Write assembly for user code (will still produce a binary, too).
    #[structopt(long = "--emit-asm", short = "-a", parse(from_os_str))]
    pub dump_assembly: Option<PathBuf>,

    /// Write binary to the given file or directory
    #[structopt(long = "--output", short = "-o", parse(from_os_str))]
    pub output: Option<PathBuf>,
}
fn main() {
    build_logger().init();

    libfirm_rs::init();

    // support compilation without any flags, as required by exercise sheet
    let single_arg = std::env::args().len() == 2;
    let no_arg_mode = std::env::args()
        .nth(1)
        .map(|p| (p.clone(), std::fs::File::open(p)))
        .map(|(p, r)| (single_arg, p, r.is_ok()));
    let cmd = if let Some((true, input, true)) = no_arg_mode {
        log::debug!("no-arg mode detected: {:?}", input);
        let input = PathBuf::from(input);
        let opts = CompileOptions {
            no_peep: false,
            pre_backend_options: PreBackendOptions::default_with_input(input),
            backend_options: BackendOptions::default(),
        };
        CliCommand::Compile(opts)
    } else {
        CliCommand::from_args()
    };

    if let Err(msg) = run_compiler(&cmd) {
        exit_with_error(&msg);
    }

    compiler_shared::timing::print();
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
        CliCommand::Check { path, lint } => cmd_check(path, *lint),
        CliCommand::CompileFirm(options) => cmd_compile_firm(options),
        CliCommand::Compile(options) => cmd_compile(options),
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

macro_rules! until_after_type_check {
    (let ($strtab:ident, $type_system:ident, $type_analysis:ident) = $input:expr) => {
        until_after_type_check!(let ($strtab, $type_system, $type_analysis) = $input, false)
    };
    (let ($strtab:ident, $type_system:ident, $type_analysis:ident) = $input:expr, $lint:expr) => {
        let input = $input;
        setup_io!(let context = input);

        let m_parser = compiler_shared::timing::Measurement::start("frontend::ast_construction");

        let mut $strtab = StringTable::new();
        let lexer = Lexer::new(&mut $strtab, &context);

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

        m_parser.stop();

        if $lint {
            let m_linter = compiler_shared::timing::Measurement::start("frontend::linter_ast");

            let mut linter = compiler_lib::linter::Linter::default();
            if let Err(lint_err) = linter.check_ast(&context, &ast) {
                log::debug!("linter error");
                context.diagnostics.error(&lint_err);
                context.diagnostics.write_statistics();
                exit(1);
            }

            m_linter.stop();
        }

        let m_typecheck = compiler_shared::timing::Measurement::start("semantics");

        let ($type_system, $type_analysis) = crate::semantics::check(&mut $strtab, &ast, &context)
            .unwrap_or_else(|()| {
                context.diagnostics.write_statistics();
                exit(1);
            });

        m_typecheck.stop();

        if $lint {
            let m_linter = compiler_shared::timing::Measurement::start("frontend::linter_semantic");

            let mut linter = compiler_lib::linter::Linter::default();
            if let Err(lint_err) = linter.check_semantic(&context, &ast, &$type_analysis) {
                log::debug!("linter error");
                context.diagnostics.error(&lint_err);
                context.diagnostics.write_statistics();
                exit(1);
            }

            m_linter.stop();
        }
    }
}

const DEFAULT_BINARY_FILENAME: &str = "a.out";

// TODO convert this to a trait?
macro_rules! compile_command_common {
    (let ($firm_ctx:ident, $bingen:ident) =
         ($pre_backend_options:expr, $backend_options:expr, $runtime:expr)) => {

        let pre_be_opts: &PreBackendOptions = $pre_backend_options;
        let be_opts: &BackendOptions = $backend_options;
        let rt: Box<dyn RTLib> = $runtime;

        until_after_type_check!(let (strtab, type_system, type_analysis) = &pre_be_opts.input);

        let binary_path = match &be_opts.output {
            Some(dir) if dir.is_dir() => dir.join(DEFAULT_BINARY_FILENAME),
            Some(file) => file.clone(),
            None => PathBuf::from(DEFAULT_BINARY_FILENAME),
        };

        let mut $bingen = BinaryGenerator::new(&binary_path)?;

        let mut $firm_ctx = compiler_lib::FirmContext::build(
            &pre_be_opts.dump_folder,
            &type_system,
            &type_analysis,
            &strtab,
            rt,
        );

        let firm_dump_opts = pre_be_opts.clone().into();
        $firm_ctx.high_level_dump(&firm_dump_opts);

        let opt_level = pre_be_opts.opt_level.clone().into();
        $firm_ctx.run_optimizations(opt_level);
    }
}

fn cmd_compile_firm(options: &CompileFirmOptions) -> Result<(), Error> {
    // --compile-firm always uses bingen, hence always use our own runtime Mjrt

    let rtlib = box runtime::Mjrt;
    compile_command_common!(let (firm_ctx, bingen) =
                            (&options.pre_backend_options, &options.backend_options, rtlib));

    let dump_asm = options
        .backend_options
        .dump_assembly
        .clone()
        .map(OutputSpecification::File);

    let res = bingen.emit_binary(&mut firm_ctx, dump_asm);
    if std::env::var("COMPRAKT_LINKER_FAILURE_KEEP_TMP").is_ok() {
        let path = bingen.stop_and_keep_temp_dir();
        eprintln!(
            "Temporary compilation directory was persisted to {:?}",
            path
        );
    }
    res
}

fn cmd_compile(options: &CompileOptions) -> Result<(), Error> {
    let rtlib: Box<dyn RTLib> = box runtime::Mjrt;

    compile_command_common!( let (firm_ctx, bingen) =
                             (&options.pre_backend_options, &options.backend_options, rtlib));

    let mut backend: Box<dyn backend::AsmBackend> = box backend::amd64::Backend {
        firm_ctx,
        no_peep: options.no_peep,
    };

    let dump_asm = options
        .backend_options
        .dump_assembly
        .clone()
        .map(OutputSpecification::File);

    let res = bingen.emit_binary(&mut *backend, dump_asm);
    if std::env::var("COMPRAKT_LINKER_FAILURE_KEEP_TMP").is_ok() {
        let path = bingen.stop_and_keep_temp_dir();
        eprintln!(
            "Temporary compilation directory was persisted to {:?}",
            path
        );
    }
    res
}

enum BinaryGeneratorState {
    PreAsmOut { asm_out_file: std::fs::File },
    PostAsmOut,
}

struct BinaryGenerator {
    temp_dir: TempDir,
    asm_out: PathBuf,
    binary_path: std::path::PathBuf,
    state: BinaryGeneratorState,
}

impl BinaryGenerator {
    pub fn new(binary_path: &Path) -> Result<BinaryGenerator, Error> {
        let temp_dir = tempdir().context(format_err!("cannot create tempdir"))?;
        let asm_out = temp_dir.path().join("a.s");
        let asm_out_file = std::fs::OpenOptions::new()
            .write(true)
            .truncate(true)
            .read(true) // for dump_asm
            .create(true)
            .open(&asm_out)?;
        Ok(BinaryGenerator {
            temp_dir,
            asm_out,
            state: BinaryGeneratorState::PreAsmOut { asm_out_file },
            binary_path: binary_path.to_owned(),
        })
    }

    pub fn asm_out(&self) -> &Path {
        &self.asm_out
    }

    pub fn stop_and_keep_temp_dir(self) -> PathBuf {
        // `into_path(.)` has the side effect "Persist the temporary directory to disk"
        self.temp_dir.into_path()
    }

    pub fn emit_binary(
        &mut self,
        backend: &mut dyn compiler_lib::backend::AsmBackend,
        dump_asm: Option<OutputSpecification>,
    ) -> Result<(), Error> {
        // move state forward
        let prev = std::mem::replace(&mut self.state, BinaryGeneratorState::PostAsmOut);
        let mut asm_out_file = match prev {
            BinaryGeneratorState::PreAsmOut { asm_out_file } => asm_out_file,
            BinaryGeneratorState::PostAsmOut => panic!("must only call emit_binary once"),
        };

        // call backend, the work is happening here
        backend
            .emit_asm(&mut asm_out_file)
            .context(format_err!("backend cannot emit assembly to tempfile"))?;

        // if requested, emit assembly by copying it from the tempfile
        let dump_asm: Option<Box<dyn std::io::Write>> = match dump_asm {
            Some(OutputSpecification::File(path)) => Some(
                box std::fs::OpenOptions::new()
                    .write(true)
                    .truncate(true)
                    .create(true)
                    .open(path)?,
            ),
            Some(OutputSpecification::Stdout) => Some(box std::io::stdout()),
            None => None,
        };
        if let Some(mut dump_asm) = dump_asm {
            // NOTE: we cannot use std::io::Tee because backend::AsmOut requires AsRawFd
            use std::io::{Seek, SeekFrom};
            asm_out_file.seek(SeekFrom::Start(0))?;
            std::io::copy(&mut asm_out_file, &mut dump_asm)
                .context(format_err!("cannot emit asm"))?;
        }

        // drop here to make sure cc sees the written file
        drop(asm_out_file);

        // get runtime library
        let runtime_path = self.temp_dir.path().join("mjrt.a");
        {
            let mut file = File::create(&runtime_path)?;
            file.write_all(mjrt::STATIC_LIB)?;
        }

        // link runtime static library with user's code
        let linker_status = Command::new("cc")
            .arg("-o")
            .arg(&self.binary_path)
            .args(mjrt::LINKER_FLAGS)
            .arg(self.asm_out())
            .arg(&runtime_path)
            .args(mjrt::LINKER_LIBS)
            .status()
            .context(CliError::LinkingFailed)?;

        if !linker_status.success() {
            Err(CliError::LinkingFailed.into())
        } else {
            Ok(())
        }
    }
}

fn cmd_check(path: &PathBuf, lint: bool) -> Result<(), Error> {
    // if the check fials, until_after_type_check exits with exit code 1
    until_after_type_check!(let (strtab, _type_system, _type_analysis) = path, lint);
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
