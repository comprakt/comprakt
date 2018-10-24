#![warn(rust_2018_idioms)]

use failure::{Error, Fail, ResultExt};
use std::{fs::File, io, path::PathBuf, process::exit};
use structopt::StructOpt;

/// An error generated by the cli interface of the compiler
///
/// We apply the logic explained in [1] meaning in the current form it's hard
/// to destructure on the error type.
///
/// NOTE: this kind of error represents an exception inside the compiler. It
/// does NOT represent lexical or semantic issues of the MiniJava source code
/// given by the user via a command line call. See `Diagnostics` for errors and
/// warnings regarding MiniJava file contents.
///
/// ---
/// [1] https://rust-lang-nursery.github.io/failure/use-error.html
#[derive(Debug, Fail)]
enum CliError {
    #[fail(display = "failed to open MiniJava file {:?}", path)]
    OpenInputError { path: PathBuf },
    #[fail(display = "failed to copy input file {:?} to stdout", input)]
    EchoError { input: PathBuf },
}

#[derive(StructOpt)]
#[structopt(name = "comprakt")]
enum CliCommand {
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
}

fn main() {
    let cmd = CliCommand::from_args();

    if let Err(msg) = run_compiler(&cmd) {
        exit_with_error(&msg);
    }
}

fn run_compiler(cmd: &CliCommand) -> Result<(), Error> {
    match cmd {
        CliCommand::Echo { path } => {
            let mut f =
                File::open(&path).context(CliError::OpenInputError { path: path.clone() })?;

            let mut stdout = io::stdout();
            io::copy(&mut f, &mut stdout).context(CliError::EchoError {
                input: path.clone(),
            })?;
        }
        CliCommand::LexerTest { path } => {
            println!("Lexin' {:?}!", path);
        }
    }

    Ok(())
}

/// Print an error in a format intended for end users and terminate
/// the program.
fn exit_with_error(err: &Error) -> ! {
    let mut stderr = io::stderr();
    print_error(&mut stderr, err).expect("unable to print error");
    exit(1);
}

/// Print error objects in a format intended for end users
fn print_error(writer: &mut dyn io::Write, err: &Error) -> Result<(), Error> {
    let mut causes = err.iter_chain();

    if let Some(err_msg) = causes.next() {
        writeln!(writer, "Error: {}", err_msg)?;
    } else {
        writeln!(writer, "Unknown Error")?;
    };

    for cause in causes {
        writeln!(writer, "    caused by: {}", cause)?;
    }

    Ok(())
}
