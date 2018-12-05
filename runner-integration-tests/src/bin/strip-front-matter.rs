use failure::{Error, Fail, ResultExt};
use runner_integration_tests::{lookup, yaml::FrontMatter};
use std::{
    io::{stderr, Write},
    path::PathBuf,
    process::exit,
};
use structopt::StructOpt;

#[derive(StructOpt)]
#[structopt(name = "strip-front-matter")]
/// Small utility that strips yaml front matter from a multi-part file.
pub struct Opts {
    /// Do not error if no front matter is found
    #[structopt(long = "allow-none")]
    allow_none: bool,
    /// Input file with yaml front matter
    #[structopt(name = "FILE", parse(from_os_str))]
    input: Option<PathBuf>,
    /// Output file, defaults to stdout
    #[structopt(short = "o", long = "output", parse(from_os_str))]
    output: Option<PathBuf>,
}

#[derive(Debug, Fail)]
pub enum StripError {
    #[fail(display = "file does not contain yaml front matter")]
    NoFrontMatter,
    #[fail(display = "cannot read input file")]
    OpenInput,
    #[fail(display = "failed to write stripped file to output")]
    OutputError,
}

pub fn strip_front_matter(opts: &Opts) -> Result<(), Error> {
    let contents = lookup::read(&opts.input).context(StripError::OpenInput)?;
    let file = FrontMatter::new(&contents);

    if !file.has_front_matter() && !opts.allow_none {
        return Err(StripError::NoFrontMatter.into());
    }

    lookup::write(&opts.output, file.without_front_matter())
        .context(StripError::OutputError)
        .map_err(From::from)
}

fn main() {
    let opts = Opts::from_args();
    if let Err(msg) = strip_front_matter(&opts) {
        exit_with_error(&msg);
    }
}

/// Print an error in a format intended for end users and terminate
/// the program.
fn exit_with_error(err: &Error) -> ! {
    let mut stderr = stderr();
    print_error(&mut stderr, err).expect("unable to print error");
    exit(1);
}

/// Print error objects in a format intended for end users
fn print_error(writer: &mut dyn Write, err: &Error) -> Result<(), Error> {
    writeln!(writer, "error: {}", err.as_fail())?;
    for cause in err.iter_causes() {
        writeln!(writer, "    caused by: {}", cause)?;
    }
    Ok(())
}
