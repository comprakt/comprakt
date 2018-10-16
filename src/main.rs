#![warn(rust_2018_idioms)]

use structopt::StructOpt;

use std::path::PathBuf;

#[derive(StructOpt)]
#[structopt(name = "comprakt")]
struct Opt {
    #[structopt(long = "echo")]
    echo: bool,
    #[structopt(name = "FILE", parse(from_os_str))]
    file: PathBuf,
}

use std::{fs::File, io};

fn main() {
    let opt = Opt::from_args();

    if opt.echo {
        let mut f = File::open(&opt.file)
            .unwrap_or_else(|e| panic!("could not open file {:?}: {}", &opt.file, e));
        let mut stdout = io::stdout();
        io::copy(&mut f, &mut stdout).expect("could not read from input or write to stdout");
        return;
    }
}
