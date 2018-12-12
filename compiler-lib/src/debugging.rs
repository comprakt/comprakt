use std::io::{self, Read, Write};

pub fn wait() {
    let mut stdin = io::stdin();
    let mut stdout = io::stdout();
    write!(stdout, "Press any key to continue...").unwrap();
    stdout.flush().unwrap();
    let _ = stdin.read(&mut [0u8]).unwrap();
    writeln!(stdout, "[Continued]").unwrap();
}
