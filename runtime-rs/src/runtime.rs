//! This is the runtime automatically linked into
//! the compiled mini java file
use std::io::{stdin, stdout, Read, Write};

extern "C" {
    // this is the static main function of the minijava
    // program. generated by libfirm, guranteed to exist.
    fn mj_main();
}

type MjInt = i32;

#[no_mangle]
extern "C" fn system_in_read() -> MjInt {
    let mut byte: [u8; 1] = [0];

    match stdin().read_exact(&mut byte) {
        Ok(()) => byte[0] as i32,
        Err(_) => -1,
    }
}

#[no_mangle]
extern "C" fn system_out_println(num: MjInt) {
    println!("{}", num);
}

#[no_mangle]
// TODO: this should write a byte. But what's a byte in MiniJava?
extern "C" fn system_out_write(num: MjInt) {
    print!("{}", num);
}

#[no_mangle]
extern "C" fn system_out_flush() {
    stdout().flush().ok();
}

fn main() {
    unsafe { mj_main() };
}
