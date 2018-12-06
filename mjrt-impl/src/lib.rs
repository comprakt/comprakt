//! This is the runtime automatically linked into
//! the compiled mini java file
use libc::{c_int, c_void};
use libc_extra::unix::stdio::stdout;
use std::{
    ffi::CStr,
    io::{stdin, Read},
};

extern "C" {
    // this is the static main function of the minijava
    // program. generated by libfirm, guaranteed to exist.
    pub fn mj_main();
}

pub type MjInt = i32;

macro_rules! mjrt_runtimeexception {
    ($fn_name:ident($($var:ident : $ty:ty),*), $description:expr) => {
        #[no_mangle]
        pub extern "C" fn $fn_name($($var : $ty),*) -> ! {
            use backtrace::Backtrace;
            let bt = Backtrace::new();
            println!(concat!($description, "\n{:?}"), $($var,)* bt);
            unsafe { libc::abort() }
        }
    };
}

mjrt_runtimeexception!(mjrt_dumpstack(), "dumpstack");
mjrt_runtimeexception!(mjrt_div_by_zero(), "division by zero");
mjrt_runtimeexception!(mjrt_null_usage(), "reference is null");
mjrt_runtimeexception!(
    mjrt_array_out_of_bounds(idx: i32, len: i32),
    "array access out of bounds: {} >= {}"
);
mjrt_runtimeexception!(
    mjrt_negative_allocation(),
    "cannot allocate less than 0 bytes"
);

#[no_mangle]
pub extern "C" fn mjrt_new(size: i32) -> *mut c_void {
    if size < 0 {
        mjrt_negative_allocation()
    }
    let size = if size > 0 {
        size
    } else {
        // calloc might return NULL if called with size = 0.
        // We cannot let this happen since mjrt_new is used for
        // MiniJava `new`, which must work for 0-sized classes.
        // This means specifically that a `==` comparison of two
        // objects of the same class must return false, which does
        // not work if we return NULL here
        1
    };
    unsafe {
        let ptr = libc::calloc(size as usize, 1);
        if cfg!(feature = "checked_new") {
            if ptr == std::ptr::null_mut() {
                panic!("allocation failed");
            }
        }
        ptr
    }
}

#[no_mangle]
pub extern "C" fn mjrt_system_in_read() -> MjInt {
    let mut byte: [u8; 1] = [0];

    match stdin().read_exact(&mut byte) {
        Ok(()) => i32::from(byte[0]),
        Err(_) => -1,
    }
}

#[no_mangle]
pub extern "C" fn mjrt_system_out_println(num: MjInt) {
    unsafe {
        libc::printf(CStr::from_bytes_with_nul_unchecked(b"%d\n\0").as_ptr(), num);
    }
}

#[no_mangle]
pub extern "C" fn mjrt_system_out_write(num: MjInt) {
    unsafe {
        libc::putchar(num as c_int);
    }
}

#[no_mangle]
pub extern "C" fn mjrt_system_out_flush() {
    unsafe {
        libc::fflush(stdout as *mut libc::FILE);
    }
}

#[cfg(not(test))]
#[no_mangle]
pub extern "C" fn main() -> c_int {
    unsafe { mj_main() };
    0
}
