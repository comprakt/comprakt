use libfirm_rs_bindings as bindings;
use std::ffi::CStr;

#[derive(Clone, Copy, PartialEq, Eq)]
pub struct Mode(bindings::mode::Type);

macro_rules! mode {
    ($name:ident, $bindings_name:ident) => {
        #[allow(non_snake_case)]
        pub fn $name() -> Mode {
            unsafe { Mode::from_libfirm(bindings::$bindings_name) }
        }
    };
}

impl Mode {
    pub fn from_libfirm(mode: bindings::mode::Type) -> Mode {
        Mode(mode)
    }

    mode!(Any, mode_ANY);
    mode!(BAD, mode_BAD);
    mode!(BB, mode_BB);
    mode!(Bs, mode_Bs);
    mode!(Bu, mode_Bu);
    mode!(D, mode_D);
    mode!(F, mode_F);
    mode!(Hs, mode_Hs);
    mode!(Hu, mode_Hu);
    mode!(Is, mode_Is);
    mode!(Iu, mode_Iu);
    mode!(Ls, mode_Ls);
    mode!(Lu, mode_Lu);
    mode!(M, mode_M);
    mode!(P, mode_P);
    mode!(T, mode_T);
    mode!(X, mode_X);
    mode!(b, mode_b);

    pub fn libfirm_mode(self) -> bindings::mode::Type {
        self.0
    }

    pub fn name(self) -> &'static CStr {
        unsafe { CStr::from_ptr(bindings::get_mode_name(self.0)) }
    }

    pub fn name_string(self) -> String {
        self.name().to_string_lossy().to_string()
    }

    pub fn size_bytes(self) -> u32 {
        unsafe { bindings::get_mode_size_bytes(self.0) }
    }

    pub fn is_mem(self) -> bool {
        self == Mode::M()
    }

    pub fn is_pointer(self) -> bool {
        self == Mode::P()
    }
}

use std::fmt::{self, Debug, Formatter};

impl Debug for Mode {
    fn fmt(&self, fmt: &mut Formatter) -> fmt::Result {
        write!(fmt, "Mode {}", self.name_string())
    }
}
