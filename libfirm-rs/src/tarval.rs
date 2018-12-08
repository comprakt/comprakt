use super::*;
use libfirm_rs_bindings::*;

// TODO move together with modes once we have a place & abstraction for them
pub fn mode_name(m: mode::Type) -> &'static CStr {
    unsafe { CStr::from_ptr(get_mode_name(m)) }
}

#[derive(Clone, Copy, From, Into)]
pub struct Tarval(*mut ir_tarval);

impl Tarval {
    #[inline]
    pub fn unknown() -> Tarval {
        unsafe { tarval_unknown }.into()
    }

    #[inline]
    pub fn bad() -> Tarval {
        unsafe { tarval_bad }.into()
    }

    #[inline]
    pub fn bool_true() -> Tarval {
        unsafe { tarval_b_true }.into()
    }

    #[inline]
    pub fn bool_false() -> Tarval {
        unsafe { tarval_b_false }.into()
    }

    #[inline]
    pub fn mj_int(val: i64) -> Tarval {
        unsafe { new_tarval_from_long(val, mode::Is) }.into()
    }

    #[inline]
    pub fn mode(self) -> mode::Type {
        unsafe { get_tarval_mode(self.0) }
    }

    #[inline]
    pub fn is_constant(self) -> bool {
        unsafe { tarval_is_constant(self.0) != 0 }
    }

    #[inline]
    pub fn is_long(self) -> bool {
        unsafe { tarval_is_long(self.0) != 0 }
    }

    #[inline]
    pub fn get_long(self) -> i64 {
        unsafe { get_tarval_long(self.0) }
    }
}

impl PartialEq for Tarval {
    // TODO write tests for this
    fn eq(&self, o: &Tarval) -> bool {
        if !self.is_constant() || !o.is_constant() {
            return self.mode() == o.mode();
        }
        if self.mode() != o.mode() {
            return false;
        }
        // numeric comparison required
        if self.is_long() && o.is_long() {
            self.get_long() == o.get_long()
        } else {
            unimplemented!()
        }
    }
}

use std::fmt::{self, Debug, Formatter};

impl Debug for Tarval {
    fn fmt(&self, fmt: &mut Formatter) -> fmt::Result {
        unsafe {
            let mut v = [0; 1024];
            tarval_snprintf(v.as_mut_ptr(), v.len(), self.0);
            let s = CStr::from_ptr(v.as_ptr()).to_string_lossy();
            let s = s.trim_start_matches("<");
            let s = s.trim_end_matches(">");
            write!(
                fmt,
                "Tarval{{{}, mode: {}}}",
                s,
                mode_name(self.mode()).to_string_lossy()
            )
        }
    }
}

macro_rules! assert_eq_modes {
    ($l:expr, $r:expr) => {{
        let lhs = $l;
        let rhs = $r;
        debug_assert_eq!(
            lhs.mode(),
            rhs.mode(),
            "modes do not match: {:?} != {:?}",
            mode_name(lhs.mode()),
            mode_name(rhs.mode())
        );
    }};
}

macro_rules! impl_binop_on_tarval {
    ($rust_trait: ident, $rust_method: ident, $firm_func: ident) => {
        impl std::ops::$rust_trait for Tarval {
            type Output = Tarval;
            fn $rust_method(self, other: Tarval) -> Tarval {
                assert_eq_modes!(self, other);
                unsafe { $firm_func(self.0, other.0) }.into()
            }
        }
    };
}

impl_binop_on_tarval!(Add, add, tarval_add);
impl_binop_on_tarval!(Sub, sub, tarval_sub);
impl_binop_on_tarval!(Mul, mul, tarval_mul);
