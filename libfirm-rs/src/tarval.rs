use super::*;
use std::ptr;

// TODO move together with modes once we have a place & abstraction for them
pub fn mode_name(m: mode::Type) -> &'static CStr {
    unsafe { CStr::from_ptr(get_mode_name(m)) }
}

#[derive(Clone, Copy, From, Into)]
pub struct Tarval(*mut ir_tarval);

impl Into<*const ir_tarval> for Tarval {
    fn into(self) -> *const ir_tarval {
        let x: *mut ir_tarval = self.into();
        x as *const _
    }
}

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
    pub fn is_bool_true(self) -> bool {
        ptr::eq(self.0, unsafe { tarval_b_true })
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

    #[inline]
    pub fn cmp(self, rel: ir_relation::Type, other: Self) -> Self {
        let actual_rel = unsafe { tarval_cmp(self.into(), other.into()) };

        if actual_rel == ir_relation::False {
            Tarval::bad()
        } else if rel & actual_rel != 0 {
            Tarval::bool_true()
        } else {
            Tarval::bool_false()
        }
    }
}

impl PartialEq for Tarval {
    // TODO write tests for this
    fn eq(&self, o: &Tarval) -> bool {
        if !self.is_constant() || !o.is_constant() {
            self.mode() == o.mode()
        } else if self.mode() != o.mode() {
            false
        } else if self.mode() == unsafe { mode::b } {
            ptr::eq(self.0, o.0)
        } else if self.is_long() && o.is_long() {
            // numeric comparison required
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
impl_binop_on_tarval!(Div, div, tarval_div);
impl_binop_on_tarval!(BitXor, bitxor, tarval_eor);
impl_binop_on_tarval!(Rem, rem, tarval_mod);

impl std::ops::Neg for Tarval {
    type Output = Tarval;

    fn neg(self) -> Tarval {
        unsafe { tarval_neg(self.0) }.into()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    /// API assertion: bitwise AND on ir_relation means logical inclusion
    /// I.e. `a & b == 0` means `a => b`
    #[test]
    fn tarval_cmp_behavior() {
        init();
        let one = Tarval::mj_int(1);
        let two = Tarval::mj_int(2);

        unsafe {
            let x = bindings::tarval_cmp(one.into(), two.into());
            assert_ne!(x & ir_relation::Less, 0);
            assert_ne!(x & ir_relation::LessEqual, 0);
            assert_eq!(x & ir_relation::Greater, 0);
        }
    }

    /// API assertion: tarval comparision with bad input results in
    /// `ir_relation::False`
    #[test]
    fn tarval_cmp_behavior_bad() {
        init();
        let bad = Tarval::bad();
        let not_bad = Tarval::mj_int(2);

        unsafe {
            assert_eq!(
                bindings::tarval_cmp(bad.into(), not_bad.into()),
                ir_relation::False
            );

            assert_eq!(
                bindings::tarval_cmp(not_bad.into(), bad.into()),
                ir_relation::False
            );
        }
    }

    /// API assertion: mode_b tarvals (`true` and `false`) are always
    ///identical (can be compered using ptr-eq)
    #[test]
    fn tarval_b_idents() {
        init();
        let tv_true = Tarval::bool_true();
        let tv_false = Tarval::bool_false();

        unsafe {
            assert_eq!(tv_true.0, bindings::tarval_b_true);
            assert_eq!(tv_false.0, bindings::tarval_b_false);
        }

        let another_tv_true = Tarval::bool_true();
        let another_tv_false = Tarval::bool_false();

        assert_eq!(tv_true.0, another_tv_true.0);
        assert_eq!(tv_false.0, another_tv_false.0);
    }
}
