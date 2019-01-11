use super::mode::Mode;
use libfirm_rs_bindings as bindings;
use std::{ffi::CStr, ptr};

#[derive(Hash, Clone, Copy, From, Into)]
pub struct Tarval(*mut bindings::ir_tarval);

impl Into<*const bindings::ir_tarval> for Tarval {
    fn into(self) -> *const bindings::ir_tarval {
        let x: *mut bindings::ir_tarval = self.into();
        x as *const _
    }
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum TarvalKind {
    Bool(bool),
    Unknown,
    Bad,
    Long(i64),
    Other,
}

impl Tarval {
    #[inline]
    pub fn unknown() -> Tarval {
        unsafe { bindings::tarval_unknown }.into()
    }

    #[inline]
    pub fn bad() -> Tarval {
        unsafe { bindings::tarval_bad }.into()
    }

    #[inline]
    pub fn bool_val(val: bool) -> Tarval {
        unsafe {
            if val {
                bindings::tarval_b_true
            } else {
                bindings::tarval_b_false
            }
        }
        .into()
    }

    #[inline]
    pub fn mj_int(val: i64) -> Tarval {
        unsafe { bindings::new_tarval_from_long(val, bindings::mode::Is) }.into()
    }

    #[inline]
    pub fn zero(mode: Mode) -> Tarval {
        unsafe { bindings::new_tarval_from_long(0, mode.libfirm_mode()) }.into()
    }

    #[inline]
    pub fn val(val: i64, mode: Mode) -> Tarval {
        unsafe { bindings::new_tarval_from_long(val, mode.libfirm_mode()) }.into()
    }

    #[inline]
    pub fn is_bool_val(self, val: bool) -> bool {
        unsafe {
            ptr::eq(
                self.0,
                if val {
                    bindings::tarval_b_true
                } else {
                    bindings::tarval_b_false
                },
            )
        }
    }

    #[inline]
    pub fn is_long(self) -> bool {
        unsafe { bindings::tarval_is_long(self.0) != 0 }
    }

    /**
     ** Returns non-zero if the tarval is a constant (i.e.
     ** NOT a reserved tarval like bad, undef, reachable etc.)
     **/
    #[inline]
    pub fn is_constant(self) -> bool {
        unsafe { bindings::tarval_is_constant(self.0) != 0 }
    }

    #[inline]
    pub fn is_bad(self) -> bool {
        unsafe { ptr::eq(self.0, bindings::tarval_bad) }
    }

    #[inline]
    pub fn is_unknown(self) -> bool {
        unsafe { ptr::eq(self.0, bindings::tarval_unknown) }
    }

    #[inline]
    pub fn mode(self) -> Mode {
        Mode::from_libfirm(unsafe { bindings::get_tarval_mode(self.0) })
    }

    pub fn kind(self) -> TarvalKind {
        if self.is_bool_val(true) {
            TarvalKind::Bool(true)
        } else if self.is_bool_val(false) {
            TarvalKind::Bool(false)
        } else if self.is_bad() {
            TarvalKind::Bad
        } else if self.is_long() {
            TarvalKind::Long(self.get_long())
        } else if self.is_unknown() {
            TarvalKind::Unknown
        } else {
            TarvalKind::Other
        }
    }

    #[inline]
    pub fn cast(self, mode: Mode) -> Option<Tarval> {
        if self.can_be_cast_into(mode) {
            Some(unsafe { bindings::tarval_convert_to(self.0, mode.libfirm_mode()) }.into())
        } else {
            None
        }
    }

    #[inline]
    pub fn can_be_cast_into(self, mode: Mode) -> bool {
        unsafe { bindings::smaller_mode(self.mode().libfirm_mode(), mode.libfirm_mode()) != 0 }
    }

    #[inline]
    pub fn get_long(self) -> i64 {
        unsafe { bindings::get_tarval_long(self.0) }
    }

    #[inline]
    pub fn lattice_cmp(self, rel: bindings::ir_relation::Type, other: Self) -> Self {
        let actual_rel = unsafe { bindings::tarval_cmp(self.into(), other.into()) };

        if actual_rel == bindings::ir_relation::False {
            Tarval::bad()
        } else {
            Tarval::bool_val(rel & actual_rel != 0)
        }
    }

    pub fn lattice_eq(self, o: Tarval) -> bool {
        self.lattice_cmp(bindings::ir_relation::Equal, o)
            .is_bool_val(true)
    }

    pub fn join(self, o: Tarval) -> Tarval {
        use self::TarvalKind::*;
        match (self.kind(), o.kind()) {
            (Bad, _) | (_, Bad) => Tarval::bad(),
            (Unknown, _) => o,
            (_, Unknown) => self,
            _ if self.lattice_eq(o) => self,
            _ => Tarval::bad(),
        }
    }
}

impl PartialEq for Tarval {
    // TODO write tests for this
    fn eq(&self, o: &Tarval) -> bool {
        match (self.kind(), o.kind()) {
            (TarvalKind::Unknown, TarvalKind::Unknown) => true,
            (TarvalKind::Bad, TarvalKind::Bad) => true,
            _ if self.kind() == o.kind() => self.lattice_eq(*o),
            _ => false,
        }
    }
}

use std::fmt::{self, Debug, Formatter};

impl Debug for Tarval {
    fn fmt(&self, fmt: &mut Formatter) -> fmt::Result {
        unsafe {
            let mut v = [0; 1024];
            bindings::tarval_snprintf(v.as_mut_ptr(), v.len(), self.0);
            let s = CStr::from_ptr(v.as_ptr()).to_string_lossy();
            let s = s.trim_start_matches("<");
            let s = s.trim_end_matches(">");
            write!(fmt, "{{{}, mode: {}}}", s, self.mode().name_string())
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
            lhs.mode().name_string(),
            rhs.mode().name_string(),
        );
    }};
}

macro_rules! impl_binop_on_tarval {
    ($rust_trait: ident, $rust_method: ident, $firm_func: path) => {
        impl std::ops::$rust_trait for Tarval {
            type Output = Tarval;
            fn $rust_method(self, other: Tarval) -> Tarval {
                assert_eq_modes!(self, other);
                unsafe { $firm_func(self.0, other.0) }.into()
            }
        }
    };
}

impl_binop_on_tarval!(Add, add, bindings::tarval_add);
impl_binop_on_tarval!(Sub, sub, bindings::tarval_sub);
impl_binop_on_tarval!(Mul, mul, bindings::tarval_mul);
impl_binop_on_tarval!(Div, div, bindings::tarval_div);
impl_binop_on_tarval!(BitXor, bitxor, bindings::tarval_eor);
impl_binop_on_tarval!(Rem, rem, bindings::tarval_mod);

impl std::ops::Neg for Tarval {
    type Output = Tarval;

    fn neg(self) -> Tarval {
        unsafe { bindings::tarval_neg(self.0) }.into()
    }
}

#[cfg(test)]
mod tests {
    use super::{super::init, *};

    /// API assertion: bitwise AND on ir_relation means logical inclusion
    /// I.e. `a & b == 0` means `a => b`
    #[test]
    fn tarval_cmp_behavior() {
        init();
        let one = Tarval::mj_int(1);
        let two = Tarval::mj_int(2);

        unsafe {
            let x = bindings::tarval_cmp(one.into(), two.into());
            assert_ne!(x & bindings::ir_relation::Less, 0);
            assert_ne!(x & bindings::ir_relation::LessEqual, 0);
            assert_eq!(x & bindings::ir_relation::Greater, 0);
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
                bindings::ir_relation::False
            );

            assert_eq!(
                bindings::tarval_cmp(not_bad.into(), bad.into()),
                bindings::ir_relation::False
            );
        }
    }

    /// API assertion: mode_b tarvals (`true` and `false`) are always
    ///identical (can be compered using ptr-eq)
    #[test]
    fn tarval_b_idents() {
        init();
        let tv_true = Tarval::bool_val(true);
        let tv_false = Tarval::bool_val(false);

        unsafe {
            assert_eq!(tv_true.0, bindings::tarval_b_true);
            assert_eq!(tv_false.0, bindings::tarval_b_false);
        }

        let another_tv_true = Tarval::bool_val(true);
        let another_tv_false = Tarval::bool_val(false);

        assert_eq!(tv_true.0, another_tv_true.0);
        assert_eq!(tv_false.0, another_tv_false.0);
    }
}
