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

use std::ops::Add;

impl Add for Tarval {
    type Output = Tarval;
    fn add(self, other: Tarval) -> Tarval {
        assert_eq_modes!(self, other);
        unsafe { tarval_add(self.0, other.0) }.into()
    }
}

// TODO other ops for Tarval

#[derive(Clone, Copy, From, Into)]
pub struct Lattice(Tarval);

// TODO impl TryFrom<Node>
pub enum Binop {
    Add,
    Phi,
}

impl Lattice {
    pub fn binop(lhs: Tarval, rhs: Tarval, op: Binop) -> Tarval {
        let unknown = Tarval::unknown().mode();
        let bad = Tarval::bad().mode();
        if lhs.mode() == unknown || rhs.mode() == unknown {
            return Tarval::unknown();
        }
        if lhs.mode() == bad || rhs.mode() == bad {
            return Tarval::bad();
        }
        assert_eq_modes!(lhs, rhs);
        match op {
            Binop::Add => lhs + rhs,
            Binop::Phi => {
                if lhs == rhs {
                    lhs
                } else {
                    Tarval::bad()
                }
            }
        }
    }
    pub fn phi<I: Iterator<Item = Tarval>>(mut vals: I) -> Tarval {
        let first = vals
            .next()
            .expect("phi must have at least one predecessor (two in fact, but we only need one)");
        vals.fold(first, |r, v| Lattice::binop(r, v, Binop::Phi))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn lattice_binop_modes_like_lecture_slides() {
        init();
        let uk = Tarval::unknown();
        let bad = Tarval::bad();
        let i = Tarval::mj_int(23);
        let j = Tarval::mj_int(42);
        let x = Lattice::binop(j, i, Binop::Add);

        assert_eq_modes!(x, i);
        assert_eq_modes!(x, j);

        let x = Lattice::binop(uk, uk, Binop::Add);
        assert_eq_modes!(x, uk);

        let x = Lattice::binop(bad, bad, Binop::Add);
        assert_eq_modes!(x, bad);

        // mixed
        let x = Lattice::binop(i, bad, Binop::Add);
        assert_eq_modes!(x, bad);

        let x = Lattice::binop(i, uk, Binop::Add);
        assert_eq_modes!(x, uk);
    }
}
