//! String table with interior mutability and O(n) insert on miss
//!
//! Interior mutability achived using UnsafeCell.
//!
//! [1]: https://users.rust-lang.org/t/get-ref-to-just-inserted-hashset-element/13021

use std::{cell::UnsafeCell, collections::HashSet, fmt};

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Symbol<'f>(&'f str);

impl Symbol<'_> {
    fn as_raw(&self) -> *const str {
        self.0 as *const str
    }
}

impl PartialEq for Symbol<'_> {
    fn eq(&self, other: &Symbol<'_>) -> bool {
        self.as_raw() as *const u8 as usize == other.as_raw() as *const u8 as usize
    }
}

impl PartialEq<str> for Symbol<'_> {
    fn eq(&self, other: &str) -> bool {
        self.0 == other
    }
}

impl fmt::Display for Symbol<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.0.fmt(f)
    }
}

#[derive(Debug, Default)]
pub struct StringTable<'f> {
    entries: UnsafeCell<HashSet<&'f str>>,
}

impl<'f> StringTable<'f> {
    pub fn new() -> Self {
        StringTable::default()
    }

    pub fn intern(&self, value: &'f str) -> Symbol<'f> {
        // Entries is private, and this is the only place where it is mutated
        let entries = unsafe { &mut *self.entries.get() };
        if !entries.contains(value) {
            entries.insert(value);
        }

        // Unwrap is safe, we just inserted it
        Symbol(entries.get(value).unwrap())
    }

    #[cfg(test)]
    fn get(&self, value: &str) -> Option<Symbol<'f>> {
        let entries = unsafe { &*self.entries.get() };
        entries.get(value).map(|s| Symbol(*s))
    }
}

#[cfg(test)]
mod tests {
    //! Tests ideas are stolen to a not insignificant degree from Reiner

    use super::*;
    use std::collections::HashMap;

    macro_rules! get {
        ($tab: expr) => {
            unsafe { &mut *$tab.entries.get() }
        };
    }

    #[test]
    fn no_duplication() {
        let strtab = StringTable::new();

        let a = strtab.intern("foo");
        let b = strtab.intern("foo");
        let c = strtab.intern("foo");
        assert_eq!(1, get!(strtab).len());
        assert_eq!(a, b);
        assert_eq!(a, c);
        assert_eq!(
            a.as_raw() as *const u8 as usize,
            b.as_raw() as *const u8 as usize
        );
        assert_eq!(
            a.as_raw() as *const u8 as usize,
            c.as_raw() as *const u8 as usize
        );

        let d = strtab.intern("bar");
        let e = strtab.intern("bar");
        let f = strtab.intern("foo");
        assert_eq!(2, get!(strtab).len());
        assert_eq!(d, e);
        assert_eq!(a, f);
        assert_eq!(
            d.as_raw() as *const u8 as usize,
            e.as_raw() as *const u8 as usize
        );
        assert_eq!(
            a.as_raw() as *const u8 as usize,
            f.as_raw() as *const u8 as usize
        );
    }

    #[test]
    fn can_resize_set() {
        let strtab = StringTable::new();
        get!(strtab).shrink_to_fit();

        let n = 100_000;
        let mut adresses = HashMap::new();

        let src: Vec<_> = (0..n).map(|i| format!("s{}", i)).collect();

        for s in src.iter() {
            let sym = strtab.intern(s).as_raw();
            adresses.insert(s, sym);
        }

        assert_eq!(n, get!(strtab).len());
        // At this point, the table probably got resized and reallocated, so let's now
        // check if all the symbols are still in the same place

        for i in 0..n {
            let s = format!("s{}", i);
            assert_eq!(
                adresses.remove(&s).unwrap() as *const u8 as usize,
                strtab.get(&s).unwrap().as_raw() as *const u8 as usize
            );
        }
    }

    #[test]
    fn can_intern_empty_string() {
        let strtab = StringTable::new();

        strtab.intern("");
        strtab.intern("");
        strtab.intern("");
        assert_eq!(1, get!(strtab).len());
    }
}
