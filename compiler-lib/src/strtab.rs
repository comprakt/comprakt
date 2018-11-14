//! String table with zero-copy and amortised O(1) insert
//!
//! [1]: https://users.rust-lang.org/t/get-ref-to-just-inserted-hashset-element/13021

use std::{collections::HashSet, fmt};

#[derive(Debug, Clone, Copy, Eq, PartialOrd, Ord, Hash)]
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
    entries: HashSet<&'f str>,
}

impl<'f> StringTable<'f> {
    pub fn new() -> Self {
        StringTable::default()
    }

    pub fn intern(&mut self, value: &'f str) -> Symbol<'f> {
        if !self.entries.contains(value) {
            self.entries.insert(value);
        }

        // Unwrap is safe, we just inserted it
        Symbol(self.entries.get(value).unwrap())
    }

    #[cfg(test)]
    fn get(&mut self, value: &str) -> Option<Symbol<'f>> {
        self.entries.get(value).map(|s| Symbol(*s))
    }
}

#[cfg(test)]
mod tests {
    //! Tests ideas are stolen to a not insignificant degree from Reiner

    use super::*;
    use std::collections::HashMap;

    #[test]
    fn no_duplication() {
        let mut strtab = StringTable::new();

        let a = strtab.intern("foo");
        let b = strtab.intern("foo");
        let c = strtab.intern("foo");
        assert_eq!(1, strtab.entries.len());
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
        assert_eq!(2, strtab.entries.len());
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
        let mut strtab = StringTable::new();
        strtab.entries.shrink_to_fit();

        let n = 100_000;
        let mut adresses = HashMap::new();

        let src: Vec<_> = (0..n).map(|i| format!("s{}", i)).collect();

        for s in src.iter() {
            let sym = strtab.intern(s).as_raw();
            adresses.insert(s, sym);
        }

        assert_eq!(n, strtab.entries.len());
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
        let mut strtab = StringTable::new();

        strtab.intern("");
        strtab.intern("");
        strtab.intern("");
        assert_eq!(1, strtab.entries.len());
    }
}
