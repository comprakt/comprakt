#![warn(
    clippy::print_stdout,
    clippy::unimplemented,
    clippy::doc_markdown,
    clippy::items_after_statements,
    clippy::match_same_arms,
    clippy::similar_names,
    clippy::single_match_else,
    clippy::use_self,
    clippy::use_debug
)]

//! String table with zero-copy and amortised O(1) insert
//!
//! [1]: https://users.rust-lang.org/t/get-ref-to-just-inserted-hashset-element/13021

use std::{
    collections::HashSet,
    fmt,
    hash::{Hash, Hasher},
};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Relation {
    Unrelated,
    Related { distance: usize },
}

impl Relation {
    pub fn is_related(self) -> bool {
        match self {
            Relation::Unrelated => false,
            _ => true,
        }
    }
}

impl std::cmp::Ord for Relation {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        use self::Relation::*;
        match (self, other) {
            (Unrelated, _) => std::cmp::Ordering::Greater,
            (_, Unrelated) => std::cmp::Ordering::Less,
            (Related { distance: d1 }, Related { distance: d2 }) => d1.cmp(d2),
        }
    }
}

impl std::cmp::PartialOrd for Relation {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

pub trait Relational {
    fn relation(&self, other: &Self) -> Relation;
}

#[derive(Debug, Clone, Copy, Eq, PartialOrd, Ord)]
pub struct Symbol<'f>(&'f str);

impl Symbol<'_> {
    fn as_raw(&self) -> *const str {
        self.0 as *const str
    }

    pub fn as_str(&self) -> &str {
        self.0
    }
}

impl<'f> Relational for Symbol<'f> {
    fn relation(&self, other: &Symbol<'f>) -> Relation {
        let distance = levenshtein::levenshtein(self.0, other.0);
        if distance <= 2 {
            Relation::Related { distance }
        } else {
            Relation::Unrelated
        }
    }
}

impl Hash for Symbol<'_> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.as_raw().hash(state)
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

const STRING_TABLE_THIS_SYMBOL: &str = "this";

impl<'f> StringTable<'f> {
    pub fn new() -> Self {
        let mut st = StringTable::default();
        st.intern(STRING_TABLE_THIS_SYMBOL);
        st
    }

    pub fn intern(&mut self, value: &'f str) -> Symbol<'f> {
        if !self.entries.contains(value) {
            self.entries.insert(value);
        }

        // Unwrap is safe, we just inserted it
        Symbol(self.entries.get(value).unwrap())
    }

    pub fn this_symbol(&self) -> Symbol<'f> {
        Symbol(self.entries.get(STRING_TABLE_THIS_SYMBOL).unwrap())
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

    macro_rules! assert_eq_sym {
        ($a:expr, $b:expr) => {
            assert_eq!($a, $b);
            // don't trust that eq impl is based on pointer comparison
            assert_eq!($a.as_raw(), $b.as_raw());
        };
    }

    #[test]
    fn no_duplication() {
        let mut strtab = StringTable::new();
        let init_len = strtab.entries.len();

        let a = strtab.intern("foo");
        let b = strtab.intern("foo");
        let c = strtab.intern("foo");
        assert_eq!(init_len + 1, strtab.entries.len());
        assert_eq_sym!(a, b);
        assert_eq_sym!(a, c);

        let d = strtab.intern("bar");
        let e = strtab.intern("bar");
        let f = strtab.intern("foo");
        assert_eq!(init_len + 2, strtab.entries.len());
        assert_eq_sym!(d, e);
        assert_eq_sym!(a, f);
    }

    #[test]
    fn can_resize_set() {
        let mut strtab = StringTable::new();
        let init_len = strtab.entries.len();
        strtab.entries.shrink_to_fit();

        let n = 100_000;
        let mut adresses = HashMap::new();

        let src: Vec<_> = (0..n).map(|i| format!("s{}", i)).collect();

        for s in src.iter() {
            let sym = strtab.intern(s).as_raw();
            adresses.insert(s, sym);
        }

        assert_eq!(init_len + n, strtab.entries.len());
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

        let pre_len = strtab.entries.len();
        strtab.intern("");
        strtab.intern("");
        strtab.intern("");
        assert_eq!(pre_len + 1, strtab.entries.len());
    }
}
