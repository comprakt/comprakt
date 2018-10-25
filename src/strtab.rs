//! String table with interior mutability and O(n) insert on miss
//!
//! Interior mutability achived using UnsafeCell.
//!
//! Using `Rc` and not `Arc`, this is not thread-safe.
//!
//! [1]: https://users.rust-lang.org/t/get-ref-to-just-inserted-hashset-element/13021

use std::{cell::UnsafeCell, collections::HashSet, rc::Rc};

pub type Symbol = Rc<str>;

#[derive(Debug)]
pub struct StringTable {
    entries: UnsafeCell<HashSet<Rc<str>>>,
}

impl StringTable {
    pub fn new() -> Self {
        StringTable {
            entries: UnsafeCell::new(HashSet::new()),
        }
    }

    pub fn intern(&self, value: &str) -> Symbol {
        // Entries is private, and this is the only place where it is mutated
        let entries = unsafe { &mut *self.entries.get() };
        if !entries.contains(value) {
            entries.insert(value.into());
        }
        Rc::clone(entries.get(value).unwrap())
    }
}
