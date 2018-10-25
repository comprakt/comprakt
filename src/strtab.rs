//! String table with interior mutability and zero-copy ~O(1) interning
//!
//! Interior mutability achived using UnsafeCell.
//!
//! Zero-copy interning is achieved via `Rc`ed Strings, thus overcoming the
//! limitation discussed in this thread [1]. Interning is ~O(1) because a
//! `HashSet` is used internally.
//!
//! Using `Rc` and not `Arc`, this is not thread-safe.
//!
//! [1]: https://users.rust-lang.org/t/get-ref-to-just-inserted-hashset-element/13021

use std::{cell::UnsafeCell, collections::HashSet, rc::Rc};

/// This is quite the complex type, but can still be derefed to `&str` as
/// `&**sym`.
pub type Symbol = Rc<String>;

#[derive(Debug)]
pub struct StringTable {
    entries: UnsafeCell<HashSet<Rc<String>>>,
}

impl StringTable {
    pub fn new() -> Self {
        StringTable {
            entries: UnsafeCell::new(HashSet::new()),
        }
    }

    pub fn intern(&self, value: String) -> Symbol {
        let value = Rc::new(value);
        // Entries is private, and this is the only place where it is mutated
        let entries = unsafe { &mut *self.entries.get() };
        entries.insert(Rc::clone(&value));
        Rc::clone(entries.get(&value).unwrap())
    }
}
