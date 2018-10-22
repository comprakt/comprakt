//! String table with interior mutability and zero-copy ~O(1) interning
//!
//! Interior mutability achived using RefCell. This never panics as
//! long as no symbols are removed. E.g. calling
//! `self.entries.borrow_mut().clear()` panics if there are still `Symbol`s
//! (i.e. `Ref`s) going around.
//!
//! Zero-copy interning is achieved via `Rc`ed Strings, thus overcoming the
//! limitation discussed in this thread [1]. Interning is ~O(1) because a
//! `HashSet` is used internally.
//!
//! [1]: https://users.rust-lang.org/t/get-ref-to-just-inserted-hashset-element/13021

use std::{
    cell::{Ref, RefCell},
    collections::HashSet,
    rc::Rc,
};

/// This is quite the complex type, but can still be derefed to `&str` as
/// `&**sym`.
pub type Symbol<'t> = Ref<'t, Rc<String>>;

#[derive(Debug)]
pub struct StringTable {
    entries: RefCell<HashSet<Rc<String>>>,
}

impl StringTable {
    pub fn new() -> Self {
        StringTable {
            entries: RefCell::new(HashSet::new()),
        }
    }

    pub fn intern(&self, value: String) -> Symbol<'_> {
        let value = Rc::new(value);
        self.entries.borrow_mut().insert(Rc::clone(&value));
        Ref::map(self.entries.borrow(), |tab| tab.get(&value).unwrap())
    }
}
