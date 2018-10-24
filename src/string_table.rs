//! Ensures that strings are allocated only once.
//!
//! It is a write-once cache; values are never evicted. The implementation is
//! NOT thread-safe.
//!
//! `StringTable` uses the "interior mutability pattern" [1].  This means a
//! `StringTable` can be shared by all compiler stages using immutable
//! references.
//!
//! ---
//!
//! Implementing this in rust without unnecessary copies and in safe code is
//! really hard! Most implementations out there are more than 500 lines long.
//!
//! We use the idea of Hero Bird [2].  The approach here is fragile. So please
//! consider the following before adapting types:
//! - We use raw pointer's to reference strings.
//! - These strings are stored in a Vec<>, which is backed by an array that
//!   may be moved if the Vec<> is resized. We use Box<> to ensure the address
//!   of strings stay the same during reallocation.
//! - Box<str> is similar to String, but has no additional capacity.
//! - Since we can have multiple raw pointer's, a HashMap can hold a second
//!   reference to the string without causing any borrow checking errors.
//!
//! ---
//! [1] https://doc.rust-lang.org/book/second-edition/ch15-05-interior-mutability.html
//! [2] https://github.com/Robbepop/string-interner
use std::{
    borrow::Cow,
    cell::RefCell,
    collections::HashMap,
    hash::{Hash, Hasher},
    rc::Rc,
};

//#[derive(Debug)]
//pub struct Symbol<'a> {
//name: &str,
//}

#[derive(Debug)]
pub struct StringTable {
    //lookup: RefCell<HashMap<InternedStringRef, InternedStringRef>>,
    //entries: RefCell<Vec<Box<str>>>,
    lookup: HashMap<InternedStringRef, usize>,
    entries: Vec<Box<str>>,
}

// NOTE: this should stay private to gurantee correctness. We generate
// temporary instances of this unit tuple for lookups in the hashmap. Leaking
// these objects would result in a crash if `as_str` is called.
#[derive(Debug, Copy, Clone, Eq)]
struct InternedStringRef(*const str);

impl InternedStringRef {
    fn as_str(&self) -> &str {
        unsafe { &*self.0 }
    }

    fn new(val: &str) -> Self {
        InternedStringRef(val as *const str)
    }
}

impl Hash for InternedStringRef {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.as_str().hash(state)
    }
}

impl PartialEq for InternedStringRef {
    fn eq(&self, other: &Self) -> bool {
        self.as_str() == other.as_str()
    }
}

impl StringTable {
    pub fn new() -> Self {
        Self {
            //entries: RefCell::new(HashMap::new()),
            lookup: HashMap::new(),
            entries: Vec::new(),
        }
    }

    pub fn intern<'a, 'b, S>(&'b mut self, raw: S) -> &'b str
    where
        S: Into<Cow<'a, str>>,
    {
        let value: Cow<'a, str> = raw.into();
        let key: &str = &value;

        if let Some(pos) = self.lookup.get(&InternedStringRef::new(key)) {
            return &self.entries[*pos];
        }

        let interned_string = value.into_owned().into_boxed_str();
        let interned_string_ref = InternedStringRef::new(&interned_string);
        self.entries.push(interned_string);
        let pos = self.entries.len() - 1;
        self.lookup.insert(interned_string_ref, pos);
        &self.entries[pos]
    }

    #[cfg(test)]
    fn assert_internal_state_size(&self, size: usize) {
        assert_eq!(size, self.lookup.len());
        assert_eq!(size, self.entries.len());
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_no_segfault() {
        let mut interner = StringTable::new();
        assert_eq!("banana", interner.intern("banana"));
        assert_eq!("pie", interner.intern("pie"));
        assert_eq!("banana", interner.intern("banana"));
        assert_eq!("banana", interner.intern("banana"));
    }

    #[test]
    fn test_single_insertion() {
        let mut interner = StringTable::new();
        interner.assert_internal_state_size(0);
        interner.intern("banana");
        interner.intern("banana");
        interner.intern("banana");
        interner.assert_internal_state_size(1);
    }
}
