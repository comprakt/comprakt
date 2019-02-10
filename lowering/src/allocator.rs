//! Arena-allocator for the LIR.

use std::{
    cell::RefCell,
    ops::{Deref, DerefMut},
};

pub struct Allocator<T> {
    elems: RefCell<Vec<*mut T>>,
}

#[allow(clippy::use_self)]
impl<T> Default for Allocator<T> {
    fn default() -> Allocator<T> {
        Self {
            elems: RefCell::new(Vec::new()),
        }
    }
}

use std::fmt;

impl<T> fmt::Debug for Allocator<T> {
    fn fmt(&self, fmt: &mut fmt::Formatter<'_>) -> fmt::Result {
        let tyname = unsafe { std::intrinsics::type_name::<T>() };
        write!(fmt, "Allocator<{}>", tyname)
    }
}

pub struct Ptr<T>(*mut T);

#[macro_export]
macro_rules! derive_ptr_debug {
    ($t:ty) => {
        impl std::fmt::Debug for Ptr<$t> {
            fn fmt(&self, fmt: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                let tyname = unsafe { std::intrinsics::type_name::<Self>() };
                write!(fmt, "Ptr<{}>", tyname)
            }
        }
    };
}

#[allow(clippy::use_self)]
impl<T> Clone for Ptr<T> {
    fn clone(&self) -> Ptr<T> {
        Ptr(self.0)
    }
}
impl<T> Copy for Ptr<T> {}

#[allow(clippy::use_self)]
impl<T> Ptr<T> {
    pub fn null() -> Self {
        Ptr(std::ptr::null_mut())
    }
    pub fn ptr_eq(a: Ptr<T>, b: Ptr<T>) -> bool {
        std::ptr::eq(a.0, b.0)
    }
}

impl<T> Deref for Ptr<T> {
    type Target = T;
    fn deref(&self) -> &T {
        unsafe { &(*self.0) }
    }
}

impl<T> DerefMut for Ptr<T> {
    fn deref_mut(&mut self) -> &mut T {
        unsafe { &mut (*self.0) }
    }
}

impl<T> Allocator<T> {
    pub fn alloc(&self, elem: T) -> Ptr<T> {
        let b = Box::into_raw(Box::new(elem));
        self.elems.borrow_mut().push(b);
        Ptr(b)
    }
}

impl<T> Drop for Allocator<T> {
    fn drop(&mut self) {
        for raw_ptr in self.elems.borrow_mut().iter().cloned() {
            unsafe {
                drop(Box::from_raw(raw_ptr));
            }
        }
    }
}

impl<T> std::borrow::Borrow<T> for Ptr<T> {
    fn borrow(&self) -> &T {
        unsafe { &*self.0 }
    }
}

#[derive(From)]
pub struct HashPtr<T>(Ptr<T>);

impl<T> HashPtr<T> {
    #[allow(dead_code)]
    pub fn as_ptr(self) -> Ptr<T> {
        self.0
    }
}

use std::hash;

impl<T> hash::Hash for HashPtr<T> {
    fn hash<H: hash::Hasher>(&self, state: &mut H) {
        ((self.0).0 as *const usize).hash(state)
    }
}

impl<T> PartialEq for HashPtr<T> {
    fn eq(&self, other: &Self) -> bool {
        Ptr::ptr_eq(self.0, other.0)
    }
}

impl<T> Eq for HashPtr<T> {}

impl<T> Deref for HashPtr<T> {
    type Target = T;
    fn deref(&self) -> &T {
        unsafe { &(*(self.0).0) }
    }
}

impl<T> DerefMut for HashPtr<T> {
    fn deref_mut(&mut self) -> &mut T {
        unsafe { &mut (*(self.0).0) }
    }
}

#[allow(clippy::use_self)]
impl<T> Clone for HashPtr<T> {
    fn clone(&self) -> HashPtr<T> {
        HashPtr(self.0)
    }
}
impl<T> Copy for HashPtr<T> {}

#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn works() {
        let allocator = Allocator::default();
        let ptr = allocator.alloc(23);
        let mut ptr2 = ptr;

        assert_eq!(*ptr, 23);
        assert_eq!(*ptr2, 23);

        *ptr2 = 42;
        assert_eq!(*ptr, 42);
        assert_eq!(*ptr2, 42);
    }
}
