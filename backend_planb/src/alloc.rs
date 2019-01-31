use std::{
    cell::RefCell,
    ops::{Deref, DerefMut},
};

pub struct Allocator<T> {
    elems: RefCell<Vec<*mut T>>,
}

impl<T> Default for Allocator<T> {
    fn default() -> Allocator<T> {
        Allocator {
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

impl<T> Clone for Ptr<T> {
    fn clone(&self) -> Ptr<T> {
        Ptr(self.0)
    }
}
impl<T> Copy for Ptr<T> {}

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
