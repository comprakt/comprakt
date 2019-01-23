use std::{hash::Hasher, rc::Rc};

#[derive(Debug)]
pub struct RefEq<T>(pub T);

impl<'a, T> std::hash::Hash for RefEq<&'a T> {
    fn hash<H>(&self, state: &mut H)
    where
        H: Hasher,
    {
        (self.0 as *const T).hash(state)
    }
}

impl<'a, 'b, T> PartialEq<RefEq<&'b T>> for RefEq<&'b T> {
    fn eq(&self, other: &'_ RefEq<&'b T>) -> bool {
        self.0 as *const T == other.0 as *const T
    }
}

impl<'a, T> Eq for RefEq<&'a T> {}

impl<T> std::hash::Hash for RefEq<Rc<T>> {
    fn hash<H>(&self, state: &mut H)
    where
        H: Hasher,
    {
        let ptr = Rc::into_raw(self.0.clone());
        ptr.hash(state);
        let _ = unsafe { Rc::from_raw(ptr) };
    }
}

impl<T> PartialEq<RefEq<Rc<T>>> for RefEq<Rc<T>> {
    fn eq(&self, other: &Self) -> bool {
        Rc::ptr_eq(&self.0, &other.0)
    }
}

impl<T> Eq for RefEq<Rc<T>> {}
