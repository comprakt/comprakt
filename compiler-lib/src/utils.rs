use std::path::PathBuf;

macro_rules! matches {
    ($expression: expr, $( $pattern: pat )|*) => {{
        match $expression {
            $( $pattern )|* => true,
            _ => false,
        }
    }};
}

macro_rules! assert_matches {
    ($expression: expr, $( $pattern: pat )|*) => {{
        if cfg!(debug_assertions) {
            match $expression {
                $( $pattern )|* => (),
                expression => panic!(
                    r#"assertion failed: `(if let pattern = expression), {}:{}:{}`
pattern: `{}`,
expression: `{:?}`"#,
                    file!(),
                    line!(),
                    column!(),
                    stringify!($( $pattern )|*),
                    expression
                ),
            }
        }
    }};
}

#[derive(Debug, Clone)]
pub enum OutputSpecification {
    Stdout,
    File(PathBuf),
}

/// Like [`std::iter::Peekable`], but can peek further ahead (needed e.g. for
/// `SLL(k)` where `k > 1`. TODO Some of this is reimplemented directly in the
/// [`crate::asciifile::PositionedChars`], maybe that
/// code should use this instead.
pub struct MultiPeekable<I>
where
    I: Iterator,
{
    iter: I,
    buffer: Vec<I::Item>,
}

impl<I> MultiPeekable<I>
where
    I: Iterator,
{
    pub fn new(iter: I) -> Self {
        MultiPeekable {
            iter,
            buffer: Vec::new(),
        }
    }

    pub fn peek(&mut self) -> Option<&I::Item> {
        self.peek_multiple(1).get(0)
    }

    /// Peek the next `n` items. Returned slice might be shorter than `n`,
    /// if iterator ended.
    pub fn peek_multiple(&mut self, n: usize) -> &[I::Item] {
        for _ in self.buffer.len()..n {
            match self.iter.next() {
                Some(item) => self.buffer.push(item),
                None => break,
            }
        }

        &self.buffer[0..self.buffer.len().min(n)]
    }
}

impl<I> Iterator for MultiPeekable<I>
where
    I: Iterator,
{
    type Item = I::Item;

    fn next(&mut self) -> Option<Self::Item> {
        self.peek()?;
        Some(self.buffer.remove(0))
    }
}

#[macro_use]
pub mod cell {

    use std::{
        cell::{Ref, RefCell, RefMut},
        fmt,
        rc::{Rc, Weak},
    };

    pub struct MutRc<T>(Rc<RefCell<T>>);
    pub struct MutWeak<T>(Weak<RefCell<T>>);

    impl<T> MutRc<T> {
        pub fn new(val: T) -> Self {
            MutRc(Rc::new(RefCell::from(val)))
        }

        pub fn downgrade(&self) -> MutWeak<T> {
            MutWeak(Rc::downgrade(&self.0))
        }

        pub fn borrow(&self) -> Ref<'_, T> {
            self.0.borrow()
        }

        pub fn borrow_mut(&self) -> RefMut<'_, T> {
            self.0.borrow_mut()
        }

        pub fn ptr_eq(this: &MutRc<T>, other: &MutRc<T>) -> bool {
            Rc::ptr_eq(&this.0, &other.0)
        }
    }

    impl<T> Clone for MutRc<T> {
        fn clone(&self) -> MutRc<T> {
            MutRc(Rc::clone(&self.0))
        }
    }

    impl<T> MutWeak<T> {
        pub fn new() -> Self {
            MutWeak(Weak::new())
        }

        pub fn upgrade(&self) -> Option<MutRc<T>> {
            Weak::upgrade(&self.0).map(MutRc)
        }

        pub fn ptr_eq(this: &MutWeak<T>, other: &MutWeak<T>) -> bool {
            Weak::ptr_eq(&this.0, &other.0)
        }
    }

    impl<T> Clone for MutWeak<T> {
        fn clone(&self) -> MutWeak<T> {
            MutWeak(Weak::clone(&self.0))
        }
    }

    macro_rules! upborrow {
        ($mut_weak: expr) => {
            MutWeak::upgrade(&$mut_weak).unwrap().borrow()
        };

        (mut $mut_weak: expr) => {
            MutWeak::upgrade(&$mut_weak).unwrap().borrow_mut()
        };
    }

    impl<T> fmt::Debug for MutRc<T>
    where
        T: fmt::Debug,
    {
        fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
            self.borrow().fmt(f)
        }
    }

    impl<T> fmt::Debug for MutWeak<T>
    where
        T: fmt::Debug,
    {
        fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
            upborrow!(self).fmt(f)
        }
    }
}
