use strtab::Symbol;

#[derive(Default)]
pub struct Locator<'f> {
    class: Option<Symbol<'f>>,
    method: Option<Symbol<'f>>,
}

impl<'f> Locator<'f> {
    pub(super) fn push(&mut self, name: Symbol<'f>) {
        if self.class.is_none() {
            self.class = Some(name);
        } else if self.method.is_none() {
            debug_assert!(self.class.is_some());
            self.method = Some(name);
        } else {
            unreachable!();
        }
    }

    pub(super) fn pop(&mut self) {
        if self.method.is_some() {
            self.method = None;
        } else if self.class.is_some() {
            debug_assert!(self.method.is_none());
            self.class = None;
        } else {
            unreachable!();
        }
    }

    #[allow(dead_code)]
    pub(super) fn method(&self) -> Symbol<'f> {
        self.method
            .expect("only checks inside of method bodies have a method location")
    }

    pub(super) fn class(&self) -> Symbol<'f> {
        self.class
            .expect("only checks inside of a class have a method location")
    }
}
