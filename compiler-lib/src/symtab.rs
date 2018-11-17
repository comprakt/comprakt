use std::collections::HashMap;

/// SymbolTable associates a Symbol `S` with a stored value `T`.
pub type SymbolTable<S, T> = HashMap<S, T>;

#[derive(Clone, Copy)]
enum ScopeIdx {
    Root,
    Dynamic(usize),
}

/// Scoped implements scoping for SymbolTable.
/// The generic type `S` is the Symbol and `T` is the value stored for that
/// symbol.
pub struct Scoped<S, T>
where
    S: std::hash::Hash + Eq + Copy,
{
    root: SymbolTable<S, T>,
    scopes: Vec<SymbolTable<S, T>>,
    visible_defs: HashMap<S, ScopeIdx>,
}

#[derive(Debug)]
pub struct CannotLeaveRootScopeError;
#[derive(Debug)]
pub struct RedefinitionError;

#[allow(clippy::new_without_default_derive)]
impl<S, T> Scoped<S, T>
where
    S: std::hash::Hash + Eq + Copy,
{
    pub fn new() -> Self {
        Scoped {
            root: SymbolTable::new(),
            scopes: Vec::new(),
            visible_defs: HashMap::new(),
        }
    }

    pub fn enter_scope(&mut self) {
        self.scopes.push(SymbolTable::new())
    }

    pub fn leave_scope(&mut self) -> Result<(), CannotLeaveRootScopeError> {
        let popped = self.scopes.pop().ok_or(CannotLeaveRootScopeError)?;
        popped.iter().for_each(|(sym, _)| {
            self.visible_defs
                .remove(sym)
                .expect("scopes inconsistent with visible_defs");
        });
        Ok(())
    }

    pub fn define(&mut self, sym: S, val: T) -> Result<(), RedefinitionError> {
        // amortized O(1) lookup in visible_defs
        if self.visible_defs.contains_key(&sym) {
            return Err(RedefinitionError);
        }
        // insert into both current_scope and visible_defs
        let (current_scope, scope_idx) = self.current_scope();
        let insert_res = current_scope.insert(sym, val);
        debug_assert!(
            insert_res.is_none(),
            "current_scope must be consistent with visible_defs"
        );
        let insert_res = self.visible_defs.insert(sym, scope_idx);
        debug_assert!(insert_res.is_none(), "we checked that above (contains_key)");
        // invariant: visible_defs and current_scope consistent
        Ok(())
    }

    pub fn visible_definition(&self, sym: S) -> Option<&T> {
        // amortized O(1) lookup to get sym's scope
        let scope = match self.visible_defs.get(&sym) {
            Some(&idx) => self.get_scope(idx),
            None => return None,
        };
        // amortized O(1) lookup of symbol in that scope
        let res = scope
            .get(&sym)
            .expect("visible_defs is inconsistent with state of symbol tables");
        Some(res)
    }

    fn current_scope(&mut self) -> (&mut SymbolTable<S, T>, ScopeIdx) {
        if !self.scopes.is_empty() {
            let idx = self.scopes.len() - 1;
            (&mut self.scopes[idx], ScopeIdx::Dynamic(idx))
        } else {
            (&mut self.root, ScopeIdx::Root)
        }
    }

    fn get_scope(&self, scope_idx: ScopeIdx) -> &SymbolTable<S, T> {
        match scope_idx {
            ScopeIdx::Root => &self.root,
            ScopeIdx::Dynamic(idx) => &self.scopes[idx],
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    macro_rules! def {
        ($scoped:expr, $s:expr) => {
            $scoped.define($s, ()).unwrap();
            $scoped
                .visible_definition($s)
                .expect("just defined successfully, should be visible");
        };
        ($scoped:expr, $s:expr, $t:expr) => {{
            $scoped.define($s, $t).unwrap();
            let vis = $scoped
                .visible_definition($s)
                .expect("just defined successfully, should be visible");
            assert_eq!(&$t, vis);
        }};
    }

    macro_rules! assert_def {
        ($scoped:expr, $s:expr) => {
            $scoped
                .visible_definition($s)
                .expect(&format!("expecting visible definition for {:?}", $s));
        };
    }

    macro_rules! assert_no_def {
        ($scoped:expr, $s:expr) => {
            let def = $scoped.visible_definition($s);
            assert_matches!(def, None);
        };
    }

    #[test]
    fn definition_inheritance_works() {
        let mut scoped = Scoped::new();
        def!(scoped, &"root");
        scoped.enter_scope();
        def!(scoped, &"l1");
        scoped.enter_scope();
        def!(scoped, &"l2");

        // at l2
        assert_def!(scoped, &"root");
        assert_def!(scoped, &"l1");
        assert_def!(scoped, &"l2");
        scoped.leave_scope().expect("not in root scope");
        // at l1
        assert_def!(scoped, &"root");
        assert_def!(scoped, &"l1");
        assert_no_def!(scoped, &"l2");
        scoped.leave_scope().expect("not in root scope");
        // at root scope
        assert_def!(scoped, &"root");
        assert_no_def!(scoped, &"l1");
        assert_no_def!(scoped, &"l2");
    }

    #[test]
    fn neighboring_scopes() {
        let mut scoped = Scoped::new();
        def!(scoped, &"inroot");
        scoped.enter_scope();
        def!(scoped, &"v");
        scoped.leave_scope().expect("not in root scope");
        scoped.enter_scope();
        def!(scoped, &"v");
        scoped.leave_scope().expect("not in root scope");
    }

    #[test]
    fn no_shadowing() {
        let mut scoped = Scoped::new();
        def!(scoped, &"23");
        scoped.enter_scope();
        let redef = scoped.define(&"23", ());
        assert_matches!(redef, Err(RedefinitionError));
    }

    #[test]
    fn over_leaves_returns_err() {
        let mut scoped: Scoped<(), ()> = Scoped::new();
        scoped.enter_scope();
        scoped.enter_scope();
        scoped.leave_scope().unwrap();
        scoped.leave_scope().unwrap();
        let ret = scoped.leave_scope();
        assert_matches!(ret, Err(CannotLeaveRootScopeError));
    }

}
