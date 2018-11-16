use std::collections::HashMap;

/// SymbolTable associates a Symbol `S` with a stored value `T`.
pub type SymbolTable<S, T> = HashMap<S, T>;

/// Scoped implements scoping for SymbolTable.
/// The generic type `S` is the Symbol and `T` is the value stored for that
/// symbol.
pub struct Scoped<S, T>
where
    S: std::hash::Hash + Eq + Copy,
{
    root: SymbolTable<S, T>,
    scopes: Vec<SymbolTable<S, T>>,
}

#[derive(Debug)]
pub struct CannotLeaveRootScopeError;
#[derive(Debug)]
pub struct RedefinitionError;

impl<S, T> Scoped<S, T>
where
    S: std::hash::Hash + Eq + Copy,
{
    pub fn new() -> Self {
        Scoped {
            root: SymbolTable::new(),
            scopes: Vec::new(),
        }
    }

    pub fn enter_scope(&mut self) {
        self.scopes.push(SymbolTable::new())
    }

    pub fn leave_scope(&mut self) -> Result<(), CannotLeaveRootScopeError> {
        self.scopes
            .pop()
            .map(|_| ())
            .ok_or(CannotLeaveRootScopeError)
    }

    pub fn define(&mut self, sym: S, val: T) -> Result<(), RedefinitionError> {
        if let Some(existing_definition) = self.visible_definition(sym) {
            return Err(RedefinitionError);
        }
        let insert_res = self.current_scope().insert(sym, val);
        debug_assert!(
            insert_res.is_none(),
            "visible_defintions broken, current_scope contained a value"
        );
        Ok(())
    }

    pub fn visible_definition(&self, sym: S) -> Option<&T> {
        // TODO O(1) access to visible definition, see {{root}}/benches/symtab.rs
        for scope in self.scope_upwards_iter() {
            if let Some(def) = scope.get(&sym) {
                return Some(def);
            }
        }

        None
    }

    fn current_scope(&mut self) -> &mut SymbolTable<S, T> {
        self.scopes.last_mut().unwrap_or(&mut self.root)
    }

    fn scope_upwards_iter(&self) -> impl Iterator<Item = &SymbolTable<S, T>> {
        self.scopes.iter().rev().chain(vec![&self.root])
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
        scoped.leave_scope();
        // at l1
        assert_def!(scoped, &"root");
        assert_def!(scoped, &"l1");
        assert_no_def!(scoped, &"l2");
        scoped.leave_scope();
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
        scoped.leave_scope();
        scoped.enter_scope();
        def!(scoped, &"v");
        scoped.leave_scope();
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
