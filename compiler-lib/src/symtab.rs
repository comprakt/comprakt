use crate::strtab::Symbol;
use std::collections::HashMap;

pub type SymbolTable<'a, T> = HashMap<Symbol<'a>, T>;

pub struct Scoped<'a, T> {
    root: SymbolTable<'a, T>,
    scopes: Vec<SymbolTable<'a, T>>,
}

#[derive(Debug)]
pub struct CannotLeaveRootScopeError;
#[derive(Debug)]
pub struct RedefinitionError;

impl<'a, T> Scoped<'a, T> {
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

    pub fn define(&mut self, sym: &Symbol<'a>, val: T) -> Result<(), RedefinitionError> {
        use std::collections::hash_map::Entry;
        // TODO This allows shadowing
        match self.current_scope().entry(*sym) {
            Entry::Occupied(_) => return Err(RedefinitionError),
            Entry::Vacant(e) => e.insert(val),
        };
        Ok(())
    }

    pub fn lookup(&self, sym: Symbol<'a>) -> Option<&T> {
        // TODO O(1) access to visible definition
        for scope in self.scope_upwards_iter() {
            if let Some(def) = scope.get(&sym) {
                return Some(def);
            }
        }

        None
    }

    fn current_scope(&mut self) -> &mut SymbolTable<'a, T> {
        self.scopes.last_mut().unwrap_or(&mut self.root)
    }

    fn scope_upwards_iter(&self) -> impl Iterator<Item = &SymbolTable<'a, T>> {
        self.scopes.iter().rev().chain(vec![&self.root])
    }
}
