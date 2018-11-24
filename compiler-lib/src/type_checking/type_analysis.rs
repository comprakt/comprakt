use crate::ast;
use std::{collections::HashMap, hash::Hash};

use super::{method_body_typechecker::*, type_system::*};

#[derive(Eq)]
pub struct RefEquality<'a, T>(&'a T);

impl<'a, T> std::hash::Hash for RefEquality<'a, T> {
    fn hash<H>(&self, state: &mut H)
    where
        H: std::hash::Hasher,
    {
        (self.0 as *const T).hash(state)
    }
}

impl<'a, 'b, T> PartialEq<RefEquality<'b, T>> for RefEquality<'a, T> {
    fn eq(&self, other: &'_ RefEquality<'b, T>) -> bool {
        self.0 as *const T == other.0 as *const T
    }
}

#[derive(Default)]
pub struct TypeAnalysis<'src, 'ast, 'ts> {
    class_types: HashMap<RefEquality<'ast, ast::ClassDeclaration<'src>>, ClassDefId<'src>>,
    expr_info: HashMap<RefEquality<'ast, ast::Expr<'src>>, ExprInfo<'src, 'ts>>,
}

impl<'src, 'ast, 'ts> TypeAnalysis<'src, 'ast, 'ts> {
    pub fn new() -> TypeAnalysis<'src, 'ast, 'ts> {
        TypeAnalysis::default()
    }

    pub fn expr_info(&self, expr: &'ast ast::Expr<'src>) -> &ExprInfo<'src, 'ts> {
        self.expr_info
            .get(&RefEquality(expr))
            .expect("after typechecking every expression should have a type")
    }

    pub fn set_expr_info(&mut self, expr: &'ast ast::Expr<'src>, expr_info: ExprInfo<'src, 'ts>) {
        self.expr_info.insert(RefEquality(expr), expr_info);
    }

    pub fn decl_set_class_id(
        &mut self,
        class_decl: &'ast ast::ClassDeclaration<'src>,
        name: ClassDefId<'src>,
    ) {
        self.class_types.insert(RefEquality(class_decl), name);
    }

    pub fn decl_get_class_id(
        &mut self,
        class_decl: &'ast ast::ClassDeclaration<'src>,
    ) -> Option<ClassDefId<'src>> {
        self.class_types
            .get(&RefEquality(class_decl))
            .map(|id| id.clone())
    }
}
