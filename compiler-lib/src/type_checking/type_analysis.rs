use super::type_system::*;
use crate::{ast, strtab::Symbol};
use std::{collections::HashMap, hash::Hash};

use std::rc::Rc;

#[derive(Eq, Debug)]
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

#[derive(Default, Debug)]
pub struct TypeAnalysis<'src, 'ast> {
    class_types: HashMap<RefEquality<'ast, ast::ClassDeclaration<'src>>, ClassDefId<'src>>,
    expr_info: HashMap<RefEquality<'ast, ast::Expr<'src>>, ExprInfo<'src, 'ast>>,
    /* for all LocalVariableDeclarations */
    local_val_defs: HashMap<RefEquality<'ast, ast::Stmt<'src>>, Rc<LocalVarDef<'src>>>,
}

impl<'src, 'ast, 'ts> TypeAnalysis<'src, 'ast> {
    pub fn new() -> TypeAnalysis<'src, 'ast> {
        TypeAnalysis::default()
    }

    pub fn expr_info(&self, expr: &'ast ast::Expr<'src>) -> &ExprInfo<'src, 'ast> {
        self.expr_info
            .get(&RefEquality(expr))
            .expect("after typechecking every expression should have a type")
    }

    pub fn set_expr_info(&mut self, expr: &'ast ast::Expr<'src>, expr_info: ExprInfo<'src, 'ast>) {
        self.expr_info.insert(RefEquality(expr), expr_info);
    }

    pub fn set_local_var_def(
        &mut self,
        local_var_def_stmt: &'ast ast::Stmt<'src>,
        var_def: Rc<LocalVarDef<'src>>,
    ) {
        self.local_val_defs
            .insert(RefEquality(local_var_def_stmt), var_def);
    }

    pub fn local_var_def(
        &mut self,
        local_var_def_stmt: &'ast ast::Stmt<'src>,
    ) -> Option<Rc<LocalVarDef<'src>>> {
        match self.local_val_defs.get(&RefEquality(local_var_def_stmt)) {
            Some(var_def) => Some(Rc::clone(var_def)),
            None => None,
        }
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
        self.class_types.get(&RefEquality(class_decl)).cloned()
    }
}

#[derive(Debug, Clone)]
pub struct LocalVarDef<'src> {
    pub name: Symbol<'src>,
    pub ty: CheckedType<'src>,
}

#[derive(Debug, Clone)]
pub struct ExprInfo<'src, 'ast> {
    pub ty: CheckedType<'src>,
    pub ref_info: Option<RefInfo<'src, 'ast>>,
}

impl<'src, 'ast> ExprInfo<'src, 'ast> {
    pub fn new(ty: CheckedType<'src>, ref_info: RefInfo<'src, 'ast>) -> Self {
        ExprInfo {
            ty,
            ref_info: Some(ref_info),
        }
    }
}

impl<'src, 'ast, 'ts> From<CheckedType<'src>> for ExprInfo<'src, 'ast> {
    fn from(item: CheckedType<'src>) -> ExprInfo<'src, 'ast> {
        ExprInfo {
            ty: item,
            ref_info: None,
        }
    }
}

#[derive(Debug, Clone)]
pub enum RefInfo<'src, 'ast> {
    GlobalVar(Symbol<'src>),
    Var(Rc<LocalVarDef<'src>>),
    Param(Rc<MethodParamDef<'src>>),
    Field(Rc<ClassFieldDef<'src>>),
    Method(Rc<ClassMethodDef<'src, 'ast>>),
    // impossible in minijava: Class(Rc<ClassDef<'src, 'ast>),
    This(Rc<ClassDef<'src, 'ast>>),
    ArrayAccess,
}
