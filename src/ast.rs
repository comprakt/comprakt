use crate::strtab::Symbol;

#[derive(Debug, PartialEq, Eq)]
pub enum BasicType {
    Void,
    Int,
    Bool,
    Ident(Symbol),
}

#[derive(Debug, PartialEq, Eq)]
pub enum Type {
    Basic(BasicType),
    ArrayOf(Box<Type>),
}

#[derive(Debug, PartialEq, Eq)]
pub struct VariableDecl {
    pub var_type: Type,
    pub name: Symbol,
}

pub type ParameterList = Vec<VariableDecl>;
