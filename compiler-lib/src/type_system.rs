use crate::{ast, context::Context, strtab::Symbol};
use std::collections::{hash_map::Entry, HashMap};
use std::fmt;

#[derive(Debug)]
pub struct TypeSystem<'src> {
    defined_classes: HashMap<Symbol<'src>, ClassDef<'src>>,
}

impl<'src> TypeSystem<'src> {
    pub fn new() -> TypeSystem<'src> {
        TypeSystem {
            defined_classes: HashMap::new(),
        }
    }

    pub fn is_type_defined(&self, name: Symbol<'src>) -> bool {
        self.defined_classes.contains_key(&name)
    }

    pub fn add_class_def(&mut self, class_def: ClassDef<'src>) -> Result<(), ()> {
        match self.defined_classes.entry(class_def.name) {
            Entry::Occupied(_) => return Err(()),
            Entry::Vacant(e) => e.insert(class_def),
        };
        Ok(())
    }

    pub fn resolve_type_ref(&self, type_ref: Symbol<'src>) -> Option<&ClassDef<'src>> {
        self.defined_classes.get(&type_ref)
    }
}

#[derive(Debug)]
pub struct ClassDef<'src> {
    pub name: Symbol<'src>,
    fields: HashMap<Symbol<'src>, ClassFieldDef<'src>>,
    methods: HashMap<Symbol<'src>, ClassMethodDef<'src>>,
}

impl<'src> ClassDef<'src> {
    pub fn new(name: Symbol<'src>) -> ClassDef<'src> {
        ClassDef {
            name,
            fields: HashMap::new(),
            methods: HashMap::new(),
        }
    }

    pub fn add_field(&mut self, field: ClassFieldDef<'src>) -> Result<(), ()> {
        match self.fields.entry(field.name) {
            Entry::Occupied(_) => return Err(()),
            Entry::Vacant(e) => e.insert(field),
        };
        Ok(())
    }

    pub fn get_field(&self, name: Symbol<'src>) -> Option<&ClassFieldDef<'src>> {
        self.fields.get(&name)
    }

    pub fn add_method(&mut self, method: ClassMethodDef<'src>) -> Result<(), ()> {
        match self.methods.entry(method.name) {
            Entry::Occupied(_) => return Err(()),
            Entry::Vacant(e) => e.insert(method),
        };
        Ok(())
    }

    pub fn get_method(&self, name: Symbol<'src>) -> Option<&ClassMethodDef<'src>> {
        self.methods.get(&name)
    }
}

#[derive(Debug)]
pub struct ClassMethodDef<'src> {
    pub name: Symbol<'src>,
    pub params: Vec<MethodParamDef<'src>>,
    pub return_ty: CheckedType<'src>,
    pub is_static: bool,
}

impl<'src> ClassMethodDef<'src> {
    pub fn new(
        name: Symbol<'src>,
        params: &ast::ParameterList<'src>,
        return_ty: CheckedType<'src>,
        is_static: bool,
    ) -> ClassMethodDef<'src> {
        ClassMethodDef {
            is_static,
            name,
            return_ty,
            params: params
                .iter()
                .map(|p| MethodParamDef {
                    name: p.name,
                    ty: CheckedType::from(&p.ty.data),
                })
                .collect(),
        }
    }
}

#[derive(Debug)]
pub struct MethodParamDef<'src> {
    pub name: Symbol<'src>,
    pub ty: CheckedType<'src>,
}

#[derive(Debug)]
pub struct ClassFieldDef<'src> {
    pub name: Symbol<'src>,
    pub ty: CheckedType<'src>,
}

// TODO Clone or not? => Store types in hashmap
#[derive(Debug, Clone, PartialEq)]
pub enum CheckedType<'src> {
    Int,
    Boolean,
    Void,
    TypeRef(Symbol<'src>),
    Array(Box<CheckedType<'src>>),
}

impl<'src> CheckedType<'src> {
    pub fn is_assignable_from(&self, other: &CheckedType<'src>) -> bool {
        self == other
    }
}

impl<'src> fmt::Display for CheckedType<'src> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use self::CheckedType::*;
        match self {
            Int => write!(f, "int"),
            Boolean => write!(f, "boolean"),
            Void => write!(f, "void"),
            TypeRef(name) => write!(f, "{}", name),
            Array(item) => write!(f, "{}[]", item),
        }
    }
}
