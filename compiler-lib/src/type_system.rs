use std::{collections::HashMap};
use crate::{strtab::Symbol};
use std::collections::hash_map::Entry;

#[derive(Debug)]
pub struct TypeSystem<'t> {
    defined_classes: HashMap<Symbol<'t>, ClassDef<'t>>,
}

impl<'t> TypeSystem<'t> {
    pub fn new() -> TypeSystem<'t> {
        TypeSystem { defined_classes: HashMap::new() }
    }

    pub fn is_type_defined(&self, name: Symbol<'t>) -> bool {
        self.defined_classes.contains_key(&name)
    }

    pub fn add_class_def(&mut self, class_def: ClassDef<'t>) -> Result<(), ()> {
        match self.defined_classes.entry(class_def.name) {
            Entry::Occupied(e) => return Err(()),
            Entry::Vacant(e) => e.insert(class_def),
        };
        Ok(())
    }

    pub fn resolve_type_reference(&self, reference: &CheckedTypeRef<'t>) -> Option<&ClassDef<'t>> {
        self.defined_classes.get(&reference.ty_name)
    }
}

#[derive(Debug)]
pub struct ClassDef<'t> {
    pub name: Symbol<'t>,
    fields: HashMap<Symbol<'t>, ClassFieldDef<'t>>,
    methods: HashMap<Symbol<'t>, ClassMethodDef<'t>>,
}

impl<'t> ClassDef<'t> {
    pub fn new(name: Symbol<'t>) -> ClassDef<'t> {
        ClassDef {
            name: name,
            fields: HashMap::new(),
            methods: HashMap::new(),
        }
    }

    pub fn add_field(&mut self, field: ClassFieldDef<'t>) -> Result<(), ()> {
        match self.fields.entry(field.name) {
            Entry::Occupied(_) => return Err(()),
            Entry::Vacant(e) => e.insert(field),
        };
        Ok(())
    }

    pub fn get_field(&self, name: &Symbol<'t>) -> Option<&ClassFieldDef<'t>> {
        self.fields.get(name)
    }

    pub fn add_method(&mut self, method: ClassMethodDef<'t>) -> Result<(), ()> {
        match self.methods.entry(method.name) {
            Entry::Occupied(_) => return Err(()),
            Entry::Vacant(e) => e.insert(method),
        };
        Ok(())
    }

    pub fn get_method(&self, name: &Symbol<'t>) -> Option<&ClassMethodDef<'t>> {
        self.methods.get(name)
    }

}

#[derive(Debug)]
pub struct ClassMethodDef<'t> {
    pub name: Symbol<'t>,
    pub params: Vec<MethodParamDef<'t>>,
    pub return_ty: CheckedType<'t>,
    pub is_static: bool,
}

#[derive(Debug)]
pub struct MethodParamDef<'t> {
    pub name: Symbol<'t>,
    pub ty: CheckedType<'t>,
}

#[derive(Debug)]
pub struct ClassFieldDef<'t> {
    pub name: Symbol<'t>,
    pub ty: CheckedType<'t>,
}

#[derive(Debug)]
pub enum CheckedType<'t> {
    Int,
    Boolean,
    Void,
    TypeRef(CheckedTypeRef<'t>),
    Array(Box<CheckedType<'t>>),
}

#[derive(Debug)]
pub struct CheckedTypeRef<'t> {
    pub ty_name: Symbol<'t>,
}

impl<'t> CheckedTypeRef<'t> {
    pub fn new(name: Symbol<'t>) -> CheckedTypeRef<'t> {
        CheckedTypeRef { ty_name: name, }
    }
}