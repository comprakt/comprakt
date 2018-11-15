use std::{collections::HashMap, rc::Rc};
use crate::{strtab::Symbol};
use std::collections::hash_map::Entry;

#[derive(Debug)]
pub struct TypeSystem {
    defined_classes: HashMap<Symbol, ClassDef>,
}

impl TypeSystem {
    pub fn new() -> TypeSystem {
        TypeSystem { defined_classes: HashMap::new() }
    }

    pub fn is_type_defined(&self, name: Symbol) -> bool {
        self.defined_classes.contains_key(&name)
    }

    pub fn add_class_def(&mut self, class_def: ClassDef) -> Result<(), ()> {
        let key = Rc::clone(&class_def.name);
        match self.defined_classes.entry(key) {
            Entry::Occupied(e) => return Err(()),
            Entry::Vacant(e) => e.insert(class_def),
        };
        Ok(())
    }

    pub fn resolve_type_reference(&self, reference: &CheckedTypeRef) -> Option<&ClassDef> {
        self.defined_classes.get(&reference.ty_name)
    }
}

#[derive(Debug)]
pub struct ClassDef {
    pub name: Symbol,
    fields: HashMap<Symbol, ClassFieldDef>,
    methods: HashMap<Symbol, ClassMethodDef>,
}

impl ClassDef {
    pub fn new(name: Symbol) -> ClassDef {
        ClassDef {
            name: name,
            fields: HashMap::new(),
            methods: HashMap::new(),
        }
    }

    pub fn add_field(&mut self, field: ClassFieldDef) -> Result<(), ()> {
        let key = Rc::clone(&field.name);
        match self.fields.entry(key) {
            Entry::Occupied(e) => return Err(()),
            Entry::Vacant(e) => e.insert(field),
        };
        Ok(())
    }

    pub fn get_field(&self, name: &Symbol) -> Result<&ClassFieldDef, ()> {
        self.fields.get(name)
    }

    pub fn add_method(&mut self, method: ClassMethodDef) -> Result<(), ()> {
        let key = Rc::clone(&method.name);
        match self.methods.entry(key) {
            Entry::Occupied(e) => return Err(()),
            Entry::Vacant(e) => e.insert(method),
        };
        Ok(())
    }

    pub fn get_method(&self, name: &Symbol) -> Result<&ClassMethodDef, ()> {
        self.methods.get(name)
    }

}

#[derive(Debug)]
pub struct ClassMethodDef {
    pub name: Symbol,
    pub params: Vec<MethodParamDef>,
    pub return_ty: CheckedType,
    pub is_static: bool,
}

#[derive(Debug)]
pub struct MethodParamDef {
    pub name: Symbol,
    pub ty: CheckedType,
}

#[derive(Debug)]
pub struct ClassFieldDef {
    pub name: Symbol,
    pub ty: CheckedType,
}

#[derive(Debug)]
pub enum CheckedType {
    Int,
    Boolean,
    Void,
    TypeRef(CheckedTypeRef),
    Array(CheckedArrayType),
}

#[derive(Debug)]
pub struct CheckedArrayType {
    // is Int, Boolean or TypeRef
    base_item_type: Box<CheckedType>,
    depth: u64, // at least 1
}

impl CheckedArrayType {
    pub fn get_item_type(&self) -> &CheckedType {
        self.base_item_type
    }
}

#[derive(Debug)]
pub struct CheckedTypeRef {
    pub ty_name: Symbol,
}

impl CheckedTypeRef {
    pub fn new(name: Symbol) -> CheckedTypeRef {
        CheckedTypeRef { ty_name: name, }
    }
}