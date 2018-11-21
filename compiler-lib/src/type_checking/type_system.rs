use crate::strtab::Symbol;
use std::{
    collections::{hash_map::Entry, HashMap},
    fmt,
};

#[derive(Debug, Default)]
pub struct TypeSystem<'src> {
    defined_classes: HashMap<Symbol<'src>, ClassDef<'src>>,
}

#[derive(Debug)]
pub struct ClassDoesNotExist;
#[derive(Debug)]
pub struct ClassAlreadyDeclared;

impl<'src> TypeSystem<'src> {
    pub fn is_type_defined(&self, name: Symbol<'src>) -> bool {
        self.defined_classes.contains_key(&name)
    }

    pub fn add_class_def(&mut self, class_def: ClassDef<'src>) -> Result<(), ClassAlreadyDeclared> {
        match self.defined_classes.entry(class_def.name) {
            Entry::Occupied(_) => return Err(ClassAlreadyDeclared),
            Entry::Vacant(e) => e.insert(class_def),
        };
        Ok(())
    }

    pub fn update_existing_class_def(
        &mut self,
        class_def: ClassDef<'src>,
    ) -> Result<(), ClassDoesNotExist> {
        match self.defined_classes.entry(class_def.name) {
            Entry::Occupied(mut e) => e.insert(class_def),
            Entry::Vacant(_) => return Err(ClassDoesNotExist),
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

    pub fn field(&self, name: Symbol<'src>) -> Option<&ClassFieldDef<'src>> {
        self.fields.get(&name)
    }

    pub fn add_method(&mut self, method: ClassMethodDef<'src>) -> Result<(), ()> {
        match self.methods.entry(method.name) {
            Entry::Occupied(_) => return Err(()),
            Entry::Vacant(e) => e.insert(method),
        };
        Ok(())
    }

    pub fn method(&self, name: Symbol<'src>) -> Option<&ClassMethodDef<'src>> {
        self.methods.get(&name)
    }

    pub fn ty(&self) -> CheckedType<'src> {
        CheckedType::TypeRef(self.name)
    }
}

#[derive(Debug)]
pub struct ClassMethodDef<'src> {
    pub name: Symbol<'src>,
    pub params: Vec<MethodParamDef<'src>>,
    pub return_ty: CheckedType<'src>,
    pub is_static: bool,
    pub is_main: bool,
}

impl<'src> ClassMethodDef<'src> {
    pub fn new(
        name: Symbol<'src>,
        params: Vec<MethodParamDef<'src>>,
        return_ty: CheckedType<'src>,
        is_static: bool,
    ) -> ClassMethodDef<'src> {
        ClassMethodDef {
            is_static,
            name,
            return_ty,
            params,
            is_main: is_static,
        }
    }
}

#[derive(Debug)]
pub struct MethodParamDef<'src> {
    pub name: Symbol<'src>,
    pub ty: CheckedType<'src>,
}

impl<'src> MethodParamDef<'src> {
    pub fn new(name: Symbol<'src>, ty: CheckedType<'src>) -> MethodParamDef<'src> {
        MethodParamDef { name, ty }
    }
}

#[derive(Debug)]
pub struct ClassFieldDef<'src> {
    pub name: Symbol<'src>,
    pub ty: CheckedType<'src>,
    pub can_write: bool,
}

// FIXME Clone or not? => Store types in hashmap
#[derive(Debug, Clone, PartialEq)]
pub enum CheckedType<'src> {
    Int,
    Boolean,
    Void,
    Null,
    TypeRef(Symbol<'src>),
    Array(Box<CheckedType<'src>>),
}

impl<'src> CheckedType<'src> {
    pub fn create_array_type(item_type: CheckedType<'src>, dimension: u64) -> CheckedType<'src> {
        if dimension == 0 {
            item_type
        } else {
            CheckedType::Array(Box::new(CheckedType::create_array_type(
                item_type,
                dimension - 1,
            )))
        }
    }

    pub fn is_nullable(&self) -> bool {
        match self {
            CheckedType::TypeRef(_) => true,
            CheckedType::Array(_) => true,
            CheckedType::Null => true,
            _ => false,
        }
    }
    pub fn is_assignable_from(&self, other: &CheckedType<'src>) -> bool {
        // FIXME there must be a better way
        (match other {
            CheckedType::Null => self.is_nullable(),
            _ => false,
        }) || self == other
    }
}

impl<'src> fmt::Display for CheckedType<'src> {
    fn fmt(&self, f: &'_ mut fmt::Formatter<'_>) -> fmt::Result {
        use self::CheckedType::*;
        match self {
            Int => write!(f, "int"),
            Boolean => write!(f, "boolean"),
            Void => write!(f, "void"),
            Null => write!(f, "null"),
            TypeRef(name) => write!(f, "{}", name),
            Array(item) => write!(f, "{}[]", item),
        }
    }
}
