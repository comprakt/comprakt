use crate::strtab::Symbol;
use std::{
    collections::{hash_map::Entry, HashMap},
    fmt,
};

#[derive(Debug)]
pub struct ClassDoesNotExist;
#[derive(Debug)]
pub struct ClassAlreadyDeclared;

#[derive(Debug, Default)]
pub struct TypeSystem<'src> {
    defined_classes: HashMap<Symbol<'src>, ClassDef<'src>>,
}

impl<'src> TypeSystem<'src> {
    pub fn is_type_defined(&self, name: Symbol<'src>) -> bool {
        self.defined_classes.contains_key(&name)
    }

    pub fn add_class_def<'a>(
        &'a mut self,
        class_def: ClassDef<'src>,
    ) -> Result<ClassDefId<'src>, ClassAlreadyDeclared> {
        match self.defined_classes.entry(class_def.name) {
            Entry::Occupied(_) => Err(ClassAlreadyDeclared),
            Entry::Vacant(e) => {
                let id = ClassDefId { id: class_def.name };
                e.insert(class_def);
                Ok(id)
            }
        }
    }

    pub fn class_mut(&mut self, id: ClassDefId<'src>) -> &mut ClassDef<'src> {
        self.defined_classes
            .get_mut(&id.id)
            .expect("Ids always point to existing classes")
    }

    pub fn class(&self, id: ClassDefId<'src>) -> &ClassDef<'src> {
        self.defined_classes
            .get(&id.id)
            .expect("Ids always point to existing classes")
    }

    pub fn lookup_class_mut(&mut self, name: Symbol<'src>) -> Option<&mut ClassDef<'src>> {
        self.defined_classes.get_mut(&name)
    }

    pub fn lookup_class(&self, name: Symbol<'src>) -> Option<(&ClassDef<'src>, ClassDefId<'src>)> {
        match self.defined_classes.get(&name) {
            Some(class) => {
                let id = ClassDefId { id: name };
                Some((class, id))
            }
            None => None,
        }
    }

    pub fn defined_classes(&self) -> &HashMap<Symbol<'_>, ClassDef<'_>> {
        &self.defined_classes
    }
}

/// A `ClassDefId` refers to a class definition.
///
/// Having an instance of this struct ensures that
/// the type system that issued this instance can
/// provide the definition of that class.
#[derive(Clone, Copy, Debug, PartialEq)]
pub struct ClassDefId<'src> {
    id: Symbol<'src>,
}

impl<'src, 'ts> From<ClassDefId<'src>> for CheckedType<'src> {
    fn from(id: ClassDefId<'src>) -> CheckedType<'src> {
        CheckedType::TypeRef(id)
    }
}

impl<'src> fmt::Display for ClassDefId<'src> {
    fn fmt(&self, f: &'_ mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.id)
    }
}

#[derive(Debug)]
pub struct ClassDef<'src> {
    // tracks how many redefinitions there are
    redefinitions: usize,
    pub name: Symbol<'src>,
    fields: HashMap<Symbol<'src>, ClassFieldDef<'src>>,
    methods: HashMap<Symbol<'src>, ClassMethodDef<'src>>,
}

impl<'src> ClassDef<'src> {
    pub fn new(name: Symbol<'src>) -> ClassDef<'src> {
        ClassDef {
            redefinitions: 0,
            name,
            fields: HashMap::new(),
            methods: HashMap::new(),
        }
    }

    pub fn get_new_redefinition_number(&mut self) -> usize {
        self.redefinitions += 1;
        self.redefinitions
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
    TypeRef(ClassDefId<'src>),
    UnknownType(Symbol<'src>),
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
            CheckedType::UnknownType(_) => true,
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
            UnknownType(name) => write!(f, "?{}", name),
            Array(item) => write!(f, "{}[]", item),
        }
    }
}
