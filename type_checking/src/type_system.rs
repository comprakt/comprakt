use parser::ast;
use std::{
    collections::{hash_map::Entry, HashMap},
    fmt,
    rc::Rc,
};
use strtab::Symbol;

#[derive(Debug)]
pub struct ClassDoesNotExist;
#[derive(Debug)]
pub struct ClassAlreadyDeclared;

#[derive(Debug, Default)]
pub struct TypeSystem<'src, 'ast> {
    pub defined_classes: HashMap<Symbol<'src>, Rc<ClassDef<'src, 'ast>>>,
}

impl<'src, 'ast> TypeSystem<'src, 'ast> {
    pub fn is_type_defined(&self, name: Symbol<'src>) -> bool {
        self.defined_classes.contains_key(&name)
    }

    pub fn add_class_def<'a>(
        &'a mut self,
        class_def: ClassDef<'src, 'ast>,
    ) -> Result<ClassDefId<'src>, ClassAlreadyDeclared> {
        match self.defined_classes.entry(class_def.name) {
            Entry::Occupied(_) => Err(ClassAlreadyDeclared),
            Entry::Vacant(e) => {
                let id = ClassDefId { id: class_def.name };
                e.insert(Rc::new(class_def));
                Ok(id)
            }
        }
    }

    pub fn class_mut(&mut self, id: ClassDefId<'src>) -> &mut ClassDef<'src, 'ast> {
        self.defined_classes
            .get_mut(&id.id)
            .and_then(Rc::get_mut)
            .expect("Ids always point to existing classes")
    }

    pub fn class(&self, id: ClassDefId<'src>) -> Rc<ClassDef<'src, 'ast>> {
        self.defined_classes
            .get(&id.id)
            .map(Rc::clone)
            .expect("Ids always point to existing classes")
    }

    pub fn lookup_class_mut(&mut self, name: Symbol<'src>) -> Option<&mut ClassDef<'src, 'ast>> {
        self.defined_classes.get_mut(&name).and_then(Rc::get_mut)
    }

    pub fn lookup_class(
        &self,
        name: Symbol<'src>,
    ) -> Option<(Rc<ClassDef<'src, 'ast>>, ClassDefId<'src>)> {
        match self.defined_classes.get(&name) {
            Some(class) => {
                let id = ClassDefId { id: name };
                Some((Rc::clone(class), id))
            }
            None => None,
        }
    }

    pub fn defined_classes(&self) -> &HashMap<Symbol<'_>, Rc<ClassDef<'_, '_>>> {
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

impl<'src> ClassDefId<'src> {
    pub fn as_str(&self) -> &str {
        self.id.as_str()
    }

    pub fn id(&self) -> Symbol<'src> {
        self.id
    }
}

#[derive(Debug)]
pub struct ClassDef<'src, 'ast> {
    // tracks how many redefinitions there are
    redefinitions: usize,
    pub name: Symbol<'src>,
    fields: HashMap<Symbol<'src>, Rc<ClassFieldDef<'src>>>,
    methods: HashMap<Symbol<'src>, Rc<ClassMethodDef<'src, 'ast>>>,
    pub comparable: bool,
}

impl<'src, 'ast> ClassDef<'src, 'ast> {
    pub fn new(name: Symbol<'src>) -> ClassDef<'src, 'ast> {
        ClassDef {
            redefinitions: 0,
            name,
            fields: HashMap::new(),
            methods: HashMap::new(),
            comparable: true,
        }
    }

    pub fn get_new_redefinition_number(&mut self) -> usize {
        self.redefinitions += 1;
        self.redefinitions
    }

    pub fn add_field(&mut self, field: ClassFieldDef<'src>) -> Result<(), ()> {
        match self.fields.entry(field.name) {
            Entry::Occupied(_) => return Err(()),
            Entry::Vacant(e) => e.insert(Rc::new(field)),
        };
        Ok(())
    }

    pub fn field_names(&self) -> Vec<&Symbol<'src>> {
        self.fields.keys().collect()
    }

    pub fn field(&self, name: Symbol<'src>) -> Option<Rc<ClassFieldDef<'src>>> {
        self.fields.get(&name).map(Rc::clone)
    }

    pub fn iter_fields<'a>(&'a self) -> impl Iterator<Item = Rc<ClassFieldDef<'src>>> + 'a {
        self.fields.iter().map(|(_, c)| Rc::clone(c))
    }

    pub fn iter_methods<'a>(&'a self) -> impl Iterator<Item = Rc<ClassMethodDef<'src, 'ast>>> + 'a {
        self.methods.iter().map(|(_, c)| Rc::clone(c))
    }

    pub fn add_method(&mut self, method: ClassMethodDef<'src, 'ast>) -> Result<(), ()> {
        match self.methods.entry(method.name) {
            Entry::Occupied(_) => return Err(()),
            Entry::Vacant(e) => e.insert(Rc::new(method)),
        };
        Ok(())
    }

    pub fn method(&self, name: Symbol<'src>) -> Option<Rc<ClassMethodDef<'src, 'ast>>> {
        self.methods.get(&name).map(Rc::clone)
    }
}

#[derive(Debug, Clone, Copy)]
pub enum BuiltinMethodBody {
    SystemOutPrintln,
    SystemOutWrite,
    SystemOutFlush,
    SystemInRead,
}

pub type Body<'src, 'ast> = asciifile::spanned::Spanned<'src, ast::Block<'src>>;

#[derive(Debug, Clone, Copy)]
pub enum ClassMethodBody<'src, 'ast> {
    Builtin(BuiltinMethodBody),
    AST(&'ast Body<'src, 'ast>),
}

#[derive(Debug)]
pub struct ClassMethodDef<'src, 'ast> {
    /// Name of the method
    pub name: Symbol<'src>,
    pub body: ClassMethodBody<'src, 'ast>,
    /// params does not include `this` for non-static / non-main methods
    pub params: Vec<Rc<MethodParamDef<'src>>>,
    pub return_ty: CheckedType<'src>,
    pub is_static: bool,
    pub is_main: bool,
}

impl<'src, 'ast> ClassMethodDef<'src, 'ast> {
    pub fn new(
        name: Symbol<'src>,
        body: ClassMethodBody<'src, 'ast>,
        params: Vec<MethodParamDef<'src>>,
        return_ty: CheckedType<'src>,
        is_static: bool,
    ) -> ClassMethodDef<'src, 'ast> {
        ClassMethodDef {
            is_static,
            name,
            body,
            return_ty,
            params: params.into_iter().map(Rc::new).collect(),
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
    /// Name of the field
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

    pub fn inner_type(&self) -> Option<&CheckedType<'src>> {
        match self {
            CheckedType::Array(ty) => Some(&*ty),
            _ => None,
        }
    }

    pub fn is_assignable_from(
        &self,
        other: &CheckedType<'src>,
        ts: &'_ TypeSystem<'src, '_>,
    ) -> bool {
        use self::CheckedType::*;

        match self {
            // dont generate errors for unknown types as they are invalid anyways
            UnknownType(_) => true,
            Int | Boolean => self == other,
            // nothing is assignable to null or void, not even expressions of type void or null.
            // This does not really matter though as void is not an inhibited type.
            // However, to improve error messages, we allow assigning void to void.
            Null | Void => self == other,
            Array(item_ty) => match other {
                Null => true,
                Array(other_item_ty) => item_ty.is_assignable_from(other_item_ty, ts),
                _ => false,
            },
            TypeRef(class_id) => {
                let class_def = ts.class(*class_id);
                class_def.comparable && (self == other || *other == Null)
            }
        }
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
