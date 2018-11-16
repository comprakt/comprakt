use crate::{ast, context::Context, strtab::Symbol};
use std::collections::{hash_map::Entry, HashMap};

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

    pub fn build(context: &Context<'src>, program: &ast::Program<'src>) -> TypeSystem<'src> {
        let mut type_system = TypeSystem::new();

        for class_decl in &program.classes {
            let mut class_def = ClassDef::new(class_decl.name);

            for member in &class_decl.members {
                use crate::ast::ClassMemberKind::*;
                match &member.kind {
                    Field(ty) => {
                        class_def.add_field(ClassFieldDef {
                            name: member.name,
                            ty: CheckedType::from(&ty.data),
                        });
                    }
                    Method(ty, params, _) => {
                        let return_ty = CheckedType::from(&ty.data);
                        class_def.add_method(ClassMethodDef::new(
                            member.name,
                            &params,
                            return_ty,
                            true,
                        ));
                    }
                    MainMethod(params, _) => {
                        class_def.add_method(ClassMethodDef::new(
                            member.name,
                            &params,
                            CheckedType::Void,
                            true,
                        ));
                    }
                }
            }

            type_system.add_class_def(class_def);
        }

        type_system
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
    fn new(
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
#[derive(Debug, Clone)]
pub enum CheckedType<'src> {
    Int,
    Boolean,
    Void,
    TypeRef(Symbol<'src>),
    Array(Box<CheckedType<'src>>),
}
