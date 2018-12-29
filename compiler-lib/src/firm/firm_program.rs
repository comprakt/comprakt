use super::type_translation::*;
use crate::{
    ref_eq::RefEq,
    type_checking::type_system::{
        ClassDef, ClassFieldDef, ClassMethodBody, ClassMethodDef, TypeSystem,
    },
};
use libfirm_rs::{
    entity::Entity,
    graph::Graph,
    types::{ClassTy, MethodTyBuilder, PrimitiveTy},
};
use std::{
    cell::RefCell,
    collections::HashMap,
    rc::{Rc, Weak},
};

pub struct FirmProgram<'src, 'ast> {
    pub classes: HashMap<RefEq<Rc<ClassDef<'src, 'ast>>>, Rc<RefCell<FirmClass<'src, 'ast>>>>,
    pub fields: HashMap<RefEq<Rc<ClassFieldDef<'src>>>, Rc<RefCell<FirmField<'src, 'ast>>>>,
    pub methods:
        HashMap<RefEq<Rc<ClassMethodDef<'src, 'ast>>>, Rc<RefCell<FirmMethod<'src, 'ast>>>>,
}

impl<'src, 'ast> FirmProgram<'src, 'ast> {
    pub fn field(
        &self,
        field_def: Rc<ClassFieldDef<'src>>,
    ) -> Result<Rc<RefCell<FirmField<'src, 'ast>>>, ()> {
        match self.fields.get(&RefEq(field_def)) {
            Some(field) => Ok(Rc::clone(field)),
            None => Err(()),
        }
    }

    pub fn method(
        &self,
        method_def: Rc<ClassMethodDef<'src, 'ast>>,
    ) -> Result<Rc<RefCell<FirmMethod<'src, 'ast>>>, ()> {
        match self.methods.get(&RefEq(method_def)) {
            Some(method) => Ok(Rc::clone(method)),
            None => Err(()),
        }
    }

    pub fn class(
        &self,
        class: Rc<ClassDef<'src, 'ast>>,
    ) -> Result<Rc<RefCell<FirmClass<'src, 'ast>>>, ()> {
        match self.classes.get(&RefEq(class)) {
            Some(class) => Ok(Rc::clone(class)),
            None => Err(()),
        }
    }
}

pub struct FirmClass<'src, 'ast> {
    pub def: &'src ClassDef<'src, 'ast>,
    pub entity: Entity,
}

pub struct FirmField<'src, 'ast> {
    pub owning_class: Weak<RefCell<FirmClass<'src, 'ast>>>,
    pub def: Rc<ClassFieldDef<'src>>,
    pub entity: Entity,
}

pub struct FirmMethod<'src, 'ast> {
    pub owning_class: Weak<RefCell<FirmClass<'src, 'ast>>>,
    pub body: ClassMethodBody<'src, 'ast>,
    pub def: Rc<ClassMethodDef<'src, 'ast>>,
    pub entity: Entity,
    pub graph: Option<Graph>,
}

impl<'src, 'ast> FirmProgram<'src, 'ast> {
    pub fn new(type_system: &'src TypeSystem<'src, 'ast>) -> FirmProgram<'src, 'ast> {
        let mut program = FirmProgram {
            classes: HashMap::new(),
            fields: HashMap::new(),
            methods: HashMap::new(),
        };
        program.add_type_system(type_system);
        program
    }

    fn add_type_system(&mut self, type_system: &'src TypeSystem<'src, 'ast>) {
        for class in type_system.defined_classes.values() {
            let class_name = class.name.as_str();
            let class_type = ClassTy::new(class_name);
            let class_entity = Entity::new_global(class_name, class_type.into());

            let firm_class = Rc::new(RefCell::new(FirmClass {
                def: class,
                entity: class_entity,
            }));

            for field in class.iter_fields() {
                self.add_field(class_type, Rc::clone(&firm_class), field);
            }

            for method in class.iter_methods() {
                if let ClassMethodBody::AST(_) = method.body {
                    self.add_method(class_type, Rc::clone(&firm_class), method);
                }
            }

            class_type.default_layout();
            self.classes.insert(RefEq(Rc::clone(class)), firm_class);
        }
    }

    fn add_field(
        &mut self,
        class_type: ClassTy,
        class: Rc<RefCell<FirmClass<'src, 'ast>>>,
        field: Rc<ClassFieldDef<'src>>,
    ) {
        let field_type =
            ty_from_checked_type(&field.ty).expect("field type must be convertible to a Firm type");

        let field_entity = Entity::new_entity(
            class_type.into(),
            &format!("{}.F.{}", class.borrow().def.name, field.name),
            field_type.into(),
        );

        self.fields.insert(
            RefEq(Rc::clone(&field)),
            Rc::new(RefCell::new(FirmField {
                owning_class: Rc::downgrade(&class),
                def: field,
                entity: field_entity,
            })),
        );
    }

    fn add_method(
        &mut self,
        class_type: ClassTy,
        class: Rc<RefCell<FirmClass<'src, 'ast>>>,
        method: Rc<ClassMethodDef<'src, 'ast>>,
    ) {
        assert!(!method.is_static || (method.is_static && method.is_main));
        let mut method_ty_builder = MethodTyBuilder::new();

        // add this parameter
        if !method.is_main {
            method_ty_builder.add_param(PrimitiveTy::ptr().into());
        }

        for param in &method.params {
            let param_type = ty_from_checked_type(&param.ty)
                .expect("parameter must be convertible to a Firm type");
            method_ty_builder.add_param(param_type);
        }
        if let Some(return_ty) = ty_from_checked_type(&method.return_ty) {
            method_ty_builder.set_res(return_ty);
        }
        let method_type = method_ty_builder.build(!method.is_main);

        let method_name = if method.is_main {
            "mj_main".to_owned()
        } else {
            format!("{}.M.{}", class.borrow().def.name, method.name)
        };

        let method_entity = Entity::new_entity(class_type.into(), &method_name, method_type.into());

        self.methods.insert(
            RefEq(Rc::clone(&method)),
            Rc::new(RefCell::new(FirmMethod {
                owning_class: Rc::downgrade(&class),
                def: Rc::clone(&method),
                entity: method_entity,
                graph: None,
                body: method.body,
            })),
        );
    }
}
