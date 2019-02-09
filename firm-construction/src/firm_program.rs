use super::safety;
use crate::{
    ref_eq::RefEq,
    runtime::Runtime,
    type_checking::type_system::{
        CheckedType, ClassDef, ClassFieldDef, ClassMethodBody, ClassMethodDef, TypeSystem,
    },
};
use libfirm_rs::{
    types::{ClassTy, MethodTyBuilder, PrimitiveTy, Ty, TyTrait},
    Entity, Graph,
};
use std::{
    cell::RefCell,
    collections::HashMap,
    rc::{Rc, Weak},
};

pub enum FirmEntity<'src, 'ast> {
    Method(FirmMethodP<'src, 'ast>),
    Class(FirmClassP<'src, 'ast>),
    Field(FirmFieldP<'src, 'ast>),
}

impl<'src, 'ast> Clone for FirmEntity<'src, 'ast> {
    fn clone(&self) -> Self {
        use self::FirmEntity::*;
        match self {
            Method(m) => Method(Rc::clone(m)),
            Class(m) => Class(Rc::clone(m)),
            Field(m) => Field(Rc::clone(m)),
        }
    }
}

pub struct FirmProgram<'src, 'ast> {
    pub classes: HashMap<RefEq<Rc<ClassDef<'src, 'ast>>>, FirmClassP<'src, 'ast>>,
    pub fields: HashMap<RefEq<Rc<ClassFieldDef<'src>>>, FirmFieldP<'src, 'ast>>,
    pub methods: HashMap<RefEq<Rc<ClassMethodDef<'src, 'ast>>>, FirmMethodP<'src, 'ast>>,
    pub entities: HashMap<Entity, FirmEntity<'src, 'ast>>,
    pub runtime: Rc<Runtime>,
    pub safety_flags: &'src [safety::Flag],
    pub known_types: RefCell<HashMap<CheckedType<'src>, Ty>>,
    pub type_system: &'src TypeSystem<'src, 'ast>,
}

pub type FirmClassP<'src, 'ast> = Rc<RefCell<FirmClass<'src, 'ast>>>;
pub struct FirmClass<'src, 'ast> {
    pub def: Rc<ClassDef<'src, 'ast>>,
    pub entity: Entity,
}

pub type FirmFieldP<'src, 'ast> = Rc<RefCell<FirmField<'src, 'ast>>>;
pub struct FirmField<'src, 'ast> {
    pub owning_class: Weak<RefCell<FirmClass<'src, 'ast>>>,
    pub def: Rc<ClassFieldDef<'src>>,
    pub entity: Entity,
}

pub type FirmMethodP<'src, 'ast> = Rc<RefCell<FirmMethod<'src, 'ast>>>;
pub struct FirmMethod<'src, 'ast> {
    pub owning_class: Weak<RefCell<FirmClass<'src, 'ast>>>,
    pub body: ClassMethodBody<'src, 'ast>,
    pub def: Rc<ClassMethodDef<'src, 'ast>>,
    pub entity: Entity,
    pub graph: Option<Graph>,
}

impl<'src, 'ast> FirmProgram<'src, 'ast> {
    pub fn new(
        type_system: &'src TypeSystem<'src, 'ast>,
        runtime: Rc<Runtime>,
        safety_flags: &'src [safety::Flag],
    ) -> FirmProgram<'src, 'ast> {
        let mut program = FirmProgram {
            classes: HashMap::new(),
            fields: HashMap::new(),
            methods: HashMap::new(),
            entities: HashMap::new(),
            known_types: RefCell::new(HashMap::new()),
            type_system,
            runtime,
            safety_flags,
        };
        program.add_type_system(type_system);
        program
    }

    fn add_type_system(&mut self, type_system: &'src TypeSystem<'src, 'ast>) {
        let mut classes = Vec::new();

        for class in type_system.defined_classes.values() {
            let class_name = class.name.as_str();
            let class_type = ClassTy::new(class_name);
            let class_entity = Entity::new_global(class_name, class_type.into());

            let firm_class = Rc::new(RefCell::new(FirmClass {
                def: Rc::clone(class),
                entity: class_entity,
            }));

            self.classes
                .insert(RefEq(Rc::clone(class)), Rc::clone(&firm_class));
            classes.push(Rc::clone(&firm_class));
            self.entities
                .insert(class_entity, FirmEntity::Class(firm_class));
        }

        for firm_class in &classes {
            let class = Rc::clone(&firm_class.borrow().def);
            let class_type = ClassTy::from(firm_class.borrow().entity.ty()).unwrap();

            for field in class.iter_fields() {
                self.add_field(class_type, Rc::clone(&firm_class), field);
            }

            for method in class.iter_methods() {
                if let ClassMethodBody::AST(_) = method.body {
                    self.add_method(class_type, Rc::clone(&firm_class), method);
                }
            }

            class_type.default_layout();
        }
    }

    fn add_field(
        &mut self,
        class_type: ClassTy,
        class: Rc<RefCell<FirmClass<'src, 'ast>>>,
        field: Rc<ClassFieldDef<'src>>,
    ) {
        let field_type = self
            .ty_from_checked_type(&field.ty)
            .expect("field type must be convertible to a Firm type");

        let field_entity = Entity::new_entity(
            class_type.into(),
            &format!("{}$F${}", class.borrow().def.name, field.name),
            field_type,
        );
        let firm_field = Rc::new(RefCell::new(FirmField {
            owning_class: Rc::downgrade(&class),
            def: Rc::clone(&field),
            entity: field_entity,
        }));

        self.fields.insert(RefEq(field), Rc::clone(&firm_field));
        self.entities
            .insert(field_entity, FirmEntity::Field(firm_field));
    }

    fn add_method(
        &mut self,
        class_type: ClassTy,
        class: FirmClassP<'src, 'ast>,
        method: Rc<ClassMethodDef<'src, 'ast>>,
    ) {
        assert!(!method.is_static || (method.is_static && method.is_main));
        let mut method_ty_builder = MethodTyBuilder::new();

        // add this parameter
        if !method.is_main {
            method_ty_builder.add_param(PrimitiveTy::ptr().into());
        }

        for param in &method.params {
            let param_type = self
                .ty_from_checked_type(&param.ty)
                .expect("parameter must be convertible to a Firm type");
            method_ty_builder.add_param(param_type);
        }
        if let Some(return_ty) = self.ty_from_checked_type(&method.return_ty) {
            method_ty_builder.set_res(return_ty);
        }
        let method_type = method_ty_builder.build(!method.is_main);

        let method_name = if method.is_main {
            self.runtime.lib.mj_main_name().to_owned()
        } else {
            format!("{}$M${}", class.borrow().def.name, method.name)
        };

        let method_entity = Entity::new_entity(class_type.into(), &method_name, method_type.into());

        let firm_method = Rc::new(RefCell::new(FirmMethod {
            owning_class: Rc::downgrade(&class),
            def: Rc::clone(&method),
            entity: method_entity,
            graph: None,
            body: method.body,
        }));

        self.methods
            .insert(RefEq(Rc::clone(&method)), Rc::clone(&firm_method));

        self.entities
            .insert(method_entity, FirmEntity::Method(firm_method));
    }

    pub fn ty_from_checked_type(&self, ct: &CheckedType<'src>) -> Option<Ty> {
        if let CheckedType::Void = ct {
            return None;
        }

        if let Some(ty) = self.known_types.borrow().get(ct) {
            return Some(*ty);
        }

        let ty = match ct {
            CheckedType::Int => PrimitiveTy::i32().into(),
            CheckedType::Void => unreachable!(),
            CheckedType::TypeRef(class_def_id) => {
                let def = self.type_system.class(*class_def_id);
                let class = self.class(def).unwrap();
                let ty = class.borrow().entity.ty();
                ty.pointer().into()
                // If, for some unforeseen reason, the line above does not work,
                // return this instead: `PrimitiveTy::ptr().into()`.
                // However, this looses the class type we are pointing at.
                // We need this information in optimizations.
            }
            CheckedType::Array(checked_type) => {
                let array_data = self
                    .ty_from_checked_type(checked_type)
                    .expect("Arrays are never of type `void`")
                    .array();

                // TODO This is a shitty "generic" array, in theory we need only a unique type
                // definition inner type
                let safe_array = ClassTy::new_anon("$Array");
                if self.safety_flags.contains(&safety::Flag::CheckArrayBounds) {
                    safe_array.new_subentity("len", PrimitiveTy::i32());
                }
                safe_array.new_subentity("data", array_data);

                safe_array.default_layout();

                safe_array.pointer().into()
            }
            CheckedType::Boolean => PrimitiveTy::bool().into(),
            CheckedType::Null => unreachable!(),
            CheckedType::UnknownType(_) => unreachable!(),
        };

        self.known_types.borrow_mut().insert(ct.clone(), ty);

        self.known_types.borrow().get(ct).cloned()
    }

    pub fn field(&self, field_def: Rc<ClassFieldDef<'src>>) -> Result<FirmFieldP<'src, 'ast>, ()> {
        match self.fields.get(&RefEq(field_def)) {
            Some(field) => Ok(Rc::clone(field)),
            None => Err(()),
        }
    }

    pub fn method(
        &self,
        method_def: Rc<ClassMethodDef<'src, 'ast>>,
    ) -> Result<FirmMethodP<'src, 'ast>, ()> {
        match self.methods.get(&RefEq(method_def)) {
            Some(method) => Ok(Rc::clone(method)),
            None => Err(()),
        }
    }

    pub fn class(&self, class: Rc<ClassDef<'src, 'ast>>) -> Result<FirmClassP<'src, 'ast>, ()> {
        match self.classes.get(&RefEq(class)) {
            Some(class) => Ok(Rc::clone(class)),
            None => Err(()),
        }
    }

    pub fn by_entity(&self, entity: Entity) -> Option<FirmEntity<'src, 'ast>> {
        match self.entities.get(&entity) {
            Some(firm_entity) => Some(firm_entity.clone()),
            None => None,
        }
    }

    pub fn method_by_entity(&self, entity: Entity) -> Option<FirmMethodP<'src, 'ast>> {
        match self.by_entity(entity) {
            Some(FirmEntity::Method(firm_method)) => Some(firm_method),
            _ => None,
        }
    }
}
