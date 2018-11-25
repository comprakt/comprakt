use crate::strtab::StringTable;

use super::{checker::SemanticContext, type_system::*};

pub struct BuiltinTypes<'src> {
    pub string: CheckedType<'src>,
}

impl<'src> BuiltinTypes<'src> {
    pub fn add_to<'ts>(
        type_system: &'ts mut TypeSystem<'src>,
        strtab: &'_ mut StringTable<'src>,
        context: &'_ mut SemanticContext<'_, 'src>,
    ) -> BuiltinTypes<'src> {
        let int_ty = CheckedType::Int;

        let reader_class_id = {
            let mut reader_class_def = ClassDef::new(strtab.intern("$Reader"));
            reader_class_def.comparable = false;
            reader_class_def
                .add_method(ClassMethodDef {
                    name: strtab.intern("read"),
                    params: vec![],
                    return_ty: int_ty.clone(),
                    is_static: false,
                    is_main: false,
                })
                .unwrap();
            type_system.add_class_def(reader_class_def).unwrap()
        };

        let writer_class_id = {
            let arg_sym = strtab.intern("data");

            let mut writer_class_def = ClassDef::new(strtab.intern("$Writer"));
            writer_class_def.comparable = false;
            writer_class_def
                .add_method(ClassMethodDef {
                    name: strtab.intern("println"),
                    params: vec![MethodParamDef::new(arg_sym, int_ty.clone())],
                    return_ty: CheckedType::Void,
                    is_static: false,
                    is_main: false,
                })
                .unwrap();
            writer_class_def
                .add_method(ClassMethodDef {
                    name: strtab.intern("write"),
                    params: vec![MethodParamDef::new(arg_sym, int_ty.clone())],
                    return_ty: CheckedType::Void,
                    is_static: false,
                    is_main: false,
                })
                .unwrap();
            writer_class_def
                .add_method(ClassMethodDef {
                    name: strtab.intern("flush"),
                    params: vec![],
                    return_ty: CheckedType::Void,
                    is_static: false,
                    is_main: false,
                })
                .unwrap();
            type_system.add_class_def(writer_class_def).unwrap()
        };

        let system_class_id = {
            let mut system_class_def = ClassDef::new(strtab.intern("$System"));
            system_class_def.comparable = false;
            system_class_def
                .add_field(ClassFieldDef {
                    name: strtab.intern("in"),
                    ty: reader_class_id.into(),
                    can_write: false,
                })
                .unwrap();
            system_class_def
                .add_field(ClassFieldDef {
                    name: strtab.intern("out"),
                    ty: writer_class_id.into(),
                    can_write: false,
                })
                .unwrap();
            type_system.add_class_def(system_class_def).unwrap()
        };

        context
            .global_vars
            .insert(strtab.intern("System"), system_class_id.into());

        let string_class_def = ClassDef::new(strtab.intern("$String"));
        let string_class_id = type_system.add_class_def(string_class_def).unwrap();

        BuiltinTypes {
            string: string_class_id.into(),
        }
    }
}
