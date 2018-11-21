use crate::{
    asciifile::{Span, Spanned},
    ast,
    context::Context,
    semantics::SemanticError,
    strtab::{StringTable, Symbol},
};
use std::{
    collections::{HashMap, HashSet},
    hash::Hash,
};

pub mod expr_typechecker;
pub mod method_body_typechecker;
pub mod type_system;

use self::{method_body_typechecker::MethodBodyTypeChecker, type_system::*};

#[derive(Eq)]
pub struct RefEquality<'a, T>(&'a T);

impl<'a, T> std::hash::Hash for RefEquality<'a, T> {
    fn hash<H>(&self, state: &mut H)
    where
        H: std::hash::Hasher,
    {
        (self.0 as *const T).hash(state)
    }
}

impl<'a, 'b, T> PartialEq<RefEquality<'b, T>> for RefEquality<'a, T> {
    fn eq(&self, other: &'_ RefEquality<'b, T>) -> bool {
        self.0 as *const T == other.0 as *const T
    }
}

pub fn check<'a, 'src>(
    mut strtab: &'_ mut StringTable<'src>,
    ast: &'a ast::AST<'src>,
    context: &Context<'src>,
) -> TypeSystem<'src> {
    let mut sem_context = SemanticContext::new(context);

    match ast {
        ast::AST::Empty => TypeSystem::default(),
        ast::AST::Program(program) => {
            let mut type_system = TypeSystem::default();
            let mut type_analysis = TypeAnalysis::new();

            let builtin_types = add_builtin_types(&mut strtab, &mut type_system, &mut sem_context);

            add_types_from_ast(
                &mut strtab,
                &mut type_system,
                &mut type_analysis,
                &builtin_types,
                &sem_context,
                program,
            );

            for class_decl in &program.classes {
                MethodBodyTypeChecker::check_methods(
                    class_decl,
                    &type_system,
                    &mut type_analysis,
                    &sem_context,
                );
            }

            type_system
        }
    }
}

pub struct SemanticContext<'ctx, 'src> {
    pub context: &'ctx Context<'src>,
    pub global_vars: HashMap<Symbol<'src>, CheckedType<'src>>,
}

impl<'ctx, 'src> SemanticContext<'ctx, 'src> {
    pub fn new(context: &'ctx Context<'src>) -> SemanticContext<'ctx, 'src> {
        SemanticContext {
            context,
            global_vars: HashMap::new(),
        }
    }

    pub fn report_error(&self, span: &Span<'src>, error: SemanticError) {
        self.context.diagnostics.error(&Spanned::new(*span, error))
    }
}

fn add_types_from_ast<'ctx, 'sem, 'src, 'a>(
    strtab: &mut StringTable<'src>,
    type_system: &'sem mut TypeSystem<'src>,
    type_analysis: &'sem mut TypeAnalysis<'src, 'a>,
    builtin_types: &BuiltinTypes<'src>,
    context: &SemanticContext<'ctx, 'src>,
    program: &'a ast::Program<'src>,
) {
    for class_decl in &program.classes {
        // first pass: find all types and add them to the type system.
        // rename them if they have invalid names
        let name = match type_system.lookup_class_mut(class_decl.name.data) {
            Some(class_def) => {
                context.report_error(
                    &class_decl.span,
                    SemanticError::RedefinitionError {
                        kind: "class".to_string(),
                        name: class_decl.name.data.to_string(),
                    },
                );

                let id = class_def.get_new_redefinition_number() + 1;
                let string = format!("${}{}", class_decl.name.data, id);
                // IMPROVEMENT do not leak memory
                strtab.intern(Box::leak(Box::new(string)))
            }
            None => class_decl.name.data,
        };

        let (_, class_def_id) = type_system
            .add_class_def(ClassDef::new(name))
            .expect("The redefinition number ensures this cannot happen");

        type_analysis.decl_set_class_id(&class_decl.data, class_def_id);
    }

    for class_decl in &program.classes {
        // second pass: scan members of all types, check their type references against
        // the first pass

        let class_def_id = type_analysis
            .decl_get_class_id(&class_decl)
            .expect("Class def was attached to this class_decl in first pass");

        for member in &class_decl.members {
            use crate::ast::ClassMemberKind::*;
            match &member.kind {
                Field(ty) => {
                    let field_type = type_analysis.checked_type_from_ty(
                        &ty.data,
                        context,
                        type_system,
                        VoidIs::Forbidden,
                    );

                    type_system
                        .get_class_mut(class_def_id)
                        .add_field(ClassFieldDef {
                            name: member.name,
                            ty: field_type,
                            can_write: true,
                        })
                        .unwrap_or_else(|_| {
                            context.report_error(
                                &member.span,
                                SemanticError::RedefinitionError {
                                    kind: "field".to_string(),
                                    name: member.name.to_string(),
                                },
                            )
                        });
                }
                Method(_, params, _) | MainMethod(params, _) => {
                    let (is_static, is_main, return_ty) = match &member.kind {
                        Field(_) => panic!("impossible"),
                        Method(return_ty, _, _) => (
                            false,
                            false,
                            type_analysis.checked_type_from_ty(
                                &return_ty.data,
                                context,
                                &type_system,
                                VoidIs::Allowed,
                            ),
                        ),
                        MainMethod(_, _) => (true, true, CheckedType::Void),
                    };

                    let mut previous_params = HashSet::new();
                    let checked_params = params
                        .iter()
                        .filter_map(|p| {
                            if previous_params.contains(&p.name) {
                                context.report_error(
                                    &p.span,
                                    SemanticError::RedefinitionError {
                                        kind: "parameter".to_string(),
                                        name: p.name.to_string(),
                                    },
                                );
                                None
                            } else {
                                previous_params.insert(p.name);
                                let ty = match p.ty.data.basic.data {
                                    ast::BasicType::MainParam => {
                                        assert!(is_main);
                                        assert_eq!(p.ty.data.array_depth, 0);
                                        CheckedType::Array(box builtin_types.string.clone())
                                    }
                                    _ => type_analysis.checked_type_from_ty(
                                        &p.ty.data,
                                        context,
                                        type_system,
                                        VoidIs::Forbidden,
                                    ),
                                };
                                Some(MethodParamDef { name: p.name, ty })
                            }
                        })
                        .collect();

                    type_system
                        .get_class_mut(class_def_id)
                        .add_method(ClassMethodDef::new(
                            member.name,
                            checked_params,
                            return_ty,
                            is_static,
                        ))
                        .unwrap_or_else(|_| {
                            context.report_error(
                                &member.span,
                                SemanticError::RedefinitionError {
                                    kind: "method".to_string(),
                                    name: member.name.to_string(),
                                },
                            )
                        });
                }
            }
        }
    }
}

struct BuiltinTypes<'src> {
    string: CheckedType<'src>,
}

fn add_builtin_types<'src>(
    strtab: &'_ mut StringTable<'src>,
    type_system: &'_ mut TypeSystem<'src>,
    context: &'_ mut SemanticContext<'_, 'src>,
) -> BuiltinTypes<'src> {
    let arg_sym = strtab.intern("$InStream");

    let int_ty = CheckedType::Int;
    let mut reader_class_def = ClassDef::new(strtab.intern("$Reader"));
    reader_class_def
        .add_method(ClassMethodDef {
            name: strtab.intern("read"),
            params: vec![],
            return_ty: int_ty.clone(),
            is_static: false,
            is_main: false,
        })
        .unwrap();

    let mut writer_class_def = ClassDef::new(strtab.intern("$Writer"));
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

    let mut system_class_def = ClassDef::new(strtab.intern("$System"));
    system_class_def
        .add_field(ClassFieldDef {
            name: strtab.intern("in"),
            ty: reader_class_def.ty(),
            can_write: false,
        })
        .unwrap();
    system_class_def
        .add_field(ClassFieldDef {
            name: strtab.intern("out"),
            ty: writer_class_def.ty(),
            can_write: false,
        })
        .unwrap();
    context
        .global_vars
        .insert(strtab.intern("System"), system_class_def.ty());

    let string_class_def = ClassDef::new(strtab.intern("$String"));
    let string = string_class_def.ty();

    type_system.add_class_def(reader_class_def).unwrap();
    type_system.add_class_def(writer_class_def).unwrap();
    type_system.add_class_def(system_class_def).unwrap();
    type_system.add_class_def(string_class_def).unwrap();

    BuiltinTypes { string }
}

#[derive(Default)]
pub struct TypeAnalysis<'src, 'a> {
    class_types: HashMap<RefEquality<'a, ast::ClassDeclaration<'src>>, ClassDefId<'src>>,
}

#[derive(Debug, Clone, Copy)]
pub enum VoidIs {
    Allowed,
    Forbidden,
}

impl<'src, 'a> TypeAnalysis<'src, 'a> {
    pub fn new() -> TypeAnalysis<'src, 'a> {
        TypeAnalysis {
            class_types: HashMap::new(),
        }
    }

    pub fn decl_set_class_id(
        &mut self,
        class_decl: &'a ast::ClassDeclaration<'src>,
        name: ClassDefId<'src>,
    ) {
        self.class_types.insert(RefEquality(class_decl), name);
    }

    pub fn decl_get_class_id(
        &mut self,
        class_decl: &'_ ast::ClassDeclaration<'src>,
    ) -> Option<ClassDefId<'src>> {
        self.class_types.get(&RefEquality(class_decl)).cloned()
    }

    pub fn checked_type_from_basic_ty(
        &mut self,
        basic_ty: &Spanned<'src, ast::BasicType<'src>>,
        context: &SemanticContext<'_, 'src>,
        type_system: &TypeSystem<'src>,
        void_handling: VoidIs,
    ) -> CheckedType<'src> {
        use self::ast::BasicType::*;
        match &basic_ty.data {
            Int => CheckedType::Int,
            Boolean => CheckedType::Boolean,
            Void => match void_handling {
                VoidIs::Allowed => CheckedType::Void,
                VoidIs::Forbidden => {
                    context.report_error(&basic_ty.span, SemanticError::VoidNotAllowed);

                    CheckedType::Void
                }
            },
            Custom(name) => {
                if !type_system.is_type_defined(*name) {
                    context.report_error(
                        &basic_ty.span,
                        SemanticError::ClassDoesNotExist {
                            class_name: name.to_string(),
                        },
                    );
                }
                CheckedType::TypeRef(*name)
            }
            // Parser only yields MainParam in that one case we handle anyways
            MainParam => unreachable!(),
        }
    }

    pub fn checked_type_from_ty(
        &mut self,
        ty: &ast::Type<'src>,
        context: &SemanticContext<'_, 'src>,
        type_system: &TypeSystem<'src>,
        void_handling: VoidIs,
    ) -> CheckedType<'src> {
        let void_handling = if ty.array_depth > 0 {
            VoidIs::Forbidden
        } else {
            void_handling
        };

        let mut checked_ty =
            self.checked_type_from_basic_ty(&ty.basic, context, type_system, void_handling);

        for _ in 0..ty.array_depth {
            checked_ty = CheckedType::Array(Box::new(checked_ty));
        }

        checked_ty
    }
}
