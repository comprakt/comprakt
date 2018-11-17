use crate::{
    asciifile::{Span, Spanned},
    ast,
    context::Context,
    method_body_typechecker::*,
    type_system::*,
    strtab::{Symbol, StringTable},
};
use std::collections::HashMap;
use failure::Fail;

#[derive(Debug, Fail)]
pub enum SemanticError {
    #[fail(display = "redefinition of {} '{}'", kind, name)]
    RedefinitionError {
        kind: String, // "class", "parameter", ...
        name: String, // name of the parameter class...
    },

    #[fail(
        display = "Usage of the parameter '{}' of the main function",
        name
    )]
    MainMethodParamUsed { name: String },

    #[fail(display = "Static methods have to be called 'main'")]
    StaticMethodNotMain,

    #[fail(
        display = "{}. definition of a static method. Only one is allowed",
        amount
    )]
    MultipleStaticMethods { amount: u64 },

    #[fail(
        display = "non-static method '{}' cannot be referenced from a static context",
        method_name
    )]
    ThisMethodInvocationInStaticMethod { method_name: String },

    #[fail(display = "cannot call static method '{}'", method_name)]
    CannotCallStaticMethod { method_name: String },

    #[fail(display = "non-static variable 'this' cannot be referenced from a static context")]
    ThisInStaticMethod,

    #[fail(display = "condition must be boolean")]
    ConditionMustBeBoolean,

    #[fail(display = "cannot lookup var or field '{}'", name)]
    CannotLookupVarOrField { name: String },

    #[fail(
        display = "cannot access non static field '{}' in static method",
        field_name
    )]
    CannotAccessNonStaticFieldInStaticMethod { field_name: String },

    #[fail(display = "method cannot return a value")]
    VoidMethodCannotReturnValue,

    #[fail(display = "method must return a value of type '{}'", ty)]
    MethodMustReturnSomething { ty: String },

    #[fail(
        display = "invalid type: Expected expression of type '{}', but was of type '{}'",
        ty_expected,
        ty_expr
    )]
    InvalidType {
        ty_expected: String,
        ty_expr: String,
    },

    #[fail(display = "cannot reference class '{}' here", class_name)]
    InvalidReferenceToClass { class_name: String },

    #[fail(display = "class '{}' does not exist", class_name)]
    ClassDoesNotExist { class_name: String },

    #[fail(display = "cannot index non-array type '{}'", ty)]
    CannotIndexNonArrayType { ty: String },

    #[fail(
        display = "method '{}' does not exist on type '{}'",
        method_name,
        ty
    )]
    MethodDoesNotExistOnType { method_name: String, ty: String },

    #[fail(
        display = "field '{}' does not exist on type '{}'",
        field_name,
        ty
    )]
    FieldDoesNotExistOnType { field_name: String, ty: String },

    #[fail(
        display = "method argument count does not match: Expected {} arguments, but found {}",
        expected_args,
        actual_args
    )]
    MethodArgCountDoesNotMatch {
        expected_args: usize,
        actual_args: usize,
    },

    #[fail(
        display = "cannot compare values of type '{}' with values of type '{}'",
        ty1,
        ty2
    )]
    CannotCompareValuesOfType1WithType2 { ty1: String, ty2: String },
}

pub fn check<'a, 'src>(
    mut strtab: &'src mut StringTable<'src>,
    ast: &'a ast::AST<'src>,
    context: &'src Context<'src>,
) -> Result<TypeSystem<'src>, ()> {
    let mut sem_context = SemanticContext::new(context);

    match ast {
        ast::AST::Empty => Ok(TypeSystem::default()),
        ast::AST::Program(program) => {
            let mut type_system = build_type_system(&sem_context, program);

            add_system_types(&mut strtab, &mut type_system, &mut sem_context);

            for class_decl in &program.classes {
                MethodBodyTypeChecker::check_methods(class_decl, &type_system, &sem_context);
            }

            Ok(type_system)
        }
    }
}

pub struct SemanticContext<'src> {
    pub context: &'src Context<'src>,
    pub global_vars: HashMap<Symbol<'src>, CheckedType<'src>>,
}

impl<'src> SemanticContext<'src> {
    pub fn new(context: &'src Context<'src>) -> SemanticContext<'src> {
        SemanticContext { context, global_vars: HashMap::new() }
    }

    pub fn report_error(&self, span: &'src Span<'src>, error: SemanticError) {
        self.context.diagnostics.error(&Spanned::new(*span, error))
    }
}

fn build_type_system<'src>(
    context: &SemanticContext<'src>,
    program: &ast::Program<'src>,
) -> TypeSystem<'src> {
    let mut type_system = TypeSystem::default();

    for class_decl in &program.classes {
        // first pass: find all types
        let class_def = ClassDef::new(class_decl.name.data);
        type_system.add_class_def(class_def).unwrap_or_else(|_| {
            context.report_error(
                &class_decl.span,
                SemanticError::RedefinitionError {
                    kind: "class".to_string(),
                    name: class_decl.name.to_string(),
                },
            )
        });
    }

    for class_decl in &program.classes {
        // second pass: scan members of all types, check their type references against the first pass
        let mut class_def = ClassDef::new(class_decl.name.data);

        for member in &class_decl.members {
            use crate::ast::ClassMemberKind::*;
            match &member.kind {
                Field(ty) => {
                    class_def
                        .add_field(ClassFieldDef {
                            name: member.name,
                            ty: checked_type_from_ty(&ty.data, Some(context), &type_system),
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
                        Method(ty, _, _) => (false, false, checked_type_from_ty(&ty.data, Some(context), &type_system)),
                        MainMethod(_, _) => (true, true, CheckedType::Void),
                    };

                    // disable parameter checking in main methods, as "String" is not a valid reference to a type
                    let type_check_context = if is_main { None } else { Some(context) };

                    let checked_params = params
                        .iter()
                        .map(|p| MethodParamDef {
                            name: p.name,
                            ty: checked_type_from_ty(&p.ty.data, type_check_context, &type_system),
                        })
                        .collect();

                    class_def
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

        type_system.update_existing_class_def(class_def).unwrap();
    }

    type_system
}

fn add_system_types<'src>(strtab: &'_ mut StringTable<'src>, type_system: &'_ mut TypeSystem<'src>, context: &'_ mut SemanticContext<'src>) {
    let arg_sym = strtab.intern("$InStream");

    let int_ty = CheckedType::Int;
    let mut reader_class_def = ClassDef::new(strtab.intern("$Reader"));
    reader_class_def.add_method(ClassMethodDef {
        name: strtab.intern("read"),
        params: vec![],
        return_ty: int_ty.clone(),
        is_static: false,
        is_main: false,
    }).unwrap();

    let mut writer_class_def = ClassDef::new(strtab.intern("$Writer"));
    writer_class_def.add_method(ClassMethodDef {
        name: strtab.intern("println"),
        params: vec![ MethodParamDef::new(arg_sym, int_ty.clone()) ],
        return_ty: CheckedType::Void,
        is_static: false,
        is_main: false,
    }).unwrap();
    writer_class_def.add_method(ClassMethodDef {
        name: strtab.intern("write"),
        params: vec![ MethodParamDef::new(arg_sym, int_ty.clone()) ],
        return_ty: CheckedType::Void,
        is_static: false,
        is_main: false,
    }).unwrap();
    writer_class_def.add_method(ClassMethodDef {
        name: strtab.intern("flush"),
        params: vec![],
        return_ty: CheckedType::Void,
        is_static: false,
        is_main: false,
    }).unwrap();
    
    let mut system_class_def = ClassDef::new(strtab.intern("$System"));
    system_class_def.add_field(ClassFieldDef {
        name: strtab.intern("in"), ty: reader_class_def.get_type()
    }).unwrap();
    system_class_def.add_field(ClassFieldDef {
        name: strtab.intern("out"), ty: writer_class_def.get_type()
    }).unwrap();
    context.global_vars.insert(strtab.intern("System"), system_class_def.get_type());

    type_system.add_class_def(reader_class_def).unwrap();
    type_system.add_class_def(writer_class_def).unwrap();
    type_system.add_class_def(system_class_def).unwrap();
}

// pass None as context to disable error reporting
// TODO LATER better ideas?
pub fn checked_type_from_basic_ty<'src>(
    basic_ty: &Spanned<'src, ast::BasicType<'src>>,
    context: Option<&SemanticContext<'src>>,
    type_system: &TypeSystem<'src>,
) -> CheckedType<'src> {
    use self::ast::BasicType::*;
    match &basic_ty.data {
        Int => CheckedType::Int,
        Boolean => CheckedType::Boolean,
        Void => CheckedType::Void,
        Custom(name) => {
            if !type_system.is_type_defined(*name) {
                if let Some(context) = context {
                    context.report_error(&basic_ty.span, SemanticError::ClassDoesNotExist {
                        class_name: name.to_string()
                    });
                }
            }
            CheckedType::TypeRef(*name)
        },
    }
}

pub fn checked_type_from_ty<'src>(
    ty: &ast::Type<'src>,
    context: Option<&SemanticContext<'src>>,
    type_system: &TypeSystem<'src>
) -> CheckedType<'src> {
    let mut checked_ty = checked_type_from_basic_ty(&ty.basic, context, type_system);

    for _ in 0..ty.array_depth {
        checked_ty = CheckedType::Array(Box::new(checked_ty));
    }

    checked_ty
}
