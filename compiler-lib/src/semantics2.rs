use crate::{
    asciifile::{Span, Spanned},
    ast,
    context::Context,
    method_body_typechecker::*,
    type_system::*,
};
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
    ast: &'a ast::AST<'src>,
    context: &'src Context<'src>,
) -> Result<TypeSystem<'src>, ()> {
    let sem_context = SemanticContext::new(context);

    match ast {
        ast::AST::Empty => Ok(TypeSystem::default()),
        ast::AST::Program(program) => {
            let type_system = build_type_system(&sem_context, program);

            // add $System


            for class_decl in &program.classes {
                MethodBodyTypeChecker::check_methods(class_decl, &type_system, &sem_context);
            }

            Ok(type_system)
        }
    }
}

pub struct SemanticContext<'src> {
    pub context: &'src Context<'src>,
}

impl<'src> SemanticContext<'src> {
    pub fn new(context: &'src Context<'src>) -> SemanticContext<'src> {
        SemanticContext { context }
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
                    let (is_static, return_ty) = match &member.kind {
                        Field(_) => panic!("impossible"),
                        Method(ty, _, _) => (false, checked_type_from_ty(&ty.data, Some(context), &type_system)),
                        MainMethod(_, _) => (true, CheckedType::Void),
                    };

                    let checked_params = params
                        .iter()
                        .map(|p| MethodParamDef {
                            name: p.name,
                            ty: checked_type_from_ty(&p.ty.data, Some(context), &type_system),
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

        let _ = type_system.update_existing_class_def(class_def);
        // FIXME crash on error here: there is always an existing class_def
    }

    type_system
}

// pass None as context to disable error reporting
// FIXME better ideas?
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
