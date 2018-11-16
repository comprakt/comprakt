use crate::{asciifile::Spanned, asciifile::Span, ast, context::Context, strtab::Symbol, symtab::*, type_system::*, method_body_typechecker::*};
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

    #[fail(display = "non-static variable 'this' cannot be referenced from a static context")]
    ThisInStaticMethod,

    #[fail(display = "condition must be boolean")]
    ConditionMustBeBoolean,

    #[fail(display = "cannot lookup var or field '{}'", name)]
    CannotLookupVarOrField { name: String },

    #[fail(display = "cannot access non static field '{}' in static method", field_name)]
    CannotAccessNonStaticFieldInStaticMethod { field_name: String },

    #[fail(display = "method cannot return a value")]
    VoidMethodCannotReturnValue,

    #[fail(display = "method must return a value of type '{}'", ty)]
    MethodMustReturnSomething { ty: String },

    #[fail(display = "Invalid return type: Expected expression of type '{}', but was of type '{}'",
        ty_return, ty_expr)]
    InvalidReturnType { ty_expr: String, ty_return: String },

    #[fail(display = "Invalid type: Expected expression of type '{}', but was of type '{}'",
        ty_expected, ty_expr)]
    InvalidType { ty_expected: String, ty_expr: String },

    #[fail(display = "cannot reference class '{}' here", class_name)]
    InvalidReferenceToClass { class_name: String },

    #[fail(display = "class '{}' does not exist", class_name)]
    ClassDoesNotExist { class_name: String },
}

pub fn check<'a, 'src>(
    ast: &'a ast::AST<'src>,
    context: &'src Context<'src>,
) -> Result<TypeSystem<'src>, ()> {
    let sem_context = SemanticContext::new(context);

    match ast {
        ast::AST::Empty => Ok(TypeSystem::new()),
        ast::AST::Program(program) => {
            let type_system = build_type_system(&sem_context, program);
            
            for class_decl in &program.classes {        
                MethodBodyTypeChecker::check_methods(
                    class_decl, &type_system, &sem_context);
            }

            Ok(type_system)
        },
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
        self.context.diagnostics.error(
            &Spanned::new(span.clone(), error)
        )
    }
}

fn build_type_system<'src>(context: &SemanticContext<'src>, program: &ast::Program<'src>) -> TypeSystem<'src> {
    let mut type_system = TypeSystem::new();

    for class_decl in &program.classes {
        let mut class_def = ClassDef::new(class_decl.name.data);

        for member in &class_decl.members {
            use crate::ast::ClassMemberKind::*;
            match &member.kind {
                Field(ty) => {
                    class_def
                        .add_field(ClassFieldDef {
                            name: member.name,
                            ty: CheckedType::from(&ty.data),
                        })
                        .unwrap_or_else(|_| {
                            context.report_error(&member.span,
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
                        Method(ty, _, _) => (false, CheckedType::from(&ty.data)),
                        MainMethod(_, _) => (true, CheckedType::Void),
                    };
                    class_def
                        .add_method(ClassMethodDef::new(
                            member.name,
                            &params,
                            return_ty,
                            is_static,
                        ))
                        .unwrap_or_else(|_| {
                            context.report_error(&member.span,
                                SemanticError::RedefinitionError {
                                    kind: "method".to_string(),
                                    name: member.name.to_string(),
                                },
                            )
                        });
                }
            }
        }

        type_system
            .add_class_def(class_def)
            .unwrap_or_else(|_| {
                context.report_error(&class_decl.span,
                    SemanticError::RedefinitionError {
                        kind: "class".to_string(),
                        name: class_decl.name.to_string(),
                    },
                )
            });
    }

    type_system
}

impl<'t> From<&ast::Type<'t>> for CheckedType<'t> {
    fn from(ty: &ast::Type<'t>) -> Self {
        let mut checked_ty = (&ty.basic.data).into();

        for _ in 0..ty.array_depth {
            checked_ty = CheckedType::Array(Box::new(checked_ty));
        }

        checked_ty
    }
}

impl<'t> From<&ast::BasicType<'t>> for CheckedType<'t> {
    fn from(basic_ty: &ast::BasicType<'t>) -> Self {
        use self::ast::BasicType::*;
        match basic_ty {
            Int => CheckedType::Int,
            Boolean => CheckedType::Boolean,
            Void => CheckedType::Void,
            Custom(symbol) => CheckedType::TypeRef(*symbol),
        }
    }
}
