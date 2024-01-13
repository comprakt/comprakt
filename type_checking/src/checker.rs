use crate::semantics::SemanticError;
use asciifile::{Span, Spanned};
use compiler_shared::context::Context;
use parser::ast;
use std::collections::{HashMap, HashSet};
use strtab::{StringTable, Symbol};

use super::{
    builtin_types::BuiltinTypes, method_body_type_checker::MethodBodyTypeChecker,
    type_analysis::TypeAnalysis, type_system::*,
};

pub fn check<'ast, 'src>(
    strtab: &'_ mut StringTable<'src>,
    ast: &'ast ast::AST<'src>,
    context: &Context<'src>,
) -> (TypeSystem<'src, 'ast>, TypeAnalysis<'src, 'ast>) {
    let mut sem_context = SemanticContext::new(context);

    let mut type_system = TypeSystem::default();
    let mut type_analysis = TypeAnalysis::new();

    if let ast::AST::Program(program) = ast {
        let builtin_types = BuiltinTypes::add_to(&mut type_system, strtab, &mut sem_context);

        add_types_from_ast(
            strtab,
            &mut type_system,
            &mut type_analysis,
            &builtin_types,
            &sem_context,
            program,
        );

        for class_decl in &program.classes {
            MethodBodyTypeChecker::check_methods(
                &class_decl,
                &type_system,
                &mut type_analysis,
                &sem_context,
            );
        }
    }

    (type_system, type_analysis)
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

// FIXME: simplify lifetimes by merging 'src and 'ast
fn add_types_from_ast<'ctx, 'src, 'ast, 'ana>(
    strtab: &mut StringTable<'src>,
    type_system: &mut TypeSystem<'src, 'ast>,
    type_analysis: &'ana mut TypeAnalysis<'src, 'ast>,
    builtin_types: &BuiltinTypes<'src>,
    context: &SemanticContext<'ctx, 'src>,
    program: &'ast ast::Program<'src>,
) {
    // first pass: find all types and add them to the type system.
    // rename them if they have invalid names
    for class_decl in &program.classes {
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

        let class_def_id = type_system
            .add_class_def(ClassDef::new(name))
            .expect("The redefinition number ensures this cannot happen");

        type_analysis.decl_set_class_id(&class_decl.data, class_def_id);
    }

    // second pass: scan members of all types, check their type references against
    // the first pass
    for class_decl in &program.classes {
        let class_def_id = type_analysis
            .decl_get_class_id(&class_decl)
            .expect("Class def was attached to this class_decl in first pass");

        for member in &class_decl.members {
            use parser::ast::ClassMemberKind::*;
            match &member.kind {
                Field(ty) => {
                    let field_type =
                        checked_type_from_ty(&ty.data, context, type_system, VoidIs::Forbidden);

                    type_system
                        .class_mut(class_def_id)
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
                Method(_, params, block) | MainMethod(params, block) => {
                    let (is_static, is_main, return_ty) = match &member.kind {
                        Field(_) => panic!("impossible"),
                        Method(return_ty, _, _) => (
                            false,
                            false,
                            checked_type_from_ty(
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
                                        debug_assert!(is_main);
                                        debug_assert_eq!(p.ty.data.array_depth, 0);
                                        CheckedType::Array(box builtin_types.string.clone())
                                    }
                                    _ => checked_type_from_ty(
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
                        .class_mut(class_def_id)
                        .add_method(ClassMethodDef::new(
                            member.name,
                            ClassMethodBody::AST(block),
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

#[derive(Debug, Clone, Copy)]
pub enum VoidIs {
    Allowed,
    Forbidden,
}

pub fn checked_type_from_basic_ty<'src, 'ast>(
    basic_ty: &'ast Spanned<'src, ast::BasicType<'src>>,
    context: &SemanticContext<'_, 'src>,
    type_system: &TypeSystem<'src, 'ast>,
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
        Custom(name) => match type_system.lookup_class(*name) {
            Some((_, class_id)) => CheckedType::TypeRef(class_id),
            None => {
                context.report_error(
                    &basic_ty.span,
                    SemanticError::ClassDoesNotExist {
                        class_name: name.to_string(),
                    },
                );
                CheckedType::UnknownType(*name)
            }
        },
        // Parser only yields MainParam in that one case we handle earlier
        MainParam => unreachable!(),
    }
}

pub fn checked_type_from_ty<'src, 'ast>(
    ty: &'ast ast::Type<'src>,
    context: &SemanticContext<'_, 'src>,
    type_system: &TypeSystem<'src, 'ast>,
    void_handling: VoidIs,
) -> CheckedType<'src> {
    let void_handling = if ty.array_depth > 0 {
        VoidIs::Forbidden
    } else {
        void_handling
    };

    let mut checked_ty = checked_type_from_basic_ty(&ty.basic, context, type_system, void_handling);

    for _ in 0..ty.array_depth {
        checked_ty = CheckedType::Array(Box::new(checked_ty));
    }

    checked_ty
}
