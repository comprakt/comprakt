use super::{type_system::*, *};
use crate::{asciifile::Spanned, ast, strtab::Symbol, symtab::*};

#[derive(Debug, Clone)]
pub struct ExprInfo<'src, 'ts> {
    pub ty: CheckedType<'src>,
    pub ref_info: Option<RefInfo<'src, 'ts>>,
}

impl<'src, 'ts> ExprInfo<'src, 'ts> {
    pub fn new(ty: CheckedType<'src>, ref_info: RefInfo<'src, 'ts>) -> ExprInfo<'src, 'ts> {
        ExprInfo {
            ty,
            ref_info: Some(ref_info),
        }
    }
}

impl<'src, 'ts> From<CheckedType<'src>> for ExprInfo<'src, 'ts> {
    fn from(item: CheckedType<'src>) -> ExprInfo<'src, 'ts> {
        ExprInfo {
            ty: item,
            ref_info: None,
        }
    }
}

#[derive(Debug, Clone)]
pub enum RefInfo<'src, 'ts> {
    GlobalVar(Symbol<'src>),
    Var(Symbol<'src>),
    Param(&'ts MethodParamDef<'src>),
    Field(&'ts ClassFieldDef<'src>),
    Method(&'ts ClassMethodDef<'src>),
    // impossible in minijava: Class(&'ts ClassDef<'src>),
    This(&'ts ClassDef<'src>),
    ArrayAccess,
}

#[derive(Clone)]
pub enum VarDef<'src, 'ts> {
    Local {
        #[allow(dead_code)]
        name: Symbol<'src>,
        ty: CheckedType<'src>,
    },
    Param(&'ts MethodParamDef<'src>),
}

pub struct MethodBodyTypeChecker<'ctx, 'src, 'ast, 'ts, 'ana> {
    pub context: &'ts SemanticContext<'ctx, 'src>,
    pub type_system: &'ts TypeSystem<'src>,
    pub current_class: &'ts ClassDef<'src>,
    pub current_method: &'ts ClassMethodDef<'src>,
    pub type_analysis: &'ana mut TypeAnalysis<'src, 'ast, 'ts>,
    pub local_scope: Scoped<Symbol<'src>, VarDef<'src, 'ts>>,
}

#[derive(Debug)]
pub struct CouldNotDetermineType;

impl<'ctx, 'src, 'ast, 'ts, 'ana> MethodBodyTypeChecker<'ctx, 'src, 'ast, 'ts, 'ana> {
    pub fn check_methods(
        class_decl: &'ast ast::ClassDeclaration<'src>,
        type_system: &'ts TypeSystem<'src>,
        type_analysis: &'ana mut TypeAnalysis<'src, 'ast, 'ts>,
        context: &'ts SemanticContext<'ctx, 'src>,
    ) {
        let current_class_id = type_analysis
            .decl_get_class_id(class_decl)
            .expect("Class has to be already defined to check methods");
        let current_class = type_system.class(current_class_id);

        for member in &class_decl.members {
            use self::ast::ClassMemberKind::*;
            match &member.kind {
                Field(_) => {}
                Method(_, _, block) | MainMethod(_, block) => {
                    let current_method = current_class
                        .method(member.name)
                        .expect("a class only has a member if it exists");

                    let mut checker = MethodBodyTypeChecker {
                        context,
                        type_system,
                        type_analysis,
                        current_class,
                        current_method,
                        local_scope: Scoped::new(),
                    };

                    for param in &current_method.params {
                        checker
                            .local_scope
                            .define(param.name, VarDef::Param(&param))
                            .expect("no double params allowed");
                    }

                    checker.check_type_block(block);
                }
            }
        }
    }

    fn check_type_block(&mut self, block: &'ast ast::Block<'src>) {
        self.local_scope.enter_scope();
        for stmt in &block.statements {
            self.check_type_stmt(stmt);
        }
        self.local_scope
            .leave_scope()
            .expect("scope of a block is not root scope");
    }

    fn check_type_stmt(&mut self, stmt: &'ast Spanned<'src, ast::Stmt<'src>>) {
        use self::ast::Stmt::*;
        match &stmt.data {
            Block(block) => self.check_type_block(block),
            Empty => {}
            If(cond, stmt, opt_else) => {
                if let Ok(ty) = self.type_expr(cond) {
                    if !CheckedType::Boolean.is_assignable_from(&ty.ty) {
                        self.context
                            .report_error(&cond.span, SemanticError::ConditionMustBeBoolean)
                    }
                }

                self.check_type_stmt(&stmt);
                if let Some(els) = opt_else {
                    self.check_type_stmt(&els);
                }
            }
            While(cond, stmt) => {
                if let Ok(ty) = self.type_expr(cond) {
                    if !CheckedType::Boolean.is_assignable_from(&ty.ty) {
                        self.context
                            .report_error(&cond.span, SemanticError::ConditionMustBeBoolean)
                    }
                }

                self.check_type_stmt(&stmt);
            }
            Expression(expr) => {
                let _ = self.type_expr(expr);
            }
            Return(expr_opt) => {
                let return_ty = &self.current_method.return_ty;

                match (expr_opt, return_ty) {
                    (None, CheckedType::Void) => {}
                    (None, _) => {
                        self.context.report_error(
                            &stmt.span,
                            SemanticError::MethodMustReturnSomething {
                                ty: return_ty.to_string(),
                            },
                        );
                    }
                    (Some(expr), CheckedType::Void) => {
                        let _ = self.type_expr(expr);
                        self.context
                            .report_error(&stmt.span, SemanticError::VoidMethodCannotReturnValue);
                    }
                    (Some(expr), _) => self.check_type(expr, return_ty),
                }
            }
            LocalVariableDeclaration(ty, name, opt_assign) => {
                let def_ty = self.type_analysis.checked_type_from_ty(
                    &ty.data,
                    self.context,
                    self.type_system,
                    VoidIs::Forbidden,
                );
                self.local_scope
                    .define(
                        name.data,
                        VarDef::Local {
                            name: name.data,
                            ty: def_ty.clone(),
                        },
                    )
                    .unwrap_or_else(|_| {
                        self.context.report_error(
                            &name.span,
                            SemanticError::RedefinitionError {
                                kind: "local var".to_string(),
                                name: name.data.to_string(),
                            },
                        )
                    });

                if let Some(assign) = opt_assign {
                    self.check_type(assign, &def_ty);
                }
            }
        }
    }
}
