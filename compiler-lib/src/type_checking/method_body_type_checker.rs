use super::{checker::*, type_analysis::*, type_system::*};
use crate::{
    asciifile::{Span, Spanned},
    ast,
    semantics::SemanticError,
    strtab::Symbol,
    symtab::*,
};

use std::rc::Rc;

#[derive(Debug, Clone)]
pub struct ExprInfo<'src, 'ast> {
    pub ty: CheckedType<'src>,
    pub ref_info: Option<RefInfo<'src, 'ast>>,
}

impl<'src, 'ast> ExprInfo<'src, 'ast> {
    pub fn new(ty: CheckedType<'src>, ref_info: RefInfo<'src, 'ast>) -> Self {
        ExprInfo {
            ty,
            ref_info: Some(ref_info),
        }
    }
}

impl<'src, 'ast, 'ts> From<CheckedType<'src>> for ExprInfo<'src, 'ast> {
    fn from(item: CheckedType<'src>) -> ExprInfo<'src, 'ast> {
        ExprInfo {
            ty: item,
            ref_info: None,
        }
    }
}

#[derive(Debug, Clone)]
pub enum RefInfo<'src, 'ast> {
    GlobalVar(Symbol<'src>),
    Var(Symbol<'src>),
    Param(Rc<MethodParamDef<'src>>),
    Field(Rc<ClassFieldDef<'src>>),
    Method(Rc<ClassMethodDef<'src, 'ast>>),
    // impossible in minijava: Class(Rc<ClassDef<'src, 'ast>),
    This(Rc<ClassDef<'src, 'ast>>),
    ArrayAccess,
}

#[derive(Clone)]
pub enum VarDef<'src> {
    Local {
        #[allow(dead_code)]
        name: Symbol<'src>,
        ty: CheckedType<'src>,
    },
    Param(Rc<MethodParamDef<'src>>),
}

pub struct MethodBodyTypeChecker<'ctx, 'src, 'ast, 'ts, 'ana> {
    pub context: &'ctx SemanticContext<'ctx, 'src>,
    pub type_system: &'ts TypeSystem<'src, 'ast>,
    pub current_class_id: ClassDefId<'src>,
    pub current_class: Rc<ClassDef<'src, 'ast>>,
    pub current_method: Rc<ClassMethodDef<'src, 'ast>>,
    pub type_analysis: &'ana mut TypeAnalysis<'src, 'ast>,
    pub local_scope: Scoped<Symbol<'src>, VarDef<'src>>,
}

#[derive(Debug)]
pub struct CouldNotDetermineType;

impl<'ctx, 'src, 'ast, 'ts, 'ana> MethodBodyTypeChecker<'ctx, 'src, 'ast, 'ts, 'ana>
where
    'ts: 'ana,
{
    pub fn check_methods(
        class_decl: &'ast ast::ClassDeclaration<'src>,
        type_system: &'ts TypeSystem<'src, 'ast>,
        type_analysis: &'ana mut TypeAnalysis<'src, 'ast>,
        context: &'ctx SemanticContext<'ctx, 'src>,
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
                        current_class_id,
                        current_class: Rc::clone(&current_class),
                        current_method: Rc::clone(&current_method),
                        local_scope: Scoped::new(),
                    };

                    for param in &current_method.params {
                        checker
                            .local_scope
                            .define(param.name, VarDef::Param(Rc::clone(param)))
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
                let return_ty = &Rc::clone(&self.current_method).return_ty;

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
                let def_ty = checked_type_from_ty(
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

    fn type_expr(
        &mut self,
        expr: &'ast Spanned<'src, ast::Expr<'src>>,
    ) -> Result<ExprInfo<'src, 'ast>, CouldNotDetermineType> {
        let t = self.type_expr_internal(expr);

        if let Ok(ref t) = t {
            self.type_analysis.set_expr_info(&expr.data, t.clone());
        }
        t
    }

    fn type_expr_internal(
        &mut self,
        expr: &'ast Spanned<'src, ast::Expr<'src>>,
    ) -> Result<ExprInfo<'src, 'ast>, CouldNotDetermineType> {
        use crate::ast::Expr::*;
        match &expr.data {
            Binary(op, lhs, rhs) => self.check_binary_expr(expr.span, *op, lhs, rhs),
            Unary(op, expr) => {
                let ty = match op {
                    ast::UnaryOp::Not => CheckedType::Boolean,
                    ast::UnaryOp::Neg => CheckedType::Int,
                };

                self.check_type(expr, &ty);

                Ok(ty.into())
            }
            FieldAccess(target_expr, name) => {
                let target_type = self.type_expr(&target_expr)?.ty;
                let target_class_def = self.resolve_to_class_def(&target_type).map_err(|e| {
                    self.context.report_error(
                        &name.span,
                        SemanticError::FieldDoesNotExistOnType {
                            field_name: name.data.to_string(),
                            ty: target_type.to_string(),
                        },
                    );
                    e
                })?;
                // IMPROVEMENT: unite error handling
                match target_class_def.field(name.data) {
                    Some(field) => Ok(ExprInfo::new(field.ty.clone(), RefInfo::Field(field))),
                    None => {
                        self.context.report_error(
                            &name.span,
                            SemanticError::FieldDoesNotExistOnType {
                                field_name: name.data.to_string(),
                                ty: target_type.to_string(),
                            },
                        );
                        Err(CouldNotDetermineType)
                    }
                }
            }

            MethodInvocation(target_expr, name, args) => {
                // e.g. "target_expr.name(arg1, arg2)"
                let target_type = self.type_expr(&target_expr)?.ty;
                let target_class_def = self.resolve_to_class_def(&target_type).map_err(|e| {
                    self.context.report_error(
                        &name.span,
                        SemanticError::MethodDoesNotExistOnType {
                            method_name: name.data.to_string(),
                            ty: target_type.to_string(),
                        },
                    );
                    e
                })?;
                // IMPROVEMENT check args if type or class_def already fails
                self.check_method_invocation(name, &target_class_def, args)
            }
            ThisMethodInvocation(name, args) => {
                // e.g. "name(arg1, arg2);"
                if self.current_method.is_static {
                    self.context.report_error(
                        &name.span,
                        SemanticError::ThisMethodInvocationInStaticMethod {
                            method_name: name.data.to_string(),
                        },
                    );
                }
                // assume the user wanted to call the method on an object
                self.check_method_invocation(name, &Rc::clone(&self.current_class), args)
            }
            ArrayAccess(target_expr, idx_expr) => {
                self.check_type(idx_expr, &CheckedType::Int);
                let target_expr_info = self.type_expr(target_expr)?;

                match target_expr_info.ty {
                    CheckedType::Array(item_type) => {
                        Ok(ExprInfo::new(*item_type, RefInfo::ArrayAccess))
                    }
                    _ => {
                        self.context.report_error(
                            &target_expr.span,
                            SemanticError::CannotIndexNonArrayType {
                                ty: target_expr_info.ty.to_string(),
                            },
                        );
                        Err(CouldNotDetermineType)
                    }
                }
            }
            Null => Ok(CheckedType::Null.into()),
            Boolean(_) => Ok(CheckedType::Boolean.into()),
            Int(_) => Ok(CheckedType::Int.into()),
            NegInt(_) => Ok(CheckedType::Int.into()),
            Var(name) => self.check_var(&name),
            This => {
                if self.current_method.is_static {
                    self.context
                        .report_error(&expr.span, SemanticError::ThisInStaticMethod);

                    Err(CouldNotDetermineType)
                } else {
                    Ok(ExprInfo::new(
                        CheckedType::TypeRef(self.current_class_id),
                        RefInfo::This(Rc::clone(&self.current_class)),
                    ))
                }
            }
            NewObject(name) => match self.type_system.lookup_class(name.data) {
                Some((_, class_def_id)) => Ok(CheckedType::TypeRef(class_def_id).into()),
                None => {
                    self.context.report_error(
                        &name.span,
                        SemanticError::ClassDoesNotExist {
                            class_name: name.data.to_string(),
                        },
                    );
                    Err(CouldNotDetermineType)
                }
            },
            NewArray(basic_ty, size_expr, dimension) => {
                // e.g new int[10][][];

                self.check_type(size_expr, &CheckedType::Int);

                let basic_ty = checked_type_from_basic_ty(
                    &basic_ty,
                    self.context,
                    self.type_system,
                    VoidIs::Forbidden,
                );
                let ty = CheckedType::create_array_type(basic_ty, dimension + 1);

                Ok(ty.into())
            }
        }
    }

    fn check_binary_expr(
        &mut self,
        span: Span<'src>,
        op: ast::BinaryOp,
        lhs: &'ast Spanned<'src, ast::Expr<'src>>,
        rhs: &'ast Spanned<'src, ast::Expr<'src>>,
    ) -> Result<ExprInfo<'src, 'ast>, CouldNotDetermineType> {
        use crate::ast::BinaryOp::*;
        match op {
            Assign => {
                let ExprInfo {
                    ty: lhs_type,
                    ref_info,
                } = self.type_expr(lhs)?;
                self.check_type(rhs, &lhs_type); // IMPROVEMENT check even on Err

                use self::RefInfo::*;
                match ref_info {
                    Some(GlobalVar(_)) | Some(Method(_)) | Some(This(_)) | None => {
                        self.context
                            .report_error(&lhs.span, SemanticError::InvalidAssignment);
                    }
                    Some(Field(field)) => {
                        if !field.can_write {
                            self.context.report_error(
                                &lhs.span,
                                SemanticError::CannotWriteToReadOnlyField {
                                    field_name: field.name.to_string(),
                                },
                            );
                        }
                    }
                    Some(Var(_)) | Some(Param(_)) | Some(ArrayAccess) => {}
                }

                Ok(lhs_type.into())
            }
            Equals | NotEquals => {
                let lhs_info = self.type_expr(&lhs);
                let rhs_info = self.type_expr(&rhs);

                let lhs_type = lhs_info?.ty;
                let rhs_type = rhs_info?.ty;

                if !lhs_type.is_assignable_from(&rhs_type)
                    && !rhs_type.is_assignable_from(&lhs_type)
                {
                    self.context.report_error(
                        &span,
                        SemanticError::CannotCompareValuesOfType1WithType2 {
                            ty1: lhs_type.to_string(),
                            ty2: rhs_type.to_string(),
                        },
                    );
                }
                Ok(CheckedType::Boolean.into())
            }
            LogicalOr | LogicalAnd => {
                self.check_type(lhs, &CheckedType::Boolean);
                self.check_type(rhs, &CheckedType::Boolean);
                Ok(CheckedType::Boolean.into())
            }
            LessThan | GreaterThan | LessEquals | GreaterEquals => {
                self.check_type(lhs, &CheckedType::Int);
                self.check_type(rhs, &CheckedType::Int);
                Ok(CheckedType::Boolean.into())
            }
            Add | Sub | Mul | Div | Mod => {
                self.check_type(lhs, &CheckedType::Int);
                self.check_type(rhs, &CheckedType::Int);
                Ok(CheckedType::Int.into())
            }
        }
    }

    fn check_type(
        &mut self,
        expr: &'ast Spanned<'src, ast::Expr<'src>>,
        expected_ty: &CheckedType<'src>,
    ) {
        if let Ok(expr_info) = self.type_expr(expr) {
            if !expected_ty.is_assignable_from(&expr_info.ty) {
                self.context.report_error(
                    &expr.span,
                    SemanticError::InvalidType {
                        ty_expected: expected_ty.to_string(),
                        ty_expr: expr_info.ty.to_string(),
                    },
                );
            }
        }
    }

    fn check_method_invocation(
        &mut self,
        method_name: &Spanned<'src, Symbol<'src>>,
        target_class_def: &ClassDef<'src, 'ast>,
        args: &'ast [Spanned<'src, ast::Expr<'src>>],
    ) -> Result<ExprInfo<'src, 'ast>, CouldNotDetermineType> {
        let method = match target_class_def.method(method_name.data) {
            Some(method) => method,
            None => {
                self.context.report_error(
                    &method_name.span,
                    SemanticError::MethodDoesNotExistOnType {
                        method_name: method_name.data.to_string(),
                        ty: target_class_def.name.to_string(),
                    },
                );

                return Err(CouldNotDetermineType);
            }
        };

        if method.is_static {
            self.context.report_error(
                &method_name.span,
                SemanticError::CannotCallStaticMethod {
                    method_name: method_name.data.to_string(),
                },
            );
        }

        if method.params.len() != args.len() {
            self.context.report_error(
                &method_name.span,
                SemanticError::MethodArgCountDoesNotMatch {
                    expected_args: method.params.len(),
                    actual_args: args.len(),
                },
            );
        }

        for (arg, param) in args.iter().zip(method.params.iter()) {
            self.check_type(arg, &param.ty);
        }

        Ok(ExprInfo::new(
            method.return_ty.clone(),
            RefInfo::Method(method),
        ))
    }

    fn resolve_to_class_def(
        &mut self,
        ty: &CheckedType<'src>,
    ) -> Result<Rc<ClassDef<'src, 'ast>>, CouldNotDetermineType> {
        match ty {
            CheckedType::TypeRef(id) => Ok(self.type_system.class(*id)),
            _ => Err(CouldNotDetermineType),
        }
    }

    /// Returned ExprInfo.ref_info is always Some(_)
    fn check_var(
        &mut self,
        var_name: &Spanned<'src, Symbol<'src>>,
    ) -> Result<ExprInfo<'src, 'ast>, CouldNotDetermineType> {
        match self.local_scope.visible_definition(var_name.data) {
            // local variable or param
            Some(VarDef::Local { ty, name }) => Ok(ExprInfo {
                ty: ty.clone(),
                ref_info: Some(RefInfo::Var(*name)),
            }),
            Some(VarDef::Param(param_def)) => {
                if self.current_method.is_main {
                    self.context.report_error(
                        &var_name.span,
                        SemanticError::MainMethodParamUsed {
                            name: var_name.data.to_string(),
                        },
                    );
                }

                Ok(ExprInfo {
                    ty: param_def.ty.clone(),
                    ref_info: Some(RefInfo::Param(Rc::clone(param_def))),
                })
            }
            None => Err(CouldNotDetermineType),
        }
        .or_else(|_| {
            // field
            match self.current_class.field(var_name.data) {
                Some(field) => {
                    if self.current_method.is_static {
                        self.context.report_error(
                            &var_name.span,
                            SemanticError::CannotAccessNonStaticFieldInStaticMethod {
                                field_name: var_name.data.to_string(),
                            },
                        );
                    }

                    Ok(ExprInfo {
                        ty: field.ty.clone(),
                        ref_info: Some(RefInfo::Field(field)),
                    })
                }
                None => Err(CouldNotDetermineType),
            }
        })
        .or_else(|_| {
            // static classes access (is not allowed).
            // Is important to check if user defines a "System" class
            match self.type_system.lookup_class(var_name.data) {
                Some((class_def, _)) => {
                    self.context.report_error(
                        &var_name.span,
                        SemanticError::InvalidReferenceToClass {
                            class_name: class_def.name.to_string(),
                        },
                    );
                    Err(CouldNotDetermineType)
                }
                None => Err(CouldNotDetermineType),
            }
        })
        .or_else(|_| match self.context.global_vars.get(&var_name.data) {
            Some(ty) => Ok(ExprInfo {
                ty: ty.clone(),
                ref_info: Some(RefInfo::GlobalVar(var_name.data)),
            }),
            None => Err(CouldNotDetermineType),
        })
        .or_else(|_| {
            self.context.report_error(
                &var_name.span,
                SemanticError::CannotLookupVarOrField {
                    name: var_name.data.to_string(),
                },
            );
            Err(CouldNotDetermineType)
        })
    }
}
