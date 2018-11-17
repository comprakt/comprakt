use crate::{asciifile::Spanned, ast, strtab::Symbol, symtab::*};

use super::{type_system::*, *};

/*
TODO return this in var lookup
enum IdentLookupResult<'src, 'sem> {
    VarDef(VarDef<'src, 'sem>),
    Param(&'sem MethodParamDef<'src>),
    ThisField(&'sem ClassFieldDef<'src>),
    Class(&'sem ClassDef<'src>),
}
*/

enum VarDef<'src, 'sem> {
    Local {
        #[allow(dead_code)]
        name: Symbol<'src>,
        ty: CheckedType<'src>,
    },
    Param(&'sem MethodParamDef<'src>),
}

pub struct MethodBodyTypeChecker<'ctx, 'src, 'sem> {
    context: &'sem SemanticContext<'ctx, 'src>,
    type_system: &'sem TypeSystem<'src>,
    current_class: &'sem ClassDef<'src>,
    current_method: &'sem ClassMethodDef<'src>,
    local_scope: Scoped<Symbol<'src>, VarDef<'src, 'sem>>,
}

#[derive(Debug)]
pub struct CouldNotDetermineType;

impl<'ctx, 'src, 'sem> MethodBodyTypeChecker<'ctx, 'src, 'sem> {
    pub fn check_methods(
        class_decl: &'sem ast::ClassDeclaration<'src>,
        type_system: &'sem TypeSystem<'src>,
        context: &'sem SemanticContext<'ctx, 'src>,
    ) {
        let current_class = type_system.resolve_type_ref(class_decl.name.data).unwrap();

        for member in &class_decl.members {
            use self::ast::ClassMemberKind::*;
            match &member.kind {
                Field(_) => {}
                Method(_, _, block) | MainMethod(_, block) => {
                    let current_method = current_class.get_method(member.name).unwrap();

                    let mut checker = MethodBodyTypeChecker {
                        context,
                        type_system,
                        current_class,
                        current_method,
                        local_scope: Scoped::new(),
                    };

                    for param in &current_method.params {
                        checker
                            .local_scope
                            .define(param.name, VarDef::Param(&param))
                            .unwrap();
                    }

                    checker.check_type_block(block);
                }
            }
        }
    }

    fn check_type_block(&mut self, block: &ast::Block<'src>) {
        self.local_scope.enter_scope();
        for stmt in &block.statements {
            self.check_type_stmt(stmt);
        }
        self.local_scope.leave_scope().unwrap();
    }

    fn check_type_stmt(&mut self, stmt: &Spanned<'src, ast::Stmt<'src>>) {
        use self::ast::Stmt::*;
        match &stmt.data {
            Block(block) => self.check_type_block(block),
            Empty => {}
            If(cond, stmt, opt_else) => {
                if let Ok(ty) = self.get_type_expr(cond) {
                    if !CheckedType::Boolean.is_assignable_from(&ty) {
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
                if let Ok(ty) = self.get_type_expr(cond) {
                    if !CheckedType::Boolean.is_assignable_from(&ty) {
                        self.context
                            .report_error(&cond.span, SemanticError::ConditionMustBeBoolean)
                    }
                }

                self.check_type_stmt(&stmt);
            }
            Expression(expr) => {
                let _ = self.get_type_expr(expr);
                // TODO validate that stmt expr is valid. Is this done in the "first pass"?
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
                        let _ = self.get_type_expr(expr);
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

    fn resolve_class(
        &mut self,
        ty: &CheckedType<'src>,
    ) -> Result<&'sem ClassDef<'src>, CouldNotDetermineType> {
        match ty {
            CheckedType::TypeRef(name) => match self.type_system.resolve_type_ref(*name) {
                None => Err(CouldNotDetermineType),
                Some(class_def) => Ok(class_def),
            },
            _ => Err(CouldNotDetermineType),
        }
    }

    fn check_type(
        &mut self,
        expr: &Spanned<'src, ast::Expr<'src>>,
        expected_ty: &CheckedType<'src>,
    ) {
        if let Ok(ty) = self.get_type_expr(expr) {
            if !expected_ty.is_assignable_from(&ty) {
                self.context.report_error(
                    &expr.span,
                    SemanticError::InvalidType {
                        ty_expected: expected_ty.to_string(),
                        ty_expr: ty.to_string(),
                    },
                );
            }
        }
    }

    fn get_type_expr(
        &mut self,
        expr: &Spanned<'src, ast::Expr<'src>>,
    ) -> Result<CheckedType<'src>, CouldNotDetermineType> {
        use crate::ast::Expr::*;
        match &expr.data {
            Binary(op, lhs, rhs) => {
                use crate::ast::BinaryOp::*;
                match op {
                    Assign => {
                        // TODO ensure that lhs is L-Value
                        match self.get_type_expr(lhs) {
                            Ok(lhs_type) => {
                                self.check_type(rhs, &lhs_type);
                                Ok(lhs_type)
                            }
                            Err(_) => Err(CouldNotDetermineType),
                        }
                    }
                    Equals | NotEquals => {
                        let lhs_type = self.get_type_expr(&lhs)?;
                        let rhs_type = self.get_type_expr(&rhs)?;

                        if !lhs_type.is_assignable_from(&rhs_type)
                            && !rhs_type.is_assignable_from(&lhs_type)
                        {
                            self.context.report_error(
                                &expr.span,
                                SemanticError::CannotCompareValuesOfType1WithType2 {
                                    ty1: lhs_type.to_string(),
                                    ty2: rhs_type.to_string(),
                                },
                            );
                        }
                        Ok(CheckedType::Boolean)
                    }
                    LogicalOr | LogicalAnd => {
                        self.check_type(lhs, &CheckedType::Boolean);
                        self.check_type(rhs, &CheckedType::Boolean);
                        Ok(CheckedType::Boolean)
                    }
                    LessThan | GreaterThan | LessEquals | GreaterEquals => {
                        self.check_type(lhs, &CheckedType::Int);
                        self.check_type(rhs, &CheckedType::Int);
                        Ok(CheckedType::Boolean)
                    }
                    Add | Sub | Mul | Div | Mod => {
                        self.check_type(lhs, &CheckedType::Int);
                        self.check_type(rhs, &CheckedType::Int);
                        Ok(CheckedType::Int)
                    }
                }
            }
            Unary(op, expr) => {
                let ty = match op {
                    ast::UnaryOp::Not => CheckedType::Int,
                    ast::UnaryOp::Neg => CheckedType::Boolean,
                };

                self.check_type(expr, &ty);

                Ok(ty)
            }
            FieldAccess(target_expr, name) => {
                let target_type = self.get_type_expr(&target_expr)?;
                let target_class_def = self.resolve_class(&target_type).map_err(|e| {
                    self.context.report_error(
                        &name.span,
                        SemanticError::FieldDoesNotExistOnType {
                            field_name: name.data.to_string(),
                            ty: target_type.to_string(),
                        },
                    );
                    e
                })?;
                match target_class_def.get_field(name.data) {
                    Some(field) => Ok(field.ty.clone()),
                    None => {
                        self.context.report_error(
                            &name.span,
                            SemanticError::FieldDoesNotExistOnType {
                                field_name: name.data.to_string(),
                                ty: target_class_def.name.to_string(),
                            },
                        );
                        Err(CouldNotDetermineType)
                    }
                }
            }

            MethodInvocation(target_expr, name, args) => {
                // e.g. "target_expr.name(arg1, arg2)"
                let target_type = self.get_type_expr(&target_expr)?;
                let target_class_def = self.resolve_class(&target_type).map_err(|e| {
                    self.context.report_error(
                        &name.span,
                        SemanticError::MethodDoesNotExistOnType {
                            method_name: name.data.to_string(),
                            ty: target_type.to_string(),
                        },
                    );
                    e
                })?;
                // TODO check args if type or class_def already fails
                self.check_method_invocation(name, target_class_def, args)
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
                self.check_method_invocation(name, &self.current_class, args)
            }
            ArrayAccess(target_expr, idx_expr) => {
                self.check_type(idx_expr, &CheckedType::Int);

                match self.get_type_expr(target_expr) {
                    Ok(CheckedType::Array(item_type)) => Ok(*item_type),
                    Ok(ty) => {
                        self.context.report_error(
                            &target_expr.span,
                            SemanticError::CannotIndexNonArrayType { ty: ty.to_string() },
                        );
                        Err(CouldNotDetermineType)
                    }
                    Err(_) => Err(CouldNotDetermineType),
                }
            }
            Null => Ok(CheckedType::Null),
            Boolean(_) => Ok(CheckedType::Boolean),
            Int(_) => Ok(CheckedType::Int),
            Var(name) => self.check_var(&name),
            This => {
                if self.current_method.is_static {
                    self.context
                        .report_error(&expr.span, SemanticError::ThisInStaticMethod);

                    Err(CouldNotDetermineType)
                } else {
                    Ok(CheckedType::TypeRef(self.current_class.name))
                }
            }
            NewObject(name) => {
                let t = CheckedType::TypeRef(name.data);
                match self.resolve_class(&t) {
                    Ok(_) => Ok(t),
                    Err(_) => {
                        self.context.report_error(
                            &name.span,
                            SemanticError::ClassDoesNotExist {
                                class_name: name.data.to_string(),
                            },
                        );
                        Err(CouldNotDetermineType)
                    }
                }
            }
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

                Ok(ty)
            }
        }
    }

    fn check_method_invocation(
        &mut self,
        method_name: &Spanned<'src, Symbol<'src>>,
        target_class_def: &ClassDef<'src>,
        args: &[Spanned<'src, ast::Expr<'src>>],
    ) -> Result<CheckedType<'src>, CouldNotDetermineType> {
        let method = match target_class_def.get_method(method_name.data) {
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

        Ok(method.return_ty.clone())
    }

    fn check_var(
        &mut self,
        var_name: &Spanned<'src, Symbol<'src>>,
    ) -> Result<CheckedType<'src>, CouldNotDetermineType> {
        match self.local_scope.visible_definition(var_name.data) {
            // local variable or param
            Some(VarDef::Local { ty, .. }) => Ok(ty.clone()),
            Some(VarDef::Param(param_def)) => {
                if self.current_method.is_main {
                    self.context.report_error(
                        &var_name.span,
                        SemanticError::MainMethodParamUsed {
                            name: var_name.data.to_string(),
                        },
                    );
                }

                Ok(param_def.ty.clone())
            }
            None => Err(CouldNotDetermineType),
        }
        .or_else(|_| {
            // field
            match self.current_class.get_field(var_name.data) {
                Some(field) => {
                    if self.current_method.is_static {
                        self.context.report_error(
                            &var_name.span,
                            SemanticError::CannotAccessNonStaticFieldInStaticMethod {
                                field_name: var_name.data.to_string(),
                            },
                        );
                    }

                    Ok(field.ty.clone())
                }
                None => Err(CouldNotDetermineType),
            }
        })
        .or_else(|_| {
            // static classes access (is not allowed).
            // Is important to check if user defines a "System" class
            match self.type_system.resolve_type_ref(var_name.data) {
                Some(class_def) => {
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
            Some(ty) => Ok(ty.clone()),
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
