use super::{method_body_typechecker::*, type_system::*, *};
use crate::{
    asciifile::{Span, Spanned},
    ast,
    strtab::Symbol,
};

impl<'ctx, 'src, 'ast, 'ts, 'ana> MethodBodyTypeChecker<'ctx, 'src, 'ast, 'ts, 'ana>  {
    pub fn type_expr(
        &mut self,
        expr: &'ast Spanned<'src, ast::Expr<'src>>,
    ) -> Result<ExprInfo<'src, 'ts>, CouldNotDetermineType> {
        let t = self.type_expr_internal(expr);

        if let Ok(ref t) = t {
            self.type_analysis.set_expr_info(&expr.data, t.clone());
        }
        t
    }

    pub fn type_expr_internal(
        &mut self,
        expr: &'ast Spanned<'src, ast::Expr<'src>>,
    ) -> Result<ExprInfo<'src, 'ts>, CouldNotDetermineType> {
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
                // IMPROVEMENT check args if type or class_def already fails
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
                        CheckedType::TypeRef(self.current_class.name),
                        RefInfo::This(self.current_class),
                    ))
                }
            }
            NewObject(name) => {
                let ty = CheckedType::TypeRef(name.data);
                match self.resolve_class(&ty) {
                    Ok(_) => Ok(ty.into()),
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

                let basic_ty = self.type_analysis.checked_type_from_basic_ty(
                    &basic_ty,
                    self.context,
                    &mut self.type_system,
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
    ) -> Result<ExprInfo<'src, 'ts>, CouldNotDetermineType> {
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

    pub fn check_type(
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
        target_class_def: &'ts ClassDef<'src>,
        args: &'ast [Spanned<'src, ast::Expr<'src>>],
    ) -> Result<ExprInfo<'src, 'ts>, CouldNotDetermineType> {
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

    fn resolve_class(
        &mut self,
        ty: &CheckedType<'src>,
    ) -> Result<&'ts ClassDef<'src>, CouldNotDetermineType> {
        match ty {
            CheckedType::TypeRef(name) => match self.type_system.lookup_class(*name) {
                None => Err(CouldNotDetermineType),
                Some(class_def) => Ok(class_def),
            },
            _ => Err(CouldNotDetermineType),
        }
    }

    /// Returned ExprInfo.ref_info is always Some(_)
    fn check_var(
        &mut self,
        var_name: &Spanned<'src, Symbol<'src>>,
    ) -> Result<ExprInfo<'src, 'ts>, CouldNotDetermineType> {
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
                    ref_info: Some(RefInfo::Param(param_def)),
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
