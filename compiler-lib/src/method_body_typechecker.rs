use crate::{asciifile::Spanned, asciifile::Span, ast, context::Context, strtab::Symbol,
    symtab::*, type_system::*, semantics2::*};
use failure::Fail;

enum IdentLookupResult<'t> {
    LocalVar(LocalVarDef<'t>),
    ThisField(ClassFieldDef<'t>),
    Class(ClassDef<'t>),
}

struct LocalVarDef<'src> {
    name: Symbol<'src>,
    ty: CheckedType<'src>,
}

pub struct MethodBodyTypeChecker<'src, 'sem> {
    context: &'sem SemanticContext<'src>,
    type_system: &'sem TypeSystem<'src>,
    current_class: &'sem ClassDef<'src>,
    current_method: &'sem ClassMethodDef<'src>,
    local_var_scope: Scoped<'src, LocalVarDef<'src>>,
}

impl<'src, 'sem> MethodBodyTypeChecker<'src, 'sem> {
    pub fn check_methods(
        class_decl: &'sem ast::ClassDeclaration<'src>,
        type_system: &'sem TypeSystem<'src>,
        context: &'sem SemanticContext<'src>,
    ) {
        let current_class = type_system.resolve_type_ref(class_decl.name).unwrap();

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
                        local_var_scope: Scoped::new(),
                    };

                    checker.check_type_block(block);
                }
            }
        }
    }

    fn check_type_block(&mut self, block: &ast::Block<'src>) {
        self.local_var_scope.enter_scope();
        for stmt in &block.statements {
            self.check_type_stmt(stmt)?;
        }
        self.local_var_scope.leave_scope().unwrap();
    }

    fn check_type_stmt(&mut self, stmt: &ast::Stmt<'src>) {
        use self::ast::Stmt::*;
        match &stmt {
            Block(block) => self.check_type_block(block),
            Empty => {},
            If(cond, stmt, opt_else) => {
                let cond_type = self.get_type_expr(cond);

                match cond_type {
                    Ok(Boolean) => {},
                    Ok(_) => {
                        self.context.report_error(&cond.span,
                            SemanticError::ConditionMustBeBoolean
                        )
                    }
                }

                self.check_type_stmt(&stmt);
                if let Some(els) = opt_else {
                    self.check_type_stmt(&els);
                }
            }
            While(cond, stmt) => {
                let cond_type = self.get_type_expr(cond);

                match cond_type {
                    Ok(Boolean) => {},
                    Ok(_) => {
                        self.context.report_error(&cond.span,
                            SemanticError::ConditionMustBeBoolean
                        )
                    }
                }

                self.check_type_stmt(&stmt);
            }
            Expression(expr) => {
                self.get_type_expr(expr);
            }
            Return(expr_opt) => {
                let ty = match expr_opt {
                    None => Ok(CheckedType::Void),
                    Some(expr) => self.get_type_expr(expr),
                };

                // todo
            }
            LocalVariableDeclaration(ty, name, opt_assign) => {
                // todo check parameters
                self.local_var_scope
                    .define(
                        name,
                        LocalVarDef {
                            name: *name,
                            ty: CheckedType::from(&ty.data),
                        },
                    )
                    .unwrap_or_else(|_| {
                        self.context.report_error(&ty.span,
                            SemanticError::RedefinitionError {
                                kind: "local var".to_string(),
                                name: name.to_string(),
                            }
                        )
                    });

                if let Some(assign) = opt_assign {
                    self.get_type_expr(assign);
                    // todo
                }
            }
        }
    }

    fn resolve_class(&mut self, ty: &CheckedType<'src>) -> Result<&'sem ClassDef<'src>, ()> {
        match ty {
            CheckedType::TypeRef(name) => match self.type_system.resolve_type_ref(*name) {
                None => Err(()),
                Some(class_def) => Ok(class_def),
            },
            _ => Err(()),
        }
    }

    fn get_type_expr(&mut self, expr: &ast::Expr<'src>) -> Result<CheckedType<'src>, ()> {
        use crate::{ast::Expr::*, type_system::*};
        match expr {
            Binary(op, lhs, rhs) => {
                let lhs_type = self.get_type_expr(lhs)?;
                let rhs_type = self.get_type_expr(rhs)?;
                // todo
                Ok(lhs_type)
            }
            Unary(op, expr) => {
                let expr_type = self.get_type_expr(expr)?;
                // todo
                Ok(expr_type)
            }
            FieldAccess(target_expr, name) => {
                let target_type = self.get_type_expr(&target_expr)?;
                let target_class = self.resolve_class(&target_type)?;
                let field = target_class.get_field(*name).ok_or(())?;

                Ok(field.ty.clone())
            }
            ArrayAccess(target_expr, idx_expr) => {
                let idx_type = self.get_type_expr(idx_expr)?;
                // todo check that idx_type is numeric
                let array_type = self.get_type_expr(target_expr)?;

                match array_type {
                    CheckedType::Array(item_type) => Ok(*item_type),
                    _ => Err(()),
                }
            }
            Null => {
                // todo
                Err(())
            }
            Boolean(_) => Ok(CheckedType::Boolean),
            Int(_) => Ok(CheckedType::Int),
            Var(name) => self.visible_definition_var(*name),
            MethodInvocation(target_expr, name, args) => {
                let target_type = self.get_type_expr(&target_expr)?;
                let target_class = self.resolve_class(&target_type)?;
                self.check_method_invocation(*name, target_class, args)
            }
            ThisMethodInvocation(name, args) => {
                if self.current_method.is_static {
                    Err(())
                } else {
                    self.check_method_invocation(*name, &self.current_class, args)
                }
            }
            This => {
                if self.current_method.is_static {
                    Err(())
                } else {
                    Ok(CheckedType::TypeRef(self.current_class.name))
                }
            }
            NewObject(name) => {
                let t = CheckedType::TypeRef(*name);
                self.resolve_class(&t);

                Ok(t)
            }
            NewArray(_basic_ty, _size, _dimension) => {
                /*let ty = basic_type_to_checked_type(basic_ty);
                self.get_type_expr(size);*/

                // new int[10][][];
                // todo
                Err(())
            }
        }
    }

    fn check_method_invocation(
        &mut self,
        method_name: Symbol<'src>,
        target_class_def: &ClassDef<'src>,
        args: &ast::ArgumentList<'src>,
    ) -> Result<CheckedType<'src>, ()> {
        let method = target_class_def.get_method(method_name).ok_or(())?;

        // todo validate args

        Ok(method.return_ty.clone())
    }

    fn visible_definition_var(&mut self, var_name: Symbol<'src>) -> Result<CheckedType<'src>, ()> {
        // params
        if let Some(local_var_def) = self.local_var_scope.visible_definition(var_name) {
            Ok(local_var_def.ty.clone())
        }
        // this fields (?)
        // classes (forbidden)
        // globals
        else {
            Err(())
        }
    }
}
