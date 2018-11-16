use crate::{asciifile::Spanned, ast, context::Context, strtab::Symbol};
use failure::{Error, Fail};
use std::{collections::HashMap, rc::Rc};
use crate::type_system::*;
use crate::symtab::*;

#[derive(Debug, Fail)]
enum SemanticError {
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
        name
    )]
    ThisMethodInvocationInStaticMethod { name: String },
    #[fail(display = "non-static variable 'this' cannot be referenced from a static context")]
    ThisInStaticMethod,
}

pub fn check<'a, 'src>(ast: &'a ast::AST<'src>, context: &Context<'src>) -> Result<(), Error> {
    match ast {
        ast::AST::Empty => {

        }
        ast::AST::Program(prog) => {
            let type_system = build_type_system(context, prog);

            //println!("{:#?}", type_system);

            context.diagnostics.info(&Spanned {
                span: prog.span.clone(),
                data: format!("test"),
            });
        }
    }
    Ok(())
}

fn build_type_system<'src>(context: &Context<'src>, program: &ast::Program<'src>) -> TypeSystem<'src> {
    let mut type_system = TypeSystem::new();
    for class_decl in &program.classes {
        let mut class_def = ClassDef::new(class_decl.name);
        
        for member in &class_decl.members {
            use crate::ast::ClassMemberKind::*;
            match &member.kind {
                Field(ty) => {
                    class_def.add_field(
                        ClassFieldDef {
                            name: member.name,
                            ty: type_to_checked_type(&ty)
                        }
                    );
                }
                Method(ty, params, _) => {
                    let return_ty = type_to_checked_type(&ty);
                    class_def.add_method(new_class_method_def(
                        member.name, &params, return_ty, true));
                }
                MainMethod(params, _) => {
                    class_def.add_method(new_class_method_def(
                        member.name, &params, CheckedType::Void, true));
                }
            }
        }

        type_system.add_class_def(class_def);
    }

    type_system
}

fn new_class_method_def<'src>(
    name: Symbol<'src>, params: &ast::ParameterList<'src>,
    return_ty: CheckedType<'src>, is_static: bool
) -> ClassMethodDef<'src> {

    ClassMethodDef {
        is_static: is_static,
        name: name,
        return_ty: return_ty,
        params: params.iter()
            .map(|p| MethodParamDef {
                name: p.name,
                ty: type_to_checked_type(&p.ty),
            })
            .collect()
    }
}

fn type_to_checked_type<'t>(ty: &ast::Type<'t>) -> CheckedType<'t> {
    let mut checked_ty = basic_type_to_checked_type(&ty.basic);

    for _ in 0..ty.array_depth {
        checked_ty = CheckedType::Array(Box::new(checked_ty));
    }
    
    checked_ty
}

fn basic_type_to_checked_type<'t>(basic_ty: &ast::BasicType<'t>) -> CheckedType<'t> {
    use self::ast::BasicType::*;
    match basic_ty {
        Int => CheckedType::Int,
        Boolean => CheckedType::Boolean,
        Void => CheckedType::Void,
        Custom(symbol) => CheckedType::TypeRef(*symbol),
    }
}

enum IdentLookupResult<'t> {
    LocalVar(LocalVarDef<'t>),
    ThisField(ClassFieldDef<'t>),
    Class(ClassDef<'t>),
}

struct LocalVarDef<'src> {
    name: Symbol<'src>,
    ty: CheckedType<'src>,
}


fn check_types<'t>(class_decl: &ast::ClassDeclaration<'t>, type_system: &'t TypeSystem<'t>, context: &'t Context<'t>) {
    let current_class = type_system.resolve_type_ref(class_decl.name).unwrap();

    for member in &class_decl.members {
        use self::ast::ClassMemberKind::*;
        match &member.kind {
            Field(ty) => {}
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

struct MethodBodyTypeChecker<'src, 'sem> {
    context: &'sem Context<'src>,
    type_system: &'sem TypeSystem<'src>,
    current_class: &'sem ClassDef<'src>,
    current_method: &'sem ClassMethodDef<'src>,
    local_var_scope: Scoped<'src, LocalVarDef<'src>>,
}

impl<'src, 'sem> MethodBodyTypeChecker<'src, 'sem> {
    fn check_type_block(&mut self, block: &ast::Block<'src>) -> Result<(), ()> {
        self.local_var_scope.enter_scope();

        // TODO Something with drop guards for scopes would be nicer
        let res = block.statements.iter().map(|stmt| self.check_type_stmt(stmt))
            .fold(Ok(()), |acc, res| acc.and(res));

        self.local_var_scope.leave_scope().unwrap();

        res
    }

    fn check_type_stmt(&mut self, stmt: &ast::Stmt<'src>) -> Result<(), ()> {
        use self::ast::Stmt::*;
        match &stmt {
            Block(block) => self.check_type_block(block),
            Empty => Ok(()),
            If(cond, stmt, opt_else) => {
                let cond_type = self.get_type_expr(cond)?;
                // todo
                self.check_type_stmt(&stmt)?;
                if let Some(els) = opt_else {
                    self.check_type_stmt(&els)?;
                }

                Ok(())
            }
            While(cond, stmt) => {
                let cond_type = self.get_type_expr(cond)?;
                // todo
                self.check_type_stmt(&stmt)?;
                Ok(())
            }
            Expression(expr) => {
                self.get_type_expr(expr)?;
                Ok(())
            }
            Return(expr_opt) => {
                let ty = match expr_opt {
                    None => CheckedType::Void,
                    Some(expr) => self.get_type_expr(expr)?
                };
                // todo

                Ok(())
            }
            LocalVariableDeclaration(ty, name, opt_assign) => {

                let checked_ty = type_to_checked_type(&ty);
                // todo check parameters
                // todo catch already defined
                self.local_var_scope.define(name,
                    LocalVarDef {
                        name: *name,
                        ty: checked_ty
                    }
                ).map_err(|_| ())?; // TODO Real error

                if let Some(assign) = opt_assign {

                }

                Ok(())
            }
        }
    }

    fn resolve_class(&mut self, ty: &CheckedType<'src>) -> Result<&'sem ClassDef<'src>, ()> {
        match ty {
            CheckedType::TypeRef(name) => {
                match self.type_system.resolve_type_ref(*name) {
                    None => Err(()),
                    Some(class_def) => Ok(class_def)
                }
            }
            _ => Err(())
        }
    }

    fn get_type_expr(&mut self, expr: &ast::Expr<'src>) -> Result<CheckedType<'src>, ()> {
        use crate::ast::Expr::*;
        use crate::type_system::*;
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
                    CheckedType::Array(item_type) => {
                        Ok(*item_type)
                    },
                    _ => {
                        Err(())
                    }
                }
            }
            Null => {
                // todo
                Err(())
            },
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
                }
                else {
                    self.check_method_invocation(*name, &self.current_class, args)
                }
            }
            This => {
                if self.current_method.is_static {
                    Err(())
                }
                else {
                    Ok(CheckedType::TypeRef(self.current_class.name))
                }
            },
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
        args: &ast::ArgumentList<'src>
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
