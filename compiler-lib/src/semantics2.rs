
use crate::{asciifile::Spanned, ast, context::Context, strtab::Symbol, visitor::NodeKind, type_system};
use failure::{format_err, Error, Fail};
use std::{collections::HashMap, rc::Rc};
use crate::type_system::*;

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

pub fn check<'a, 'f>(ast: &'a ast::AST<'f>, context: &Context<'_>) -> Result<(), Error> {
    match ast {
        ast::AST::Empty => {

        }
        ast::AST::Program(prog) => {
            let type_system = build_type_system(context, prog);

            context.diagnostics.info(&Spanned {
                span: prog.span.clone(),
                data: format!("test"),
            });  
        }
    }
    Ok(())
}

fn build_type_system<'t>(context: &'_ Context<'_>, program: &'_ ast::Program<'t>) -> TypeSystem<'t> {
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

fn new_class_method_def<'t>(
    name: Symbol<'t>, params: &ast::ParameterList<'t>,
    return_ty: CheckedType<'t>, is_static: bool
) -> ClassMethodDef<'t> {

    ClassMethodDef {
        is_static: is_static,
        name: name,
        return_ty: return_ty,
        params: params.iter()
            .map(|p| type_system::MethodParamDef {
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
        Custom(symbol) => {
            CheckedType::TypeRef(CheckedTypeRef::new(*symbol))
        }
    }
}

/*
impl<'context> TypeChecker<'context> {

    pub fn new(context: &'context Context<'context>) -> TypeChecker<'context> {
        TypeChecker {
            context: context,
            type_system: TypeSystem::new(),
            this_type: None,
        }
    }

    pub fn check<'ast>(&mut self, ast: &'_ ast::AST<'ast>) {
        match ast {
            ast::AST::Empty => {

            }
            ast::AST::Program(prog) => {
                self.build_type_system(prog);

                self.context.diagnostics.info(&Spanned {
                    span: prog.span.clone(),
                    data: format!("test"),
                });  
            }
        }
    }
}*/
/*
struct TypeChecker<'context> {
    context: &'context Context<'context>,
    type_system: &'context TypeSystem,
    current_class: Option<type_system::ClassDef>,
    in_static_method: bool,
}

impl<'t> TypeChecker<'t> {
    pub fn new(context: &'t Context<'t>, type_system: &'t TypeSystem) -> TypeChecker<'t> {
        TypeChecker {
            context: context,
            type_system: type_system,
            current_class: None,
            in_static_method: false,
        }
    }

    fn check_types<'ast>(&mut self, class_decl: ast::ClassDeclaration<'ast>) {

        let current_class_ref = CheckedTypeReference::new(&class_decl.name);
        let current_class = self.type_system.resolve_type_reference(this_type_ref).unwrap();
        self.current_class = Some(current_class);

        for member in class_decl.members {
            match member.kind {
                Field(ty) => {}
                Method(ty, params, block) => {
                    self.check_type_block(block);
                }
                MainMethod(params, block) => {
                    self.check_type_block(block);
                }
            }
        }
    }

    fn check_type_block<'ast>(&mut self, block: &ast::Block<'ast>) -> Result<(), ()> {
        for stmt in block.statements {
            self.check_type_stmt(&stmt)?;
        }
        Ok(())
    }

    fn check_type_stmt<'ast>(&mut self, stmt: &ast::Stmt<'ast>) -> Result<(), ()> {
        match stmt {
            Block(block) => self.check_type_block(block),
            Empty => Ok(()),
            If(cond, stmt, opt_else) => {
                let cond_type = self.get_type_expr(cond)?;
                    
                self.check_type_stmt(&stmt)?;
                self.check_type_stmt(&opt_else)?;
                Ok(())
            }
            While(cond, stmt) => {
                self.check_type_stmt(&stmt)?;
                Ok(())
            }
            Expression(expr) => {
                self.get_type_expr(expr)?;
                Ok(())
            }
            Return(expr_opt) => {
                match expr_opt {
                    None => {

                    }
                    Some(expr) => {
                        self.get_type_expr(expr)?;
                    }
                }
                Ok(())
            }
            LocalVariableDeclaration(ty, name, opt_assign) => {
                Ok(())
            }
        }
    }

    fn get_type_expr<'ast>(&mut self, expr: &ast::Expr<'ast>) -> Result<CheckedType, ()> {
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
                let target_class = self.resolve(target_type)?;
                let field = target_class.get_field(name)?;

                field.ty
            }
            ArrayAccess(target_expr, idx_expr) => {
                let idx_type = self.get_type_expr(idx_expr);
                // todo check that idx_type is numeric
                let array_type = self.get_type_expr(idx_expr);

                match array_type {
                    CheckedType::Array(arrayType) => {
                        Ok(arrayType.get_item_type())
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
            Boolean(val) => Ok(Type::Boolean),
            Int(val) => Ok(Type::Int),
            Var(name) => self.lookup_var(&name),
            MethodInvocation(target_expr, name, args) => {
                let target_type = self.get_type_expr(&target_expr)?;
                let target_class = self.resolve(target_type)?;
                self.check_method_invocation(&name, &target_class, args)
            }
            ThisMethodInvocation(name, args) => {
                if self.in_static_method {
                    Err(())
                }
                else {
                    // there is always a current_class
                    let current_class = self.current_class.unwrap();
                    self.check_method_invocation(&name, &current_class, args)
                }
            }
            This => {
                if self.in_static_method {
                    Err(())
                }
                else {
                    let current_class = self.current_class.unwrap();
                    current_class.name
                }
            },
            NewObject(name) => {
                let class_type_ref = CheckedTypeReference::new(Rc::clone(name));
                let t = CheckedType::TypeReference(class_type_ref);
                self.resolve(t); // todo

                Ok(t)
            }
            NewArray(basic_ty, size, dimension) => {
                let ty = basic_type_to_type(basic_ty);
                
                // new int[10][][];
                // todo
                Err(())
            }
        }
    }

    fn check_method_invocation(&mut self, method_name: &'_ Symbol,
        class_decl: &'_ ast::ClassDeclaration<'_>, args: &'_ ast::ArgumentList<'_>
    ) -> Result<CheckedType, ()> {
        let method = class_decl.get_method(method_name);
        // todo validate args
        method.get_result_type()
    }

    fn lookup_var(&mut self, var_name: &Symbol) -> Result<CheckedType, ()> {
        // local vars
        // this fields (?)
        // classes (forbidden)
        // globals
        Err(())
    }
}

enum LookupResult {
    LocalVar(),
    ThisField(),
    Class(),
}



*/