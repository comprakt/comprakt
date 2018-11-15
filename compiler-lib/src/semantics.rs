use crate::{asciifile::Spanned, ast, context::Context, strtab::Symbol, visitor::NodeKind};
use failure::{format_err, Error, Fail};
use std::{collections::HashMap, rc::Rc};

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

type ClassesAndMembers<'a, 'f> = HashMap<
    (Symbol<'a>, Symbol<'a>, ast::ClassMemberKindDiscriminants),
    &'a Spanned<'f, ast::ClassMember<'f>>,
>;

pub fn check<'a, 'f>(ast: &'a ast::AST<'f>, context: &Context<'_>) -> Result<(), Error> {
    let mut first_pass_visitor = ClassesAndMembersVisitor::new(context);
    first_pass_visitor.do_visit(&NodeKind::from(ast));

    context.diagnostics.abort_if_errored();

    // Let's draw some pretty annotations that show we classified classes + their
    // members and method correctly
    for ((classsym, membersym, memberytypediscr), decl) in &first_pass_visitor.classes_and_members {
        // FIXME: Need ClassMember.name be Spanned instead of Symbol
        let highlightspan = &decl.span;
        context.diagnostics.info(&Spanned {
            span: highlightspan.clone(),
            data: format!("{}.{} ({})", classsym, membersym, memberytypediscr),
        });
    }

    // Now the second pass, this one will do (in one traversal)
    //  - name analysis of the AST
    //  - type checking of expressions
    SecondPass::visit(ast, context, &first_pass_visitor.classes_and_members)
}

struct ClassesAndMembersVisitor<'a, 'f, 'cx> {
    context: &'cx Context<'cx>,
    classes_and_members: ClassesAndMembers<'a, 'f>,
    static_method_found: u64,
}

impl<'a, 'f, 'cx> ClassesAndMembersVisitor<'a, 'f, 'cx> {
    pub fn new(context: &'cx Context<'_>) -> Self {
        Self {
            context,
            classes_and_members: HashMap::new(),
            static_method_found: 0,
        }
    }

    fn do_visit(&mut self, node: &NodeKind<'a, 'f>) {
        use self::{ast, NodeKind::*};
        node.for_each_child(&mut |child| {
            match child {
                AST(_) | ClassDeclaration(_) => (),
                Program(prog) => {
                    for class in &prog.classes {
                        for member in &class.members {
                            let discr = ast::ClassMemberKindDiscriminants::from(&member.kind);
                            self.classes_and_members.insert(
                                (class.name, member.name, discr),
                                &member,
                            );
                        }
                    }
                }
                ClassMember(member) => {
                    if let ast::ClassMemberKind::MainMethod(params, block) = &member.kind {
                        debug_assert!(params.len() == 1);
                        self.static_method_found += 1;
                        if member.name.to_string() != "main" {
                            self.context.diagnostics.error(&Spanned {
                                span: member.span.clone(),
                                data: SemanticError::StaticMethodNotMain,
                            });
                        }
                        if self.static_method_found > 1 {
                            self.context.diagnostics.error(&Spanned {
                                span: member.span.clone(),
                                data: SemanticError::MultipleStaticMethods {
                                    amount: self.static_method_found,
                                },
                            })
                        }
                        self.visit_static_block(&NodeKind::from(block), &params[0].name);
                    }
                }

                _ => (),
            };

            self.do_visit(&child)
        });
    }

    fn visit_static_block(&mut self, node: &NodeKind<'a, 'f>, arg_name: &Symbol<'_>) {
        use self::NodeKind::*;
        node.for_each_child(&mut |child| {
            match child {
                Stmt(stmt) => {
                    if let ast::Stmt::LocalVariableDeclaration(_, name, _) = &stmt.data {
                        if arg_name == name {
                            self.context.diagnostics.error(&Spanned {
                                span: stmt.span.clone(),
                                data: SemanticError::RedefinitionError {
                                    kind: "parameter".to_string(),
                                    name: arg_name.to_string(),
                                },
                            })
                        }
                    }
                }
                Expr(expr) => match &expr.data {
                    ast::Expr::Var(name) => {
                        if arg_name == name {
                            self.context.diagnostics.error(&Spanned {
                                span: expr.span.clone(),
                                data: SemanticError::MainMethodParamUsed {
                                    name: arg_name.to_string(),
                                },
                            });
                        }
                    }
                    ast::Expr::This => {
                        self.context.diagnostics.error(&Spanned {
                            span: expr.span.clone(),
                            data: SemanticError::ThisInStaticMethod,
                        });
                    }
                    ast::Expr::ThisMethodInvocation(name, _) => {
                        // This is for sure an error since there is only one static method and this
                        // is the main function. We cannot call the main function from within the
                        // main function, since we don't have a `String` type to create an argument
                        // for the main function. We also cannot use the argument of the main
                        // function to call the main function.
                        self.context.diagnostics.error(&Spanned {
                            span: expr.span.clone(),
                            data: SemanticError::ThisMethodInvocationInStaticMethod {
                                name: name.to_string(),
                            },
                        });
                    }
                    _ => (),
                },
                _ => (),
            }

            self.visit_static_block(&child, arg_name)
        });
    }
}

type ScopeDefs<'a, T> = HashMap<Symbol<'a>, T>;

struct Scope<'a, T> {
    parent: Option<Rc<Scope<'a, T>>>,
    definitions: ScopeDefs<'a, T>,
}

impl<'a, T> Scope<'a, T> {
    pub fn root() -> Rc<Self> {
        Rc::new(Scope {
            parent: None,
            definitions: ScopeDefs::new(),
        })
    }
    pub fn enter_scope(parent: Rc<Self>) -> Self {
        Scope {
            parent: Some(parent),
            definitions: ScopeDefs::new(),
        }
    }
    pub fn leave(self) -> Option<Rc<Self>> {
        self.parent
    }
    pub fn define(&mut self, sym: &Symbol<'a>, val: T) -> Result<(), ()> {
        use std::collections::hash_map::Entry;
        match self.definitions.entry(*sym) {
            Entry::Occupied(o) => return Err(()),
            Entry::Vacant(e) => e.insert(val),
        };
        Ok(())
    }
}

struct SecondPass;

// TODO proper enum, but Type is definitely necessary
type VarDef<'a, 'f> = &'a Spanned<'f, ast::Type<'a>>;
struct TyDef; // TODO
struct MethDef; // TODO

impl SecondPass {
    pub fn visit<'a, 'f>(
        ast: &'a ast::AST<'f>,
        context: &Context<'_>,
        _classes_and_members: &ClassesAndMembers<'a, 'f>,
    ) -> Result<(), Error> {
        // TODO java.lang.System.out scope
        // TODO ontop of that, scope of this package
        let mut vardefs: Rc<Scope<'a, VarDef<'a, 'f>>> = Scope::root();
        let mut tydefs: Rc<Scope<'a, TyDef>> = Scope::root();
        let mut methdefs: Rc<Scope<'a, MethDef>> = Scope::root();
        Self::do_visit(&NodeKind::from(ast), context, vardefs, tydefs, methdefs)
    }

    fn do_visit<'a, 'f>(
        n: &NodeKind<'a, 'f>,
        context: &Context<'_>,
        vardefs: Rc<Scope<'a, VarDef<'a, 'f>>>,
        tydefs: Rc<Scope<'a, TyDef>>,
        methdefs: Rc<Scope<'a, MethDef>>,
    ) -> Result<(), Error> {
        use self::NodeKind::*;
        match n {
            ClassMember(m) if m.kind.is_method() => {
                // Assume types have been checked
                // TODO enforce somewhere that main has String[] arg type
                let params = m.kind.method_params().unwrap();
                let mut vardefs = Scope::enter_scope(vardefs);
                for p in &params.data {
                    vardefs
                        .define(&p.name, &p.ty)
                        .or(Err(format_err!("re-definition of variable in same scope")))?;
                }

                let _body = m.kind.method_body().unwrap();
                println!("TODO analyze body statements for {}", m.name);
                Ok(())
            }
            x => x
                .for_each_child(&mut |c| {
                    Self::do_visit(
                        &NodeKind::from(c),
                        context,
                        Rc::clone(&vardefs),
                        Rc::clone(&tydefs),
                        Rc::clone(&methdefs),
                    )
                })
                .unwrap_or(Ok(())),
        }
    }
}
