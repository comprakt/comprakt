use crate::{
    asciifile::{MaybeSpanned, Span, Spanned},
    ast,
    context::Context,
    strtab::{self, Symbol},
    visitor::NodeKind,
};
use failure::Fail;
use std::collections::HashMap;

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
    #[fail(display = "Only the 'main' method can be static")]
    StaticMethodNotMain,
    #[fail(display = "No 'main' method found")]
    NoMainMethod,
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
    #[fail(display = "method '{}' might not return", method_name)]
    MightNotReturn { method_name: String },
}

type ClassesAndMembers<'a, 'f> = HashMap<
    (Symbol<'a>, Symbol<'a>, ast::ClassMemberKindDiscriminants),
    &'a Spanned<'f, ast::ClassMember<'f>>,
>;

/// `check` returns an `Err` iff at least one errors was emitted through
/// `context`.
pub fn check<'a, 'f>(
    strtab: &mut strtab::StringTable<'f>,
    ast: &'a ast::AST<'f>,
    context: &Context<'f>,
) -> Result<(), ()> {
    let mut first_pass_visitor = ClassesAndMembersVisitor::new(context);
    first_pass_visitor.do_visit(&NodeKind::from(ast));

    // Check if a static method was found. If multiple static methods were found or
    // the static method is not called `main` the error is already emitted in
    // the visitor
    if first_pass_visitor.static_method_found == 0 {
        context
            .diagnostics
            .error(&MaybeSpanned::WithoutSpan(SemanticError::NoMainMethod));
    }

    if context.diagnostics.errored() {
        return Err(());
    }

    crate::type_checking::check(strtab, &ast, &context);
    if context.diagnostics.errored() {
        return Err(());
    }
    Ok(())
}

struct ClassesAndMembersVisitor<'a, 'f, 'cx> {
    context: &'cx Context<'cx>,
    classes_and_members: ClassesAndMembers<'a, 'f>,
    static_method_found: u64,
    class_member_to_its_span: HashMap<*const ast::ClassMember<'f>, Span<'f>>,
}

impl<'a, 'f, 'cx> ClassesAndMembersVisitor<'a, 'f, 'cx> {
    pub fn new(context: &'cx Context<'_>) -> Self {
        Self {
            context,
            classes_and_members: HashMap::new(),
            static_method_found: 0,
            class_member_to_its_span: HashMap::new(),
        }
    }

    fn do_visit(&mut self, node: &NodeKind<'a, 'f>) {
        use self::{ast, NodeKind::*};
        node.for_each_child(&mut |child| {
            match child {
                ClassDeclaration(decl) => {
                    let decl_node = NodeKind::from(decl);
                    decl_node.for_each_child(&mut |member_node| {
                        let member_decl: &Spanned<'_, ast::ClassMember<'_>> = match member_node {
                            NodeKind::ClassMember(m) => m,
                            _ => panic!("class children are expected to be class members"),
                        };
                        self.class_member_to_its_span
                            .insert(&member_decl.data as *const _, member_decl.span);
                    });
                }
                Program(prog) => {
                    for class in &prog.classes {
                        for member in &class.members {
                            let discr = ast::ClassMemberKindDiscriminants::from(&member.kind);
                            self.classes_and_members
                                .insert((class.name.data, member.name, discr), &member);
                        }
                    }
                }

                ClassMember(member) => {
                    if let ast::ClassMemberKind::MainMethod(params, block) = &member.kind {
                        debug_assert!(params.len() == 1);
                        self.static_method_found += 1;
                        if &member.name != "main" {
                            self.context.diagnostics.error(&Spanned {
                                span: member.span,
                                data: SemanticError::StaticMethodNotMain,
                            });
                        }
                        if self.static_method_found > 1 {
                            self.context.diagnostics.error(&Spanned {
                                span: member.span,
                                data: SemanticError::MultipleStaticMethods {
                                    amount: self.static_method_found,
                                },
                            })
                        }
                        self.visit_static_block(&NodeKind::from(block), &params[0].name);
                    }

                    match &member.kind {
                        ast::ClassMemberKind::Method(ty, pl, block)
                            if ty.basic.data != ast::BasicType::Void =>
                        {
                            let ptr = (&member.data) as *const _;
                            let member_decl = self.class_member_to_its_span.get(&ptr).expect(
                                "must have current_member_decl while while visiting ClassMember",
                            );
                            let highlight_span = Span::from_positions(&[
                                member_decl.start_position(),
                                pl.span.end_position(),
                            ])
                            .unwrap();
                            self.check_method_always_returns(&member.name, highlight_span, block)
                        }
                        _ => (),
                    }
                }

                _ => (),
            };

            self.do_visit(&child)
        });
    }

    fn check_method_always_returns(
        &self,
        method_name: &Symbol<'_>,
        hightlight_span: Span<'_>,
        method_body: &Spanned<'_, ast::Block<'_>>,
    ) {
        fn always_returns<'t>(stmt: &Spanned<'t, ast::Stmt<'t>>) -> bool {
            match &stmt.data {
                // An if-else stmt always returns iff both arms always return
                ast::Stmt::If(_, then_arm, else_arm) => {
                    let then_arm_always_returns = always_returns(&*then_arm);
                    let else_arm_always_returns = else_arm
                        .as_ref()
                        .map_or(true, |else_arm| always_returns(&*else_arm));

                    then_arm_always_returns && else_arm_always_returns
                }

                // An empty block does not return
                ast::Stmt::Block(block) if block.statements.is_empty() => false,
                // A non-empty block always returns iff all of its statements
                // always return
                ast::Stmt::Block(block) => block.statements.iter().all(always_returns),

                // A return stmt always returns
                ast::Stmt::Return(_) => true,

                // All other stmts do not always return
                _ => false,
            }
        }

        // FIXME de-duplicate empty block logic from always_returns
        if method_body.statements.is_empty() || !method_body.statements.iter().all(always_returns) {
            self.context.diagnostics.error(&Spanned {
                span: hightlight_span,
                data: SemanticError::MightNotReturn {
                    method_name: format!("{}", method_name),
                },
            });
        }
    }

    fn visit_static_block(&mut self, node: &NodeKind<'a, 'f>, arg_name: &Symbol<'_>) {
        use self::NodeKind::*;
        node.for_each_child(&mut |child| {
            match child {
                Stmt(stmt) => {
                    if let ast::Stmt::LocalVariableDeclaration(_, name, _) = &stmt.data {
                        if arg_name == &name.data {
                            self.context.diagnostics.error(&Spanned {
                                span: stmt.span,
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
                        if arg_name == &name.data {
                            self.context.diagnostics.error(&Spanned {
                                span: expr.span,
                                data: SemanticError::MainMethodParamUsed {
                                    name: arg_name.to_string(),
                                },
                            });
                        }
                    }
                    ast::Expr::This => {
                        self.context.diagnostics.error(&Spanned {
                            span: expr.span,
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
                            span: expr.span,
                            data: SemanticError::ThisMethodInvocationInStaticMethod {
                                name: name.data.to_string(),
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
