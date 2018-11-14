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
}

type ClassesAndMembers<'a, 'f> = HashMap<
    (Symbol, Symbol, ast::ClassMemberKindDiscriminants),
    &'a Spanned<'f, ast::ClassMember<'f>>,
>;

pub fn check<'a, 'f>(ast: &'a ast::AST<'f>, context: &context::Context<'_>) -> Result<(), Error> {
    let mut first_pass_visitor = ClassesAndMembersVisitor::new();
    first_pass_visitor.do_visit(&NodeKind::from(ast))?;

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

struct ClassesAndMembersVisitor<'a, 'f> {
    classes_and_members: ClassesAndMembers<'a, 'f>,
}

impl<'a, 'f> ClassesAndMembersVisitor<'a, 'f> {
    pub fn new() -> Self {
        Self {
            classes_and_members: HashMap::new(),
        }
    }

    fn do_visit(
        &mut self,
        node: &NodeKind<'a, 'f>,
    ) -> Result<(), Error> {
        use self::{ast, NodeKind::*};
        match node {
            AST(ast::AST::Program(p)) => {
                for class in &p.data.classes {
                    for member in &class.members {
                        let discr = ast::ClassMemberKindDiscriminants::from(&member.kind);
                        self.classes_and_members.insert(
                            (Rc::clone(&class.name), Rc::clone(&member.name), discr),
                            &member,
                        );
                    }
                }
                Ok(())
            }
            x => {
                return Err(format_err!(
                    "unexpected ast node passed to first pass: {:?}",
                    x
                ))
            }
        }
    }
}

type ScopeDefs<T> = HashMap<Symbol, T>;

struct Scope<T> {
    parent: Option<Rc<Scope<T>>>,
    definitions: ScopeDefs<T>,
}

impl<T> Scope<T> {
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
    pub fn define(&mut self, sym: &Symbol, val: T) -> Result<(), ()> {
        use std::collections::hash_map::Entry;
        let key = Rc::clone(sym);
        match self.definitions.entry(key) {
            Entry::Occupied(o) => return Err(()),
            Entry::Vacant(e) => e.insert(val),
        };
        Ok(())
    }
}

struct SecondPass;

// TODO proper enum, but Type is definitely necessary
type VarDef<'a, 'f> = &'a Spanned<'f, ast::Type>;
struct TyDef; // TODO
struct MethDef; // TODO

impl SecondPass {
    pub fn visit<'a, 'f>(
        ast: &'a ast::AST<'f>,
        context: &context::Context<'_>,
        _classes_and_members: &ClassesAndMembers<'a, 'f>,
    ) -> Result<(), Error> {
        // TODO java.lang.System.out scope
        // TODO ontop of that, scope of this package
        let mut vardefs: Rc<Scope<VarDef<'a, 'f>>> = Scope::root();
        let mut tydefs: Rc<Scope<TyDef>> = Scope::root();
        let mut methdefs: Rc<Scope<MethDef>> = Scope::root();
        Self::do_visit(&NodeKind::from(ast), context, vardefs, tydefs, methdefs)
    }

    fn do_visit<'a, 'f>(
        n: &NodeKind<'a, 'f>,
        context: &context::Context<'_>,
        vardefs: Rc<Scope<VarDef<'a, 'f>>>,
        tydefs: Rc<Scope<TyDef>>,
        methdefs: Rc<Scope<MethDef>>,
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
