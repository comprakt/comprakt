use crate::{asciifile::Spanned, ast, context, strtab::Symbol, visitor::NodeKind};
use failure::{format_err, Error, ResultExt};
use std::{collections::HashMap, rc::Rc};

type ClassesAndMembers<'a, 'f> = HashMap<
    (Symbol, Symbol, ast::ClassMemberKindDiscriminants),
    &'a Spanned<'f, ast::ClassMember<'f>>,
>;

pub fn check<'a, 'f>(ast: &'a ast::AST<'f>, context: &context::Context<'_>) -> Result<(), Error> {
    let classes_and_members = ClassesAndMembersVisitor::visit(ast, context)?;

    // Let's draw some pretty annotations that show we classified classes + their
    // members and method correctly
    for ((classsym, membersym, memberytypediscr), decl) in &classes_and_members {
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
    SecondPass::visit(ast, context, &classes_and_members)
}

struct ClassesAndMembersVisitor;

impl ClassesAndMembersVisitor {
    pub fn visit<'a, 'f>(
        ast: &'a ast::AST<'f>,
        context: &context::Context<'_>,
    ) -> Result<ClassesAndMembers<'a, 'f>, Error> {
        let mut res = HashMap::new();
        Self::do_visit(&NodeKind::from(ast), context, &mut res)?;
        Ok(res)
    }

    fn do_visit<'a, 'f>(
        n: &NodeKind<'a, 'f>,
        _context: &context::Context<'_>,
        res: &mut ClassesAndMembers<'a, 'f>,
    ) -> Result<(), Error> {
        use self::{ast, NodeKind::*};
        match n {
            AST(ast::AST::Program(p)) => {
                for class in &p.data.classes {
                    for member in class.members.iter() {
                        let discr = ast::ClassMemberKindDiscriminants::from(&member.kind);
                        res.insert(
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
