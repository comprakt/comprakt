//! The visitor module for the AST.
//!
//! The visitor is based on an Enum over every AST node.
//!
//! Example usages:
//! ```rust
//! struct MyVisitor<'a, 'f> {
//!     some_data: Vec<u32>,
//! }
//!
//! impl<'a, 'f> MyVisitor<'a, 'f> {
//!     fn new() -> Self {
//!         Self {
//!             some_data: Vec::new(),
//!         }
//!     }
//!
//!     fn do_visit(&mut self, node: &NodeKind<'a, 'f>) {
//!         use self::NodeKind::*;
//!         node.for_each_child(&mut |child| {
//!             match child {
//!                 AST(_) => (),
//!                 Program(_) => {
//!                     // some code
//!                 }
//!                 _ => ()
//!             }
//!
//!             self.do_visit(&child)
//!         })
//!     }
//! }
//! ```
use crate::{
    asciifile::Spanned,
    ast::{self, *},
};
use strum_macros::EnumDiscriminants;

#[strum_discriminants(derive(Display))]
#[derive(EnumDiscriminants)]
pub enum NodeKind<'a, 't> {
    AST(&'a AST<'t>),
    Program(&'a Spanned<'t, Program<'t>>),
    ClassDeclaration(&'a Spanned<'t, ClassDeclaration<'t>>),
    ClassMember(&'a Spanned<'t, ClassMember<'t>>),
    Parameter(&'a Spanned<'t, Parameter<'t>>),
    ParameterList(&'a Spanned<'t, ParameterList<'t>>),
    Type(&'a Spanned<'t, Type<'t>>),
    BasicType(&'a BasicType<'t>),
    Block(&'a Spanned<'t, Block<'t>>),
    Stmt(&'a Spanned<'t, Stmt<'t>>),
    Expr(&'a Spanned<'t, Expr<'t>>),
    BinaryOp(&'a BinaryOp),
    UnaryOp(&'a UnaryOp),
}

macro_rules! gen_nodekind_match {
    ($nodekindvar:expr, $varname:ident => $rhs:expr) => {
        match $nodekindvar {
            NodeKind::AST($varname) => $rhs,
            NodeKind::ClassDeclaration($varname) => $rhs,
            NodeKind::Program($varname) => $rhs,
            NodeKind::ClassMember($varname) => $rhs,
            NodeKind::Parameter($varname) => $rhs,
            NodeKind::ParameterList($varname) => $rhs,
            NodeKind::Type($varname) => $rhs,
            NodeKind::BasicType($varname) => $rhs,
            NodeKind::Block($varname) => $rhs,
            NodeKind::Stmt($varname) => $rhs,
            NodeKind::Expr($varname) => $rhs,
            NodeKind::BinaryOp($varname) => $rhs,
            NodeKind::UnaryOp($varname) => $rhs,
        }
    };
}

use std::fmt::Debug;
impl Debug for NodeKind<'_, '_> {
    fn fmt(&self, fmt: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        gen_nodekind_match!(self, v => v.fmt(fmt))
    }
}

pub trait VisitResult {
    fn stop_visit(&self) -> bool;
}

impl VisitResult for () {
    fn stop_visit(&self) -> bool {
        false
    }
}

impl<X, E> VisitResult for Result<X, E> {
    fn stop_visit(&self) -> bool {
        self.is_err()
    }
}

impl<'a, 't> NodeKind<'a, 't> {
    /// Visit the children of `self`, invoking `cb` on each.
    /// If `self` has no children, this method returns `None, otherwise
    /// `Some(res)` where `res` is the return value of `cb`.
    #[allow(clippy::cyclomatic_complexity)]
    pub fn for_each_child<R>(&self, cb: &mut dyn FnMut(NodeKind<'a, 't>) -> R) -> Option<R>
    where
        R: VisitResult,
    {
        use self::NodeKind::*;
        macro_rules! ccb {
            ($astvar:expr) => {{
                let res = cb(NodeKind::from($astvar));
                if res.stop_visit() {
                    return Some(res);
                }
                Some(res)
            }};
            (for_each $vec:expr) => {{
                let iterable = $vec;
                let mut ret = None;
                for n in iterable {
                    let res = cb(NodeKind::from(n));
                    if res.stop_visit() {
                        return Some(res);
                    }
                    ret = Some(res);
                }
                ret
            }};
        }
        match self {
            AST(ast) => {
                use crate::ast::AST::*;
                match ast {
                    Empty => None,
                    Program(p) => ccb!(p),
                }
            }
            Program(p) => ccb!(for_each p.classes.iter()),
            ClassDeclaration(d) => ccb!(for_each d.members.iter()),
            ClassMember(cm) => {
                use crate::ast::ClassMemberKind::*;
                match &cm.kind {
                    Field(t) => ccb!(t),
                    Method(ty, pl, block) => {
                        ccb!(ty);
                        cb(NodeKind::ParameterList(&pl));
                        ccb!(block)
                    }
                    MainMethod(_, block) => ccb!(block),
                }
            }
            Parameter(p) => ccb!(&p.ty),
            ParameterList(l) => ccb!(for_each l.iter()),
            Type(t) => Some(cb(NodeKind::from(&t.basic))),
            BasicType(_) => None,
            Block(b) => ccb!(for_each b.statements.iter()),
            Stmt(s) => {
                use crate::ast::Stmt::*;
                match &s.data {
                    Empty => None,
                    Block(b) => ccb!(b),
                    Expression(e) => ccb!(e.as_ref()),
                    If(expr, then_stmt, else_stmt) => {
                        ccb!(expr.as_ref());
                        ccb!(then_stmt.as_ref());
                        ccb!(for_each else_stmt.iter().map(|x| x.as_ref()))
                    }
                    LocalVariableDeclaration(t, _, expr) => {
                        ccb!(t);
                        ccb!(for_each expr.iter().map(|x| x.as_ref()))
                    }
                    Return(expr) => ccb!(for_each expr.iter().map(|x| x.as_ref())),
                    While(cond, stmt) => {
                        ccb!(cond.as_ref());
                        ccb!(stmt.as_ref())
                    }
                }
            }
            Expr(e) => {
                use crate::ast::Expr::*;
                match &e.data {
                    Binary(_, lhs, rhs) => {
                        ccb!(lhs.as_ref());
                        ccb!(rhs.as_ref())
                    }
                    Unary(_, expr) => ccb!(expr.as_ref()),
                    MethodInvocation(target_expr, _, al) => {
                        ccb!(target_expr.as_ref());
                        ccb!(for_each al.data.iter())
                    }
                    FieldAccess(target_expr, _) => ccb!(target_expr.as_ref()),
                    ArrayAccess(target_expr, idx_expr) => {
                        ccb!(target_expr.as_ref());
                        ccb!(idx_expr.as_ref())
                    }
                    Null | Boolean(_) | Int(_) | Var(_) | This => None,
                    ThisMethodInvocation(_, al) => ccb!(for_each al.iter()),
                    NewObject(_) => None,
                    NewArray(_, expr, _) => ccb!(expr.as_ref()),
                }
            }
            BinaryOp(_) | UnaryOp(_) => None,
        }
    }
}

macro_rules! gen_from_ast {
    ($nodekind:path, $spanned_asttype:ty) => {
        impl<'a, 'f> From<&'a $spanned_asttype> for NodeKind<'a, 'f> {
            fn from(a: &'a $spanned_asttype) -> Self {
                $nodekind(a)
            }
        }
    };
}

gen_from_ast!(NodeKind::AST<'a, 'f>, ast::AST<'f>);
gen_from_ast!(NodeKind::Program<'a, 'f>, Spanned<'f, ast::Program<'f>>);
gen_from_ast!(
    NodeKind::ClassDeclaration<'a, 'f>,
    Spanned<'f, ast::ClassDeclaration<'f>>
);
gen_from_ast!(
    NodeKind::ClassMember<'a, 'f>,
    Spanned<'f, ast::ClassMember<'f>>
);
gen_from_ast!(NodeKind::Parameter<'a, 'f>, Spanned<'f, ast::Parameter<'f>>);
gen_from_ast!(
    NodeKind::ParameterList<'a, 'f>,
    Spanned<'f, ast::ParameterList<'f>>
);
gen_from_ast!(NodeKind::Type<'a, 'f>, Spanned<'f, ast::Type<'f>>);
gen_from_ast!(NodeKind::BasicType<'a, 'f>, ast::BasicType<'f>);
gen_from_ast!(NodeKind::Block<'a, 'f>, Spanned<'f, ast::Block<'f>>);
gen_from_ast!(NodeKind::Stmt<'a, 'f>, Spanned<'f, ast::Stmt<'f>>);
gen_from_ast!(NodeKind::Expr<'a, 'f>, Spanned<'f, ast::Expr<'f>>);
gen_from_ast!(NodeKind::BinaryOp<'a, 'f>, ast::BinaryOp);
gen_from_ast!(NodeKind::UnaryOp<'a, 'f>, ast::UnaryOp);
