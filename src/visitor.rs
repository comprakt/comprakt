use crate::ast::{self, *};
use strum_macros::EnumDiscriminants;
use asciifile::Spanned;

#[strum_discriminants(derive(Display))]
#[derive(EnumDiscriminants)]
pub enum NodeKind<'a, 't> {
    AST(&'a AST<'t>),
    Program(&'a Spanned<'t, Program<'t>>),
    ClassDeclaration(&'a Spanned<'t, ClassDeclaration<'t>>),
    ClassMember(&'a Spanned<'t, ClassMember<'t>>),
    Parameter(&'a Spanned<'t, Parameter<'t>>),
    ParameterList(&'a Spanned<'t, ParameterList<'t>>),
    Type(&'a Spanned<'t, Type>),
    BasicType(&'a BasicType),
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
            NodeKind::PostfixOp($varname) => $rhs,
        }
    };
}

use std::fmt::Debug;
impl Debug for NodeKind<'_, '_> {
    fn fmt(&self, fmt: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        gen_nodekind_match!(self, v => v.fmt(fmt))
    }
}

impl<'a, 't> NodeKind<'a, 't> {
    pub fn for_each_child(&self, cb: &mut dyn FnMut(NodeKind<'a, 't>)) {
        use self::NodeKind::*;
        macro_rules! ccb {
            ($astvar:expr) => {
                cb(NodeKind::from($astvar))
            };
        }
        match self {
            AST(ast) => {
                use crate::ast::AST::*;
                match ast {
                    Empty => (),
                    Program(p) => ccb!(p),
                }
            }
            Program(p) => p.classes.iter().for_each(|x| ccb!(x)),
            ClassDeclaration(d) => d.members.iter().for_each(|x| ccb!(x)),
            ClassMember(cm) => {
                use crate::ast::ClassMemberKind::*;
                match &cm.kind {
                    Field(t) => ccb!(t),
                    Method(ty, pl, block) => {
                        ccb!(ty);
                        cb(NodeKind::ParameterList(&pl));
                        ccb!(block);
                    }
                    MainMethod(_, block) => {
                        ccb!(block);
                    }
                }
            }
            Parameter(p) => {
                ccb!(&p.ty);
            }
            ParameterList(l) => l.iter().for_each(|x| ccb!(x)),
            Type(t) => cb(NodeKind::from(&t.basic)),
            BasicType(_) => (),
            Block(b) => b.statements.iter().for_each(|x| ccb!(x)),
            Stmt(s) => {
                use crate::ast::Stmt::*;
                match &s.data {
                    Empty => (),
                    Block(b) => ccb!(b),
                    Expression(e) => ccb!(e.as_ref()),
                    If(expr, then_stmt, else_stmt) => {
                        ccb!(expr.as_ref());
                        ccb!(then_stmt.as_ref());
                        else_stmt.iter().for_each(|x| ccb!(x.as_ref()));
                    }
                    LocalVariableDeclaration(t, _, expr) => {
                        ccb!(t);
                        expr.iter().for_each(|x| ccb!(x.as_ref()));
                    }
                    Return(expr) => expr.iter().for_each(|x| ccb!(x.as_ref())),
                    While(cond, stmt) => {
                        ccb!(cond.as_ref());
                        ccb!(stmt.as_ref());
                    }
                }
            }
            Expr(e) => {
                use crate::ast::Expr::*;
                match &e.data {
                    Binary(_, lhs, rhs) => {
                        ccb!(lhs.as_ref());
                        ccb!(rhs.as_ref());
                    }
                    Unary(_, expr) => ccb!(expr.as_ref()),
                    MethodInvocation(target_expr, _, al) => {
                        ccb!(target_expr.as_ref());
                        al.iter().for_each(|x| ccb!(x));
                    }
                    FieldAccess(target_expr, _) => ccb!(target_expr.as_ref()),
                    ArrayAccess(target_expr, idx_expr) => {
                        ccb!(target_expr.as_ref());
                        ccb!(idx_expr.as_ref());
                    }
                    Null | Boolean(_) | Int(_) | Var(_) | This => (),
                    ThisMethodInvocation(_, al) => al.iter().for_each(|a| ccb!(a)),
                    NewObject(_) => (),
                    NewArray(_, expr, _) => ccb!(expr.as_ref()),
                }
            }
            BinaryOp(_) | UnaryOp(_) => (),
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
gen_from_ast!(NodeKind::ClassMember<'a, 'f>, Spanned<'f, ast::ClassMember<'f>>);
gen_from_ast!(NodeKind::Parameter<'a, 'f>, Spanned<'f, ast::Parameter<'f>>);
gen_from_ast!(NodeKind::ParameterList<'a, 'f>, Spanned<'f, ast::ParameterList<'f>>);
gen_from_ast!(NodeKind::Type<'a, 'f>, Spanned<'f, ast::Type>);
gen_from_ast!(NodeKind::BasicType<'a, 'f>, ast::BasicType);
gen_from_ast!(NodeKind::Block<'a, 'f>, Spanned<'f, ast::Block<'f>>);
gen_from_ast!(NodeKind::Stmt<'a, 'f>, Spanned<'f, ast::Stmt<'f>>);
gen_from_ast!(NodeKind::Expr<'a, 'f>, Spanned<'f, ast::Expr<'f>>);
gen_from_ast!(NodeKind::BinaryOp<'a, 'f>, ast::BinaryOp);
gen_from_ast!(NodeKind::UnaryOp<'a, 'f>, ast::UnaryOp);
