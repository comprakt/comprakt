use crate::ast::{self, *};
use strum_macros::EnumDiscriminants;

#[strum_discriminants(derive(Display))]
#[derive(EnumDiscriminants)]
pub enum NodeKind<'a, 't> {
    AST(&'a AST<'t>),
    Program(&'a Program<'t>),
    ClassDeclaration(&'a ClassDeclaration<'t>),
    ClassMember(&'a ClassMember<'t>),
    Parameter(&'a Parameter<'t>),
    ParameterList(&'a ParameterList<'t>),
    Type(&'a Type),
    BasicType(&'a BasicType),
    Block(&'a Block<'t>),
    Stmt(&'a Stmt<'t>),
    Expr(&'a Expr<'t>),
    BinaryOp(&'a BinaryOp),
    UnaryOp(&'a UnaryOp),
    PostfixOp(&'a PostfixOp<'t>),
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
                cb(NodeKind::from(&$astvar.data))
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
                        cb(NodeKind::ParameterList(&pl.data));
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
                match s {
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
                match e {
                    Assignment(lhs, rhs) => {
                        ccb!(lhs.as_ref());
                        rhs.iter().for_each(|x| ccb!(x));
                    }
                    Binary(_, lhs, rhs) => {
                        ccb!(lhs.as_ref());
                        ccb!(rhs.as_ref());
                    }
                    Unary(_, expr) => ccb!(expr.as_ref()),
                    Postfix(expr, posts) => {
                        ccb!(expr.as_ref());
                        posts.iter().for_each(|p| ccb!(p));
                    }
                    Null | Boolean(_) | Int(_) | Var(_) | This => (),
                    MethodInvocation(_, al) => al.iter().for_each(|a| ccb!(a)),
                    NewObject(_) => (),
                    NewArray(_, expr, _) => ccb!(expr.as_ref()),
                }
            }
            BinaryOp(_) | UnaryOp(_) => (),
            PostfixOp(oc) => {
                use crate::ast::PostfixOp::*;
                match oc {
                    MethodInvocation(_, al) => al.iter().for_each(|x| ccb!(x)),
                    FieldAccess(_) => (),
                    ArrayAccess(expr) => ccb!(expr.as_ref()),
                }
            }
        }
    }
}

macro_rules! gen_from_ast {
    ($nodekind:path, $asttype:ty) => {
        impl<'a, 'f> From<&'a $asttype> for NodeKind<'a, 'f> {
            fn from(a: &'a $asttype) -> Self {
                $nodekind(a)
            }
        }
    };
}

gen_from_ast!(NodeKind::AST<'a, 'f>, ast::AST<'f>);
gen_from_ast!(NodeKind::Program<'a, 'f>, ast::Program<'f>);
gen_from_ast!(
    NodeKind::ClassDeclaration<'a, 'f>,
    ast::ClassDeclaration<'f>
);
gen_from_ast!(NodeKind::ClassMember<'a, 'f>, ast::ClassMember<'f>);
gen_from_ast!(NodeKind::Parameter<'a, 'f>, ast::Parameter<'f>);
gen_from_ast!(NodeKind::ParameterList<'a, 'f>, ast::ParameterList<'f>);
gen_from_ast!(NodeKind::Type<'a, 'f>, ast::Type);
gen_from_ast!(NodeKind::BasicType<'a, 'f>, ast::BasicType);
gen_from_ast!(NodeKind::Block<'a, 'f>, ast::Block<'f>);
gen_from_ast!(NodeKind::Stmt<'a, 'f>, ast::Stmt<'f>);
gen_from_ast!(NodeKind::Expr<'a, 'f>, ast::Expr<'f>);
gen_from_ast!(NodeKind::BinaryOp<'a, 'f>, ast::BinaryOp);
gen_from_ast!(NodeKind::UnaryOp<'a, 'f>, ast::UnaryOp);
gen_from_ast!(NodeKind::PostfixOp<'a, 'f>, ast::PostfixOp<'f>);