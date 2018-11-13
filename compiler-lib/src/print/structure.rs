use crate::ast;
use failure::{format_err, Error};

trait AstNode {
    fn as_ast_node(&self, printer: &mut Printer<'_>) -> std::io::Result<()>;
}

macro_rules! impl_astnode_struct {
    (lt_struct => $lt:lifetime, $type:ty) => {
        impl<$lt> AstNode for $type {
            fn as_ast_node(&self, printer: &mut Printer<'_>) -> std::io::Result<()> {
                let type_name = unsafe { (std::intrinsics::type_name::<$type>()) };
                printer.print(format_args!("{}", type_name))
            }
        }
    };
    (simple => $type:ty) => {
        impl AstNode for $type {
            fn as_ast_node(&self, printer: &mut Printer<'_>) -> std::io::Result<()> {
                let type_name = unsafe { (std::intrinsics::type_name::<$type>()) };
                printer.print(format_args!("{}", type_name))
            }
        }
    };
    (discr => $lt:lifetime, $type:ty, $discrtype_from:path) => {
        impl<$lt> AstNode for $type {
            fn as_ast_node(&self, printer: &mut Printer<'_>) -> std::io::Result<()> {
                let type_name = unsafe { (std::intrinsics::type_name::<$type>()) };
                let discr = $discrtype_from(self);
                printer.print(format_args!("{} ({})", type_name, discr.to_string()))
            }
        }
    };
}

impl_astnode_struct!(discr     => 't, ast::AST<'t>, ast::ASTDiscriminants::from);
impl_astnode_struct!(lt_struct => 't, ast::Program<'t>);
impl_astnode_struct!(lt_struct => 't, ast::ClassDeclaration<'t>);
impl AstNode for ast::ClassMember<'_> {
    fn as_ast_node(&self, printer: &mut Printer<'_>) -> std::io::Result<()> {
        let type_name = unsafe { (std::intrinsics::type_name::<Self>()) };
        let kind_discr = ast::ClassMemberKindDiscriminants::from(&self.kind);
        printer.print(format_args!("{} (kind = {})", type_name, kind_discr))
    }
}
#[rustfmt::skip]
impl_astnode_struct!(discr     => 't, ast::ClassMemberKind<'t>, ast::ClassMemberKindDiscriminants::from);
impl_astnode_struct!(lt_struct => 't, ast::Parameter<'t>);
impl_astnode_struct!(lt_struct => 't, ast::ParameterList<'t>);
impl_astnode_struct!(simple    => ast::Type);
impl AstNode for ast::BasicType {
    fn as_ast_node(&self, printer: &mut Printer<'_>) -> std::io::Result<()> {
        let type_name = unsafe { (std::intrinsics::type_name::<Self>()) };
        let discr = ast::BasicTypeDiscriminants::from(self);
        printer.print(format_args!("{} ({})", type_name, discr))
    }
}
impl_astnode_struct!(lt_struct => 't, ast::Block<'t>);
impl_astnode_struct!(discr     => 't, ast::Stmt<'t>, ast::StmtDiscriminants::from);
impl_astnode_struct!(discr     => 't, ast::Expr<'t>, ast::ExprDiscriminants::from);
impl_astnode_struct!(simple    => ast::BinaryOp);
impl_astnode_struct!(simple    => ast::UnaryOp);

struct Printer<'w> {
    writer: &'w mut dyn std::io::Write,
    indent: usize,
}

impl<'w> Printer<'w> {
    fn add(&mut self, x: isize) -> Result<(), Error> {
        let mut indent = self.indent as isize;
        indent += x;
        if indent < 0 {
            return Err(format_err!("setting indent below 0"));
        }
        self.indent = indent as usize;
        Ok(())
    }
    fn print(&mut self, args: std::fmt::Arguments<'_>) -> std::io::Result<()> {
        writeln!(self.writer, "{}{}", " ".repeat(self.indent), args)
    }
}

use std::io::Write;
impl<'w> Write for Printer<'w> {
    fn write(&mut self, b: &[u8]) -> std::io::Result<usize> {
        write!(self.writer, "{}", " ".repeat(self.indent))?;
        let ret = self.writer.write(b)?;
        Ok(ret)
    }
    fn flush(&mut self) -> std::io::Result<()> {
        self.writer.flush()
    }
}

use crate::visitor::*;

pub fn print<'f>(ast: &ast::AST<'f>, out: &mut dyn std::io::Write) -> Result<(), Error> {
    let mut printer = Printer {
        indent: 0,
        writer: out,
    };
    let n = NodeKind::from(ast);
    do_structureprint(&n, &mut printer)
}

fn do_structureprint(n: &NodeKind<'_, '_>, printer: &mut Printer<'_>) -> Result<(), Error> {
    gen_nodekind_match!(n, a => a.as_ast_node(printer))?;
    printer.add(2)?;
    n.for_each_child(&mut |child| {
        // TODO refactor
        //   pub fn for_each_child(&self, cb: &mut dyn FnMut(NodeKind<'a, 't>))
        // into
        //   pub fn for_each_child<R>(&self, cb: &mut dyn FnMut(NodeKind<'a, 't>) -> R)
        // -> R
        do_structureprint(&child, printer).unwrap(); // after refactor, unwrap can go
    });
    printer.add(-2)
}
