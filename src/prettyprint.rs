use crate::{ast, context};
use failure::Error;

use crate::visitor::NodeKind;

struct IndentPrinter<'w> {
    writer: &'w mut dyn std::io::Write,
    indent: usize,
}

impl<'w> IndentPrinter<'w> {
    fn new(writer: &'w mut dyn std::io::Write) -> IndentPrinter<'w> {
        IndentPrinter { writer, indent: 0 }
    }

    fn indent(&mut self, x: isize) {
        let mut indent = self.indent as isize;
        indent += x;
        if indent < 0 {
            panic!("setting indent below 0");
        }
        self.indent = indent as usize;
    }
}

use std::io::Write;

impl Write for IndentPrinter<'_> {
    fn write(&mut self, b: &[u8]) -> std::io::Result<usize> {
        self.writer.write(b)
    }
    fn flush(&mut self) -> std::io::Result<()> {
        self.writer.flush()
    }
}

pub fn prettyprint<'f, 'c>(
    program: &ast::Program<'f>,
    _context: &context::Context<'c>,
) -> Result<(), Error> {
    let mut stdout = std::io::stdout();
    let mut printer = IndentPrinter::new(&mut stdout);
    do_prettyprint(&NodeKind::from(program), &mut printer);
    Ok(())
}

#[allow(clippy::cyclomatic_complexity)]
fn do_prettyprint(n: &NodeKind<'_, '_>, printer: &mut IndentPrinter<'_>) {
    use crate::visitor::NodeKind::*;
    match n {
        Program(program) => {
            let mut classes: Vec<&ast::ClassDeclaration<'_>> =
                program.classes.iter().map(|c| c.get_data()).collect();
            classes.sort_by_key(|c| &c.name);
            classes
                .into_iter()
                .for_each(|class| do_prettyprint(&NodeKind::from(class), printer));
            writeln!(printer);
        }
        ClassDeclaration(decl) => {
            writeln!(printer, "class {} {{", decl.name);
            printer.indent(1);
            let mut members: Vec<&ast::ClassMember<'_>> =
                decl.members.iter().map(|c| c.get_data()).collect();
            members.sort_by(|a, b| {
                use crate::ast::ClassMemberKindDiscriminants::*;
                let akind = ast::ClassMemberKindDiscriminants::from(&a.kind);
                let bkind = ast::ClassMemberKindDiscriminants::from(&b.kind);
                match (akind, bkind) {
                    (Field, Field)
                    | (Method, Method)
                    | (MainMethod, MainMethod)
                    | (Method, MainMethod)
                    | (MainMethod, Method) => a.name.cmp(&b.name),
                    (Method, Field) | (MainMethod, Field) => std::cmp::Ordering::Less,
                    (Field, Method) | (Field, MainMethod) => std::cmp::Ordering::Greater,
                }
            });
            members
                .into_iter()
                .for_each(|member| do_prettyprint(&NodeKind::from(member), printer));
            printer.indent(-1);
            writeln!(printer, "}}");
        }
        ClassMember(member) => {
            use crate::ast::ClassMemberKind::*;
            match &member.kind {
                Field(ty) => {
                    write!(printer, "public ");
                    do_prettyprint(&NodeKind::from(&ty.data), printer);
                    write!(printer, " {};", member.name);
                }
                Method(ty, params, block) => {
                    write!(printer, "public ");
                    do_prettyprint(&NodeKind::from(&ty.data), printer);
                    write!(printer, " {}(", member.name);
                    do_prettyprint(&NodeKind::from(&params.data), printer);
                    write!(printer, ") ");
                    do_prettyprint(&NodeKind::from(&block.data), printer);
                }
                MainMethod(param_name, block) => {
                    write!(printer, "public static void main(String[] {}) ", param_name);
                    do_prettyprint(&NodeKind::from(&block.data), printer);
                }
            }
            writeln!(printer);
        }
        Parameter(param) => {
            do_prettyprint(&NodeKind::from(&param.ty.data), printer);
            write!(printer, " {}", param.name);
        }
        ParameterList(params) => {
            for (i, param) in params.iter().enumerate() {
                do_prettyprint(&NodeKind::from(&param.data), printer);
                if i != params.len() - 1 {
                    write!(printer, ", ");
                }
            }
        }
        Type(ty) => {
            do_prettyprint(&NodeKind::from(&ty.basic), printer);
            write!(printer, "{}", "[]".repeat(ty.array_depth as usize));
        }
        BasicType(basic_ty) => {
            use crate::ast::BasicType::*;
            let _ = match basic_ty {
                Int => write!(printer, "int"),
                Boolean => write!(printer, "boolean"),
                Void => write!(printer, "void"),
                Custom(name) => write!(printer, "{}", name),
            };
        }
        Block(block) => {
            if block.statements.is_empty() {
                write!(printer, "{{ }}");
            } else {
                writeln!(printer, "{{");
                printer.indent(1);
                for stmt in &block.statements {
                    do_prettyprint(&NodeKind::from(&stmt.data), printer);
                    writeln!(printer);
                }
                printer.indent(-1);
                write!(printer, "}}");
            }
        }
        Stmt(stmt) => {
            use crate::ast::Stmt::*;
            match stmt {
                Block(block) => do_prettyprint(&NodeKind::from(&block.data), printer),
                Empty => {
                    write!(printer, ";");
                }
                If(cond, stmt, opt_else) => {
                    write!(printer, "if (");
                    do_prettyprint(&NodeKind::from(&cond.data), printer);
                    if let ast::Stmt::Block(_) = stmt.data {
                        write!(printer, ") ");
                        do_prettyprint(&NodeKind::from(&stmt.data), printer);
                        if opt_else.is_some() {
                            write!(printer, " ");
                        }
                    } else {
                        writeln!(printer, ")");
                        printer.indent(1);
                        do_prettyprint(&NodeKind::from(&stmt.data), printer);
                        printer.indent(-1);
                        if opt_else.is_some() {
                            writeln!(printer);
                        }
                    }
                    if let Some(els) = opt_else {
                        match els.data {
                            If(..) | Block(..) => {
                                write!(printer, "else ");
                                do_prettyprint(&NodeKind::from(&els.data), printer)
                            }
                            _ => {
                                writeln!(printer);
                                printer.indent(1);
                                do_prettyprint(&NodeKind::from(&els.data), printer);
                                printer.indent(-1);
                            }
                        }
                    }
                }
                While(cond, stmt) => {
                    write!(printer, "while (");
                    do_prettyprint(&NodeKind::from(&cond.data), printer);
                    if let ast::Stmt::Block(_) = stmt.data {
                        write!(printer, ") ");
                        do_prettyprint(&NodeKind::from(&stmt.data), printer);
                    } else {
                        writeln!(printer, ")");
                        printer.indent(1);
                        do_prettyprint(&NodeKind::from(&stmt.data), printer);
                        printer.indent(-1);
                    }
                }
                Expression(expr) => {
                    do_prettyprint(&NodeKind::from(&expr.data), printer);
                    write!(printer, ";");
                }
                Return(expr_opt) => {
                    write!(printer, "return");
                    if let Some(expr) = expr_opt {
                        write!(printer, " ");
                        do_prettyprint(&NodeKind::from(&expr.data), printer);
                    }
                    write!(printer, ";");
                }
                LocalVariableDeclaration(ty, name, opt_assign) => {
                    do_prettyprint(&NodeKind::from(&ty.data), printer);
                    write!(printer, " {}", name);
                    if let Some(assign) = opt_assign {
                        write!(printer, " = ");
                        do_prettyprint(&NodeKind::from(&assign.data), printer);
                    }
                    write!(printer, ";");
                }
            };
        }
        Expr(expr) => {
            use crate::ast::Expr::*;
            match expr {
                Assignment(assignee, assignements) => {
                    do_prettyprint(&NodeKind::from(&assignee.data), printer);
                    for assigned in assignements {
                        write!(printer, " = ");
                        do_prettyprint(&NodeKind::from(&assigned.data), printer);
                    }
                }
                Binary(op, lhs, rhs) => {
                    write!(printer, "(");
                    do_prettyprint(&NodeKind::from(&lhs.data), printer);
                    do_prettyprint(&NodeKind::from(op), printer);
                    do_prettyprint(&NodeKind::from(&rhs.data), printer);
                    write!(printer, ")");
                }
                Unary(ops, expr) => {
                    write!(printer, "(");
                    for op in ops {
                        do_prettyprint(&NodeKind::from(op), printer);
                    }
                    do_prettyprint(&NodeKind::from(&expr.data), printer);
                    write!(printer, ")");
                }
                Postfix(prime, post_ops) => {
                    do_prettyprint(&NodeKind::from(&prime.data), printer);
                    for post in post_ops {
                        do_prettyprint(&NodeKind::from(&post.data), printer);
                    }
                }
                Null => {
                    write!(printer, "null");
                }
                Boolean(val) => {
                    write!(printer, "{}", val);
                }
                Int(val) => {
                    write!(printer, "{}", val);
                }
                Var(name) => {
                    write!(printer, "{}", name);
                }
                MethodInvocation(name, args) => {
                    write!(printer, "{}(", name);
                    for (i, arg) in args.data.iter().enumerate() {
                        do_prettyprint(&NodeKind::from(&arg.data), printer);

                        if i != args.len() - 1 {
                            write!(printer, ", ");
                        }
                    }
                    write!(printer, ")");
                }
                This => {
                    write!(printer, "this");
                }
                NewObject(name) => {
                    write!(printer, "new {}()", name);
                }
                NewArray(basic_ty, size, brackets) => {
                    write!(printer, "new ");
                    do_prettyprint(&NodeKind::from(basic_ty), printer);
                    write!(printer, "[");
                    do_prettyprint(&NodeKind::from(&size.data), printer);
                    write!(printer, "]{}", "[]".repeat(*brackets as usize));
                }
            }
        }
        BinaryOp(bin_op) => {
            use crate::ast::BinaryOp::*;
            let _ = match bin_op {
                Equals => write!(printer, " == "),
                NotEquals => write!(printer, " != "),
                LessThan => write!(printer, " < "),
                GreaterThan => write!(printer, " > "),
                LessEquals => write!(printer, " <= "),
                GreaterEquals => write!(printer, " >= "),
                LogicalOr => write!(printer, " || "),
                LogicalAnd => write!(printer, " && "),
                Add => write!(printer, " + "),
                Sub => write!(printer, " - "),
                Mul => write!(printer, " * "),
                Div => write!(printer, " / "),
                Mod => write!(printer, " % "),
            };
        }
        UnaryOp(unary_op) => {
            use crate::ast::UnaryOp::*;
            let _ = match unary_op {
                Not => write!(printer, "!"),
                Neg => write!(printer, "-"),
            };
        }
        PostfixOp(postfix_op) => {
            use crate::ast::PostfixOp::*;
            match postfix_op {
                MethodInvocation(name, args) => {
                    write!(printer, ".{}(", name);
                    for (i, arg) in args.data.iter().enumerate() {
                        do_prettyprint(&NodeKind::from(&arg.data), printer);

                        if i != args.len() - 1 {
                            write!(printer, ", ");
                        }
                    }
                    write!(printer, ")");
                }
                FieldAccess(name) => {
                    write!(printer, ".{}", name);
                }
                ArrayAccess(expr) => {
                    write!(printer, "[");
                    do_prettyprint(&NodeKind::from(&expr.data), printer);
                    write!(printer, "]");
                }
            }
        }
    }
}
