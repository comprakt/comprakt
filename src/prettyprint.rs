use crate::{ast, context};
use failure::Error;

use crate::visitor::NodeKind;

struct IndentPrinter<'w> {
    writer: &'w mut dyn std::io::Write,
    indent: usize,
    indent_on_next_write: bool,
}

impl<'w> IndentPrinter<'w> {
    fn new(writer: &'w mut dyn std::io::Write) -> IndentPrinter<'w> {
        IndentPrinter { writer, indent: 0, indent_on_next_write: false }
    }

    fn print(&mut self, args: std::fmt::Arguments) {
        self.indent_if_required();
        write!(self.writer, "{}", args);
    }

    fn newline(&mut self) {
        writeln!(self.writer);
        self.indent_on_next_write = true;
    }

    fn println(&mut self, args: std::fmt::Arguments) {
        self.print(args);
        self.newline();
    }

    fn indent_if_required(&mut self) {
        if self.indent_on_next_write {
            for i in 0..self.indent {
                write!(self.writer, "\t");
            }
            self.indent_on_next_write = false;
        }
    }

    fn indent(&mut self) { self.indent_var(1); }

    fn outdent(&mut self) { self.indent_var(-1); }

    fn indent_var(&mut self, x: isize) {
        let mut indent = self.indent as isize;
        indent += x;
        if indent < 0 {
            panic!("setting indent below 0");
        }
        self.indent = indent as usize;
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

fn do_prettyprint(n: &NodeKind<'_, '_>, printer: &mut IndentPrinter<'_>) {
    do_prettyprint_ex(n, printer, false)
}

#[allow(clippy::cyclomatic_complexity)]
fn do_prettyprint_ex(n: &NodeKind<'_, '_>, printer: &mut IndentPrinter<'_>, parenthesize_expr: bool) {
    use crate::visitor::NodeKind::*;
    match n {
        Program(program) => {
            let mut classes: Vec<&ast::ClassDeclaration<'_>> =
                program.classes.iter().map(|c| c.get_data()).collect();
            classes.sort_by_key(|c| &c.name);
            classes
                .into_iter()
                .for_each(|class| do_prettyprint(&NodeKind::from(class), printer));
            printer.newline();
        }
        ClassDeclaration(decl) => {
            printer.println(format_args!("class {} {{", decl.name));
            printer.indent();
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
            printer.outdent();
            printer.println(format_args!("}}"));
        }
        ClassMember(member) => {
            use crate::ast::ClassMemberKind::*;
            match &member.kind {
                Field(ty) => {
                    printer.print(format_args!("public "));
                    do_prettyprint(&NodeKind::from(&ty.data), printer);
                    printer.print(format_args!(" {};", member.name));
                }
                Method(ty, params, block) => {
                    printer.print(format_args!("public "));
                    do_prettyprint(&NodeKind::from(&ty.data), printer);
                    printer.print(format_args!(" {}(", member.name));
                    do_prettyprint(&NodeKind::from(&params.data), printer);
                    printer.print(format_args!(") "));
                    do_prettyprint(&NodeKind::from(&block.data), printer);
                }
                MainMethod(param_name, block) => {
                    printer.print(format_args!("public static void main(String[] {})) ", param_name));
                    do_prettyprint(&NodeKind::from(&block.data), printer);
                }
            }
            printer.newline();
        }
        Parameter(param) => {
            do_prettyprint(&NodeKind::from(&param.ty.data), printer);
            printer.print(format_args!(" {}", param.name));
        }
        ParameterList(params) => {
            for (i, param) in params.iter().enumerate() {
                do_prettyprint(&NodeKind::from(&param.data), printer);
                if i != params.len() - 1 {
                    printer.print(format_args!(", "));
                }
            }
        }
        Type(ty) => {
            do_prettyprint(&NodeKind::from(&ty.basic), printer);
            printer.print(format_args!("{}", "[]".repeat(ty.array_depth as usize)));
        }
        BasicType(basic_ty) => {
            use crate::ast::BasicType::*;
            let _ = match basic_ty {
                Int => printer.print(format_args!("int")),
                Boolean => printer.print(format_args!("boolean")),
                Void => printer.print(format_args!("void")),
                Custom(name) => printer.print(format_args!("{}", name)),
            };
        }
        Block(block) => {
            if block.statements.is_empty() {
                printer.print(format_args!("{{ }}"));
            } else {
                printer.println(format_args!("{{"));
                printer.indent();
                for stmt in &block.statements {
                    do_prettyprint(&NodeKind::from(&stmt.data), printer);
                    printer.newline();
                }
                printer.outdent();
                printer.print(format_args!("}}"));
            }
        }
        Stmt(stmt) => {
            use crate::ast::Stmt::*;
            match stmt {
                Block(block) => do_prettyprint(&NodeKind::from(&block.data), printer),
                Empty => {
                    printer.print(format_args!(";"));
                }
                If(cond, stmt, opt_else) => {
                    printer.print(format_args!("if ("));
                    do_prettyprint(&NodeKind::from(&cond.data), printer);
                    if let ast::Stmt::Block(_) = stmt.data {
                        printer.print(format_args!(") "));
                        do_prettyprint(&NodeKind::from(&stmt.data), printer);
                        if opt_else.is_some() {
                            printer.print(format_args!(" "));
                        }
                    } else {
                        printer.println(format_args!(")"));
                        printer.indent();
                        do_prettyprint(&NodeKind::from(&stmt.data), printer);
                        printer.outdent();
                        if opt_else.is_some() {
                            printer.newline();
                        }
                    }
                    if let Some(els) = opt_else {
                        match els.data {
                            If(..) | Block(..) => {
                                printer.print(format_args!("else "));
                                do_prettyprint(&NodeKind::from(&els.data), printer)
                            }
                            _ => {
                                printer.newline();
                                printer.indent();
                                do_prettyprint(&NodeKind::from(&els.data), printer);
                                printer.outdent();
                            }
                        }
                    }
                }
                While(cond, stmt) => {
                    printer.print(format_args!("while ("));
                    do_prettyprint(&NodeKind::from(&cond.data), printer);
                    printer.print(format_args!(")"));
                    if let ast::Stmt::Block(_) = stmt.data {
                        printer.print(format_args!(" "));
                        do_prettyprint(&NodeKind::from(&stmt.data), printer);
                    } else {
                        printer.newline();
                        printer.indent();
                        do_prettyprint(&NodeKind::from(&stmt.data), printer);
                        printer.outdent();
                    }
                }
                Expression(expr) => {
                    do_prettyprint(&NodeKind::from(&expr.data), printer);
                    printer.print(format_args!(";"));
                }
                Return(expr_opt) => {
                    printer.print(format_args!("return"));
                    if let Some(expr) = expr_opt {
                        printer.print(format_args!(" "));
                        do_prettyprint(&NodeKind::from(&expr.data), printer);
                    }
                    printer.print(format_args!(";"));
                }
                LocalVariableDeclaration(ty, name, opt_assign) => {
                    do_prettyprint(&NodeKind::from(&ty.data), printer);
                    printer.print(format_args!(" {}", name));
                    if let Some(assign) = opt_assign {
                        printer.print(format_args!(" = "));
                        do_prettyprint(&NodeKind::from(&assign.data), printer);
                    }
                    printer.print(format_args!(";"));
                }
            };
        }
        Expr(expr) => {
            use crate::ast::Expr::*;
            match expr {
                Assignment(assignee, assignements) => {
                    do_prettyprint(&NodeKind::from(&assignee.data), printer);
                    for assigned in assignements {
                        printer.print(format_args!(" = "));
                        do_prettyprint(&NodeKind::from(&assigned.data), printer);
                    }
                }
                Binary(op, lhs, rhs) => {
                    printer.print(format_args!("("));
                    do_prettyprint(&NodeKind::from(&lhs.data), printer);
                    do_prettyprint(&NodeKind::from(op), printer);
                    do_prettyprint(&NodeKind::from(&rhs.data), printer);
                    printer.print(format_args!(")"));
                }
                Unary(ops, expr) => {
                    printer.print(format_args!("("));
                    for op in ops {
                        do_prettyprint(&NodeKind::from(op), printer);
                    }
                    do_prettyprint(&NodeKind::from(&expr.data), printer);
                    printer.print(format_args!(")"));
                }
                Postfix(prime, post_ops) => {
                    do_prettyprint(&NodeKind::from(&prime.data), printer);
                    for post in post_ops {
                        do_prettyprint(&NodeKind::from(&post.data), printer);
                    }
                }
                Null => {
                    printer.print(format_args!("null"));
                }
                Boolean(val) => {
                    printer.print(format_args!("{}", val));
                }
                Int(val) => {
                    printer.print(format_args!("{}", val));
                }
                Var(name) => {
                    printer.print(format_args!("{}", name));
                }
                MethodInvocation(name, args) => {
                    printer.print(format_args!("{}(", name));
                    for (i, arg) in args.data.iter().enumerate() {
                        do_prettyprint(&NodeKind::from(&arg.data), printer);

                        if i != args.len() - 1 {
                            printer.print(format_args!(", "));
                        }
                    }
                    printer.print(format_args!(")"));
                }
                This => {
                    printer.print(format_args!("this"));
                }
                NewObject(name) => {
                    printer.print(format_args!("new {}())", name));
                }
                NewArray(basic_ty, size, brackets) => {
                    printer.print(format_args!("new "));
                    do_prettyprint(&NodeKind::from(basic_ty), printer);
                    printer.print(format_args!("["));
                    do_prettyprint(&NodeKind::from(&size.data), printer);
                    printer.print(format_args!("]{}", "[]".repeat(*brackets as usize)));
                }
            }
        }
        BinaryOp(bin_op) => {
            use crate::ast::BinaryOp::*;
            let _ = match bin_op {
                Equals => printer.print(format_args!(" == ")),
                NotEquals => printer.print(format_args!(" != ")),
                LessThan => printer.print(format_args!(" < ")),
                GreaterThan => printer.print(format_args!(" > ")),
                LessEquals => printer.print(format_args!(" <= ")),
                GreaterEquals => printer.print(format_args!(" >= ")),
                LogicalOr => printer.print(format_args!(" || ")),
                LogicalAnd => printer.print(format_args!(" && ")),
                Add => printer.print(format_args!(" + ")),
                Sub => printer.print(format_args!(" - ")),
                Mul => printer.print(format_args!(" * ")),
                Div => printer.print(format_args!(" / ")),
                Mod => printer.print(format_args!(" % ")),
            };
        }
        UnaryOp(unary_op) => {
            use crate::ast::UnaryOp::*;
            let _ = match unary_op {
                Not => printer.print(format_args!("!")),
                Neg => printer.print(format_args!("-")),
            };
        }
        PostfixOp(postfix_op) => {
            use crate::ast::PostfixOp::*;
            match postfix_op {
                MethodInvocation(name, args) => {
                    printer.print(format_args!(".{}(", name));
                    for (i, arg) in args.data.iter().enumerate() {
                        do_prettyprint(&NodeKind::from(&arg.data), printer);

                        if i != args.len() - 1 {
                            printer.print(format_args!(", "));
                        }
                    }
                    printer.print(format_args!(")"));
                }
                FieldAccess(name) => {
                    printer.print(format_args!(".{}", name));
                }
                ArrayAccess(expr) => {
                    printer.print(format_args!("["));
                    do_prettyprint(&NodeKind::from(&expr.data), printer);
                    printer.print(format_args!("]"));
                }
            }
        }
    }
}
