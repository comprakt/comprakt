use crate::{ast, context, visitor::NodeKind};
use failure::Error;

struct IndentPrinter<'w> {
    writer: &'w mut dyn std::io::Write,
    indent: usize,
    indent_on_next_write: bool,
}

impl<'w> IndentPrinter<'w> {
    fn new(writer: &'w mut dyn std::io::Write) -> IndentPrinter<'w> {
        IndentPrinter {
            writer,
            indent: 0,
            indent_on_next_write: false,
        }
    }

    fn print(&mut self, args: std::fmt::Arguments<'_>) {
        self.indent_if_required();
        write!(self.writer, "{}", args);
    }

    fn print_str(&mut self, str: &str) {
        self.indent_if_required();
        write!(self.writer, "{}", str);
    }

    fn newline(&mut self) {
        writeln!(self.writer);
        self.indent_on_next_write = true;
    }

    fn println(&mut self, args: std::fmt::Arguments<'_>) {
        self.print(args);
        self.newline();
    }

    fn indent_if_required(&mut self) {
        if self.indent_on_next_write {
            write!(self.writer, "{}", &"\t".repeat(self.indent));
            self.indent_on_next_write = false;
        }
    }

    fn indent(&mut self) {
        self.indent_var(1);
    }

    fn outdent(&mut self) {
        self.indent_var(-1);
    }

    fn indent_var(&mut self, x: isize) {
        let mut indent = self.indent as isize;
        indent += x;
        if indent < 0 {
            panic!("setting indent below 0");
        }
        self.indent = indent as usize;
    }
}

pub fn prettyprint(ast: &ast::AST<'_>, _context: &context::Context<'_>) -> Result<(), Error> {
    let mut stdout = std::io::stdout();
    let mut printer = IndentPrinter::new(&mut stdout);
    do_prettyprint(&NodeKind::from(ast), &mut printer);
    Ok(())
}

#[allow(clippy::cyclomatic_complexity)]
fn do_prettyprint(n: &NodeKind<'_, '_>, printer: &mut IndentPrinter<'_>) {
    use crate::visitor::NodeKind::*;
    match n {
        AST(ast) => {
            use crate::ast::AST::*;
            match ast {
                Empty => (), // TODO newline?
                Program(p) => do_prettyprint(&NodeKind::Program(&p.data), printer),
            }
        }

        Program(program) => {
            let mut classes: Vec<&ast::ClassDeclaration<'_>> =
                program.classes.iter().map(|c| &c.data).collect();
            classes.sort_by_key(|c| &c.name);
            classes
                .into_iter()
                .for_each(|class| do_prettyprint(&NodeKind::from(class), printer));
        }

        ClassDeclaration(decl) => {
            printer.print(format_args!("class {} {{", decl.name));
            let mut members: Vec<&ast::ClassMember<'_>> =
                decl.members.iter().map(|c| &c.data).collect();
            members.sort_by(|x, y| compare_class_member(x, y));
            if !members.is_empty() {
                printer.println(format_args!(""));
                printer.indent();
                members
                    .into_iter()
                    .for_each(|member| do_prettyprint(&NodeKind::from(member), printer));
                printer.outdent();
            } else {
                printer.print(format_args!(" "));
            }
            printer.println(format_args!("}}"));
        }

        ClassMember(member) => {
            use crate::ast::ClassMemberKind::*;
            match &member.kind {
                Field(ty) => {
                    printer.print_str(&"public ");
                    do_prettyprint(&NodeKind::from(&ty.data), printer);
                    printer.print(format_args!(" {};", member.name));
                }
                Method(ty, params, block) => {
                    printer.print_str(&"public ");
                    do_prettyprint(&NodeKind::from(&ty.data), printer);
                    printer.print(format_args!(" {}(", member.name));
                    do_prettyprint(&NodeKind::from(&params.data), printer);
                    printer.print_str(&") ");
                    do_prettyprint(&NodeKind::from(&block.data), printer);
                }
                MainMethod(param_name, block) => {
                    printer.print(format_args!(
                        "public static void {}(String[] {}) ",
                        member.name, param_name
                    ));
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
                    printer.print_str(&", ");
                }
            }
        }

        Type(ty) => {
            do_prettyprint(&NodeKind::from(&ty.basic), printer);
            printer.print(format_args!("{}", "[]".repeat(ty.array_depth as usize)));
        }

        BasicType(basic_ty) => {
            use crate::ast::BasicType::*;
            match basic_ty {
                Int => printer.print_str(&"int"),
                Boolean => printer.print_str(&"boolean"),
                Void => printer.print_str(&"void"),
                Custom(name) => printer.print(format_args!("{}", name)),
            }
        }

        Block(block) => {
            // all() returns true on empty iterators
            if block
                .statements
                .iter()
                .all(|stmt| stmt.data == crate::ast::Stmt::Empty)
            {
                printer.print_str(&"{ }");
            } else {
                printer.print_str(&"{");
                printer.newline();
                printer.indent();
                for stmt in &block.statements {
                    match stmt.data {
                        crate::ast::Stmt::Empty => {}
                        _ => {
                            do_prettyprint(&NodeKind::from(&stmt.data), printer);
                            printer.newline();
                        }
                    }
                }
                printer.outdent();
                printer.print_str(&"}");
            }
        }

        Stmt(stmt) => {
            use crate::ast::Stmt::*;
            match stmt {
                Block(block) => do_prettyprint(&NodeKind::from(&block.data), printer),
                Empty => {}
                If(cond, stmt, opt_else) => {
                    printer.print_str(&"if (");
                    // no parenthesizes for expressions in if conditions
                    do_prettyprint(&NodeKind::from(&cond.data), printer);
                    printer.print_str(&")");
                    if let ast::Stmt::Block(_) = stmt.data {
                        printer.print_str(&" ");
                        do_prettyprint(&NodeKind::from(&stmt.data), printer);
                        if opt_else
                            .as_ref()
                            .map_or(false, |els| !matches!(els.data, Empty))
                        {
                            printer.print_str(&" ");
                        }
                    } else {
                        printer.newline();
                        printer.indent();
                        do_prettyprint(&NodeKind::from(&stmt.data), printer);
                        printer.outdent();
                        if opt_else
                            .as_ref()
                            .map_or(false, |els| !matches!(els.data, Empty))
                        {
                            printer.newline();
                        }
                    }
                    if let Some(els) = opt_else {
                        match els.data {
                            Empty => (),
                            _ => match els.data {
                                If(..) | Block(..) => {
                                    printer.print_str(&"else ");
                                    do_prettyprint(&NodeKind::from(&els.data), printer)
                                }
                                _ => {
                                    printer.print_str(&"else");
                                    printer.newline();
                                    printer.indent();
                                    do_prettyprint(&NodeKind::from(&els.data), printer);
                                    printer.outdent();
                                }
                            },
                        }
                    }
                }

                While(cond, stmt) => {
                    printer.print_str(&"while (");
                    // no parenthesizes for expressions in while conditions
                    do_prettyprint(&NodeKind::from(&cond.data), printer);
                    printer.print_str(&")");
                    if let ast::Stmt::Block(_) = stmt.data {
                        printer.print_str(&" ");
                        do_prettyprint(&NodeKind::from(&stmt.data), printer);
                    } else {
                        printer.newline();
                        printer.indent();
                        do_prettyprint(&NodeKind::from(&stmt.data), printer);
                        printer.outdent();
                    }
                }

                Expression(expr) => {
                    // no parenthesizes for expressions in expression statements
                    do_prettyprint(&NodeKind::from(&expr.data), printer);
                    printer.print_str(&";");
                }

                Return(expr_opt) => {
                    printer.print_str(&"return");
                    if let Some(expr) = expr_opt {
                        printer.print_str(&" ");
                        // no parenthesizes for expressions in return statements
                        do_prettyprint(&NodeKind::from(&expr.data), printer);
                    }
                    printer.print_str(&";");
                }

                LocalVariableDeclaration(ty, name, opt_assign) => {
                    do_prettyprint(&NodeKind::from(&ty.data), printer);
                    printer.print(format_args!(" {}", name));
                    if let Some(assign) = opt_assign {
                        printer.print_str(&" = ");
                        // no parenthesizes for expressions in declaration initializations
                        do_prettyprint(&NodeKind::from(&assign.data), printer);
                    }
                    printer.print_str(&";");
                }
            };
        }

        Expr(expr) => do_prettyprint_expr(expr, printer),

        BinaryOp(bin_op) => {
            printer.print_str(&" ");
            use crate::ast::BinaryOp::*;
            match bin_op {
                Equals => printer.print_str(&"=="),
                NotEquals => printer.print_str(&"!="),
                LessThan => printer.print_str(&"<"),
                GreaterThan => printer.print_str(&">"),
                LessEquals => printer.print_str(&"<="),
                GreaterEquals => printer.print_str(&">="),
                LogicalOr => printer.print_str(&"||"),
                LogicalAnd => printer.print_str(&"&&"),
                Add => printer.print_str(&"+"),
                Sub => printer.print_str(&"-"),
                Mul => printer.print_str(&"*"),
                Div => printer.print_str(&"/"),
                Mod => printer.print_str(&"%"),
            }
            printer.print_str(&" ");
        }

        UnaryOp(unary_op) => {
            use crate::ast::UnaryOp::*;
            match unary_op {
                Not => printer.print_str(&"!"),
                Neg => printer.print_str(&"-"),
            }
        }

        PostfixOp(postfix_op) => {
            use crate::ast::PostfixOp::*;
            match postfix_op {
                MethodInvocation(name, args) => {
                    printer.print(format_args!(".{}(", name));
                    print_argument_list(&args.data, printer);
                    printer.print_str(&")");
                }
                FieldAccess(name) => {
                    printer.print(format_args!(".{}", name));
                }
                ArrayAccess(expr) => {
                    // no parenthesizes for array index expressions
                    printer.print_str(&"[");
                    do_prettyprint_expr(&expr.data, printer);
                    printer.print_str(&"]");
                }
            }
        }
    }
}

fn compare_class_member(a: &ast::ClassMember<'_>, b: &ast::ClassMember<'_>) -> std::cmp::Ordering {
    use crate::ast::ClassMemberKind::*;
    match (&a.kind, &b.kind) {
        (Field(..), Field(..))
        | (Method(..), Method(..))
        | (MainMethod(..), MainMethod(..))
        | (Method(..), MainMethod(..))
        | (MainMethod(..), Method(..)) => a.name.cmp(&b.name),
        (Method(..), Field(..)) | (MainMethod(..), Field(..)) => std::cmp::Ordering::Less,
        (Field(..), Method(..)) | (Field(..), MainMethod(..)) => std::cmp::Ordering::Greater,
    }
}

// clippy::ptr-arg wants args to be a slice of Expr,
// but that doesn't improve expressiveness here
#[allow(clippy::ptr_arg)]
fn print_argument_list(args: &crate::ast::ArgumentList<'_>, printer: &mut IndentPrinter<'_>) {
    for (i, arg) in args.iter().enumerate() {
        // no parenthesizes for arguments in function calls
        do_prettyprint_expr(&arg.data, printer);

        if i != args.len() - 1 {
            printer.print_str(&", ");
        }
    }
}

fn do_prettyprint_expr_parenthesized<'a, 't>(
    expr: &'a crate::ast::Expr<'t>,
    printer: &mut IndentPrinter<'_>,
) {
    use crate::ast::Expr::*;
    let parenthesize = match expr {
        Int(_) | Boolean(_) | Null | This | Var(_) => false,
        _ => true,
    };
    if parenthesize {
        printer.print_str(&"(");
    }
    do_prettyprint_expr(expr, printer);
    if parenthesize {
        printer.print_str(&")");
    }
}

fn do_prettyprint_expr<'a, 't>(expr: &'a crate::ast::Expr<'t>, printer: &mut IndentPrinter<'_>) {
    use crate::ast::Expr::*;
    match expr {
        Assignment(assignee, assignments) => {
            // assignee: a, assignments: [b, c, d]
            // => "a = (b = (c = d))"
            do_prettyprint_expr_parenthesized(&assignee.data, printer);
            for (i, assigned) in assignments.iter().enumerate() {
                printer.print_str(&" = ");
                if i != assignments.len() - 1 {
                    printer.print_str(&"(");
                }
                do_prettyprint_expr_parenthesized(&assigned.data, printer);
            }
            for _ in assignments.iter().skip(1) {
                printer.print_str(&")");
            }
        }
        Binary(op, lhs, rhs) => {
            do_prettyprint_expr_parenthesized(&lhs.data, printer);
            do_prettyprint(&NodeKind::from(op), printer);
            do_prettyprint_expr_parenthesized(&rhs.data, printer);
        }
        Unary(ops, expr) => {
            for (i, op) in ops.iter().enumerate() {
                do_prettyprint(&NodeKind::from(op), printer);
                if i != ops.len() - 1 {
                    printer.print_str(&"(");
                }
            }
            do_prettyprint_expr_parenthesized(&expr.data, printer);
            for _ in ops.iter().skip(1) {
                printer.print_str(&")");
            }
        }
        Postfix(prime, post_ops) => {
            for _ in post_ops.iter().skip(1) {
                printer.print_str(&"(");
            }
            do_prettyprint_expr_parenthesized(&prime.data, printer);
            for (i, post) in post_ops.iter().enumerate() {
                if i > 0 {
                    printer.print_str(&")");
                }
                do_prettyprint(&NodeKind::from(&post.data), printer);
            }
        }
        Null => printer.print_str(&"null"),
        Boolean(val) => printer.print(format_args!("{}", val)),
        Int(val) => printer.print(format_args!("{}", val)),
        Var(name) => printer.print(format_args!("{}", name)),
        MethodInvocation(name, args) => {
            printer.print(format_args!("{}(", name));
            print_argument_list(&args.data, printer);
            printer.print_str(&")");
        }
        This => printer.print_str(&"this"),
        NewObject(name) => {
            printer.print(format_args!("new {}()", name));
        }
        NewArray(basic_ty, size, brackets) => {
            printer.print_str(&"new ");
            do_prettyprint(&NodeKind::from(basic_ty), printer);
            printer.print_str(&"[");
            do_prettyprint_expr(&size.data, printer);
            printer.print(format_args!("]{}", "[]".repeat(*brackets as usize)));
        }
    }
}
