use crate::{ast, visitor::NodeKind};
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

pub fn print(ast: &ast::AST<'_>, out: &mut dyn std::io::Write) -> Result<(), Error> {
    let mut printer = IndentPrinter::new(out);
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
                Program(p) => do_prettyprint(&NodeKind::Program(&p), printer),
            }
        }

        Program(program) => {
            let mut classes = program.classes.clone();
            classes.sort_by_key(|c| c.clone().data.name.data);
            classes
                .into_iter()
                .for_each(|class| do_prettyprint(&NodeKind::from(&class), printer));
        }

        ClassDeclaration(decl) => {
            printer.print(format_args!("class {} {{", decl.name));
            let mut members = decl.members.clone();
            members.sort_by(|x, y| compare_class_member(x, y));
            if !members.is_empty() {
                printer.println(format_args!(""));
                printer.indent();
                members
                    .into_iter()
                    .for_each(|member| do_prettyprint(&NodeKind::from(&member), printer));
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
                    do_prettyprint(&NodeKind::from(ty), printer);
                    printer.print(format_args!(" {};", member.name));
                }
                Method(ty, params, block) => {
                    printer.print_str(&"public ");
                    do_prettyprint(&NodeKind::from(ty), printer);
                    printer.print(format_args!(" {}(", member.name));
                    do_prettyprint(&NodeKind::from(params), printer);
                    printer.print_str(&") ");
                    do_prettyprint(&NodeKind::from(block), printer);
                }
                MainMethod(params, block) => {
                    printer.print(format_args!(
                        "public static void {}(String[] {}) ",
                        member.name, params.data[0].name,
                    ));
                    do_prettyprint(&NodeKind::from(block), printer);
                }
            }
            printer.newline();
        }

        Parameter(param) => {
            do_prettyprint(&NodeKind::from(&param.ty), printer);
            printer.print(format_args!(" {}", param.name));
        }

        ParameterList(params) => {
            for (i, param) in params.iter().enumerate() {
                do_prettyprint(&NodeKind::from(param), printer);
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
            match basic_ty.data {
                Int => printer.print_str(&"int"),
                Boolean => printer.print_str(&"boolean"),
                Void => printer.print_str(&"void"),
                Custom(name) => printer.print(format_args!("{}", name)),
            }
        }

        Block(block) => {
            // all() returns true on empty iterators
            if is_empty_block(block) {
                printer.print_str(&"{ }");
            } else {
                printer.print_str(&"{");
                printer.newline();
                printer.indent();
                for stmt in &block.statements {
                    match stmt.data {
                        crate::ast::Stmt::Empty => {}
                        _ => {
                            do_prettyprint(&NodeKind::from(stmt), printer);
                            printer.newline();
                        }
                    }
                }
                printer.outdent();
                printer.print_str(&"}");
            }
        }

        Stmt(stmt) => do_prettyprint_stmt(stmt, printer, true),

        Expr(expr) => do_prettyprint_expr(expr, printer),

        BinaryOp(bin_op) => {
            printer.print_str(&" ");
            use crate::ast::BinaryOp::*;
            match bin_op {
                Assign => printer.print_str(&"="),
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

fn do_prettyprint_expr<'a, 't>(expr: &'a ast::Expr<'t>, printer: &mut IndentPrinter<'_>) {
    use crate::ast::Expr::*;
    match expr {
        Binary(op, lhs, rhs) => {
            do_prettyprint_expr_parenthesized(&lhs.data, printer);
            do_prettyprint(&NodeKind::from(op), printer);
            do_prettyprint_expr_parenthesized(&rhs.data, printer);
        }
        Unary(op, expr) => {
            do_prettyprint(&NodeKind::from(op), printer);
            do_prettyprint_expr_parenthesized(&expr.data, printer);
        }
        MethodInvocation(target_expr, name, args) => {
            do_prettyprint_expr_parenthesized(target_expr, printer);
            printer.print(format_args!(".{}(", name));
            print_argument_list(&args.data, printer);
            printer.print_str(&")");
        }
        FieldAccess(target_expr, name) => {
            do_prettyprint_expr_parenthesized(target_expr, printer);
            printer.print(format_args!(".{}", name));
        }
        ArrayAccess(target_expr, idx_expr) => {
            do_prettyprint_expr_parenthesized(target_expr, printer);
            // no parenthesizes for array index expressions
            printer.print_str(&"[");
            do_prettyprint_expr(&idx_expr.data, printer);
            printer.print_str(&"]");
        }
        Null => printer.print_str(&"null"),
        Boolean(val) => printer.print(format_args!("{}", val)),
        Int(val) => printer.print(format_args!("{}", val)),
        Var(name) => printer.print(format_args!("{}", name)),
        ThisMethodInvocation(name, args) => {
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

#[allow(clippy::block_in_if_condition_stmt)]
fn do_prettyprint_stmt(
    stmt: &ast::Stmt<'_>,
    printer: &mut IndentPrinter<'_>,
    ignore_empty_else: bool,
) {
    use crate::ast::Stmt::*;
    match stmt {
        Block(block) => do_prettyprint(&NodeKind::from(block), printer),
        Empty => {
            printer.print_str(&";");
        }
        If(cond, stmt, opt_else) => {
            printer.print_str(&"if (");
            // no parenthesizes for expressions in if conditions
            do_prettyprint(&NodeKind::from(&**cond), printer);
            printer.print_str(&")");

            if let ast::Stmt::Block(block) = &stmt.data {
                printer.print_str(&" ");
                do_prettyprint(&NodeKind::from(&**stmt), printer);
                if opt_else.as_ref().map_or(false, |els| {
                    !(ignore_empty_else && matches!(els.data, Empty))
                }) {
                    if is_empty_block(block) {
                        printer.newline();
                    } else {
                        printer.print_str(&" ");
                    }
                }
            } else {
                printer.newline();
                printer.indent();
                match stmt.data {
                    ast::Stmt::If(..) => {
                        if let Some(els) = opt_else {
                            if matches!(els.data, ast::Stmt::Empty) && ignore_empty_else {
                                do_prettyprint_stmt(&stmt.data, printer, true);
                            } else {
                                do_prettyprint_stmt(&stmt.data, printer, false);
                            }
                        } else {
                            do_prettyprint_stmt(&stmt.data, printer, ignore_empty_else);
                        }
                    }
                    _ => do_prettyprint(&NodeKind::from(&**stmt), printer),
                };
                printer.outdent();
                if opt_else.as_ref().map_or(false, |els| {
                    !(ignore_empty_else && matches!(els.data, Empty))
                }) {
                    printer.newline();
                }
            }
            if let Some(els) = opt_else {
                match els.data {
                    Empty if ignore_empty_else => (),
                    _ => match els.data {
                        If(..) | Block(..) => {
                            printer.print_str(&"else ");
                            do_prettyprint(&NodeKind::from(&**els), printer)
                        }
                        _ => {
                            printer.print_str(&"else");
                            printer.newline();
                            printer.indent();
                            do_prettyprint(&NodeKind::from(&**els), printer);
                            printer.outdent();
                        }
                    },
                }
            }
        }

        While(cond, stmt) => {
            printer.print_str(&"while (");
            // no parenthesizes for expressions in while conditions
            do_prettyprint(&NodeKind::from(&**cond), printer);
            printer.print_str(&")");
            if let ast::Stmt::Block(_) = stmt.data {
                printer.print_str(&" ");
                do_prettyprint(&NodeKind::from(&**stmt), printer);
            } else {
                printer.newline();
                printer.indent();
                do_prettyprint(&NodeKind::from(&**stmt), printer);
                printer.outdent();
            }
        }

        Expression(expr) => {
            // no parenthesizes for expressions in expression statements
            do_prettyprint(&NodeKind::from(&**expr), printer);
            printer.print_str(&";");
        }

        Return(expr_opt) => {
            printer.print_str(&"return");
            if let Some(expr) = expr_opt {
                printer.print_str(&" ");
                // no parenthesizes for expressions in return statements
                do_prettyprint(&NodeKind::from(&**expr), printer);
            }
            printer.print_str(&";");
        }

        LocalVariableDeclaration(ty, name, opt_assign) => {
            do_prettyprint(&NodeKind::from(ty), printer);
            printer.print(format_args!(" {}", name));
            if let Some(assign) = opt_assign {
                printer.print_str(&" = ");
                // no parenthesizes for expressions in declaration initializations
                do_prettyprint(&NodeKind::from(&**assign), printer);
            }
            printer.print_str(&";");
        }
    };
}

fn is_empty_block(block: &ast::Block<'_>) -> bool {
    block
        .statements
        .iter()
        .all(|stmt| stmt.data == ast::Stmt::Empty)
}
