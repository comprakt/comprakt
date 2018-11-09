use crate::{ast, context};
use failure::Error;

use crate::visitor::NodeKind;

struct IndentPrinter<'w> {
    writer: &'w mut dyn std::io::Write,
    indent: usize,
}

impl<'w> IndentPrinter<'w> {
    fn new(writer: &'w mut dyn std::io::Write) -> IndentPrinter<'w> {
        return IndentPrinter { writer, indent: 0 };
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
        x => {
            x.for_each_child(&mut |child| {
                do_prettyprint(&child, pri);
            });
        }
    }
}
