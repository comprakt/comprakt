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

fn do_prettyprint(n: &NodeKind<'_, '_>, pri: &mut IndentPrinter<'_>) {
    use crate::visitor::NodeKind::*;
    match n {
        Program(p) => {
            let mut classes: Vec<&ast::ClassDeclaration<'_>> =
                p.classes.iter().map(|c| c.get_data()).collect();
            classes.sort_by_key(|c| &c.name);
            classes
                .into_iter()
                .for_each(|x| do_prettyprint(&NodeKind::from(x), pri));
            writeln!(pri, "");
        }
        ClassDeclaration(d) => {
            write!(pri, "class {} {{", d.name);
            pri.indent(1);
            let mut members: Vec<&ast::ClassMember<'_>> =
                d.members.iter().map(|c| c.get_data()).collect();
            members.sort_by(|a, b| {
                use crate::ast::ClassMemberKindDiscriminants::*;
                let akind = ast::ClassMemberKindDiscriminants::from(&a.kind);
                let bkind = ast::ClassMemberKindDiscriminants::from(&b.kind);
                if akind == bkind
                    || (akind == Method && bkind == MainMethod)
                    || (akind == MainMethod && bkind == Method)
                {
                    return match a.name < b.name {
                        // TODO better way to compare strings
                        true => std::cmp::Ordering::Less,
                        false => match a.name > b.name {
                            true => std::cmp::Ordering::Greater,
                            false => std::cmp::Ordering::Equal,
                        },
                    };
                }
                match (akind, bkind) {
                    (x, y) if x == y => std::cmp::Ordering::Equal,
                    (Method, Field) | (MainMethod, Field) => std::cmp::Ordering::Less,
                    _ => std::cmp::Ordering::Greater, // TODO correct?
                }
            });
            members
                .into_iter()
                .for_each(|x| do_prettyprint(&NodeKind::from(x), pri));
            pri.indent(-1);
            write!(pri, "}}");
        }
        x => {
            x.for_each_child(&mut |child| {
                do_prettyprint(&child, pri);
            });
        }
    }
}
