#![warn(clippy::print_stdout)]
use failure::{Error, Fail, ResultExt};
use regex::Regex;
use std::{
    fs::File,
    io::{self, Read},
    path::PathBuf,
    process::exit,
};
use structopt::StructOpt;
use termcolor::{ColorChoice, StandardStream};

use compiler_lib::{
    asciifile::{AsciiFile, Span, Spanned},
    ast::{self, Expr},
    context::{self, Context},
    lexer::{Lexer, TokenKind},
    parser::Parser,
    semantics,
    strtab::StringTable,
    type_checking::{
        type_analysis::{ExprInfo, RefInfo, TypeAnalysis},
        type_system::CheckedType,
    },
    visitor::NodeKind,
};

#[derive(Debug, Fail)]
pub enum CliError {
    #[fail(display = "invalid regular expression")]
    InvalidRegex,
    #[fail(display = "cannot open input file {:?}", path)]
    OpenInput { path: PathBuf },
    #[fail(display = "cannot decode input file {:?}", path)]
    Ascii { path: PathBuf },
}

#[derive(StructOpt)]
#[structopt(name = "ast-inspector")]
pub struct CliCommand {
    /// MiniJava file that should be parsed into an AST.
    #[structopt(name = "FILE", parse(from_os_str))]
    input: PathBuf,
    /// a regular expression used to filter the source code.
    /// Only matching AST elements will be dumped. Defaults to
    /// filtering nothing.
    #[structopt(short = "c", long = "content", default_value = "")]
    regex_content: Regex,
    /// a regular expression used to filter the source code.
    /// Only matching AST elements will be dumped. Defaults to
    /// filtering nothing.
    #[structopt(short = "k", long = "node-kind", default_value = "")]
    regex_node_kind: Regex,
    /// a regular expression used to filter the source code.
    /// Only expressions with matching type information will be dumped.
    /// Defaults to filtering nothing.
    #[structopt(short = "t", long = "type-of-expression", default_value = "")]
    regex_type: Regex,
    /// stop after first regex match
    #[structopt(short = "1", long = "only-first")]
    only_first_match: bool,
}

fn main() {
    let cmd = CliCommand::from_args();

    if let Err(msg) = do_main(cmd) {
        exit_with_error(&msg);
    }
}

fn do_main(cmd: CliCommand) -> Result<(), Error> {
    let input = read_input(&cmd)
        .context(CliError::OpenInput {
            path: cmd.input.clone(),
        })
        .context(CliError::OpenInput {
            path: cmd.input.clone(),
        })?;

    let ascii_file = AsciiFile::new(input.as_bytes()).context(CliError::Ascii {
        path: cmd.input.clone(),
    })?;

    let stdout = StandardStream::stdout(ColorChoice::Auto);
    let context = Context::new(&ascii_file, Box::new(stdout));

    let mut strtab = StringTable::new();
    let lexer = Lexer::new(&mut strtab, &context);

    // adapt lexer to fail on first error
    // filter whitespace and comments
    let unforgiving_lexer = lexer.filter_map(|result| match result {
        Ok(token) => match token.data {
            TokenKind::Whitespace | TokenKind::Comment(_) => None,
            _ => Some(token),
        },
        Err(lexical_error) => {
            context.diagnostics.error(&lexical_error);
            context.diagnostics.write_statistics();
            exit(1);
        }
    });

    let mut parser = Parser::new(unforgiving_lexer);

    let program_ast = match parser.parse() {
        Ok(p) => p,
        Err(parser_error) => {
            context.diagnostics.error(&parser_error);
            context.diagnostics.write_statistics();
            exit(1);
        }
    };

    // TODO: type system should not write to context directly!!!
    let (_type_system, type_analysis) = semantics::check(&mut strtab, &program_ast, &context)
        .unwrap_or_else(|()| {
            context.diagnostics.write_statistics();
            exit(1);
        });

    let ast_inspector = AstInspector::new(cmd, &context, &type_analysis);

    ast_inspector.run(&program_ast)
}

fn read_input(cmd: &CliCommand) -> Result<String, Error> {
    let mut f = File::open(&cmd.input)?;
    let mut contents = String::new();
    f.read_to_string(&mut contents)?;
    Ok(contents)
}

// TODO: deduplicate. Can also be found in compiler-cli
/// Print an error in a format intended for end users and terminate
/// the program.
fn exit_with_error(err: &Error) -> ! {
    let mut stderr = io::stderr();
    print_error(&mut stderr, err).expect("unable to print error");
    exit(1);
}

/// Print error objects in a format intended for end users
fn print_error(writer: &mut dyn io::Write, err: &Error) -> Result<(), Error> {
    writeln!(writer, "error: {}", err.as_fail())?;
    for cause in err.iter_causes() {
        writeln!(writer, "caused by: {}", cause)?;
    }
    Ok(())
}

#[derive(Debug, Fail)]
pub enum Messages {
    #[fail(display = "Matched kind '{}'", kind)]
    Matched { kind: String },
    #[fail(display = "Matched kind 'expression' of type `{}`", typing)]
    MatchedExpr { typing: String },
}

pub struct AstInspector<'f> {
    cfg: CliCommand,
    context: &'f context::Context<'f>,
    type_analysis: &'f TypeAnalysis<'f, 'f>,
}

impl<'f> AstInspector<'f> {
    pub fn new(
        cfg: CliCommand,
        context: &'f context::Context<'f>,
        type_analysis: &'f TypeAnalysis<'f, 'f>,
    ) -> Self {
        Self {
            cfg,
            context,
            type_analysis,
        }
    }

    fn print_expr_if_match(&self, expr: &Spanned<'_, Expr<'_>>) {
        let expr_info = self.type_analysis.expr_info(expr);
        let expr_description = expr_info_to_str(&expr_info);

        if self.is_match("expression", &expr.span)
            && self.cfg.regex_type.is_match(&expr_description)
        {
            self.context.diagnostics.info(&Spanned {
                span: expr.span,
                data: Messages::MatchedExpr {
                    typing: expr_description,
                },
            });

            self.has_matched();
        }
    }

    fn print_if_match(&self, kindname: &str, span: &Span) {
        if self.is_match(kindname, span) {
            self.context.diagnostics.info(&Spanned {
                span: *span,
                data: Messages::Matched {
                    kind: kindname.to_string(),
                },
            });

            self.has_matched();
        }
    }

    fn is_match(&self, kindname: &str, span: &Span) -> bool {
        self.cfg.regex_node_kind.is_match(kindname)
            && self.cfg.regex_content.is_match(span.as_str())
    }

    fn has_matched(&self) {
        if self.cfg.only_first_match {
            self.context
                .diagnostics
                .info(&"Stopping after first match since `--only-first` was given.");
            exit(1);
        }
    }

    pub fn run(&self, ast: &ast::AST<'f>) -> Result<(), Error> {
        self.recurse(&NodeKind::from(ast))
    }
    pub fn recurse(&self, parent: &NodeKind<'_, '_>) -> Result<(), Error> {
        parent.for_each_child(&mut |child| {
            match child {
                NodeKind::AST(_ast) => {
                    //&'a AST<'t>
                }
                NodeKind::Program(spanned) => {
                    //&'a Spanned<'t, Program<'t>>
                    self.print_if_match("program", &spanned.span)
                }
                NodeKind::ClassDeclaration(spanned) => {
                    //&'a Spanned<'t, ClassDeclaration<'t>>
                    self.print_if_match("class declaration", &spanned.span)
                }
                NodeKind::ClassMember(spanned) => {
                    //&'a Spanned<'t, ClassMember<'t>>
                    self.print_if_match("class member", &spanned.span)
                }
                NodeKind::Parameter(spanned) => {
                    //&'a Spanned<'t, Parameter<'t>>
                    self.print_if_match("parameter", &spanned.span)
                }
                NodeKind::ParameterList(spanned) => {
                    //&'a Spanned<'t, ParameterList<'t>>
                    self.print_if_match("parameter list", &spanned.span)
                }
                NodeKind::Type(spanned) => {
                    //&'a Spanned<'t, Type>
                    self.print_if_match("type", &spanned.span)
                }
                NodeKind::BasicType(_basic_type) => {
                    //&'a BasicType
                }
                NodeKind::Block(spanned) => {
                    //&'a Spanned<'t, Block<'t>>
                    self.print_if_match("block", &spanned.span)
                }
                NodeKind::Stmt(spanned) => {
                    //&'a Spanned<'t, Stmt<'t>>
                    self.print_if_match("statement", &spanned.span)
                }
                NodeKind::Expr(spanned) => {
                    //&'a Spanned<'t, Expr<'t>>
                    self.print_expr_if_match(spanned)
                }
                NodeKind::BinaryOp(_binary_op) => {
                    //&'a BinaryOp
                }
                NodeKind::UnaryOp(_unary_op) => {
                    //&'a UnaryOp
                }
            };

            self.recurse(&child)
        });

        Ok(())
    }
}

fn checked_type_to_str(ty: &CheckedType<'_>) -> String {
    use self::CheckedType::*;
    match ty {
        Int => "integer".to_string(),
        Boolean => "boolean".to_string(),
        Void => "void".to_string(),
        Null => "null".to_string(),
        TypeRef(class_def) => format!("reference-type '{}'", class_def.as_str()),
        UnknownType(symbol) => format!("unknown type '{}'", symbol.as_str()),
        Array(ty) => format!("array of {}", checked_type_to_str(&*ty)),
    }
}

fn expr_info_to_str(expr: &ExprInfo<'_, '_>) -> String {
    let ty_description = checked_type_to_str(&expr.ty);

    if let Some(ref ref_info) = expr.ref_info {
        format!(
            "{} associated with {}",
            ty_description,
            ref_info_to_str(ref_info)
        )
    } else {
        ty_description
    }
}

fn ref_info_to_str(ty: &RefInfo<'_, '_>) -> String {
    use self::RefInfo::*;
    match ty {
        GlobalVar(symbol) => format!("global variable '{}'", symbol.as_str()),
        Var(local) => format!("local variable definition '{}'", local.name.as_str()),
        Param(param) => format!("method parameter definition '{}'", param.name.as_str()),
        Field(field) => format!(
            "{}field definition '{}'",
            if field.can_write { "" } else { "read-only " },
            field.name.as_str()
        ),
        Method(method) => format!(
            "{}{}method definition '{}'",
            if method.is_static { "static " } else { "" },
            if method.is_main { "main " } else { "" },
            method.name.as_str()
        ),
        This(class) => format!(
            "{} class definition '{}'",
            if class.comparable {
                "comparable"
            } else {
                "incomparable"
            },
            class.name.as_str()
        ),
        ArrayAccess => "an array access".to_string(),
    }
}
