#![feature(box_syntax)]
#![warn(
    clippy::print_stdout,
    clippy::unimplemented,
    clippy::doc_markdown,
    clippy::items_after_statements,
    clippy::match_same_arms,
    clippy::similar_names,
    clippy::single_match_else,
    clippy::use_self
)]

pub mod ast;
mod spantracker;

#[macro_use]
pub mod visitor;

#[macro_use]
extern crate derive_more;

use crate::spantracker::*;
use asciifile::{
    MaybeSpanned::{self, *},
    Span, Spanned,
};
use diagnostics::MessageLevel;
use lexer::{IntLit, Keyword, Operator, Token, TokenKind};
use strtab::Symbol;

use failure::Fail;
use itertools::Itertools;

use std::{
    collections::{HashSet, VecDeque},
    fmt,
    ops::{Deref, DerefMut},
};

type Precedence = usize;
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Assoc {
    Left,
    Right,
}
#[rustfmt::skip]
const BINARY_OPERATORS: &[(Operator, ast::BinaryOp, Precedence, Assoc)] = &[
    (Operator::Star,              ast::BinaryOp::Mul,           1, Assoc::Left),
    (Operator::Slash,             ast::BinaryOp::Div,           1, Assoc::Left),
    (Operator::Percent,           ast::BinaryOp::Mod,           1, Assoc::Left),

    (Operator::Plus,              ast::BinaryOp::Add,           2, Assoc::Left),
    (Operator::Minus,             ast::BinaryOp::Sub,           2, Assoc::Left),

    (Operator::LeftChevron,       ast::BinaryOp::LessThan,      3, Assoc::Left),
    (Operator::LeftChevronEqual,  ast::BinaryOp::LessEquals,    3, Assoc::Left),
    (Operator::RightChevron,      ast::BinaryOp::GreaterThan,   3, Assoc::Left),
    (Operator::RightChevronEqual, ast::BinaryOp::GreaterEquals, 3, Assoc::Left),

    (Operator::DoubleEqual,       ast::BinaryOp::Equals,        4, Assoc::Left),
    (Operator::ExclaimEqual,      ast::BinaryOp::NotEquals,     4, Assoc::Left),

    (Operator::DoubleAmpersand,   ast::BinaryOp::LogicalAnd,    5, Assoc::Left),
    (Operator::DoublePipe,        ast::BinaryOp::LogicalOr,     6, Assoc::Left),

    (Operator::Equal,             ast::BinaryOp::Assign,       10, Assoc::Right),

];

lazy_static::lazy_static! {
    static ref VANILLA_MODE: bool = std::env::var("VANILLA").is_ok();
}

#[rustfmt::skip]
#[derive(Debug, Clone, Fail)]
pub enum SyntaxError {
    #[fail(display = "expected {}, found {}", expected, actual)]
    UnexpectedToken {
        actual: String,
        expected: String, // Has to be string, because Alternatives has a lifetime
    },
    #[fail(display = "unexpected end of file")]
    UnexpectedEOF,
}

impl<'f> From<EOF> for MaybeSpanned<'f, SyntaxError> {
    fn from(_: EOF) -> Self {
        MaybeSpanned::WithoutSpan(SyntaxError::UnexpectedEOF)
    }
}

trait ExpectedToken<'f>: fmt::Debug + fmt::Display + Copy + Into<TaggedExpectedToken<'f>> {
    type Yields;
    fn matching(&self, token: &TokenKind<'f>) -> Option<Self::Yields>;

    fn matches(&self, token: &TokenKind<'f>) -> bool {
        self.matching(token).is_some()
    }
}

#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq, Display, PartialOrd, Ord)]
enum TaggedExpectedToken<'f> {
    Exactly(Exactly<'f>),
    BinaryOp(BinaryOp),
    UnaryOp(UnaryOp),
    Identifier(Identifier),
    ExactlyIdentifier(ExactlyIdentifier<'f>),
    IntegerLiteral(IntegerLiteral),
}

macro_rules! gen_from_expected_token {
    ($name: ident) => {
        gen_from_expected_token!($name, $name);
    };
    ($variant: ident, $name: ty) => {
        impl<'f> From<$name> for TaggedExpectedToken<'f> {
            fn from(src: $name) -> TaggedExpectedToken<'f> {
                TaggedExpectedToken::$variant(src)
            }
        }
    };
}

gen_from_expected_token!(Exactly, Exactly<'f>);
gen_from_expected_token!(BinaryOp);
gen_from_expected_token!(UnaryOp);
gen_from_expected_token!(Identifier);
gen_from_expected_token!(ExactlyIdentifier, ExactlyIdentifier<'f>);
gen_from_expected_token!(IntegerLiteral);

struct Alternatives<'f>(HashSet<TaggedExpectedToken<'f>>);

impl<'f> Alternatives<'f> {
    fn new() -> Self {
        Alternatives(HashSet::new())
    }
}

impl<'f> Deref for Alternatives<'f> {
    type Target = HashSet<TaggedExpectedToken<'f>>;
    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl<'f> DerefMut for Alternatives<'f> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

impl<'f> fmt::Display for Alternatives<'f> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut alternatives: Vec<_> = self.0.iter().collect();
        alternatives.sort();
        if alternatives.is_empty() {
            panic!("musn't be called with empty set")
        } else if alternatives.len() == 1 {
            write!(f, "{}", alternatives[0])
        } else {
            let last = alternatives.pop().unwrap();
            let alts = alternatives.drain(..).format(", ");
            write!(f, "{} or {}", alts, last)
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Display)]
#[display(fmt = "{}", _0)]
struct Exactly<'f>(TokenKind<'f>);
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Display)]
#[display(fmt = "a binary operator")]
struct BinaryOp;
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Display)]
#[display(fmt = "a unary operator")]
struct UnaryOp;
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Display)]
#[display(fmt = "an identifier")]
struct Identifier;
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Display)]
#[display(fmt = "identifier '{}'", _0)]
struct ExactlyIdentifier<'s>(&'s str);
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Display)]
#[display(fmt = "an integer literal")]
struct IntegerLiteral;

impl<'f> From<Operator> for Exactly<'f> {
    fn from(op: Operator) -> Self {
        Exactly(TokenKind::Operator(op))
    }
}

impl<'f> From<Keyword> for Exactly<'f> {
    fn from(kw: Keyword) -> Self {
        Exactly(TokenKind::Keyword(kw))
    }
}

fn exactly<'f>(thing: impl Into<Exactly<'f>>) -> Exactly<'f> {
    thing.into()
}

impl<'f> ExpectedToken<'f> for Exactly<'f> {
    type Yields = ();
    fn matching(&self, token: &TokenKind<'f>) -> Option<Self::Yields> {
        if &self.0 == token {
            Some(())
        } else {
            None
        }
    }
}

impl<'f> ExpectedToken<'f> for BinaryOp {
    type Yields = (ast::BinaryOp, Precedence, Assoc);
    fn matching(&self, token: &TokenKind<'f>) -> Option<Self::Yields> {
        match token {
            TokenKind::Operator(op) => BINARY_OPERATORS
                .iter()
                .find(|(this_op, _, _, _)| this_op == op)
                .map(|(_, op, prec, assoc)| (*op, *prec, *assoc)),
            _ => None,
        }
    }
}

impl<'f> ExpectedToken<'f> for UnaryOp {
    type Yields = ast::UnaryOp;
    fn matching(&self, token: &TokenKind<'f>) -> Option<Self::Yields> {
        match token {
            TokenKind::Operator(Operator::Exclaim) => Some(ast::UnaryOp::Not),
            TokenKind::Operator(Operator::Minus) => Some(ast::UnaryOp::Neg),
            _ => None,
        }
    }
}

impl<'f> ExpectedToken<'f> for Identifier {
    type Yields = Symbol<'f>;
    fn matching(&self, token: &TokenKind<'f>) -> Option<Self::Yields> {
        match token {
            TokenKind::Identifier(ident) => Some(*ident),
            _ => None,
        }
    }
}

impl<'s> ExpectedToken<'s> for ExactlyIdentifier<'s> {
    type Yields = Symbol<'s>;
    fn matching(&self, token: &TokenKind<'s>) -> Option<Self::Yields> {
        match token {
            TokenKind::Identifier(ident) if ident == self.0 => Some(*ident),
            _ => None,
        }
    }
}

impl<'f> ExpectedToken<'f> for IntegerLiteral {
    type Yields = IntLit<'f>;
    fn matching(&self, token: &TokenKind<'f>) -> Option<Self::Yields> {
        match token {
            TokenKind::IntegerLiteral(lit) => Some(lit),
            _ => None,
        }
    }
}

type SyntaxResult<'f, T> = Result<T, MaybeSpanned<'f, SyntaxError>>;
type ParserResult<'f, T> = SyntaxResult<'f, Spanned<'f, T>>;
type BoxedResult<'f, T> = SyntaxResult<'f, Box<Spanned<'f, T>>>;

pub struct Parser<'f, I>
where
    I: Iterator<Item = Token<'f>>,
{
    lexer: SpanTracker<'f, I>,
    alternatives: VecDeque<Alternatives<'f>>,
}

macro_rules! spanned {
    ($self:expr, $code:expr) => {{
        let start = $self.lexer.peek_span();
        let data: SyntaxResult<'f, _> = $code;
        let data = data?;
        let end = $self
            .lexer
            .prev_span()
            .expect(concat!(
                "Bug! We can't represent an empty range. ",
                "The `$code` should have consumed at least one token"
            ))
            .end_position();

        // We check the error of `start` only now, because we trust the `$code`
        // to handle the error better than we could.
        let start = start?.start_position();

        let res: SyntaxResult<'f, _> = Ok(Spanned {
            span: Span::new(start, end),
            data: data,
        });

        res
    }};
}

impl<'f, I> Parser<'f, I>
where
    I: Iterator<Item = Token<'f>>,
{
    pub fn new(lexer: I) -> Self {
        Parser {
            lexer: SpanTracker::new(lexer),
            alternatives: VecDeque::new(),
        }
    }

    fn eat<E>(&mut self, want: E) -> SyntaxResult<'f, Spanned<'f, E::Yields>>
    where
        E: ExpectedToken<'f>,
    {
        let actual = self.lexer.next()?;
        let mut current_alternatives = self
            .alternatives
            .pop_front()
            .unwrap_or_else(Alternatives::new);

        want.matching(&actual.data)
            .map(|yielded| actual.map_ref(|_| yielded))
            .ok_or_else(|| {
                current_alternatives.insert(want.into());
                WithSpan(Spanned {
                    span: actual.span,
                    data: SyntaxError::UnexpectedToken {
                        actual: actual.data.to_string(),
                        expected: current_alternatives.to_string(),
                    },
                })
            })
    }

    /// This cannot fail, because the only possible error is EOF, and that is
    /// covered by `None`
    fn eat_optional<E>(&mut self, want: E) -> Option<Spanned<'f, E::Yields>>
    where
        E: ExpectedToken<'f>,
    {
        let got = self.lexer.peek().ok()?;

        if let Some(yielded) = want.matching(&got.data) {
            let got = self.lexer.next().unwrap();
            self.alternatives.pop_front();
            Some(got.map(|_| yielded))
        } else {
            if self.alternatives.get(0).is_none() {
                self.alternatives.push_back(Alternatives::new())
            }
            self.alternatives[0].insert(want.into());
            None
        }
    }

    fn tastes_like<E>(&mut self, want: E) -> bool
    where
        E: ExpectedToken<'f>,
    {
        self.nth_tastes_like(0, want)
    }

    fn nth_tastes_like<E>(&mut self, n: usize, want: E) -> bool
    where
        E: ExpectedToken<'f>,
    {
        let tastes_good = self
            .lexer
            .peek_nth(n)
            .map(|got| want.matches(&got.data))
            .unwrap_or(false);

        if !tastes_good {
            while self.alternatives.len() <= n {
                self.alternatives.push_back(Alternatives::new());
            }
            self.alternatives[n].insert(want.into());
        }

        tastes_good
    }

    pub fn parse(&mut self) -> Result<ast::AST<'f>, MaybeSpanned<'f, SyntaxError>> {
        if self.lexer.eof() {
            Ok(ast::AST::Empty)
        } else {
            Ok(ast::AST::Program(self.parse_program()?))
        }
    }

    fn parse_program(&mut self) -> ParserResult<'f, ast::Program<'f>> {
        spanned!(self, {
            let mut attrs = Vec::new();
            if !*VANILLA_MODE {
                loop {
                    if let Ok(TokenKind::Operator(Operator::Slash)) =
                        self.lexer.peek().map(|t| t.data)
                    {
                        if let Ok(TokenKind::Operator(Operator::Slash)) =
                            self.lexer.peek_nth(1).map(|t| t.data)
                        {
                            if let Ok(TokenKind::Operator(Operator::QuestionMark)) =
                                self.lexer.peek_nth(2).map(|t| t.data)
                            {
                                if let Ok(TokenKind::Operator(Operator::Exclaim)) =
                                    self.lexer.peek_nth(3).map(|t| t.data)
                                {
                                    self.eat(Exactly(TokenKind::Operator(Operator::Slash)))?;
                                    self.eat(Exactly(TokenKind::Operator(Operator::Slash)))?;
                                    self.eat(Exactly(TokenKind::Operator(Operator::QuestionMark)))?;
                                    self.eat(Exactly(TokenKind::Operator(Operator::Exclaim)))?;
                                    attrs.push(self.parse_attribute()?);
                                    continue;
                                }
                            }
                        }
                    }

                    break;
                }
            }
            let mut classes = Vec::new();
            while !self.lexer.eof() {
                classes.push(self.parse_class_declaration()?);
            }

            Ok(ast::Program { classes, attrs })
        })
    }

    fn parse_class_declaration(&mut self) -> ParserResult<'f, ast::ClassDeclaration<'f>> {
        spanned!(self, {
            let mut attrs = Vec::new();
            if !*VANILLA_MODE {
                while self
                    .eat_optional(Exactly(TokenKind::Operator(Operator::Slash)))
                    .is_some()
                {
                    self.eat(Exactly(TokenKind::Operator(Operator::Slash)))?;
                    self.eat(Exactly(TokenKind::Operator(Operator::QuestionMark)))?;
                    attrs.push(self.parse_attribute()?);
                }
            }

            self.eat(exactly(Keyword::Class))?;
            let name = self.eat(Identifier)?;

            let mut members = Vec::new();
            self.eat(exactly(Operator::LeftBrace))?;
            while self.eat_optional(exactly(Operator::RightBrace)).is_none() {
                members.push(self.parse_class_member()?);
            }

            Ok(ast::ClassDeclaration {
                name,
                members,
                attrs,
            })
        })
    }

    fn parse_class_member(&mut self) -> ParserResult<'f, ast::ClassMember<'f>> {
        spanned!(self, {
            let mut attrs = Vec::new();
            if !*VANILLA_MODE {
                while self
                    .eat_optional(Exactly(TokenKind::Operator(Operator::Slash)))
                    .is_some()
                {
                    self.eat(Exactly(TokenKind::Operator(Operator::Slash)))?;
                    self.eat(Exactly(TokenKind::Operator(Operator::QuestionMark)))?;
                    attrs.push(self.parse_attribute()?);
                }
            }

            self.eat(exactly(Keyword::Public))?;
            let is_static = self.eat_optional(exactly(Keyword::Static)).is_some();

            Ok(if is_static {
                // Main Method
                // Consume exactly `void IDENT(String[] IDENT)`
                self.eat(exactly(Keyword::Void))?;
                let name = self.eat(Identifier)?.data;
                let params_begin = self.eat(exactly(Operator::LeftParen))?;
                let java_string = self.eat(ExactlyIdentifier("String"))?;
                self.eat(exactly(Operator::LeftBracket))?;
                let java_string_array_close = self.eat(exactly(Operator::RightBracket))?;
                let string_array_type = Spanned {
                    span: Span::combine(&java_string.span, &java_string_array_close.span),
                    data: ast::Type {
                        // treat String[] as an opaque type
                        basic: Spanned::new(java_string.span, ast::BasicType::MainParam),
                        array_depth: 0,
                    },
                };
                let param_name = self.eat(Identifier)?;
                let params_end = self.eat(exactly(Operator::RightParen))?;
                let params = Spanned {
                    data: vec![Spanned {
                        data: ast::Parameter {
                            ty: string_array_type,
                            name: param_name.data,
                        },
                        span: Span::combine(&java_string.span, &param_name.span),
                    }],
                    span: Span::combine(&params_begin.span, &params_end.span),
                };

                self.skip_method_rest()?;
                let body = self.parse_block()?;

                let kind = ast::ClassMemberKind::MainMethod(params, body);
                ast::ClassMember { kind, name, attrs }
            } else {
                let ty = self.parse_type()?;
                let name = self.eat(Identifier)?.data;

                let kind = if self.tastes_like(exactly(Operator::LeftParen)) {
                    // Method
                    let params = self.parse_parameter_declarations()?;
                    self.skip_method_rest()?;
                    let body = self.parse_block()?;

                    ast::ClassMemberKind::Method(ty, params, body)
                } else {
                    // Field
                    self.eat(exactly(Operator::Semicolon))?;
                    ast::ClassMemberKind::Field(ty)
                };

                ast::ClassMember { kind, name, attrs }
            })
        })
    }

    fn parse_attribute(&mut self) -> ParserResult<'f, ast::Attribute<'f>> {
        spanned!(self, {
            let attr = self.eat(Identifier)?;
            if let Some(lvl) = MessageLevel::from_string(attr.as_str()) {
                self.eat(Exactly(TokenKind::Operator(Operator::Colon)))?;
                let name = self.eat(Identifier)?;
                Ok(ast::Attribute::LintLevel(lvl, name))
            } else {
                match attr.as_str() {
                    "inline" => Ok(ast::Attribute::Inline(true)),
                    "noinline" => Ok(ast::Attribute::Inline(false)),
                    _ => Err(MaybeSpanned::WithSpan(Spanned::new(
                        attr.span,
                        SyntaxError::UnexpectedToken {
                            expected: "valid attribute name".to_string(),
                            actual: format!("`{}`", attr.data),
                        },
                    ))),
                }
            }
        })
    }

    fn skip_method_rest(&mut self) -> SyntaxResult<'f, ()> {
        if self.eat_optional(exactly(Keyword::Throws)).is_some() {
            self.eat(Identifier)?;
        }

        Ok(())
    }

    fn parse_parameter_declarations(&mut self) -> ParserResult<'f, ast::ParameterList<'f>> {
        self.parse_parnethesized_list(|parser| parser.parse_parameter())
    }

    fn parse_parameter(&mut self) -> ParserResult<'f, ast::Parameter<'f>> {
        spanned!(self, {
            let ty = self.parse_type()?;
            let name = self.eat(Identifier)?.data;
            Ok(ast::Parameter { ty, name })
        })
    }

    fn parse_type(&mut self) -> ParserResult<'f, ast::Type<'f>> {
        spanned!(self, {
            let basic = self.parse_basic_type()?;

            let mut array_depth = 0;
            while self.eat_optional(exactly(Operator::LeftBracket)).is_some() {
                self.eat(exactly(Operator::RightBracket))?;
                array_depth += 1;
            }

            Ok(ast::Type { basic, array_depth })
        })
    }

    fn parse_basic_type(&mut self) -> SyntaxResult<'f, Spanned<'f, ast::BasicType<'f>>> {
        Ok(
            if let Some(basic) = self.eat_optional(exactly(Keyword::Int)) {
                basic.map(|_| ast::BasicType::Int)
            } else if let Some(basic) = self.eat_optional(exactly(Keyword::Boolean)) {
                basic.map(|_| ast::BasicType::Boolean)
            } else if let Some(basic) = self.eat_optional(exactly(Keyword::Void)) {
                basic.map(|_| ast::BasicType::Void)
            } else {
                self.eat(Identifier)?.map(ast::BasicType::Custom)
            },
        )
    }

    fn parse_block(&mut self) -> ParserResult<'f, ast::Block<'f>> {
        spanned!(self, {
            self.eat(exactly(Operator::LeftBrace))?;

            let mut statements = Vec::new();
            while self.eat_optional(exactly(Operator::RightBrace)).is_none() {
                statements.push(self.parse_block_statement()?);
            }

            Ok(ast::Block { statements })
        })
    }

    fn parse_statement(&mut self) -> ParserResult<'f, ast::Stmt<'f>> {
        self.parse_statement_or_local_var(false)
    }

    fn parse_block_statement(&mut self) -> ParserResult<'f, ast::Stmt<'f>> {
        self.parse_statement_or_local_var(true)
    }

    // Using a bool-flag for *LocalVarDeclStatement* allows us to delay the
    // descision on weather the statement at point is a *LocalVarDeclStatement*
    fn parse_statement_or_local_var(
        &mut self,
        allow_local_var_decl: bool,
    ) -> ParserResult<'f, ast::Stmt<'f>> {
        spanned!(self, {
            use self::ast::Stmt::*;

            Ok(if self.tastes_like(exactly(Operator::LeftBrace)) {
                Block(self.parse_block()?)
            } else if self.eat_optional(exactly(Operator::Semicolon)).is_some() {
                // empty statement
                Empty
            } else if self.eat_optional(exactly(Keyword::If)).is_some() {
                self.eat(exactly(Operator::LeftParen))?;
                let cond = self.parse_expression()?;
                self.eat(exactly(Operator::RightParen))?;

                let if_arm = self.parse_statement()?;
                let else_arm = if self.eat_optional(exactly(Keyword::Else)).is_some() {
                    Some(self.parse_statement()?)
                } else {
                    None
                };

                If(cond, box if_arm, else_arm.map(Box::new))
            } else if self.eat_optional(exactly(Keyword::While)).is_some() {
                self.eat(exactly(Operator::LeftParen))?;
                let cond = self.parse_expression()?;
                self.eat(exactly(Operator::RightParen))?;

                let body = self.parse_statement()?;

                While(cond, box body)
            } else if self.eat_optional(exactly(Keyword::Return)).is_some() {
                let expr = if !self.tastes_like(exactly(Operator::Semicolon)) {
                    Some(self.parse_expression()?)
                } else {
                    None
                };

                self.eat(exactly(Operator::Semicolon))?;

                Return(expr)
            } else if allow_local_var_decl
                && (self.tastes_like(exactly(Keyword::Int))
                    || self.tastes_like(exactly(Keyword::Boolean))
                    || self.tastes_like(exactly(Keyword::Void))
                    || self.tastes_like(Identifier))
                && (self.nth_tastes_like(1, Identifier)
                    || (self.nth_tastes_like(1, exactly(Operator::LeftBracket))
                        && self.nth_tastes_like(2, exactly(Operator::RightBracket))))
            {
                // next (0th) tastes like *BasicType*
                // and next after that (1st) like *Identifier* or 1st+2nd like '[]'
                // Local var decl
                let ty = self.parse_type()?;
                let name = self.eat(Identifier)?;
                let init = if self.eat_optional(exactly(Operator::Equal)).is_some() {
                    Some(self.parse_expression()?)
                } else {
                    None
                };

                self.eat(exactly(Operator::Semicolon))?;

                LocalVariableDeclaration(ty, name, init)
            } else {
                let expr = self.parse_expression()?;
                self.eat(exactly(Operator::Semicolon))?;

                Expression(expr)
            })
        })
    }

    fn parse_expression(&mut self) -> BoxedResult<'f, ast::Expr<'f>> {
        self.parse_binary_expression()
    }

    /// This uses an adapted version of Djikstras original "Shunting Yard"
    /// algorithm [1]. While this traditionally only converts to RPN, it is
    /// combined with an RPN evaluator [2] to build the AST of the
    /// expression on the fly instead. This can also be seen
    /// as a non-recursive variant of precedence climbing [3].
    ///
    /// [1]: https://en.wikipedia.org/wiki/Shunting-yard_algorithm
    /// [2]: https://en.wikipedia.org/wiki/Reverse_Polish_notation#Postfix_evaluation_algorithm
    /// [3]: https://eli.thegreenplace.net/2012/08/02/parsing-expressions-by-precedence-climbing
    fn parse_binary_expression(&mut self) -> BoxedResult<'f, ast::Expr<'f>> {
        fn rpn_eval<'f>(
            operand_stack: &mut Vec<Box<Spanned<'f, ast::Expr<'f>>>>,
            op: ast::BinaryOp,
        ) {
            debug_assert!(operand_stack.len() >= 2); // Invariant: we only construct valid RPN
            let rhs = operand_stack.pop().unwrap();
            let lhs = operand_stack.pop().unwrap();
            let result =
                lhs.combine_boxed_with_boxed(rhs, |lhs, rhs| ast::Expr::Binary(op, lhs, rhs));
            operand_stack.push(box result);
        }
        let mut operator_stack = Vec::new();
        let mut operand_stack = Vec::new();

        operand_stack.push(self.parse_unary_expression()?);

        // Convert to RPN, but "evaluate" RPN on-the-fly (where "evaluate" means
        // constructing an AST)
        while let Some((op, prec, assoc)) = self.eat_optional(BinaryOp).map(|spanned| spanned.data)
        {
            // This is the part that replaces the recursion from precedence climbing.
            // Instead, we use an explicit `operator_stack` of operands that we need to
            // defer because we have one with higher precedence in our hands
            while operator_stack
                .last()
                .map_or(false, |(_, top_prec, top_assoc)| {
                    *top_prec < prec || *top_assoc == Assoc::Left && *top_prec == prec
                })
            {
                let (top_op, _, _) = operator_stack.pop().unwrap();
                rpn_eval(&mut operand_stack, top_op);
            }

            operator_stack.push((op, prec, assoc));
            operand_stack.push(self.parse_unary_expression()?);
        }

        // Consume remaining operators
        for (op, _, _) in operator_stack.drain(..).rev() {
            rpn_eval(&mut operand_stack, op)
        }

        debug_assert_eq!(operand_stack.len(), 1);
        Ok(operand_stack.remove(0))
    }

    fn parse_unary_expression(&mut self) -> BoxedResult<'f, ast::Expr<'f>> {
        let mut ops = Vec::new();
        while let Some(op) = self.eat_optional(UnaryOp) {
            ops.push(op);
        }

        let mut expr = if ops.last().map_or(false, |op| op.data == ast::UnaryOp::Neg) {
            if let Some(lit) = self.eat_optional(IntegerLiteral) {
                // Unwrap is safe, because `.map_or(false, ...`
                let op = ops.pop().unwrap();
                box op.combine_with(lit, |_, lit| ast::Expr::NegInt(lit))
            } else {
                self.parse_postfix_expression()?
            }
        } else {
            self.parse_postfix_expression()?
        };

        for op in ops {
            expr = box op.combine_with_boxed(expr, |op, expr| ast::Expr::Unary(op.data, expr));
        }

        Ok(expr)
    }

    fn parse_postfix_expression(&mut self) -> BoxedResult<'f, ast::Expr<'f>> {
        let mut expr = self.parse_primary_expression()?;

        loop {
            expr = box if self.eat_optional(exactly(Operator::Dot)).is_some() {
                let adressee = self.eat(Identifier)?;

                if self.tastes_like(exactly(Operator::LeftParen)) {
                    // method call: EXPR.ident(arg1, arg2, ...)
                    let args = self.parse_parameter_values()?;

                    expr.combine_boxed_with(args, |expr, args| {
                        ast::Expr::MethodInvocation(expr, adressee, args)
                    })
                } else {
                    // member reference: EXPR.ident
                    expr.combine_boxed_with(adressee, ast::Expr::FieldAccess)
                }
            } else if self.eat_optional(exactly(Operator::LeftBracket)).is_some() {
                // array access: EXPR[EXPR]
                let index_expr = self.parse_expression()?;
                let spanned = self.eat(exactly(Operator::RightBracket))?;

                // Can't use Spanned::combine because the `]` is not included in the span of the
                // index_expr
                Spanned {
                    span: Span::combine(&expr.span, &spanned.span),
                    data: ast::Expr::ArrayAccess(expr, index_expr),
                }
            } else {
                break;
            };
        }

        Ok(expr)
    }

    fn parse_primary_expression(&mut self) -> BoxedResult<'f, ast::Expr<'f>> {
        spanned!(self, {
            use self::ast::Expr::*;

            Ok(if let Some(adressee) = self.eat_optional(Identifier) {
                if self.tastes_like(exactly(Operator::LeftParen)) {
                    // function call
                    let params = self.parse_parameter_values()?;
                    ThisMethodInvocation(adressee, params)
                } else {
                    // var ref
                    Var(adressee)
                }
            } else if self.eat_optional(exactly(Operator::LeftParen)).is_some() {
                // parenthesized expression
                let expr = self.parse_expression()?;
                self.eat(exactly(Operator::RightParen))?;

                // TODO Early return is dirty.. But we need it because we don't want a
                // `ExprKind:Parenthesized` (for abstraction)
                return Ok(expr);
            } else if self.eat_optional(exactly(Keyword::New)).is_some() {
                if self.nth_tastes_like(1, exactly(Operator::LeftParen)) {
                    // new object expression
                    let new_type = self.eat(Identifier)?;

                    self.eat(exactly(Operator::LeftParen))?;
                    self.eat(exactly(Operator::RightParen))?;
                    NewObject(new_type)
                } else {
                    // new array expression
                    let new_type = self.parse_basic_type()?;
                    self.eat(exactly(Operator::LeftBracket))?;
                    let first_index_expr = self.parse_expression()?;
                    self.eat(exactly(Operator::RightBracket))?;

                    let mut array_depth = 0;
                    while self.tastes_like(exactly(Operator::LeftBracket))
                        && self.nth_tastes_like(1, exactly(Operator::RightBracket))
                    {
                        self.eat(exactly(Operator::LeftBracket))?;
                        self.eat(exactly(Operator::RightBracket))?;
                        array_depth += 1;
                    }

                    NewArray(new_type, first_index_expr, array_depth)
                }
            } else if self.eat_optional(exactly(Keyword::Null)).is_some() {
                Null
            } else if self.eat_optional(exactly(Keyword::False)).is_some() {
                Boolean(false)
            } else if self.eat_optional(exactly(Keyword::True)).is_some() {
                Boolean(true)
            } else if self.eat_optional(exactly(Keyword::This)).is_some() {
                This
            } else {
                let lit = self.eat(IntegerLiteral)?;
                Int(lit)
            })
        })
        .map(Box::new)
    }

    fn parse_parameter_values(&mut self) -> ParserResult<'f, ast::ArgumentList<'f>> {
        self.parse_parnethesized_list(|parser| Ok(*parser.parse_expression()?))
    }

    fn parse_parnethesized_list<F, T>(&mut self, parse_element: F) -> ParserResult<'f, Vec<T>>
    where
        F: Fn(&mut Self) -> SyntaxResult<'f, T>,
    {
        spanned!(self, {
            let mut list = Vec::new();
            self.eat(exactly(Operator::LeftParen))?;

            if !self.tastes_like(exactly(Operator::RightParen)) {
                list.push(parse_element(self)?);
                while self.eat_optional(exactly(Operator::Comma)).is_some() {
                    list.push(parse_element(self)?);
                }
            }

            self.eat(exactly(Operator::RightParen))?;

            Ok(list)
        })
    }
}

#[cfg(test)]
#[allow(clippy::print_stdout, clippy::string_lit_as_bytes)]
mod tests {
    use super::*;
    use asciifile::AsciiFile;
    use compiler_shared::context::Context;
    use lexer::Lexer;
    use strtab::StringTable;
    use utils::assert_matches;

    macro_rules! lex_input {
        ($itervar:ident = $input:expr) => {
            let mut strtab = StringTable::new();
            let input = AsciiFile::new($input.as_bytes()).unwrap();
            let ctx = Context::dummy(&input);
            let $itervar = Lexer::new(&mut strtab, &ctx)
                .map(|r| r.unwrap())
                .filter(|t| match t.data {
                    TokenKind::Whitespace | TokenKind::Comment(_) => false,
                    _ => true,
                });
        };
        ($itervar:ident = $input:expr; $context_name:ident = context) => {
            use termcolor::{ColorChoice, StandardStream};
            let mut strtab = StringTable::new();
            let input = AsciiFile::new($input.as_bytes()).unwrap();
            let stderr = StandardStream::stderr(ColorChoice::Auto);
            let $context_name = Context::new(&input, box stderr);
            let $itervar = Lexer::new(&mut strtab, &$context_name)
                .map(|r| r.unwrap())
                .filter(|t| match t.data {
                    TokenKind::Whitespace | TokenKind::Comment(_) => false,
                    _ => true,
                });
        };
    }

    #[test]
    fn iter_test() {}

    #[test]
    fn hello_world() {
        lex_input!(
            lx = r#"
            class Foo {
                public static void main( String[] args) {
                    System.out.println(42);
                }
            }
        "#
        );
        assert_matches!(Parser::new(lx).parse(), Ok(_))
    }

    #[test]
    fn missing_semicolon() {
        lex_input!(
            lx = r#"
            class Foo {
                public static void main( String[] args) {
                    System.out.println(42)
                }
            }
        "#
        );
        assert_matches!(Parser::new(lx).parse(), Err(_))
    }

    #[test]
    fn invalid_basic_type() {
        lex_input!(
            lx = r#"
            class Foo {
                public abstract foo() {
                    System.out.println(42);
                }
            }
        "#
        );
        assert_matches!(Parser::new(lx).parse(), Err(_));
    }

    #[test]
    fn invalid_statement() {
        lex_input!(
            lx = r#"
            class Foo {
                public static void main(String[] args) {
                    []42;
                }
            }
        "#
        );
        assert_matches!(Parser::new(lx).parse(), Err(_));
    }

    #[test]
    fn invalid_expression() {
        lex_input!(
            lx = r#"
            class Foo {
                public static void main(String[] args) {
                    return + 42;
                }
            }
        "#
        );
        assert_matches!(Parser::new(lx).parse(), Err(_));
    }

    #[test]
    fn valid_expression() {
        lex_input!(
            lx = r#"
            class Foo {
                public static void main(String[] args) {
                    return 11 + 42 * 31;
                }
            }
        "#
        );
        assert_matches!(Parser::new(lx).parse(), Ok(_));
    }

    #[test]
    fn assignment_expression() {
        lex_input!(
            lx = r#"
            class Foo {
                public static void main(String[] args) {
                    return x || y = z  ;
                }
            }
        "#
        );
        assert_matches!(Parser::new(lx).parse(), Ok(_))
    }

    #[test]
    fn spanning_whole_program() {
        lex_input!(
            lx = r#"
            class Foo {
                public static void main(String[] args) {
                    return x || y = z  ;
                }
            }
        "#; ctx = context
        );

        let ast = Parser::new(lx).parse();
        println!("{:?}", ast);

        let prog = match ast {
            Ok(ast::AST::Program(prog)) => prog,
            _ => panic!("ast parsing failed!"),
        };

        let start = prog.span.start_position();
        let end = prog.span.end_position();

        // Not part of the assertion, but gives a really easy to understand
        // error message in case the assertion fails. Expected output is:
        //
        //
        // info: span for AST node 'whole program with trimmed whitespace'
        //    |
        //  2 |             class Foo {
        //    |             ^^^^^^^^^^^
        //  3 |                 public static void main(String[] args) {
        //    | ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
        //  4 |                     return x || y = z  ;
        //    | ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
        //  5 |                 }
        //    | ^^^^^^^^^^^^^^^^^
        //  6 |             }
        //    | ^^^^^^^^^^^^^

        ctx.diagnostics.info(&Spanned {
            span: prog.span,
            data: "span for AST node 'whole program with trimmed whitespace'",
        });

        assert_eq!(start.line_number(), 2);
        assert_eq!(start.column(), 12);
        assert_eq!(start.byte_offset(), 13);
        assert_eq!(end.line_number(), 6);
        assert_eq!(end.column(), 12);
        assert_eq!(end.byte_offset(), 153);
    }

    #[test]
    fn else_with_empty_statement() {
        lex_input!(lx = r#"if(angry) {} else;"#);
        let mut p = Parser::new(lx);
        p.parse_statement()
            .map_err(|e| println!("{:?}", e))
            .unwrap();
    }

    mod expr {
        use super::*;
        use crate::ast::{BinaryOp::*, *};

        #[test]
        fn foo() {
            // (3 + (4 * 7)) + ((9 / 7) * 42)
            lex_input!(lx = r#"3 + 4 * 7 + 9 / 7 * 42"#);
            let expr = Parser::new(lx).parse_binary_expression().unwrap().data;

            match expr {
                Expr::Binary(op, lhs, rhs) => {
                    assert_eq!(op, Add);
                    // lhs = 3 + (4 * 7)
                    match lhs.data {
                        Expr::Binary(op, lhs, rhs) => {
                            assert_eq!(op, Add);
                            assert_eq!(
                                lhs.data,
                                Expr::Int(Spanned {
                                    span: lhs.span,
                                    data: "3",
                                })
                            );

                            // rhs = 4 * 7
                            match rhs.data {
                                Expr::Binary(op, lhs, rhs) => {
                                    assert_eq!(op, Mul);
                                    assert_eq!(
                                        lhs.data,
                                        Expr::Int(Spanned {
                                            span: lhs.span,
                                            data: "4",
                                        })
                                    );
                                    assert_eq!(
                                        rhs.data,
                                        Expr::Int(Spanned {
                                            span: rhs.span,
                                            data: "7",
                                        })
                                    );
                                }
                                expr => panic!("not a binary expr: {:#?}", expr),
                            }
                        }
                        expr => panic!("not a binary expr: {:#?}", expr),
                    };
                    // rhs = (9 / 7) * 42
                    match rhs.data {
                        Expr::Binary(op, lhs, rhs) => {
                            assert_eq!(op, Mul);
                            // lhs = 9 / 7
                            match lhs.data {
                                Expr::Binary(op, lhs, rhs) => {
                                    assert_eq!(op, Div);
                                    assert_eq!(
                                        lhs.data,
                                        Expr::Int(Spanned {
                                            span: lhs.span,
                                            data: "9"
                                        })
                                    );
                                    assert_eq!(
                                        rhs.data,
                                        Expr::Int(Spanned {
                                            span: rhs.span,
                                            data: "7",
                                        })
                                    );
                                }
                                expr => panic!("not a binary expr: {:#?}", expr),
                            }
                            assert_eq!(
                                rhs.data,
                                Expr::Int(Spanned {
                                    span: rhs.span,
                                    data: "42",
                                })
                            );
                        }
                        expr => panic!("not a binary expr: {:#?}", expr),
                    }
                }

                expr => panic!("not a binary expr: {:#?}", expr),
            }
        }
    }

    use mjtest::SyntaxTestCase;
    use mjtest_macros::gen_syntax_tests;

    fn do_mjtest_syntax_test(tc: &SyntaxTestCase) {
        use self::SyntaxTestCase::*;
        println!("file name: {:?}", tc.file_name());
        let contents = std::fs::read(tc.path()).unwrap();
        let contents = String::from_utf8(contents).unwrap();
        lex_input!(lx = &contents);
        let res = Parser::new(lx).parse();
        match (tc, res) {
            (Valid(_), Ok(_)) | (Invalid(_), Err(_)) => (),
            (tc, res) => {
                println!("test case: {:?}", tc);
                println!("result:    {:?}", res);
                assert!(false);
            }
        }
    }
    gen_syntax_tests!((
        do_mjtest_syntax_test,
        // releaseonly
        [
            "empty_blocks.java",
            "expression_500parens_left.mj",
            "expression_500parens_right.mj",
            "nested_blocks.java",
            "a_very_long_expression.mj",
            "lots_of_methods.mj",
            "if_chain.java",
        ]
    ));

    mod phase2_tests {
        use super::*;

        #[test]
        fn invalid_main_method() {
            lex_input!(
                lx = r#"
                class Foo {
                    public static void main(int[] args) {
                        System.out.println(42);
                    }
                }
            "#
            );
            assert_matches!(Parser::new(lx).parse(), Err(_))
        }

        #[test]
        fn invalid_new_object_expression() {
            lex_input!(
                lx = r#"
                class Foo {
                    public static void main(String[] args) {
                        int x = new int();
                    }
                }
            "#
            );
            assert_matches!(Parser::new(lx).parse(), Err(_))
        }
    }
}
