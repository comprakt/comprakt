use crate::{
    lexer::{Keyword, Operator, Spanned, Token, TokenKind},
    strtab::Symbol,
};

use std::{fmt, iter::Peekable};

type Precedence = usize;
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Assoc {
    Left,
    Right,
}
const BINARY_OPERATORS: &[(Operator, Precedence, Assoc)] = &[];

#[derive(Debug, Clone)]
pub enum SyntaxError<'f> {
    // TODO Rather panic? If `MissingEOF` is instantiated, we have most certainly a bug
    MissingEOF,
    UnexpectedToken {
        got: Token<'f>,
        expected: String, // TODO This is temporary, shouldn't be string
    },
}

pub trait ExpectedToken: fmt::Debug + fmt::Display {
    type Yields;
    fn matching(&self, token: &TokenKind) -> Option<Self::Yields>;

    fn matches(&self, token: &TokenKind) -> bool {
        self.matching(token).is_some()
    }
}

#[derive(Debug, Clone, Display)]
#[display(fmt = "{}", _0)]
struct Exactly(TokenKind);
#[derive(Debug, Clone, Display)]
#[display(fmt = "a binary operator")]
struct BinaryOp;
#[derive(Debug, Clone, Display)]
#[display(fmt = "a unary operator")]
struct UnaryOp;
#[derive(Debug, Clone, Display)]
#[display(fmt = "an identifier")]
struct Identifier;
#[derive(Debug, Clone, Display)]
#[display(fmt = "an integer literal")]
struct IntegerLiteral;
#[derive(Debug, Clone, Display)]
#[display(fmt = "EOF")]
struct EOF;

impl From<Operator> for Exactly {
    fn from(op: Operator) -> Self {
        Exactly(TokenKind::Operator(op))
    }
}

impl From<Keyword> for Exactly {
    fn from(kw: Keyword) -> Self {
        Exactly(TokenKind::Keyword(kw))
    }
}

impl ExpectedToken for Exactly {
    type Yields = ();
    fn matching(&self, token: &TokenKind) -> Option<Self::Yields> {
        if &self.0 == token {
            Some(())
        } else {
            None
        }
    }
}

impl ExpectedToken for BinaryOp {
    type Yields = (Operator, Precedence, Assoc);
    fn matching(&self, token: &TokenKind) -> Option<Self::Yields> {
        match token {
            // TODO Linear search. meh.
            TokenKind::Operator(op) => BINARY_OPERATORS
                .iter()
                .find(|(this_op, _, _)| this_op == op)
                .map(|elt| *elt),
            _ => None,
        }
    }
}

impl ExpectedToken for UnaryOp {
    type Yields = ();
    fn matching(&self, token: &TokenKind) -> Option<Self::Yields> {
        match token {
            TokenKind::Operator(Operator::Exclaim) | TokenKind::Operator(Operator::Minus) => {
                Some(())
            }
            _ => None,
        }
    }
}

impl ExpectedToken for Identifier {
    type Yields = Symbol;
    fn matching(&self, token: &TokenKind) -> Option<Self::Yields> {
        match token {
            TokenKind::Identifier(ident) => Some(ident.clone()),
            _ => None,
        }
    }
}

impl ExpectedToken for IntegerLiteral {
    type Yields = Symbol;
    fn matching(&self, token: &TokenKind) -> Option<Self::Yields> {
        match token {
            TokenKind::IntegerLiteral(lit) => Some(lit.clone()),
            _ => None,
        }
    }
}

impl ExpectedToken for EOF {
    type Yields = ();
    fn matching(&self, token: &TokenKind) -> Option<Self::Yields> {
        match token {
            TokenKind::EOF => Some(()),
            _ => None,
        }
    }
}

impl fmt::Display for SyntaxError<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use self::SyntaxError::*;

        match self {
            MissingEOF => write!(f, "lexer should yield EOF at end of iterator"),
            UnexpectedToken { got, expected } => {
                write!(f, "unexpected token: got {} expected {}", got, expected)
            }
        }
    }
}

type SyntaxResult<'f, T> = Result<T, SyntaxError<'f>>;
// TODO Ok-value should be AST
type ParserResult<'f> = Result<(), SyntaxError<'f>>;

pub struct Parser<'f, I>
where
    I: Iterator<Item = Token<'f>>,
{
    lexer: Peekable<I>,
    eof_token: Option<Token<'f>>,
}

impl<'f, I> Parser<'f, I>
where
    I: Iterator<Item = Token<'f>>,
{
    pub fn new(lexer: I) -> Self {
        Parser {
            lexer: lexer.peekable(),
            eof_token: None,
        }
    }

    pub fn parse(&mut self) -> ParserResult<'f> {
        self.parse_program()
    }

    /// Hide `lexer.next() == None` as `next() == EOF`
    fn next(&mut self) -> SyntaxResult<'f, Token<'f>> {
        match self.lexer.next() {
            Some(token) => {
                self.eof_token = if token.data == TokenKind::EOF {
                    // Clone is cheap because token data is EOF
                    Some(token.clone())
                } else {
                    None
                };

                Ok(token)
            }
            None => self.eof_token.clone().ok_or(SyntaxError::MissingEOF),
        }
    }

    /// Hide `lexer.peek() == None` as `peek() == EOF`
    // TODO DRY! Nearly same as next()
    fn peek(&mut self) -> SyntaxResult<'f, &Token<'f>> {
        match self.lexer.peek() {
            Some(token) => {
                self.eof_token = if token.data == TokenKind::EOF {
                    // Clone is cheap because token data is EOF
                    Some(token.clone())
                } else {
                    None
                };

                Ok(token)
            }
            None => self.eof_token.as_ref().ok_or(SyntaxError::MissingEOF),
        }
    }

    /// In average compilers this is sometimes called `expect`
    fn omnomnom<E, G>(&mut self, want: G) -> SyntaxResult<'f, Spanned<'f, E::Yields>>
    where
        E: ExpectedToken,
        G: Into<E>,
    {
        let want = want.into();
        let got = self.next()?;

        want.matching(&got.data)
            .map(|yielded| got.map(|_| yielded))
            .ok_or_else(|| SyntaxError::UnexpectedToken {
                got: got,
                expected: want.to_string(),
            })
    }

    /// An average programmer might call this `try_read`, or a similarily
    /// whack-ass name
    fn omnomnoptional<E, G>(&mut self, want: G) -> SyntaxResult<'f, Option<Spanned<'f, E::Yields>>>
    where
        E: ExpectedToken,
        G: Into<E>,
    {
        let want = want.into();
        let got = self.peek()?;

        Ok(want
            .matching(&got.data)
            .map(|yielded| self.next().unwrap().map(|_| yielded)))
    }

    fn tastes_like<E, G>(&mut self, want: G) -> SyntaxResult<'f, bool>
    where
        E: ExpectedToken,
        G: Into<E>,
    {
        self.peek().map(|got| want.into().matches(&got.data))
    }

    fn parse_program(&mut self) -> ParserResult<'f> {
        while self.omnomnoptional(EOF)?.is_none() {
            self.parse_class_declaration()?;
        }

        Ok(())
    }

    fn parse_class_declaration(&mut self) -> ParserResult<'f> {
        self.omnomnom(Keyword::Class)?;
        self.omnomnom(Identifier)?;

        self.omnomnom(Operator::LeftBrace)?;
        while self.omnomnoptional(Operator::RightBrace)?.is_none() {
            self.parse_class_member()?;
        }

        Ok(())
    }

    fn parse_class_member(&mut self) -> ParserResult<'f> {
        self.omnomnom(Keyword::Public)?;

        self.omnomnoptional(Keyword::Static)?;
        self.parse_type()?;
        self.omnomnom(Identifier)?;

        if self.omnomnoptional(Operator::LeftParen)?.is_some() {
            // method or main method

            if !self.tastes_like(Operator::RightParen)? {
                self.parse_parameters()?;
            }

            self.omnomnom(Operator::RightParen)?;

            if self.omnomnoptional(Keyword::Throws)?.is_some() {
                self.omnomnom(Identifier)?;
            }

            self.parse_block()?;
        } else {
            self.omnomnom(Operator::Semicolon)?;
        }

        Ok(())
    }

    fn parse_parameters(&mut self) -> ParserResult<'f> {
        self.parse_parameter()?;
        while self.omnomnoptional(Operator::Comma)?.is_some() {
            self.parse_parameter()?;
        }

        Ok(())
    }

    fn parse_parameter(&mut self) -> ParserResult<'f> {
        self.parse_type()?;
        self.omnomnom(Identifier)?;

        Ok(())
    }

    fn parse_type(&mut self) -> ParserResult<'f> {
        self.parse_basic_type()?;

        while self.omnomnoptional(Operator::LeftBracket)?.is_some() {
            self.omnomnom(Operator::RightBracket)?;
            // Array Type
        }

        Ok(())
    }

    fn parse_basic_type(&mut self) -> ParserResult<'f> {
        if self.omnomnoptional(Keyword::Int)?.is_some()
            || self.omnomnoptional(Keyword::Boolean)?.is_some()
            || self.omnomnoptional(Keyword::Void)?.is_some()
            || self.omnomnoptional(Identifier)?.is_some()
        {
            Ok(())
        } else {
            unimplemented!()
        }
    }

    fn parse_block(&mut self) -> ParserResult<'f> {
        self.omnomnom(Operator::LeftBracket)?;

        while self.omnomnoptional(Operator::RightBracket)?.is_none() {
            self.parse_block_statement()?;
        }

        Ok(())
    }

    fn parse_statement(&mut self) -> ParserResult<'f> {
        self.parse_statement_or_local_var(false)
    }

    fn parse_block_statement(&mut self) -> ParserResult<'f> {
        self.parse_statement_or_local_var(true)
    }

    // Using a bool-flag for *LocalVarDeclStatement* allows us to delay the
    // descision on weather the statement at point is a *LocalVarDeclStatement*
    fn parse_statement_or_local_var(&mut self, allow_local_var_decl: bool) -> ParserResult<'f> {
        if self.tastes_like(Operator::LeftBracket)? {
            self.parse_block()
        } else if self.omnomnoptional(Operator::Semicolon)?.is_some() {
            // empty statement
            Ok(())
        } else if self.omnomnoptional(Keyword::If)?.is_some() {
            self.omnomnom(Operator::LeftParen)?;
            self.parse_expression()?;
            self.omnomnom(Operator::RightParen)?;

            self.parse_statement()
        } else if self.omnomnoptional(Keyword::While)?.is_some() {
            self.omnomnom(Operator::LeftParen)?;
            self.parse_expression()?;
            self.omnomnom(Operator::RightParen)?;

            self.parse_statement()
        } else if self.omnomnoptional(Keyword::Return)?.is_some() {
            if !self.tastes_like(Operator::Semicolon)? {
                self.parse_expression()?;
            }

            self.omnomnom(Operator::Semicolon)?;

            Ok(())
        } else if allow_local_var_decl {
            self.parse_type()?;
            self.omnomnom(Identifier)?;
            if self.omnomnoptional(Operator::Equal)?.is_some() {
                self.parse_expression()?;
            }

            self.omnomnom(Operator::Semicolon)?;

            Ok(())
        } else {
            // TODO Error
            unimplemented!()
        }
    }

    fn parse_expression(&mut self) -> ParserResult<'f> {
        self.parse_binary_expression(0)
    }

    /// Uses precedence climbing
    fn parse_binary_expression(&mut self, min_precedence: usize) -> ParserResult<'f> {
        self.parse_expression()?;

        // tries to read some binary operators
        while let Some((op, mut prec, assoc)) =
            self.omnomnoptional(BinaryOp)?.map(|spanned| spanned.data)
        {
            if prec > min_precedence {
                break;
            }

            if assoc == Assoc::Left {
                prec += 1;
            }

            self.omnomnom(op)?;
            // TODO Not tail recursive
            self.parse_binary_expression(prec)?;
        }

        Ok(())
    }

    fn parse_unary_expr(&mut self) -> ParserResult<'f> {
        // try read prefixOp
        if self.omnomnoptional(UnaryOp)?.is_some() {
            self.parse_unary_expr()
        } else {
            self.parse_postfix_expression()
        }
    }

    fn parse_postfix_expression(&mut self) -> ParserResult<'f> {
        self.parse_primary_expression()?;

        loop {
            if self.omnomnoptional(Operator::Dot)?.is_some() {
                self.omnomnom(Identifier)?;

                if self.omnomnoptional(Operator::LeftParen)?.is_some() {
                    // method call: EXPR.ident(arg1, arg2, ...)
                    self.parse_parenthesized_argument_list()?;
                } else {
                    // member reference: EXPR.ident
                }
            } else if self.omnomnoptional(Operator::LeftBracket)?.is_some() {
                // array access: EXPR[EXPR]
                self.parse_expression()?;
                self.omnomnom(Operator::RightBracket)?;
            } else {
                break;
            }
        }

        Ok(())
    }

    fn parse_primary_expression(&mut self) -> ParserResult<'f> {
        if self.omnomnoptional(Identifier)?.is_some() {
            if self.tastes_like(Operator::LeftParen)? {
                // function call
                self.parse_parenthesized_argument_list()
            } else {
                // var ref
                Ok(())
            }
        } else if self.omnomnoptional(Operator::LeftParen)?.is_some() {
            // parenthesized expression
            self.parse_expression()?;
            self.omnomnom(Operator::RightParen)?;

            Ok(())
        } else if self.omnomnoptional(Keyword::New)?.is_some() {
            self.parse_basic_type()?;

            if self.omnomnoptional(Operator::LeftParen)?.is_some() {
                // new object expression
                self.omnomnom(Operator::RightParen)?;
            } else {
                // new array expression
                self.omnomnom(Operator::LeftBrace)?;
                self.parse_expression()?;
                self.omnomnom(Operator::RightBrace)?;

                while self.omnomnoptional(Operator::LeftBrace)?.is_some() {
                    self.omnomnom(Operator::RightBrace)?;
                }
            }

            Ok(())
        } else if self.omnomnoptional(Keyword::Null)?.is_some()
            || self.omnomnoptional(Keyword::False)?.is_some()
            || self.omnomnoptional(Keyword::True)?.is_some()
            || self.omnomnoptional(Keyword::This)?.is_some()
            || self.omnomnoptional(IntegerLiteral)?.is_some()
        {
            Ok(())
        } else {
            unimplemented!()
        }
    }

    fn parse_parenthesized_argument_list(&mut self) -> ParserResult<'f> {
        self.omnomnom(Operator::LeftParen)?;
        while self.omnomnoptional(Operator::RightParen)?.is_none() {
            self.parse_expression()?;
        }
        self.omnomnom(Operator::RightParen)?;

        Ok(())
    }
}
