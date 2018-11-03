//TODO remove after handling of else
#![allow(clippy::if_same_then_else)]

use crate::{
    ast::*,
    lexer::{Keyword, Operator, Spanned, Token, TokenKind},
    strtab::Symbol,
    utils::MultiPeekable,
};

use std::fmt;

type Precedence = usize;
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Assoc {
    Left,
    #[allow(dead_code)]
    Right,
}
#[rustfmt::skip]
const BINARY_OPERATORS: &[(Operator, Precedence, Assoc)] = &[
    (Operator::Star,              1, Assoc::Left),
    (Operator::Slash,             1, Assoc::Left),
    (Operator::Percent,           1, Assoc::Left),

    (Operator::Plus,              2, Assoc::Left),
    (Operator::Minus,             2, Assoc::Left),

    (Operator::LeftChevron,       3, Assoc::Left),
    (Operator::LeftChevronEqual,  3, Assoc::Left),
    (Operator::RightChevron,      3, Assoc::Left),
    (Operator::RightChevronEqual, 3, Assoc::Left),

    (Operator::DoubleEqual,       4, Assoc::Left),
    (Operator::ExclaimEqual,      4, Assoc::Left),

    (Operator::DoubleAmpersand,   5, Assoc::Left),

    (Operator::DoublePipe,        6, Assoc::Left),
];

#[derive(Debug, Clone)]
pub enum SyntaxError<'f> {
    // TODO Rather panic? If `MissingEOF` is instantiated, we have most certainly a bug
    MissingEOF,
    UnexpectedToken {
        got: Token<'f>,
        expected: String, // TODO This is temporary, shouldn't be string
    },

    InvalidMainMethod,
    InvalidNewObjectExpression,
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
                .cloned(),
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

            InvalidNewObjectExpression => write!(f, "invalid new object expression"),
            InvalidMainMethod => write!(f, "invalid main method"),
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
    lexer: MultiPeekable<I>,
    eof_token: Option<Token<'f>>,
}

impl<'f, I> Parser<'f, I>
where
    I: Iterator<Item = Token<'f>>,
{
    pub fn new(lexer: I) -> Self {
        Parser {
            lexer: MultiPeekable::new(lexer),
            eof_token: None,
        }
    }

    pub fn parse(&mut self) -> ParserResult<'f> {
        self.parse_program()
    }

    /// Hide `lexer.next() == None` as `next() == EOF`
    fn next(&mut self) -> SyntaxResult<'f, Token<'f>> {
        self.peek()?;
        match self.lexer.next() {
            Some(token) => Ok(token),
            None => self.eof_token.clone().ok_or(SyntaxError::MissingEOF),
        }
    }

    /// Hide `lexer.peek() == None` as `peek() == EOF`
    fn peek(&mut self) -> SyntaxResult<'f, &Token<'f>> {
        self.peek_nth(0)
    }

    fn peek_nth(&mut self, n: usize) -> SyntaxResult<'f, &Token<'f>> {
        let v = self.lexer.peek_multiple(n + 1);

        for token in v.iter() {
            self.eof_token = if token.data == TokenKind::EOF {
                // Clone is cheap because token data is EOF
                Some((*token).clone())
            } else {
                None
            };
        }

        match v.get(n) {
            Some(token) => Ok(token),
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
                got,
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
        self.omnomnoptional_if(want, |_| true)
    }

    /// Only consume token if pred(E::Yields) holds
    fn omnomnoptional_if<E, G, P>(
        &mut self,
        want: G,
        pred: P,
    ) -> SyntaxResult<'f, Option<Spanned<'f, E::Yields>>>
    where
        E: ExpectedToken,
        G: Into<E>,
        P: Fn(&E::Yields) -> bool,
    {
        let want = want.into();
        let got = self.peek()?;

        Ok(want.matching(&got.data).filter(&pred).map(|yielded| {
            let got = self.next().unwrap();
            got.map(|_| yielded)
        }))
    }

    fn tastes_like<E, G>(&mut self, want: G) -> SyntaxResult<'f, bool>
    where
        E: ExpectedToken,
        G: Into<E>,
    {
        self.nth_tastes_like(0, want)
    }

    fn nth_tastes_like<E, G>(&mut self, n: usize, want: G) -> SyntaxResult<'f, bool>
    where
        E: ExpectedToken,
        G: Into<E>,
    {
        self.peek_nth(n).map(|got| want.into().matches(&got.data))
    }

    fn parse_program(&mut self) -> ParserResult<'f> {
        while self.omnomnoptional::<EOF, _>(EOF)?.is_none() {
            self.parse_class_declaration()?;
        }

        Ok(())
    }

    fn parse_class_declaration(&mut self) -> ParserResult<'f> {
        self.omnomnom::<Exactly, _>(Keyword::Class)?;
        self.omnomnom::<Identifier, _>(Identifier)?;

        self.omnomnom::<Exactly, _>(Operator::LeftBrace)?;
        while self
            .omnomnoptional::<Exactly, _>(Operator::RightBrace)?
            .is_none()
        {
            self.parse_class_member()?;
        }

        Ok(())
    }

    fn parse_class_member(&mut self) -> ParserResult<'f> {
        self.omnomnom::<Exactly, _>(Keyword::Public)?;

        let is_static = self
            .omnomnoptional::<Exactly, _>(Keyword::Static)?
            .is_some();
        let return_type = self.parse_type()?;
        self.omnomnom::<Identifier, _>(Identifier)?;

        if self
            .omnomnoptional::<Exactly, _>(Operator::LeftParen)?
            .is_some()
        {
            // method or main method

            let params = if !self.tastes_like::<Exactly, _>(Operator::RightParen)? {
                self.parse_parameters()?
            } else {
                ParameterList::new()
            };

            self.omnomnom::<Exactly, _>(Operator::RightParen)?;

            if self
                .omnomnoptional::<Exactly, _>(Keyword::Throws)?
                .is_some()
            {
                self.omnomnom::<Identifier, _>(Identifier)?;
            }

            // Check that "static" => Type=void && ParameterList==(String[] IDENT)
            // TODO Should be handled during semantical analysis
            if is_static
                && (return_type != Type::Basic(BasicType::Void)
                    || params.len() != 1
                    || params[0].var_type
                        != Type::ArrayOf(box Type::Basic(BasicType::Ident(Symbol::from("String")))))
            {
                return Err(SyntaxError::InvalidMainMethod);
            }

            self.parse_block()?;
        } else {
            self.omnomnom::<Exactly, _>(Operator::Semicolon)?;
        }

        Ok(())
    }

    fn parse_parameters(&mut self) -> SyntaxResult<'f, ParameterList> {
        let mut param_list = ParameterList::new();

        param_list.push(self.parse_parameter()?);
        while self
            .omnomnoptional::<Exactly, _>(Operator::Comma)?
            .is_some()
        {
            param_list.push(self.parse_parameter()?);
        }

        Ok(param_list)
    }

    fn parse_parameter(&mut self) -> SyntaxResult<'f, VariableDecl> {
        let var_type = self.parse_type()?;
        let name = self.omnomnom::<Identifier, _>(Identifier)?;

        Ok(VariableDecl {
            var_type,
            name: name.data,
        })
    }

    fn parse_type(&mut self) -> SyntaxResult<'f, Type> {
        let mut parsed_type = Type::Basic(self.parse_basic_type()?);

        while self
            .omnomnoptional::<Exactly, _>(Operator::LeftBracket)?
            .is_some()
        {
            self.omnomnom::<Exactly, _>(Operator::RightBracket)?;
            parsed_type = Type::ArrayOf(box parsed_type);
        }

        Ok(parsed_type)
    }

    fn parse_basic_type(&mut self) -> SyntaxResult<'f, BasicType> {
        if self.omnomnoptional::<Exactly, _>(Keyword::Int)?.is_some() {
            Ok(BasicType::Int)
        } else if self
            .omnomnoptional::<Exactly, _>(Keyword::Boolean)?
            .is_some()
        {
            Ok(BasicType::Bool)
        } else if self.omnomnoptional::<Exactly, _>(Keyword::Void)?.is_some() {
            Ok(BasicType::Void)
        } else if let Some(sym) = self.omnomnoptional::<Identifier, _>(Identifier)? {
            Ok(BasicType::Ident(sym.data))
        } else {
            Err(SyntaxError::UnexpectedToken {
                got: self.next()?,
                expected: "keywod int, boolean, void or an identifier".to_string(),
            })
        }
    }

    fn parse_block(&mut self) -> ParserResult<'f> {
        self.omnomnom::<Exactly, _>(Operator::LeftBrace)?;

        while self
            .omnomnoptional::<Exactly, _>(Operator::RightBrace)?
            .is_none()
        {
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
        if self.tastes_like::<Exactly, _>(Operator::LeftBrace)? {
            self.parse_block()
        } else if self
            .omnomnoptional::<Exactly, _>(Operator::Semicolon)?
            .is_some()
        {
            // empty statement
            Ok(())
        } else if self.omnomnoptional::<Exactly, _>(Keyword::If)?.is_some() {
            self.omnomnom::<Exactly, _>(Operator::LeftParen)?;
            self.parse_expression()?;
            self.omnomnom::<Exactly, _>(Operator::RightParen)?;

            self.parse_statement()?;
            if let Ok(Some(_)) = self.omnomnoptional::<Exactly, _>(Keyword::Else) {
                self.parse_statement()?;
            }

            Ok(())
        } else if self.omnomnoptional::<Exactly, _>(Keyword::While)?.is_some() {
            self.omnomnom::<Exactly, _>(Operator::LeftParen)?;
            self.parse_expression()?;
            self.omnomnom::<Exactly, _>(Operator::RightParen)?;

            self.parse_statement()
        } else if self
            .omnomnoptional::<Exactly, _>(Keyword::Return)?
            .is_some()
        {
            if !self.tastes_like::<Exactly, _>(Operator::Semicolon)? {
                self.parse_expression()?;
            }

            self.omnomnom::<Exactly, _>(Operator::Semicolon)?;

            Ok(())
        } else if allow_local_var_decl {
            // next (0th) tastes like *BasicType*
            // and next after that (1st) like *Identifier* or 1st+2nd like '[]'
            if (self.tastes_like::<Exactly, _>(Keyword::Int)?
                || self.tastes_like::<Exactly, _>(Keyword::Boolean)?
                || self.tastes_like::<Exactly, _>(Keyword::Void)?
                || self.tastes_like::<Identifier, _>(Identifier)?)
                && (self.nth_tastes_like::<Identifier, _>(1, Identifier)?
                    || (self.nth_tastes_like::<Exactly, _>(1, Operator::LeftBracket)?
                        && self.nth_tastes_like::<Exactly, _>(2, Operator::RightBracket)?))
            {
                // Local var decl
                self.parse_type()?;
                self.omnomnom::<Identifier, _>(Identifier)?;
                if self
                    .omnomnoptional::<Exactly, _>(Operator::Equal)?
                    .is_some()
                {
                    self.parse_expression()?;
                }

                self.omnomnom::<Exactly, _>(Operator::Semicolon)?;
            } else {
                self.parse_expression_statement()?;
            }

            Ok(())
        } else {
            self.parse_expression_statement()
        }
    }

    fn parse_expression_statement(&mut self) -> ParserResult<'f> {
        self.parse_expression()?;
        self.omnomnom::<Exactly, _>(Operator::Semicolon)?;

        Ok(())
    }

    fn parse_expression(&mut self) -> ParserResult<'f> {
        self.parse_binary_expression(0)?;

        // Assignment expression
        while self
            .omnomnoptional::<Exactly, _>(Operator::Equal)?
            .is_some()
        {
            self.parse_binary_expression(0)?;
        }

        Ok(())
    }

    /// Uses precedence climbing
    fn parse_binary_expression(&mut self, min_precedence: usize) -> ParserResult<'f> {
        self.parse_unary_expression()?;

        // tries to read some binary operators
        while let Some((_, mut prec, assoc)) = self
            .omnomnoptional_if::<BinaryOp, _, _>(BinaryOp, |(_, prec, _)| *prec >= min_precedence)?
            .map(|spanned| spanned.data)
        {
            if assoc == Assoc::Left {
                prec += 1;
            }

            self.parse_binary_expression(prec)?;
        }

        Ok(())
    }

    fn parse_unary_expression(&mut self) -> ParserResult<'f> {
        // try read prefixOp
        if self.omnomnoptional::<UnaryOp, _>(UnaryOp)?.is_some() {
            self.parse_unary_expression()
        } else {
            self.parse_postfix_expression()
        }
    }

    fn parse_postfix_expression(&mut self) -> ParserResult<'f> {
        self.parse_primary_expression()?;

        loop {
            if self.omnomnoptional::<Exactly, _>(Operator::Dot)?.is_some() {
                self.omnomnom::<Identifier, _>(Identifier)?;

                if self.tastes_like::<Exactly, _>(Operator::LeftParen)? {
                    // method call: EXPR.ident(arg1, arg2, ...)
                    self.parse_parenthesized_argument_list()?;
                } else {
                    // member reference: EXPR.ident
                }
            } else if self
                .omnomnoptional::<Exactly, _>(Operator::LeftBracket)?
                .is_some()
            {
                // array access: EXPR[EXPR]
                self.parse_expression()?;
                self.omnomnom::<Exactly, _>(Operator::RightBracket)?;
            } else {
                break;
            }
        }

        Ok(())
    }

    fn parse_primary_expression(&mut self) -> ParserResult<'f> {
        if self.omnomnoptional::<Identifier, _>(Identifier)?.is_some() {
            if self.tastes_like::<Exactly, _>(Operator::LeftParen)? {
                // function call
                self.parse_parenthesized_argument_list()
            } else {
                // var ref
                Ok(())
            }
        } else if self
            .omnomnoptional::<Exactly, _>(Operator::LeftParen)?
            .is_some()
        {
            // parenthesized expression
            self.parse_expression()?;
            self.omnomnom::<Exactly, _>(Operator::RightParen)?;

            Ok(())
        } else if self.omnomnoptional::<Exactly, _>(Keyword::New)?.is_some() {
            let new_type = self.parse_basic_type()?;

            if self
                .omnomnoptional::<Exactly, _>(Operator::LeftParen)?
                .is_some()
            {
                // new object expression
                self.omnomnom::<Exactly, _>(Operator::RightParen)?;

                // TODO should be handled during semantical analysis
                if matches!(new_type, BasicType::Void | BasicType::Int | BasicType::Bool) {
                    return Err(SyntaxError::InvalidNewObjectExpression);
                }
            } else {
                // new array expression
                self.omnomnom::<Exactly, _>(Operator::LeftBracket)?;
                self.parse_expression()?;
                self.omnomnom::<Exactly, _>(Operator::RightBracket)?;

                while self
                    .omnomnoptional::<Exactly, _>(Operator::LeftBracket)?
                    .is_some()
                {
                    self.omnomnom::<Exactly, _>(Operator::RightBracket)?;
                }
            }

            Ok(())
        } else if self.omnomnoptional::<Exactly, _>(Keyword::Null)?.is_some()
            || self.omnomnoptional::<Exactly, _>(Keyword::False)?.is_some()
            || self.omnomnoptional::<Exactly, _>(Keyword::True)?.is_some()
            || self.omnomnoptional::<Exactly, _>(Keyword::This)?.is_some()
            || self
                .omnomnoptional::<IntegerLiteral, _>(IntegerLiteral)?
                .is_some()
        {
            Ok(())
        } else {
            Err(SyntaxError::UnexpectedToken {
                got: self.next()?,
                expected: "primary expression".to_string(),
            })
        }
    }

    fn parse_parenthesized_argument_list(&mut self) -> ParserResult<'f> {
        self.omnomnom::<Exactly, _>(Operator::LeftParen)?;

        if !self.tastes_like::<Exactly, _>(Operator::RightParen)? {
            self.parse_expression()?;
            while self
                .omnomnoptional::<Exactly, _>(Operator::Comma)?
                .is_some()
            {
                self.parse_expression()?;
            }
        }

        self.omnomnom::<Exactly, _>(Operator::RightParen)?;

        Ok(())
    }
}

#[cfg(test)]
#[allow(clippy::string_lit_as_bytes)]
mod tests {
    use super::*;
    use crate::{asciifile::AsciiFile, context::Context, lexer::Lexer, strtab::StringTable};

    macro_rules! lex_input {
        ($itervar:ident = $input:expr) => {
            let strtab = StringTable::new();
            let input = AsciiFile::new($input.as_bytes()).unwrap();
            let ctx = Context::dummy(&input);
            let $itervar = Lexer::new(&strtab, &ctx)
                .map(|r| r.unwrap())
                .filter(|t| match t {
                    Spanned {
                        data: TokenKind::Whitespace,
                        ..
                    }
                    | Spanned {
                        data: TokenKind::Comment(_),
                        ..
                    } => false,
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
    fn else_with_empty_statement() {
        lex_input!(lx = r#"if(angry) {} else;"#);
        let mut p = Parser::new(lx);
        p.parse_statement().map_err(|e| println!("{}", e)).unwrap();
    }

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
            assert_matches!(Parser::new(lx).parse(), Err(SyntaxError::InvalidMainMethod))
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
            assert_matches!(
                Parser::new(lx).parse(),
                Err(SyntaxError::InvalidNewObjectExpression)
            )
        }
    }
}
