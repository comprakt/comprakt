use crate::{
    asciifile::{Position, PositionedChar, PositionedChars},
    strtab::*,
};
use std::{convert::TryFrom, fmt, result::Result};

macro_rules! match_op {
    ($input:expr, $( ($token_string:expr, $token:expr) ),+: $len:expr, $default:expr) => {{
        match $input.peek_multiple($len) {
            $(
                $token_string => match_op!($input, $len, $token),
            )+
            _ => $default,
        }
    }};
    ($input:expr, $len:expr, $right:expr) => {{
        // Unwraps are safe, because this is only called after token of length $len,
        // is already matched and thus contained in $input
        let PositionedChar(begin, _) = $input.next().unwrap();
        let mut end = begin;
        #[allow(clippy::reverse_range_loop)] // Macro might be called with with $len=1
        for _ in 1..$len {
            let PositionedChar(pos, _) = $input.next().unwrap();
            end = pos;
        }

        Some(Token::new(begin, end, TokenKind::Operator($right)))
    }};
}

#[derive(Debug)]
pub enum TokenKind<'t> {
    Keyword(Keyword),
    Operator(Operator),
    Identifier(Symbol<'t>),
    IntegerLiteral(Symbol<'t>),
    Comment(String),
    UnclosedComment(String),
    UnexpectedCharacter(char),
    Whitespace,
    EOF,
}

impl<'t> fmt::Display for TokenKind<'t> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use self::TokenKind::*;

        match self {
            Keyword(keyword) => write!(f, "{}", keyword),
            Operator(operator) => write!(f, "{}", operator),
            Identifier(symbol) => write!(f, "identifier {}", symbol),
            IntegerLiteral(symbol) => write!(f, "integer literal {}", symbol),
            Comment(body) => write!(f, "/*{}*/", body),
            UnclosedComment(body) => write!(f, "//{}", body),
            UnexpectedCharacter(_chr) => write!(f, "[UNEXPECTED CHAR]"),
            Whitespace => write!(f, " "),
            EOF => write!(f, "EOF"),
        }
    }
}

#[derive(Debug)]
pub enum Keyword {
    Abstract,
    Assert,
    Boolean,
    Break,
    Byte,
    Case,
    Catch,
    Char,
    Class,
    Const,
    Continue,
    Default,
    Double,
    Do,
    Else,
    Enum,
    Extends,
    False,
    Finally,
    Final,
    Float,
    For,
    Goto,
    If,
    Implements,
    Import,
    InstanceOf,
    Interface,
    Int,
    Long,
    Native,
    New,
    Null,
    Package,
    Private,
    Protected,
    Public,
    Return,
    Short,
    Static,
    StrictFp,
    Super,
    Switch,
    Synchronized,
    This,
    Throws,
    Throw,
    Transient,
    True,
    Try,
    Void,
    Volatile,
    While,
}

impl fmt::Display for Keyword {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use self::Keyword::*;
        write!(
            f,
            "{}",
            match self {
                Abstract => "abstract",
                Assert => "assert",
                Boolean => "boolean",
                Break => "break",
                Byte => "byte",
                Case => "case",
                Catch => "catch",
                Char => "char",
                Class => "class",
                Const => "const",
                Continue => "continue",
                Default => "default",
                Double => "double",
                Do => "do",
                Else => "else",
                Enum => "enum",
                Extends => "extends",
                False => "false",
                Finally => "finally",
                Final => "final",
                Float => "float",
                For => "for",
                Goto => "goto",
                If => "if",
                Implements => "implements",
                Import => "import",
                InstanceOf => "instanceof",
                Interface => "interface",
                Int => "int",
                Long => "long",
                Native => "native",
                New => "new",
                Null => "null",
                Package => "package",
                Private => "private",
                Protected => "protected",
                Public => "public",
                Return => "return",
                Short => "short",
                Static => "static",
                StrictFp => "strictfp",
                Super => "super",
                Switch => "switch",
                Synchronized => "synchronized",
                This => "this",
                Throws => "throws",
                Throw => "throw",
                Transient => "transient",
                True => "true",
                Try => "try",
                Void => "void",
                Volatile => "volatile",
                While => "while",
            }
        )
    }
}

impl TryFrom<&str> for Keyword {
    type Error = ();

    fn try_from(s: &str) -> Result<Keyword, Self::Error> {
        use self::Keyword::*;

        match s {
            "abstract" => Ok(Abstract),
            "assert" => Ok(Assert),
            "boolean" => Ok(Boolean),
            "break" => Ok(Break),
            "byte" => Ok(Byte),
            "case" => Ok(Case),
            "catch" => Ok(Catch),
            "char" => Ok(Char),
            "class" => Ok(Class),
            "const" => Ok(Const),
            "continue" => Ok(Continue),
            "default" => Ok(Default),
            "double" => Ok(Double),
            "do" => Ok(Do),
            "else" => Ok(Else),
            "enum" => Ok(Enum),
            "extends" => Ok(Extends),
            "false" => Ok(False),
            "finally" => Ok(Finally),
            "final" => Ok(Final),
            "float" => Ok(Float),
            "for" => Ok(For),
            "goto" => Ok(Goto),
            "if" => Ok(If),
            "implements" => Ok(Implements),
            "import" => Ok(Import),
            "instanceof" => Ok(InstanceOf),
            "interface" => Ok(Interface),
            "int" => Ok(Int),
            "long" => Ok(Long),
            "native" => Ok(Native),
            "new" => Ok(New),
            "null" => Ok(Null),
            "package" => Ok(Package),
            "private" => Ok(Private),
            "protected" => Ok(Protected),
            "public" => Ok(Public),
            "return" => Ok(Return),
            "short" => Ok(Short),
            "static" => Ok(Static),
            "strictfp" => Ok(StrictFp),
            "super" => Ok(Super),
            "switch" => Ok(Switch),
            "synchronized" => Ok(Synchronized),
            "this" => Ok(This),
            "throws" => Ok(Throws),
            "throw" => Ok(Throw),
            "transient" => Ok(Transient),
            "true" => Ok(True),
            "try" => Ok(Try),
            "void" => Ok(Void),
            "volatile" => Ok(Volatile),
            "while" => Ok(While),
            _ => Err(()),
        }
    }
}

// Use non-semantic names, since e.g. '<' might mean more than 'less-than'
#[derive(Debug)]
pub enum Operator {
    ExclaimEqual,
    Exclaim,
    LeftParen,
    RightParen,
    StarEqual,
    Star,
    DoublePlus,
    PlusEqual,
    Plus,
    Comma,
    MinusEqual,
    DoubleMinus,
    Minus,
    Dot,
    SlashEqual,
    Slash,
    Colon,
    Semicolon,
    DoubleLeftChevronEqual,
    DoubleLeftChevron,
    LeftChevronEqual,
    LeftChevron,
    DoubleEqual,
    Equal,
    RightChevronEqual,
    DoubleRightChevronEqual,
    TripleRightChevronEqual,
    TripleRightChevron,
    DoubleRightChevron,
    RightChevron,
    QuestionMark,
    PercentEqual,
    Percent,
    AmpersandEqual,
    DoubleAmpersand,
    Ampersand,
    LeftBracket,
    RightBracket,
    CaretEqual,
    Caret,
    LeftBrace,
    RightBrace,
    Tilde,
    PipeEqual,
    DoublePipe,
    Pipe,
}

impl fmt::Display for Operator {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use self::Operator::*;
        write!(
            f,
            "{}",
            match self {
                TripleRightChevronEqual => ">>>=",
                DoubleLeftChevronEqual => "<<=",
                DoubleRightChevronEqual => ">>=",
                TripleRightChevron => ">>>",
                ExclaimEqual => "!=",
                StarEqual => "*=",
                DoublePlus => "++",
                PlusEqual => "+=",
                MinusEqual => "-=",
                DoubleMinus => "--",
                SlashEqual => "/=",
                DoubleLeftChevron => "<<",
                LeftChevronEqual => "<=",
                DoubleEqual => "==",
                RightChevronEqual => ">=",
                DoubleRightChevron => ">>",
                PercentEqual => "%=",
                AmpersandEqual => "&=",
                DoubleAmpersand => "&&",
                CaretEqual => "^=",
                PipeEqual => "|=",
                DoublePipe => "||",
                Exclaim => "!",
                LeftParen => "(",
                RightParen => ")",
                Star => "*",
                Plus => "+",
                Comma => ",",
                Minus => "-",
                Dot => ".",
                Slash => "/",
                Colon => ":",
                Semicolon => ";",
                LeftChevron => "<",
                Equal => "=",
                RightChevron => ">",
                QuestionMark => "?",
                Percent => "%",
                Ampersand => "&",
                LeftBracket => "[",
                RightBracket => "]",
                Caret => "^",
                LeftBrace => "{",
                RightBrace => "}",
                Tilde => "~",
                Pipe => "|",
            }
        )
    }
}

#[derive(Debug)]
pub struct Span {
    pub start: Position,
    pub end: Position,
}

#[derive(Debug)]
pub struct Token<'t> {
    pub span: Span,
    pub data: TokenKind<'t>,
}

impl<'t> Token<'t> {
    fn new(start: Position, end: Position, value: TokenKind<'t>) -> Self {
        Token {
            span: Span { start, end },
            data: value,
        }
    }
}

pub struct Lexer<'t, I>
where
    I: Iterator<Item = char>,
{
    input: PositionedChars<I>,
    strtab: &'t StringTable,
    eof: bool,
}

impl<'t, I> Lexer<'t, I>
where
    I: Iterator<Item = char>,
{
    pub fn new(input: PositionedChars<I>, strtab: &'t StringTable) -> Self {
        Lexer {
            input,
            strtab,
            eof: false,
        }
    }

    fn lex_token(&mut self) -> Token<'t> {
        match self.input.peek() {
            Some('a'..='z') | Some('A'..='Z') | Some('_') => self.lex_identifier_or_keyword(),

            Some('0'..='9') => self.lex_integer_literal(),

            Some(c) if c.is_whitespace() => self.lex_whitespace(),

            Some(_) if self.input.peek_multiple(2) == "/*" => self.lex_comment(),

            Some(_) => self.lex_operator().unwrap_or_else(|| {
                let PositionedChar(pos, c) = self.input.next().unwrap();
                Token::new(pos, pos, TokenKind::UnexpectedCharacter(c))
            }),

            None => {
                let pos = self.input.eof_position();
                Token::new(pos, pos, TokenKind::EOF)
            }
        }
    }

    fn lex_identifier_or_keyword(&mut self) -> Token<'t> {
        assert_matches!(
            self.input.peek(),
            Some('a'..='z') | Some('A'..='Z') | Some('_')
        );

        self.lex_while(
            |c| matches!(c, 'a'..='z' | 'A'..='Z' | '0'..='9'),
            |ident, strtab, _| match Keyword::try_from(ident.as_ref()) {
                Ok(keyword) => TokenKind::Keyword(keyword),
                Err(_) => TokenKind::Identifier(strtab.intern(ident)),
            },
        )
    }

    fn lex_integer_literal(&mut self) -> Token<'t> {
        assert_matches!(self.input.peek(), Some('0'..='9'));

        self.lex_while(
            |c| matches!(c, '0'..='9'),
            |lit, strtab, _| TokenKind::IntegerLiteral(strtab.intern(lit)),
        )
    }

    fn lex_comment(&mut self) -> Token<'t> {
        assert_eq!(self.input.peek_multiple(2), "/*");

        self.input.next();
        self.input.next();

        let token = self.lex_while_multiple(
            2,
            |s| s != "*/",
            |text, _, eof_reached| {
                if eof_reached {
                    TokenKind::UnclosedComment(text)
                } else {
                    TokenKind::Comment(text)
                }
            },
        );

        assert_eq!(self.input.peek_multiple(2), "*/");

        self.input.next();
        self.input.next();

        token
    }

    fn lex_whitespace(&mut self) -> Token<'t> {
        assert!(self.input.peek().unwrap().is_whitespace());

        self.lex_while(|c| c.is_whitespace(), |_, _, _| TokenKind::Whitespace)
    }

    #[allow(clippy::cyclomatic_complexity)]
    fn lex_operator(&mut self) -> Option<Token<'t>> {
        use self::Operator::*;

        match_op!(
            self.input,
            (">>>=", TripleRightChevronEqual):
            4,
            match_op!(
                self.input,
                ("<<=", DoubleLeftChevronEqual),
                (">>=", DoubleRightChevronEqual),
                (">>>", TripleRightChevron):
                3,
                match_op!(
                    self.input,
                    ("!=", ExclaimEqual),
                    ("*=", StarEqual),
                    ("++", DoublePlus),
                    ("+=", PlusEqual),
                    ("-=", MinusEqual),
                    ("--", DoubleMinus),
                    ("/=", SlashEqual),
                    ("<<", DoubleLeftChevron),
                    ("<=", LeftChevronEqual),
                    ("==", DoubleEqual),
                    (">=", RightChevronEqual),
                    (">>", DoubleRightChevron),
                    ("%=", PercentEqual),
                    ("&=", AmpersandEqual),
                    ("&&", DoubleAmpersand),
                    ("^=", CaretEqual),
                    ("|=", PipeEqual),
                    ("||", DoublePipe):
                    2,
                    match_op!(
                        self.input,
                        ("!", Exclaim),
                        ("(", LeftParen),
                        (")", RightParen),
                        ("*", Star),
                        ("+", Plus),
                        (",", Comma),
                        ("-", Minus),
                        (".", Dot),
                        ("/", Slash),
                        (":", Colon),
                        (";", Semicolon),
                        ("<", LeftChevron),
                        ("=", Equal),
                        (">", RightChevron),
                        ("?", QuestionMark),
                        ("%", Percent),
                        ("&", Ampersand),
                        ("[", LeftBracket),
                        ("]", RightBracket),
                        ("^", Caret),
                        ("{", LeftBrace),
                        ("}", RightBrace),
                        ("~", Tilde),
                        ("|", Pipe):
                        1,
                        None
                    )
                )
            )
        )
    }

    /// Like `lex_while_multiple`, but only check characters
    fn lex_while<P, D>(&mut self, predicate: P, make_token_data: D) -> Token<'t>
    where
        P: Fn(char) -> bool,
        D: FnOnce(String, &'t StringTable, bool) -> TokenKind<'t>,
    {
        // Unwrap is safe, because EOF case is handled by lex_while_multiple
        self.lex_while_multiple(1, |s| predicate(s.chars().next().unwrap()), make_token_data)
    }

    /// Consume n characters at a time while `predicate` returns `true`. Intern
    /// the resulting string and convert the resulting symbol to a token
    /// using `make_token_data`. `preddicate` is never given a less than `n`
    /// chars. In that case, the loop is terminated and `make_token_data` is
    /// called with 3rd argument set to `true`.
    fn lex_while_multiple<P, D>(&mut self, n: usize, predicate: P, make_token_data: D) -> Token<'t>
    where
        P: Fn(&str) -> bool,
        D: FnOnce(String, &'t StringTable, bool) -> TokenKind<'t>,
    {
        let mut chars = String::new();
        let PositionedChar(start_pos, first_char) = self.input.next().unwrap();
        chars.push(first_char);

        let mut end_pos = start_pos;
        while self.input.try_peek_multiple(n).map_or(false, &predicate) {
            // Unwrap is safe, because `map_or` catches EOF case
            let PositionedChar(pos, c) = self.input.next().unwrap();
            chars.push(c);
            end_pos = pos;
        }

        Token::new(
            start_pos,
            end_pos,
            make_token_data(chars, self.strtab, self.input.eof_reached()),
        )
    }
}

impl<'t, I> Iterator for Lexer<'t, I>
where
    I: Iterator<Item = char>,
{
    type Item = Token<'t>;

    fn next(&mut self) -> Option<Self::Item> {
        if self.eof {
            None
        } else {
            let t = self.lex_token();
            if let TokenKind::EOF = t.data {
                self.eof = true
            }
            Some(t) // TODO Some(EOF) vs. None
        }
    }
}
