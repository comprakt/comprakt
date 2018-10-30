use crate::{
    asciifile::{AsciiFileIterator, Position, PositionedChar},
    context::Context,
    strtab::*,
};
use failure::Fail;
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
        debug_assert!($len >= 1);
        let mut it = $input.by_ref().take($len);
        let PositionedChar(begin, _) = it.next().unwrap();
        let end = it.last().map(|pc| pc.0).unwrap_or(begin);
        Some(Ok(Token::new(begin, end, TokenKind::Operator($right))))
    }};
}

pub type TokenResult = Result<Token, LexicalError>;

pub type Token = Spanned<TokenKind>;
pub type LexicalError = Spanned<ErrorKind>;

#[derive(Debug)]
pub struct Spanned<T> {
    pub span: Span,
    pub data: T,
}

impl<T> fmt::Display for Spanned<T>
where
    T: fmt::Display,
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{} at {}", self.data, self.span)
    }
}

impl<T> Spanned<T> {
    fn new(start: Position, end: Position, value: T) -> Self {
        Spanned {
            span: Span { start, end },
            data: value,
        }
    }
}

impl Fail for LexicalError {}

#[derive(Debug)]
pub enum TokenKind {
    Keyword(Keyword),
    Operator(Operator),
    Identifier(Symbol),
    IntegerLiteral(Symbol),
    Comment(String),
    Whitespace,
    EOF,
}

impl fmt::Display for TokenKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use self::TokenKind::*;

        match self {
            Keyword(keyword) => write!(f, "{}", keyword),
            Operator(operator) => write!(f, "{}", operator),
            Identifier(symbol) => write!(f, "identifier {}", symbol),
            IntegerLiteral(symbol) => write!(f, "integer literal {}", symbol),
            Comment(body) => write!(f, "/*{}*/", body),
            Whitespace => write!(f, " "),
            EOF => write!(f, "EOF"),
        }
    }
}

#[derive(Debug, Fail)]
pub enum ErrorKind {
    #[fail(display = "unclosed comment '{}'", 0)]
    UnclosedComment(String),
    #[fail(display = "unexpected character '{}'", 0)]
    UnexpectedCharacter(char),
}

#[derive(Debug, Fail)]
pub enum Warning {
    #[fail(display = "confusing usage of comment separator inside a comment")]
    CommentSeparatorInsideComment,
}

#[derive(Debug)]
pub struct Span {
    pub start: Position,
    pub end: Position,
}

impl Span {
    pub fn is_single_char(&self) -> bool {
        if self.start.row != self.end.row {
            return false;
        }
        // ignore inconsisent end before start
        self.end
            .col
            .checked_sub(self.start.col)
            .map(|d| d <= 1)
            .unwrap_or(false)
    }

    /// Check if a span extends over multiple lines
    ///
    /// This will consider spans that contain a single trailing
    /// whitespace, e.g. "a\n" as multiline.
    pub fn is_multiline(&self) -> bool {
        self.start.row != self.end.row
    }
}

impl fmt::Display for Span {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if self.is_single_char() {
            write!(f, "{}", self.start)
        } else {
            write!(f, "{}-{}", self.start, self.end)
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

pub struct Lexer<'t> {
    input: AsciiFileIterator<'t>,
    context: &'t Context<'t>,
    eof: bool,
}

fn is_minijava_whitespace(c: char) -> bool {
    match c {
        ' ' | '\n' | '\r' | '\t' => true,
        _ => false,
    }
}

impl<'t> Lexer<'t> {
    pub fn new(context: &'t Context<'t>) -> Self {
        let input = context.file.iter();

        Self {
            context,
            input,
            eof: false,
        }
    }

    fn lex_token(&mut self) -> Option<TokenResult> {
        Some(match self.input.peek() {
            Some('a'..='z') | Some('A'..='Z') | Some('_') => self.lex_identifier_or_keyword(),

            Some('1'..='9') => self.lex_integer_literal(),

            Some('0') => {
                let PositionedChar(pos, character) = self.input.next().unwrap();
                let mut buf = [0; 1];
                // won't panic because we know character is '0', hence 1 byte
                let as_str = character.encode_utf8(&mut buf);
                Ok(Token::new(
                    pos,
                    pos,
                    TokenKind::IntegerLiteral(self.context.strtab.intern(as_str)),
                ))
            }

            Some(c) if is_minijava_whitespace(c) => self.lex_whitespace(),

            Some(_) if self.input.peek_multiple(2) == "/*" => self.lex_comment(),

            Some(_) => self.lex_operator().unwrap_or_else(|| {
                let PositionedChar(pos, c) = self.input.next().unwrap();
                Err(LexicalError::new(
                    pos,
                    pos,
                    ErrorKind::UnexpectedCharacter(c),
                ))
            }),

            None if self.eof => return None, // Early return to not wrap in surrounding `Some`
            None => {
                self.eof = true;
                let pos = self.input.current_position();
                Ok(Token::new(pos, pos, TokenKind::EOF))
            }
        })
    }

    fn lex_identifier_or_keyword(&mut self) -> TokenResult {
        assert_matches!(
            self.input.peek(),
            Some('a'..='z') | Some('A'..='Z') | Some('_')
        );

        self.lex_while(
            |c, _, _| matches!(c, 'a'..='z' | 'A'..='Z' | '0'..='9' | '_'),
            |ident, strtab, _| {
                Ok(match Keyword::try_from(ident.as_ref()) {
                    Ok(keyword) => TokenKind::Keyword(keyword),
                    Err(_) => TokenKind::Identifier(strtab.intern(&ident)),
                })
            },
        )
    }

    fn lex_integer_literal(&mut self) -> TokenResult {
        assert_matches!(self.input.peek(), Some('1'..='9'));

        self.lex_while(
            |c, _, _| matches!(c, '0'..='9'),
            |lit, strtab, _| Ok(TokenKind::IntegerLiteral(strtab.intern(&lit))),
        )
    }

    fn lex_comment(&mut self) -> TokenResult {
        debug_assert_eq!(self.input.peek_multiple(2), "/*");

        self.input.next();
        self.input.next();

        let token = self.lex_while_multiple(
            2,
            |s, span, context| {
                if s == "/*" {
                    context.warning(Spanned {
                        span,
                        data: box Warning::CommentSeparatorInsideComment,
                    });
                }
                s != "*/"
            },
            |text, _, eof_reached| {
                if eof_reached {
                    Err(ErrorKind::UnclosedComment(text))
                } else {
                    Ok(TokenKind::Comment(text))
                }
            },
        );

        if token.is_ok() {
            // At least 2 chars left in input
            debug_assert_eq!(self.input.peek_multiple(2), "*/");
        }

        self.input.next();
        self.input.next();

        token
    }

    fn lex_whitespace(&mut self) -> TokenResult {
        debug_assert!(is_minijava_whitespace(self.input.peek().unwrap()));
        self.lex_while(
            |c, _, _| is_minijava_whitespace(c),
            |_, _, _| Ok(TokenKind::Whitespace),
        )
    }

    #[allow(clippy::cyclomatic_complexity)]
    fn lex_operator(&mut self) -> Option<TokenResult> {
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
    fn lex_while<P, D>(&mut self, predicate: P, make_token: D) -> TokenResult
    where
        P: Fn(char, Span, &'t Context<'t>) -> bool,
        D: FnOnce(String, &'t StringTable, bool) -> Result<TokenKind, ErrorKind>,
    {
        // Unwrap is safe, because EOF case is handled by lex_while_multiple
        self.lex_while_multiple(
            1,
            |s, span, context| predicate(s.chars().next().unwrap(), span, context),
            make_token,
        )
    }

    /// Consume n characters at a time while `predicate` returns `true`. Intern
    /// the resulting string and convert the resulting symbol to a token
    /// using `make_token`. `preddicate` is never given a less than `n`
    /// chars. In that case, the loop is terminated and `make_token` is
    /// called with 3rd argument set to `true`.
    fn lex_while_multiple<P, D>(&mut self, n: usize, predicate: P, make_token: D) -> TokenResult
    where
        P: Fn(&str, Span, &'t Context<'t>) -> bool,
        D: FnOnce(String, &'t StringTable, bool) -> Result<TokenKind, ErrorKind>,
    {
        let mut chars = String::new();
        let start_pos = self.input.current_position();
        let mut end_pos = start_pos;
        loop {
            if let Some(peeked) = self.input.try_peek_multiple(n) {
                // TODO: for error reporting, work around peek() not returning a
                // Span/Position!!! But peek() actually contains logic to
                // surpress the position, so changing the signature of peek to
                // return a Span or Position might be the correct decision!!!
                let span = Span {
                    start: end_pos.consume(&peeked[0..1]),
                    end: end_pos.consume(peeked),
                };

                if !predicate(peeked, span, &self.context) {
                    break;
                }
            } else {
                // We know there is an EOF within the next n characters, but we still need to
                // consume them
                while let Some(PositionedChar(pos, c)) = self.input.next() {
                    chars.push(c);
                    end_pos = pos;
                }
                break;
            }

            // TODO: map_or is nowhere mentioned in the file!!!
            // Unwrap is safe, because `map_or` catches EOF case
            let PositionedChar(pos, c) = self.input.next().unwrap();
            chars.push(c);
            end_pos = pos;
        }

        make_token(chars, &self.context.strtab, self.input.eof_reached())
            .map(|kind| Token::new(start_pos, end_pos, kind))
            .map_err(|kind| LexicalError::new(start_pos, end_pos, kind))
    }
}

impl<'t> Iterator for Lexer<'t> {
    type Item = TokenResult;

    fn next(&mut self) -> Option<Self::Item> {
        self.lex_token()
    }
}

#[cfg(test)]
mod tests {
    #[test]
    fn minijava_whitespace_invalid_chars_rejected() {
        let chars = "\x07\x08\x0c\x0b"; // \a \b \f \v
        for c in chars.chars() {
            println!("{:?}", c);
            assert_eq!(super::is_minijava_whitespace(c), false)
        }
    }
}
