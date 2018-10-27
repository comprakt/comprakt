use crate::{
    asciifile::{Span, StrFrame},
    strtab::*,
};
use failure::Fail;
use std::{convert::TryFrom, fmt, result::Result};

macro_rules! match_op {
    ($current_frame:expr, { $( $token_string:expr => $token:expr ),+, _ => $default:expr }) => {{
        match () {
            $(
                () if $current_frame.extend_so_that_eq($token_string).is_ok() => {
                    let (span, _) = $current_frame.finish();

                    Some(Ok(Token::new(span, TokenKind::Operator($token))))
                }
            )+
            _ => $default,
        }
    }};
}

pub type TokenResult<'s> = Result<Token<'s>, LexicalError>;

pub type Token<'s> = Spanned<TokenKind<'s>>;
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
    fn new(span: Span, value: T) -> Self {
        Spanned { span, data: value }
    }
}

impl Fail for LexicalError {}

#[derive(Debug)]
pub enum TokenKind<'s> {
    Keyword(Keyword),
    Operator(Operator),
    Identifier(Symbol<'s>),
    IntegerLiteral(Symbol<'s>),
    Comment(String),
    Whitespace,
    EOF,
}

impl<'s> fmt::Display for TokenKind<'s> {
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

pub struct Lexer<'t, 's> {
    current_frame: StrFrame<'s>,
    strtab: &'t StringTable,
    eof: bool,
}

fn is_minijava_whitespace(c: char) -> bool {
    match c {
        ' ' | '\n' | '\r' | '\t' => true,
        _ => false,
    }
}

impl<'t, 's> Lexer<'t, 's> {
    pub fn new(input: StrFrame<'s>, strtab: &'t StringTable) -> Self {
        Lexer {
            current_frame: input,
            strtab,
            eof: false,
        }
    }

    fn lex_token(&mut self) -> Option<TokenResult<'s>> {
        println!("{:?}", self.current_frame);
        assert!(self.current_frame.is_empty());

        match self.current_frame.extend_by(1) {
            Ok(s) => {
                assert_eq!(s.len(), 1);
                Some(match s.chars().next().unwrap() {
                    'a'..='z' | 'A'..='Z' | '_' => self.lex_identifier_or_keyword(),

                    '1'..='9' => self.lex_integer_literal(),

                    '0' => {
                        let (span, c) = self.current_frame.finish();
                        Ok(Token::new(
                            span,
                            TokenKind::IntegerLiteral(self.strtab.intern(c)),
                        ))
                    }

                    c if is_minijava_whitespace(c) => self.lex_whitespace(),

                    _ if self.current_frame.extend_so_that_eq("/*").is_ok() => self.lex_comment(),

                    _ => self.lex_operator().unwrap_or_else(|| {
                        let (span, s) = self.current_frame.finish();
                        assert_eq!(s.len(), 1);
                        let c = s.chars().next().unwrap();

                        Err(LexicalError::new(span, ErrorKind::UnexpectedCharacter(c)))
                    }),
                })
            }

            Err(err) => {
                assert!(err.eof_reached());

                if self.eof {
                    None
                } else {
                    self.eof = true; // Next time, return None

                    self.current_frame.extend_to_eof();
                    let (pos, remains) = self.current_frame.finish();
                    assert!(remains.is_empty()); // We already were at EOF

                    Some(Ok(Token::new(pos, TokenKind::EOF)))
                }
            }
        }
    }

    fn lex_identifier_or_keyword(&mut self) -> TokenResult<'s> {
        assert_matches!(
            self.current_frame.first(),
            Some('a'..='z') | Some('A'..='Z') | Some('_')
        );

        self.lex_while(
            |c| matches!(c, 'a'..='z' | 'A'..='Z' | '0'..='9' | '_'),
            |ident, strtab, _| {
                Ok(match Keyword::try_from(ident) {
                    Ok(keyword) => TokenKind::Keyword(keyword),
                    Err(_) => TokenKind::Identifier(strtab.intern(&ident)),
                })
            },
        )
    }

    fn lex_integer_literal(&mut self) -> TokenResult<'s> {
        assert_matches!(self.current_frame.first(), Some('1'..='9'));

        self.lex_while(
            |c| matches!(c, '0'..='9'),
            |lit, strtab, _| Ok(TokenKind::IntegerLiteral(strtab.intern(&lit))),
        )
    }

    fn lex_comment(&mut self) -> TokenResult<'s> {
        let res = self.current_frame.trim_head(2); // Don't need "/*"
        assert_eq!(res, Ok("/*"));

        let token = self.lex_while_multiple(
            2, // Lookahead
            |s| s != "*/",
            |text, _, eof_reached| {
                if eof_reached {
                    Err(ErrorKind::UnclosedComment(text.to_string()))
                } else {
                    Ok(TokenKind::Comment(text.to_string()))
                }
            },
        );

        assert!(self.current_frame.is_empty());
        if token.is_ok() {
            // At least 2 chars left in input, which we don't care about
            let res = self.current_frame.extend_by(2);
            assert_eq!(res, Ok("*/"));
            let (_, text) = self.current_frame.finish();
            assert_eq!(text, "*/");
        }

        token
    }

    fn lex_whitespace(&mut self) -> TokenResult<'s> {
        assert!(is_minijava_whitespace(self.current_frame.first().unwrap()));
        self.lex_while(is_minijava_whitespace, |_, _, _| Ok(TokenKind::Whitespace))
    }

    #[allow(clippy::cyclomatic_complexity)]
    fn lex_operator(&mut self) -> Option<TokenResult<'s>> {
        use self::Operator::*;

        // It's important that these are sorted by length (decreasingly)
        match_op!(self.current_frame, {
            // Length 4
            ">>>=" => TripleRightChevronEqual,
            // Length 3
            "<<=" => DoubleLeftChevronEqual,
            ">>=" => DoubleRightChevronEqual,
            ">>>" => TripleRightChevron,
            // Length 2
            "!=" => ExclaimEqual,
            "*=" => StarEqual,
            "++" => DoublePlus,
            "+=" => PlusEqual,
            "-=" => MinusEqual,
            "--" => DoubleMinus,
            "/=" => SlashEqual,
            "<<" => DoubleLeftChevron,
            "<=" => LeftChevronEqual,
            "==" => DoubleEqual,
            ">=" => RightChevronEqual,
            ">>" => DoubleRightChevron,
            "%=" => PercentEqual,
            "&=" => AmpersandEqual,
            "&&" => DoubleAmpersand,
            "^=" => CaretEqual,
            "|=" => PipeEqual,
            "||" => DoublePipe,
            "!" => Exclaim,
            // Length 1
            "(" => LeftParen,
            ")" => RightParen,
            "*" => Star,
            "+" => Plus,
            "," => Comma,
            "-" => Minus,
            "." => Dot,
            "/" => Slash,
            ":" => Colon,
            ";" => Semicolon,
            "<" => LeftChevron,
            "=" => Equal,
            ">" => RightChevron,
            "?" => QuestionMark,
            "%" => Percent,
            "&" => Ampersand,
            "[" => LeftBracket,
            "]" => RightBracket,
            "^" => Caret,
            "{" => LeftBrace,
            "}" => RightBrace,
            "~" => Tilde,
            "|" => Pipe,
            _ => None
        })
    }

    /// Like `lex_while_multiple`, but only check characters
    fn lex_while<P, D>(&mut self, predicate: P, make_token: D) -> TokenResult<'s>
    where
        P: Fn(char) -> bool,
        D: FnOnce(&'s str, &'t StringTable, bool) -> Result<TokenKind<'s>, ErrorKind>,
    {
        // Unwrap is safe, because EOF case is handled by lex_while_multiple
        self.lex_while_multiple(
            1,
            |s| {
                assert_eq!(s.len(), 1);
                predicate(s.chars().next().unwrap())
            },
            make_token,
        )
    }

    /// Consume n characters at a time while `predicate` returns `true`. Intern
    /// the resulting string and convert the resulting symbol to a token
    /// using `make_token`. `preddicate` is never given a less than `n`
    /// chars. In that case, the loop is terminated and `make_token` is
    /// called with 3rd argument set to `true`.
    fn lex_while_multiple<P, D>(&mut self, n: usize, predicate: P, make_token: D) -> TokenResult<'s>
    where
        P: Fn(&str) -> bool,
        D: FnOnce(&'s str, &'t StringTable, bool) -> Result<TokenKind<'s>, ErrorKind>,
    {
        let res = self.current_frame.extend_to(n).and_then(|_| {
            self.current_frame.extend_while(n, |candidate| {
                candidate
                    .get((candidate.len() - n)..)
                    .map_or(false, &predicate)
            })
        });

        let (span, chars) = self.current_frame.finish();
        let eof_reached = res.map_or_else(|err| err.eof_reached(), |_| false);

        make_token(chars, self.strtab, eof_reached)
            .map(|kind| Token::new(span, kind))
            .map_err(|kind| LexicalError::new(span, kind))
    }
}

impl<'t, 's> Iterator for Lexer<'t, 's> {
    type Item = TokenResult<'s>;

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
