use crate::{
    asciifile::{Position, PositionIterator, Span, Spanned},
    context::Context,
    diagnostics::u8_to_printable_representation,
    strtab::*,
};
use failure::Fail;
use std::{convert::TryFrom, fmt, result::Result};

macro_rules! match_op {
    ($input:expr, $( ($token_string:expr, $token:expr) ),+: $len:expr, $default:expr) => {{
        // unwrap is safe because we have already peeked ahead, ensuring EOF is not the case
        let span = $input.peek_at_most($len).unwrap();
        match span.as_str() {
            $(
                $token_string => match_op!($input, span, $len, $token),
            )+
            _ => $default,
        }
    }};
    ($input:expr, $span:ident, $len:expr, $right:expr) => {{
        // Unwraps are safe, because this is only called after token of length $len,
        // is already matched and thus contained in $input
        debug_assert!($len >= 1);
        for _ in 0..$len { $input.next().unwrap(); }
        Some(Ok(Token::new($span, TokenKind::Operator($right))))
    }};
}

pub type TokenResult<'f> = Result<Token<'f>, LexicalError<'f>>;

pub type Token<'f> = Spanned<'f, TokenKind<'f>>;
pub type LexicalError<'f> = Spanned<'f, ErrorKind>;

pub type IntLit<'f> = &'f str;

#[derive(Debug, PartialEq, Eq, Clone, Copy, Hash, PartialOrd, Ord, Display)]
/// Keywords are single-ticked, operators back-ticked
pub enum TokenKind<'f> {
    #[display(fmt = "'{}'", _0)]
    Keyword(Keyword),
    #[display(fmt = "`{}`", _0)]
    Operator(Operator),
    #[display(fmt = "identifier `{}`", _0)]
    Identifier(Symbol<'f>),
    #[display(fmt = "integer literal `{}`", _0)]
    IntegerLiteral(IntLit<'f>),
    #[display(fmt = "a comment")]
    Comment(&'f str),
    #[display(fmt = "whitespace")]
    Whitespace,
}

#[derive(Debug, Fail)]
pub enum ErrorKind {
    UnclosedComment,
    UnexpectedCharacter(u8),
}

impl fmt::Display for ErrorKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match *self {
            ErrorKind::UnclosedComment => write!(f, "unclosed comment"),
            ErrorKind::UnexpectedCharacter(byte) => fmt_unexpected_character(f, byte),
        }
    }
}

fn fmt_unexpected_character(f: &mut fmt::Formatter<'_>, byte: u8) -> fmt::Result {
    match byte as char {
        '\n' => write!(f, "Unexpected newline"),
        '\\' => write!(f, "Unexpected backslash"),
        '\'' => write!(f, "Unexpected single quote"),
        '"' => write!(f, "Unexpected double quote"),
        chr if chr.is_whitespace() => write!(
            f,
            "Unexpected whitespace '{}'",
            u8_to_printable_representation(byte)
        ),
        chr if chr.is_control() => write!(
            f,
            "Unexpected control character '{}'",
            u8_to_printable_representation(byte)
        ),
        _ => write!(
            f,
            "Unexpected character '{}'",
            u8_to_printable_representation(byte)
        ),
    }
}

#[derive(Debug, Fail)]
pub enum Warning {
    #[fail(display = "confusing usage of comment separator inside a comment")]
    CommentSeparatorInsideComment,
}

#[derive(Debug, PartialEq, Eq, Clone, Hash, PartialOrd, Ord, Copy)]
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
#[derive(Debug, PartialEq, Eq, Clone, Hash, PartialOrd, Ord, Copy)]
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

pub struct Lexer<'f, 's> {
    input: PositionIterator<'f>,
    strtab: &'s mut StringTable<'f>,
    context: &'f Context<'f>,
}

/// Test if the given characters are whitespace characters according
/// to the MiniJava specification
fn is_minijava_whitespace(c: char) -> bool {
    match c {
        ' ' | '\n' | '\r' | '\t' => true,
        _ => false,
    }
}

impl<'f, 's> Lexer<'f, 's> {
    pub fn new(strtab: &'s mut StringTable<'f>, context: &'f Context<'f>) -> Self {
        let input = context.file.iter();

        Self {
            context,
            strtab,
            input,
        }
    }

    fn lex_token(&mut self) -> Option<TokenResult<'f>> {
        match self.input.peek() {
            Some(position) => Some(match position.chr() {
                'a'..='z' | 'A'..='Z' | '_' => self.lex_identifier_or_keyword(),
                '1'..='9' => self.lex_integer_literal(),
                '0' => self.lex_zero_integer_literal(),
                c if is_minijava_whitespace(c) => self.lex_whitespace(),
                '/' if self.input.matches("/*") => self.lex_comment(),

                _ => self.lex_operator().unwrap_or_else(|| {
                    Err(LexicalError::new(
                        position.to_single_char_span(),
                        ErrorKind::UnexpectedCharacter(position.byte()),
                    ))
                }),
            }),

            None => None,
        }
    }

    fn lex_zero_integer_literal(&mut self) -> TokenResult<'f> {
        let position = self.input.next().unwrap();
        Ok(Token::new(
            position.to_single_char_span(),
            TokenKind::IntegerLiteral("0"),
        ))
    }

    fn lex_identifier_or_keyword(&mut self) -> TokenResult<'f> {
        assert_matches!(self.input.peek().unwrap().chr(), 'a'..='z' | 'A'..='Z' | '_');

        let span = self.lex_while(
            |position, _| matches!(position.chr(), 'a'..='z' | 'A'..='Z' | '0'..='9' | '_'),
        );

        let kind = match Keyword::try_from(span.as_str()) {
            Ok(keyword) => TokenKind::Keyword(keyword),
            Err(_) => TokenKind::Identifier(self.strtab.intern(span.as_str())),
        };

        Ok(Token::new(span, kind))
    }

    fn lex_integer_literal(&mut self) -> TokenResult<'f> {
        assert_matches!(self.input.peek().unwrap().chr(), '1'..='9');

        let span = self.lex_while(|position, _| matches!(position.chr(), '0'..='9'));

        let kind = TokenKind::IntegerLiteral(span.as_str());

        Ok(Token::new(span, kind))
    }

    fn lex_comment(&mut self) -> TokenResult<'f> {
        debug_assert!(self.input.matches("/*"));

        let comment_start = self.input.next().unwrap();
        self.input.next().unwrap();

        let comment_body = self.lex_while_multiple(2, |span, context| {
            if span.as_str() == "/*" {
                context.diagnostics.warning(&Spanned {
                    span: span.clone(),
                    data: Warning::CommentSeparatorInsideComment,
                });
            }
            span.as_str() != "*/"
        });

        if self.input.eof_reached() {
            let span = comment_body.extend_to_position(&comment_start);
            Err(LexicalError::new(span, ErrorKind::UnclosedComment))
        } else {
            debug_assert_eq!(self.input.peek_exactly(2).unwrap().as_str(), "*/");
            self.input.next().unwrap();
            let comment_end = self.input.next().unwrap();

            let span = Span::new(comment_start, comment_end);

            Ok(Token::new(span, TokenKind::Comment(comment_body.as_str())))
        }
    }

    fn lex_whitespace(&mut self) -> TokenResult<'f> {
        debug_assert!(is_minijava_whitespace(self.input.peek().unwrap().chr()));
        let span = self.lex_while(|position, _| is_minijava_whitespace(position.chr()));
        Ok(Token::new(span, TokenKind::Whitespace))
    }

    #[allow(clippy::cyclomatic_complexity)]
    fn lex_operator(&mut self) -> Option<TokenResult<'f>> {
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

    /// Helper method that collects characters from input until a user specified
    /// predicte is no longer true.
    fn lex_while<P>(&mut self, predicate: P) -> Span<'f>
    where
        P: Fn(Position<'f>, &'f Context<'f>) -> bool,
    {
        self.lex_while_multiple(1, |span, context| predicate(span.start_position(), context))
    }

    /// This consumes the input one character at a time until the user supplied
    /// predicate is no longer true. The predicate is allowed a lookahead of
    /// up to `n` characters. The returned range might be shorter iff the end
    /// of the file is being reached.
    ///
    /// Like the other methods starting with `lex_*` in this struct, the code
    /// assumes that the next character is guaranteed to exist and is
    /// consumed by the caller.
    // TODO: This function would best be replaced by some iterator method on
    // Position or Span. In its current form its already very close to a
    // `input.by_ref().take_while(|pos| predicate(pos.iter().peek_at_most(n)))`.
    //
    // NOTE, regarding the previous version of this code using peek_exactly(n):
    // this only works by chance. The up to n-1 characters unwound on EOF
    // are not verifiable by the caller and are just glued onto the output?!
    // It just works because
    // - the only direct caller is lex_comment, which can consume anything during an
    //   unexpected EOF.
    // - all indirect callers with n=1 skip the while loop consuming the remaining
    //   input because next() returns None during the first call.
    //
    // Resulting TODO: Given that there is only one real caller. The abstraction
    // should probably just be removed.
    fn lex_while_multiple<P>(&mut self, n: usize, predicate: P) -> Span<'f>
    where
        P: Fn(Span<'f>, &'f Context<'f>) -> bool,
    {
        // start the span with the next character only and extend
        // it on each loop iteration. This assumes that the character
        // is going to be consumed.
        let mut consumed = self.input.peek().unwrap().to_single_char_span();

        while let Some(peeked) = self.input.peek_at_most(n) {
            if !predicate(peeked, &self.context) {
                break;
            }

            // Unwrap is safe, because the call is guarded by a `Some(.) = peek_at_most(.)`
            // which would be None if there is no remaining input.
            let position = self.input.next().unwrap();
            consumed = consumed.extend_to_position(&position);
        }

        consumed
    }
}

impl<'f, 's> Iterator for Lexer<'f, 's> {
    type Item = TokenResult<'f>;

    fn next(&mut self) -> Option<Self::Item> {
        self.lex_token()
    }
}

#[cfg(test)]
mod tests {

    use super::is_minijava_whitespace;
    use crate::{
        lexer::{Keyword, Operator, TokenKind},
        print::lextest,
        strtab::StringTable,
    };
    use failure::Error;
    use std::io;

    // TODO: duplicated across compilercli and compilerlib
    fn write_token<O: io::Write>(out: &mut O, token: &TokenKind<'_>) -> Result<(), Error> {
        match token {
            TokenKind::Whitespace | TokenKind::Comment(_) => Ok(()),
            _ => {
                writeln!(out, "{}", lextest::Output::new(&token))?;
                Ok(())
            }
        }
    }

    fn lexer_test_with_tokens(tokens: Vec<TokenKind<'_>>) -> String {
        let mut o = Vec::new();
        for token in tokens.into_iter() {
            let res = write_token(&mut o, &token);
            assert!(res.is_ok());
        }

        String::from_utf8(o).expect("output must be utf8")
    }

    #[test]
    fn minijava_whitespace() {
        let chars = "\x07\x08\x0c\x0b"; // \a \b \f \v
        for c in chars.chars() {
            println!("{:?}", c);
            assert_eq!(is_minijava_whitespace(c), false)
        }
    }

    #[test]
    fn newline_per_token() {
        let tokens = vec![
            TokenKind::Operator(Operator::Ampersand),
            TokenKind::Keyword(Keyword::Int),
        ];
        let tokens_len = tokens.len();
        let o = lexer_test_with_tokens(tokens);
        assert_eq!(o.lines().count(), tokens_len);
    }

    #[test]
    fn no_whitespace_and_comments() {
        let tokens = vec![
            TokenKind::Operator(Operator::Ampersand),
            TokenKind::Whitespace,
            TokenKind::IntegerLiteral("foo"),
            TokenKind::Comment("comment"),
            TokenKind::Keyword(Keyword::If),
        ];
        let o = lexer_test_with_tokens(tokens);
        assert_eq!(o.lines().count(), 3);
        assert!(!o.contains("comment"));
        assert_eq!(&o, "&\ninteger literal foo\nif\n")
    }

    #[test]
    fn keywords_as_is() {
        let tokens = vec![TokenKind::Keyword(Keyword::Float)];
        let o = lexer_test_with_tokens(tokens);
        assert_eq!(&o, "float\n");
    }

    #[test]
    fn operators_as_is() {
        let o = lexer_test_with_tokens(vec![TokenKind::Operator(Operator::Caret)]);
        assert_eq!(&o, "^\n");
    }

    #[test]
    fn ident_prefix() {
        let mut st = StringTable::new();
        let o = lexer_test_with_tokens(vec![TokenKind::Identifier(st.intern("an_identifier"))]);
        assert_eq!(&o, "identifier an_identifier\n");
    }

    #[test]
    fn integer_literal_prefix() {
        let o = lexer_test_with_tokens(vec![TokenKind::IntegerLiteral("2342")]);
        assert_eq!(&o, "integer literal 2342\n");
    }
}
