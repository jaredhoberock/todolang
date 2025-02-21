use crate::source_location::SourceSpan;
use derive_more::Display;

#[derive(Debug, Clone, PartialEq)]
pub enum TokenKind {
    // Keywords
    And,
    Assert,
    Class,
    Else,
    False,
    For,
    Fun,
    If,
    Match,
    Nil,
    Or,
    Print,
    Return,
    Super,
    This,
    True,
    Var,
    While,

    // Single-character tokens
    Bang,
    Colon,
    Comma,
    Dot,
    Equal,
    Greater,
    LeftBrace,
    LeftParen,
    Less,
    Minus,
    Plus,
    RightBrace,
    RightParen,
    Semicolon,
    Slash,
    Star,
    Underscore,

    // Two-character tokens
    Arrow,
    BangEqual,
    EqualEqual,
    EqualGreater,
    GreaterEqual,
    LessEqual,

    // Literals
    Identifier,
    String,
    Number,

    // Special
    Eof,
    Error,
}

macro_rules! define_TokenKind_from_keyword_lexeme {
    ($($variant:ident => $str:expr),* $(,)?) => {
        impl TokenKind {
            pub fn from_keyword_lexeme(lexeme: &str) -> Option<TokenKind> {
                match lexeme {
                    $($str => Some(TokenKind::$variant),)*
                    _ => None
                }
            }
        }
    }
}

define_TokenKind_from_keyword_lexeme! {
    And => "and",
    Assert => "assert",
    Class => "class",
    Else => "else",
    False => "false",
    For => "for",
    Fun => "fun",
    If => "if",
    Match => "match",
    Nil => "nil",
    Or => "or",
    Print => "print",
    Return => "return",
    Super => "super",
    This => "this",
    True => "true",
    Var => "var",
    While => "while",
}

impl std::fmt::Display for TokenKind {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            TokenKind::And => write!(f, "and"),
            TokenKind::Arrow => write!(f, "->"),
            TokenKind::Assert => write!(f, "assert"),
            TokenKind::Bang => write!(f, "!"),
            TokenKind::BangEqual => write!(f, "!="),
            TokenKind::Class => write!(f, "class"),
            TokenKind::Colon => write!(f, ":"),
            TokenKind::Comma => write!(f, ","),
            TokenKind::Dot => write!(f, "."),
            TokenKind::Else => write!(f, "else"),
            TokenKind::Eof => write!(f, "eof"),
            TokenKind::Error => write!(f, "error"),
            TokenKind::Equal => write!(f, "="),
            TokenKind::EqualEqual => write!(f, "=="),
            TokenKind::EqualGreater => write!(f, "=>"),
            TokenKind::False => write!(f, "false"),
            TokenKind::For => write!(f, "for"),
            TokenKind::Fun => write!(f, "fun"),
            TokenKind::Greater => write!(f, ">"),
            TokenKind::GreaterEqual => write!(f, ">="),
            TokenKind::Identifier => write!(f, "identifier"),
            TokenKind::If => write!(f, "if"),
            TokenKind::LeftBrace => write!(f, "{{"),
            TokenKind::LeftParen => write!(f, "("),
            TokenKind::Less => write!(f, "<"),
            TokenKind::LessEqual => write!(f, "<="),
            TokenKind::Match => write!(f, "match"),
            TokenKind::Minus => write!(f, "-"),
            TokenKind::Nil => write!(f, "nil"),
            TokenKind::Number => write!(f, "number"),
            TokenKind::Or => write!(f, "or"),
            TokenKind::Plus => write!(f, "+"),
            TokenKind::Print => write!(f, "print"),
            TokenKind::Return => write!(f, "return"),
            TokenKind::RightBrace => write!(f, "}}"),
            TokenKind::RightParen => write!(f, ")"),
            TokenKind::Semicolon => write!(f, ";"),
            TokenKind::Slash => write!(f, "/"),
            TokenKind::Star => write!(f, "*"),
            TokenKind::String => write!(f, "string"),
            TokenKind::Super => write!(f, "super"),
            TokenKind::This => write!(f, "this"),
            TokenKind::True => write!(f, "true"),
            TokenKind::Underscore => write!(f, "_"),
            TokenKind::Var => write!(f, "var"),
            TokenKind::While => write!(f, "while"),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum TokenLiteral {
    String(String),
    Number(f64),
}

impl TokenLiteral {
    pub fn as_number(self) -> f64 {
        match self {
            TokenLiteral::Number(n) => n,
            _ => panic!("Expected TokenLiteral::Number, but found something else"),
        }
    }

    pub fn as_string(self) -> String {
        match self {
            TokenLiteral::String(s) => s,
            _ => panic!("Expected TokenLiteral::String, but found something else"),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Display)]
#[display(fmt = "{lexeme}")]
pub struct Token {
    pub kind: TokenKind,
    pub lexeme: String,
    pub literal: Option<TokenLiteral>,
    pub location: SourceSpan,
}

impl Token {
    pub fn new(kind: TokenKind, lexeme: String, location: SourceSpan) -> Self {
        Token {
            kind,
            lexeme,
            literal: None,
            location,
        }
    }

    pub fn new_string_literal(
        kind: TokenKind,
        lexeme: String,
        literal: String,
        location: SourceSpan,
    ) -> Self {
        Token {
            kind,
            lexeme,
            literal: Some(TokenLiteral::String(literal)),
            location,
        }
    }

    pub fn new_number_literal(
        kind: TokenKind,
        lexeme: String,
        literal: f64,
        location: SourceSpan,
    ) -> Self {
        Token {
            kind,
            lexeme,
            literal: Some(TokenLiteral::Number(literal)),
            location,
        }
    }
}
