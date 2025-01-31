use crate::source_location::SourceLocation;
use crate::token::{Token, TokenKind};

#[cfg(test)]
use crate::token::TokenLiteral;

pub struct Lexer<'a> {
    source: &'a str,
    loc: SourceLocation,
    emitted_eof: bool,
}

impl<'a> Lexer<'a> {
    pub fn new(source: &'a str) -> Self {
        Lexer {
            source,
            loc: SourceLocation::new(),
            emitted_eof: false,
        }
    }

    fn is_at_end(&self) -> bool {
        self.source.is_empty()
    }

    fn peek(&self) -> char {
        self.source.chars().next().unwrap_or('\0')
    }

    fn peek_next(&self) -> char {
        self.source.chars().nth(1).unwrap_or('\0')
    }

    fn advance_char(&mut self) -> char {
        let c = self.peek();
        self.source = &self.source[self.peek().len_utf8()..];
        self.loc.advance_column(c);
        c
    }

    fn match_char(&mut self, expected: char) -> bool {
        if !self.is_at_end() && self.peek() == expected {
            self.advance_char();
            true
        } else {
            false
        }
    }

    fn scan_string(&mut self, start_loc: SourceLocation) -> Token {
        // include opening quote in lexeme
        let mut lexeme = String::from("\"");

        while !self.is_at_end() && self.peek() != '"' {
            let c = self.peek();
            if c == '\n' {
                self.loc.advance_line(c);
            }
            lexeme.push(self.advance_char());
        }

        if self.is_at_end() {
            return Token::new(
                TokenKind::Error,
                String::from("Unterminated string"),
                start_loc,
            );
        }

        // consume and include closing quote
        lexeme.push(self.advance_char());

        // For now, literal is same as lexeme (no escape sequences)
        Token::with_string_literal(
            TokenKind::String,
            lexeme.clone(),
            lexeme[1..lexeme.len() - 1].to_string(),
            start_loc,
        )
    }

    fn scan_number(&mut self, first_digit: char, start_loc: SourceLocation) -> Token {
        let mut lexeme = String::from(first_digit);

        while !self.is_at_end() && self.peek().is_ascii_digit() {
            lexeme.push(self.advance_char());
        }

        // look for fractional part
        if !self.is_at_end() && self.peek() == '.' && self.peek_next().is_ascii_digit() {
            // consume the decimal
            lexeme.push(self.advance_char());

            while !self.is_at_end() && self.peek().is_ascii_digit() {
                lexeme.push(self.advance_char());
            }
        }

        let literal: f64 = lexeme.parse().unwrap();
        Token::with_number_literal(TokenKind::Number, lexeme, literal, start_loc)
    }

    fn is_identifier_char(c: char) -> bool {
        c.is_ascii_alphanumeric() || c == '_'
    }

    fn scan_identifier(&mut self, first_char: char, start_loc: SourceLocation) -> Token {
        let mut lexeme = String::from(first_char);

        while !self.is_at_end() && Self::is_identifier_char(self.peek()) {
            lexeme.push(self.advance_char());
        }

        let kind = TokenKind::from_keyword_lexeme(&lexeme)
            .unwrap_or(TokenKind::Identifier);

        Token::new(kind, lexeme, start_loc)
    }
}

impl<'a> Iterator for Lexer<'a> {
    type Item = Token;

    fn next(&mut self) -> Option<Self::Item> {
        if self.emitted_eof {
            return None;
        }

        // consume characters until either eof or the next lexeme has been found
        while !self.is_at_end() {
            let c_loc = self.loc.clone();
            let c = self.advance_char();

            let token = match c {
                // single-character lexemes
                ':' => Token::new(TokenKind::Colon, String::from(":"), c_loc),
                '_' => Token::new(TokenKind::Underscore, String::from("_"), c_loc),
                '(' => Token::new(TokenKind::LeftParen, String::from("("), c_loc),
                ')' => Token::new(TokenKind::RightParen, String::from(")"), c_loc),
                '{' => Token::new(TokenKind::LeftBrace, String::from("{"), c_loc),
                '}' => Token::new(TokenKind::RightBrace, String::from("}"), c_loc),
                ',' => Token::new(TokenKind::Comma, String::from(","), c_loc),
                '.' => Token::new(TokenKind::Dot, String::from("."), c_loc),
                '-' => Token::new(TokenKind::Minus, String::from("-"), c_loc),
                '+' => Token::new(TokenKind::Plus, String::from("+"), c_loc),
                ';' => Token::new(TokenKind::Semicolon, String::from(";"), c_loc),
                '*' => Token::new(TokenKind::Star, String::from("*"), c_loc),

                // two-character lexemes
                '!' => {
                    if self.match_char('=') {
                        Token::new(TokenKind::BangEqual, String::from("!="), c_loc)
                    } else {
                        Token::new(TokenKind::Bang, String::from("!"), c_loc)
                    }
                }
                '=' => {
                    if self.match_char('=') {
                        Token::new(TokenKind::EqualEqual, String::from("=="), c_loc)
                    } else if self.match_char('>') {
                        Token::new(TokenKind::EqualGreater, String::from("=>"), c_loc)
                    } else {
                        Token::new(TokenKind::Equal, String::from("="), c_loc)
                    }
                }
                '<' => {
                    if self.match_char('=') {
                        Token::new(TokenKind::LessEqual, String::from("<="), c_loc)
                    } else {
                        Token::new(TokenKind::Less, String::from("<"), c_loc)
                    }
                }
                '>' => {
                    if self.match_char('=') {
                        Token::new(TokenKind::GreaterEqual, String::from(">="), c_loc)
                    } else {
                        Token::new(TokenKind::Greater, String::from(">"), c_loc)
                    }
                }

                // newline
                '\n' => {
                    self.loc.advance_line(c);
                    continue;
                }

                // slash
                '/' => {
                    if self.match_char('/') {
                        // consume until the end of the line
                        while !self.is_at_end() && self.peek() != '\n' {
                            self.advance_char();
                        }
                        continue;
                    } else {
                        Token::new(TokenKind::Slash, String::from("/"), c_loc)
                    }
                }

                // whitespace
                ' ' | '\r' | '\t' => continue,

                // string literals
                '"' => self.scan_string(c_loc),

                // catch-all for numbers and identifiers
                c => {
                    if c.is_ascii_digit() {
                        self.scan_number(c, c_loc)
                    } else if c == '_' || c.is_ascii_alphabetic() {
                        self.scan_identifier(c, c_loc)
                    } else {
                        Token::new(
                            TokenKind::Error,
                            String::from("Unexpected character"),
                            c_loc,
                        )
                    }
                }
            };

            return Some(token);
        }

        self.emitted_eof = true;
        Some(Token::new(TokenKind::Eof, String::new(), self.loc.clone()))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_single_char_tokens() {
        let source = ":(){},.-+;*";
        let tokens: Vec<Token> = Lexer::new(source).collect();

        let expected_kinds = [
            TokenKind::Colon,
            TokenKind::LeftParen,
            TokenKind::RightParen,
            TokenKind::LeftBrace,
            TokenKind::RightBrace,
            TokenKind::Comma,
            TokenKind::Dot,
            TokenKind::Minus,
            TokenKind::Plus,
            TokenKind::Semicolon,
            TokenKind::Star,
            TokenKind::Eof,
        ];

        assert_eq!(tokens.len(), expected_kinds.len());
        for (token, expected_kind) in tokens.iter().zip(expected_kinds.iter()) {
            assert_eq!(token.kind, *expected_kind);
        }
    }

    #[test]
    fn test_two_char_tokens() {
        let source = "== != <= >=";
        let tokens: Vec<Token> = Lexer::new(source).collect();

        let expected_kinds = [
            TokenKind::EqualEqual,
            TokenKind::BangEqual,
            TokenKind::LessEqual,
            TokenKind::GreaterEqual,
            TokenKind::Eof,
        ];

        assert_eq!(tokens.len(), expected_kinds.len());
        for (token, expected_kind) in tokens.iter().zip(expected_kinds.iter()) {
            assert_eq!(token.kind, *expected_kind);
        }
    }

    #[test]
    fn test_string_literal() {
        let source = "\"hello world\"";
        let tokens: Vec<Token> = Lexer::new(source).collect();

        assert_eq!(tokens.len(), 2); // string token + EOF
        assert_eq!(tokens[0].kind, TokenKind::String);
        assert_eq!(tokens[0].lexeme, "\"hello world\"");
        assert_eq!(
            tokens[0].literal,
            Some(TokenLiteral::String("hello world".to_string()))
        );
    }

    #[test]
    fn test_number_literal() {
        let source = "123 123.456";
        let tokens: Vec<Token> = Lexer::new(source).collect();

        assert_eq!(tokens.len(), 3); // Two numbers + EOF
        assert_eq!(tokens[0].kind, TokenKind::Number);
        assert_eq!(tokens[0].lexeme, "123");
        assert_eq!(tokens[0].literal, Some(TokenLiteral::Number(123.0)));

        assert_eq!(tokens[1].kind, TokenKind::Number);
        assert_eq!(tokens[1].lexeme, "123.456");
        assert_eq!(tokens[1].literal, Some(TokenLiteral::Number(123.456)));
    }

    #[test]
    fn test_keywords() {
        let source = "if else true false print";
        let tokens: Vec<Token> = Lexer::new(source).collect();

        let expected_kinds = [
            TokenKind::If,
            TokenKind::Else,
            TokenKind::True,
            TokenKind::False,
            TokenKind::Print,
            TokenKind::Eof,
        ];

        assert_eq!(tokens.len(), expected_kinds.len());
        for (token, expected_kind) in tokens.iter().zip(expected_kinds.iter()) {
            assert_eq!(token.kind, *expected_kind);
        }
    }

    #[test]
    fn test_identifiers() {
        let source = "foo bar baz";
        let tokens: Vec<Token> = Lexer::new(source).collect();

        assert_eq!(tokens.len(), 4); // Three identifiers + EOF
        for i in 0..3 {
            assert_eq!(tokens[i].kind, TokenKind::Identifier);
        }
    }

    #[test]
    fn test_unterminated_string() {
        let source = "\"unterminated";
        let tokens: Vec<Token> = Lexer::new(source).collect();

        assert_eq!(tokens.len(), 2); // error token + EOF
        assert_eq!(tokens[0].kind, TokenKind::Error);
    }

    #[test]
    fn test_comments() {
        let source = "// this is a comment\nprint";
        let tokens: Vec<Token> = Lexer::new(source).collect();

        assert_eq!(tokens.len(), 2); // just print + EOF
        assert_eq!(tokens[0].kind, TokenKind::Print);
    }
}
