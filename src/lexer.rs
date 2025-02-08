use crate::source_location::{SourceLocation, SourceSpan};
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
        self.loc.advance(c);
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
            lexeme.push(self.advance_char());
        }

        if self.is_at_end() {
            return Token::new(
                TokenKind::Error,
                String::from("Unterminated string"),
                SourceSpan::new(start_loc, self.loc.clone())
            );
        }

        // consume and include closing quote
        lexeme.push(self.advance_char());

        // For now, literal is same as lexeme (no escape sequences)
        Token::new_string_literal(
            TokenKind::String,
            lexeme.clone(),
            lexeme[1..lexeme.len() - 1].to_string(),
            SourceSpan::new(start_loc, self.loc.clone())
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
        Token::new_number_literal(
            TokenKind::Number, 
            lexeme, 
            literal, 
            SourceSpan::new(start_loc, self.loc.clone())
        )
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

        Token::new(
            kind, 
            lexeme, 
            SourceSpan::new(start_loc, self.loc.clone())
        )
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
                ':' => Token::new(TokenKind::Colon,      String::from(":"), SourceSpan::new(c_loc, self.loc.clone())),
                '_' => Token::new(TokenKind::Underscore, String::from("_"), SourceSpan::new(c_loc, self.loc.clone())),
                '(' => Token::new(TokenKind::LeftParen,  String::from("("), SourceSpan::new(c_loc, self.loc.clone())),
                ')' => Token::new(TokenKind::RightParen, String::from(")"), SourceSpan::new(c_loc, self.loc.clone())),
                '{' => Token::new(TokenKind::LeftBrace,  String::from("{"), SourceSpan::new(c_loc, self.loc.clone())),
                '}' => Token::new(TokenKind::RightBrace, String::from("}"), SourceSpan::new(c_loc, self.loc.clone())),
                ',' => Token::new(TokenKind::Comma,      String::from(","), SourceSpan::new(c_loc, self.loc.clone())),
                '.' => Token::new(TokenKind::Dot,        String::from("."), SourceSpan::new(c_loc, self.loc.clone())),
                '+' => Token::new(TokenKind::Plus,       String::from("+"), SourceSpan::new(c_loc, self.loc.clone())),
                ';' => Token::new(TokenKind::Semicolon,  String::from(";"), SourceSpan::new(c_loc, self.loc.clone())),
                '*' => Token::new(TokenKind::Star,       String::from("*"), SourceSpan::new(c_loc, self.loc.clone())),

                // two-character lexemes
                '-' => {
                    if self.match_char('>') {
                        Token::new(TokenKind::Arrow, String::from("->"), SourceSpan::new(c_loc, self.loc.clone()))
                    } else {
                        Token::new(TokenKind::Minus, String::from("-"), SourceSpan::new(c_loc, self.loc.clone()))
                    }
                }
                '!' => {
                    if self.match_char('=') {
                        Token::new(TokenKind::BangEqual, String::from("!="), SourceSpan::new(c_loc, self.loc.clone()))
                    } else {
                        Token::new(TokenKind::Bang, String::from("!"), SourceSpan::new(c_loc, self.loc.clone()))
                    }
                }
                '=' => {
                    if self.match_char('=') {
                        Token::new(TokenKind::EqualEqual, String::from("=="), SourceSpan::new(c_loc, self.loc.clone()))
                    } else if self.match_char('>') {
                        Token::new(TokenKind::EqualGreater, String::from("=>"), SourceSpan::new(c_loc, self.loc.clone()))
                    } else {
                        Token::new(TokenKind::Equal, String::from("="), SourceSpan::new(c_loc, self.loc.clone()))
                    }
                }
                '<' => {
                    if self.match_char('=') {
                        Token::new(TokenKind::LessEqual, String::from("<="), SourceSpan::new(c_loc, self.loc.clone()))
                    } else {
                        Token::new(TokenKind::Less, String::from("<"), SourceSpan::new(c_loc, self.loc.clone()))
                    }
                }
                '>' => {
                    if self.match_char('=') {
                        Token::new(TokenKind::GreaterEqual, String::from(">="), SourceSpan::new(c_loc, self.loc.clone()))
                    } else {
                        Token::new(TokenKind::Greater, String::from(">"), SourceSpan::new(c_loc, self.loc.clone()))
                    }
                }

                // newline
                '\n' => continue,

                // slash
                '/' => {
                    if self.match_char('/') {
                        // consume until the end of the line
                        while !self.is_at_end() && self.peek() != '\n' {
                            self.advance_char();
                        }
                        continue;
                    } else {
                        Token::new(TokenKind::Slash, String::from("/"), SourceSpan::new(c_loc, self.loc.clone()))
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
                            SourceSpan::new(c_loc, self.loc.clone()),
                        )
                    }
                }
            };

            return Some(token);
        }

        self.emitted_eof = true;
        Some(Token::new(
            TokenKind::Eof, 
            String::new(), 
            SourceSpan::new(self.loc.clone(), self.loc.clone())
        ))
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
        let source = "-> == != <= >=";
        let tokens: Vec<Token> = Lexer::new(source).collect();

        let expected_kinds = [
            TokenKind::Arrow,
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

    #[test]
    fn test_token_location() {
        // Example source. The string literal should have a location that covers exactly "\"Hello\"".
        let source = "var x = \"Hello\";";
        let tokens: Vec<Token> = Lexer::new(source).collect();
    
        // Find the token for the string literal.
        let string_token = tokens
            .iter()
            .find(|token| token.kind == TokenKind::String)
            .expect("Expected a string token");
    
        // Use the token's location to extract the substring from the source.
        let location_str = string_token.location.as_str(source);
    
        // Check that the substring matches the expected lexeme.
        assert_eq!(location_str, "\"Hello\"", "The token's location did not match the expected substring");
    
        // Verify that the start and end offsets are correct.
        // In this source, the string literal starts at byte offset 8:
        // "var x = " (8 bytes) then the string literal.
        // And it should end at offset 15 ("\"Hello\"" is 7 bytes long).
        assert_eq!(string_token.location.start.offset, 8, "Unexpected start offset");
        assert_eq!(string_token.location.end.offset, 15, "Unexpected end offset");
    }

    #[test]
    fn test_location_with_newline() {
        // The source string contains a newline between "foo" and "bar".
        // "foo" is on the first line and "bar" is on the second.
        let source = "foo\nbar";
        let tokens: Vec<Token> = Lexer::new(source).collect();
    
        // We expect three tokens:
        // 1. An identifier for "foo"
        // 2. An identifier for "bar"
        // 3. An EOF token
        assert_eq!(tokens.len(), 3);
    
        // Check the first token ("foo")
        let token_foo = &tokens[0];
        assert_eq!(token_foo.kind, TokenKind::Identifier);
        assert_eq!(token_foo.lexeme, "foo");
    
        // "foo" occupies the first 3 bytes (offsets 0..3)
        assert_eq!(token_foo.location.start.offset, 0, "Expected 'foo' to start at offset 0");
        assert_eq!(token_foo.location.end.offset, 3, "Expected 'foo' to end at offset 3");
        assert_eq!(token_foo.location.as_str(source), "foo");
    
        // Check the second token ("bar")
        let token_bar = &tokens[1];
        assert_eq!(token_bar.kind, TokenKind::Identifier);
        assert_eq!(token_bar.lexeme, "bar");
    
        // Since we have a newline after "foo", which is 1 byte (LF),
        // "bar" should start at offset 4. ("foo" covers offsets 0..3, newline is offset 3, then "bar" is 4..7.)
        assert_eq!(token_bar.location.start.offset, 4, "Expected 'bar' to start at offset 4");
        assert_eq!(token_bar.location.end.offset, 7, "Expected 'bar' to end at offset 7");
        assert_eq!(token_bar.location.as_str(source), "bar");
    
        // Optionally, you can also check the line and column numbers.
        // For instance, "foo" should be on line 1, "bar" on line 2.
        assert_eq!(token_foo.location.start.line, 1, "Expected 'foo' to be on line 1");
        assert_eq!(token_bar.location.start.line, 2, "Expected 'bar' to be on line 2");
    }
}
