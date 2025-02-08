use crate::ast::untyped::*;
use crate::token::*;

extern crate restore_macros;
use restore_macros::restore_state_on_err;

#[derive(Debug)]
pub struct ParseError {
    message: String,
    remaining_len: usize,
    pub error_token: Option<Token>,
}

impl ParseError {
    pub fn new(message: String, remaining: &[Token]) -> Self {
        ParseError {
            message,
            remaining_len: remaining.len(),
            error_token: if remaining.is_empty() {
                None
            } else {
                Some(remaining[0].clone())
            },
        }
    }

    // This returns a closure that takes a ParseError and returns a ParseError whose
    // message has been formatted with the given fmt string
    pub fn format_message(fmt: &'static str) -> impl FnOnce(ParseError) -> ParseError {
        |e| ParseError {
            // format the message of e using the fmt string
            message: fmt.replace("{}", &e.message),
            // preserve the other details of the original ParseError
            remaining_len: e.remaining_len,
            error_token: e.error_token,
        }
    }

    // Take the error with the least remaining input (most progress)
    pub fn combine(errors: Vec<ParseError>) -> ParseError {
        errors.into_iter().min_by_key(|e| e.remaining_len).unwrap()
    }
}

impl std::fmt::Display for ParseError {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{}", self.message)
    }
}

#[derive(Clone)]
struct Parser<'a> {
    remaining: &'a [Token],
}

impl<'a> Parser<'a> {
    fn new(remaining: &'a [Token]) -> Parser<'a> {
        Parser { remaining }
    }

    // almost all of the methods below use the restore_state_on_err macro
    // to reset the state of the Parser (i.e., the remaining unparsed input)
    // back to its original state upon entering the method when
    // these methods fail a parse
    //
    // this allows us to try to parse one alternative, automatically backtrack,
    // try the next alternative, etc.
    //
    // this macro hooks into save_state & restore_state
    fn save_state(&self) -> &'a [Token] {
        return self.remaining
    }

    fn restore_state(&mut self, old_remaining: &'a [Token]) {
        self.remaining = old_remaining
    }

    #[restore_state_on_err]
    fn token(&mut self, kind: TokenKind) -> Result<Token, ParseError> {
        if self.remaining.is_empty() || self.remaining[0].kind != kind {
            return Err(ParseError::new(
                format!("Expected '{}'", kind),
                self.remaining,
            ));
        }

        let result = self.remaining[0].clone();
        self.remaining = &self.remaining[1..];
        Ok(result)
    }

    #[restore_state_on_err]
    fn comma(&mut self) -> Result<Token, ParseError> {
        self.token(TokenKind::Comma)
    }

    #[restore_state_on_err]
    fn either_token(&mut self, kind0: TokenKind, kind1: TokenKind) -> Result<Token, ParseError> {
        let tok0 = self.token(kind0);
        if tok0.is_ok() {
            return tok0;
        }

        let tok1 = self.token(kind1);
        if tok1.is_ok() {
            return tok1;
        }

        let errors = vec![tok0.unwrap_err(), tok1.unwrap_err()];

        Err(ParseError::combine(errors))
    }

    #[restore_state_on_err]
    fn any_comparison_operator(&mut self) -> Result<Token, ParseError> {
        let gt = self.token(TokenKind::Greater);
        if gt.is_ok() {
            return gt;
        }

        let gte = self.token(TokenKind::GreaterEqual);
        if gte.is_ok() {
            return gte;
        }

        let lt = self.token(TokenKind::Less);
        if lt.is_ok() {
            return lt;
        }

        let lte = self.token(TokenKind::LessEqual);
        if lte.is_ok() {
            return lte;
        }

        Err(ParseError::new(
            "Expected comparison operator".to_string(),
            self.remaining,
        ))
    }

    fn identifier(&mut self) -> Result<Token, ParseError> {
        self.token(TokenKind::Identifier)
    }

    fn number_literal(&mut self) -> Result<Literal, ParseError> {
        self.token(TokenKind::Number)
            .map(|token| {
                Literal {
                    value: LiteralValue::Number(token.literal.unwrap().as_number()),
                    span: token.span
                }
            })
    }

    fn string_literal(&mut self) -> Result<Literal, ParseError> {
        self.token(TokenKind::String)
            .map(|token| {
                Literal {
                    value: LiteralValue::String(token.literal.unwrap().as_string()),
                    span: token.span
                }
            })
    }

    fn true_literal(&mut self) -> Result<Literal, ParseError> {
        self.token(TokenKind::True)
            .map(|token| {
                Literal {
                    value: LiteralValue::Bool(true),
                    span: token.span
                }
            })
    }

    fn false_literal(&mut self) -> Result<Literal, ParseError> {
        self.token(TokenKind::False)
            .map(|token| {
                Literal {
                    value: LiteralValue::Bool(false),
                    span: token.span
                }
            })
    }

    // literal := number_literal | string_literal | true_literal | false_literal
    #[restore_state_on_err]
    fn literal(&mut self) -> Result<Literal, ParseError> {
        let number = self.number_literal();
        if number.is_ok() {
            return number;
        }

        let string = self.string_literal();
        if string.is_ok() {
            return string;
        }

        let true_ = self.true_literal();
        if true_.is_ok() {
            return true_;
        }

        let false_ = self.false_literal();
        if false_.is_ok() {
            return false_;
        }

        let errors = vec![
            number.unwrap_err(),
            string.unwrap_err(),
            true_.unwrap_err(),
            false_.unwrap_err(),
        ];

        Err(ParseError::combine(errors))
    }

    // literal_expression := literal
    fn literal_expression(&mut self) -> Result<Expression, ParseError> {
         self.literal().map(Expression::Literal)
    }

    // block_expression := "{" (statement)* expression? "}"
    #[restore_state_on_err]
    fn block_expression(&mut self) -> Result<Expression, ParseError> {
        let lbrace = self
            .token(TokenKind::LeftBrace)
            .map_err(ParseError::format_message("{} before block"))?;

        let mut statements = Vec::new();
        let mut last_expr = None;

        // loop until we encounter the closing brace
        let rbrace = loop {
            if let Ok(rbrace) = self.token(TokenKind::RightBrace) {
                break rbrace;
            }

            // try to parse a statement
            let maybe_stmt = self.statement();
            if let Ok(stmt) = maybe_stmt {
                statements.push(stmt);
                continue;
            }

            // try to parse an expression
            let maybe_expr = self.expression();
            if let Ok(e) = maybe_expr {
                last_expr = Some(Box::new(e));
                break self.token(TokenKind::RightBrace)
                    .map_err(ParseError::format_message("{} after block"))?;
            }

            // combine errors and return
            let errors = vec![
                maybe_stmt.unwrap_err(),
                maybe_expr.unwrap_err(),
            ];
            return Err(ParseError::combine(errors))
        };

        Ok(Expression::Block{
            lbrace, 
            statements,
            last_expr, 
            rbrace,
        })
    }

    // variable_expression := identifier
    fn variable_expression(&mut self) -> Result<Expression, ParseError> {
        self.identifier().map(|name| Expression::Variable { name })
    }

    // primary_expression := literal_expression | variable
    #[restore_state_on_err]
    fn primary_expression(&mut self) -> Result<Expression, ParseError> {
        let literal = self.literal_expression();
        if literal.is_ok() {
            return literal;
        }

        let var = self.variable_expression();
        if var.is_ok() {
            return var;
        }

        let errors = vec![
            literal.unwrap_err(),
            var.unwrap_err(),
        ];

        Err(ParseError::combine(errors))
    }

    // arguments := expression ( "," expression )*
    #[restore_state_on_err]
    fn arguments(&mut self) -> Result<Vec<Expression>, ParseError> {
        let mut result = Vec::new();

        loop {
            if result.len() == 255 {
                return Err(ParseError::new(
                    "Can't have more than 255 arguments".to_string(),
                    self.remaining,
                ));
            }

            let err_fmt_str = if result.len() > 0 {
                "{} after ',' in argument list"
            } else {
                "{}"
            };

            let expr = self
                .expression()
                .map_err(ParseError::format_message(err_fmt_str))?;

            result.push(expr);

            // look for a comma to indicate another argument
            if let Err(_) = self.token(TokenKind::Comma) {
                break;
            }
        }

        Ok(result)
    }

    // call := primary_expression ( "." identifier | "(" arguments? ")" )*
    #[restore_state_on_err]
    fn call(&mut self) -> Result<Expression, ParseError> {
        let mut result = self.primary_expression()?;

        loop {
            let tok = match self.either_token(TokenKind::LeftParen, TokenKind::Dot) {
                Ok(t) => t,
                Err(_) => break,
            };

            result = match tok.kind {
                TokenKind::LeftParen => {
                    // look for a closing ')'
                    match self.token(TokenKind::RightParen) {
                        Ok(closing_paren) => {
                            Expression::Call { 
                                callee: Box::new(result), 
                                arguments: Vec::new(),
                                closing_paren,
                            }
                        }
                        _ => {
                            let arguments = self
                                .arguments()
                                .map_err(ParseError::format_message("{} after '('"))?;
                            let closing_paren = self
                                .token(TokenKind::RightParen)
                                .map_err(ParseError::format_message("{} after arguments"))?;
                            Expression::Call {
                                callee: Box::new(result),
                                arguments, 
                                closing_paren,
                            }
                        }
                    }
                }

                _ => unreachable!("either_token only returns Dot or LeftParen"),
            };
        }

        Ok(result)
    }

    // factor := unary ( ( "/" | "*" ) ) unary )*
    #[restore_state_on_err]
    fn factor(&mut self) -> Result<Expression, ParseError> {
        let mut result = self.unary()?;

        while let Ok(op) = self.either_token(TokenKind::Slash, TokenKind::Star) {
            // parse the rhs
            let rhs = self.unary()?;

            result = Expression::Binary {
                lhs: Box::new(result),
                op,
                rhs: Box::new(rhs),
            }
        }

        Ok(result)
    }

    // term := factor ( ( "-" | "+" ) factor )*
    #[restore_state_on_err]
    fn term(&mut self) -> Result<Expression, ParseError> {
        let mut result = self.factor()?;

        while let Ok(op) = self.either_token(TokenKind::Minus, TokenKind::Plus) {
            // parse the rhs
            let rhs = self.factor()?;

            result = Expression::Binary {
                lhs: Box::new(result),
                op,
                rhs: Box::new(rhs),
            }
        }

        Ok(result)
    }

    // comparison := term ( ( ">" | ">=" | "<" | "<=" ) term )*
    fn comparison(&mut self) -> Result<Expression, ParseError> {
        let mut result = self.term()?;

        while let Ok(op) = self.any_comparison_operator() {
            // parse the rhs
            let rhs = self.term()?;

            result = Expression::Binary {
                lhs: Box::new(result),
                op,
                rhs: Box::new(rhs),
            }
        }

        Ok(result)
    }

    // equality := comparison ( ( "!=" | "==" ) comparison )*
    #[restore_state_on_err]
    fn equality(&mut self) -> Result<Expression, ParseError> {
        let mut result = self.comparison()?;

        while let Ok(op) = self.either_token(TokenKind::BangEqual, TokenKind::EqualEqual) {
            // parse the rhs
            let rhs = self.comparison()?;

            result = Expression::Binary {
                lhs: Box::new(result),
                op,
                rhs: Box::new(rhs),
            }
        }

        Ok(result)
    }

    // unary := ( "|" | "-" ) | call
    #[restore_state_on_err]
    fn unary(&mut self) -> Result<Expression, ParseError> {
        if let Ok(op) = self.either_token(TokenKind::Bang, TokenKind::Minus) {
            Ok(Expression::Unary {
                op,
                operand: Box::new(self.unary()?),
            })
        } else {
            self.call()
        }
    }

    // logical_and := equality ( "and" equality)*
    #[restore_state_on_err]
    fn logical_and(&mut self) -> Result<Expression, ParseError> {
        let mut result = self.equality()?;

        while let Ok(op) = self.token(TokenKind::And) {
            // parse the rhs
            let rhs = self.equality()?;

            result = Expression::Binary {
                lhs: Box::new(result),
                op,
                rhs: Box::new(rhs),
            }
        }

        Ok(result)
    }

    // logical_or := logical_and ( "or" logical_and)*
    #[restore_state_on_err]
    fn logical_or(&mut self) -> Result<Expression, ParseError> {
        let mut result = self.logical_and()?;

        while let Ok(op) = self.token(TokenKind::Or) {
            // parse the rhs
            let rhs = self.logical_and()?;

            result = Expression::Binary {
                lhs: Box::new(result),
                op,
                rhs: Box::new(rhs),
            }
        }

        Ok(result)
    }

    // expression := logical_or
    #[restore_state_on_err]
    fn expression(&mut self) -> Result<Expression, ParseError> {
        self.logical_or()
    }

    // type_expression := identifier
    #[restore_state_on_err]
    fn type_expression(&mut self) -> Result<TypeExpression, ParseError> {
        Ok(TypeExpression{ identifier: self.identifier()? })
    }

    // parameter := identifier type_ascription
    #[restore_state_on_err]
    fn parameter(&mut self) -> Result<Parameter, ParseError> {
        Ok(Parameter { name: self.identifier()?, ascription: self.type_ascription()? })
    }

    // parameters := parameter ( "," parameter )*
    #[restore_state_on_err]
    fn parameters(&mut self) -> Result<Vec<Parameter>, ParseError> {
        let mut params = vec![self.parameter()?];

        while self.token(TokenKind::Comma).is_ok() {
            let param = self
                .parameter()
                .map_err(ParseError::format_message("{} in function parameter list"))?;
            params.push(param);
        }

        Ok(params)
    }

    // function_declaration := "fun" identifier "(" parameters? ")" block_expression
    #[restore_state_on_err]
    fn function_declaration(&mut self) -> Result<Declaration, ParseError> {
        let _fun = self
            .token(TokenKind::Fun)
            .map_err(ParseError::format_message("{} before function name"))?;
        let name = self.identifier()?;
        let _lparen = self.token(TokenKind::LeftParen)?;

        let mut parameters = Vec::new();
        if let Ok(params) = self.parameters() {
            parameters = params;
        }

        let _rparen = self.token(TokenKind::RightParen)?;
        let body = self.block_expression()?;

        Ok(Declaration::Function {
            name,
            parameters,
            body,
        })
    }

    // type_ascription := ":" type_expression
    #[restore_state_on_err]
    fn type_ascription(&mut self) -> Result<TypeAscription, ParseError> {
        let colon = self.token(TokenKind::Colon)?;
        let expr = self.type_expression()?;
        Ok(TypeAscription{ colon, expr })
    }

    // variable_declaration := "var" identifier type_ascription "=" expression ";"
    #[restore_state_on_err]
    fn variable_declaration(&mut self) -> Result<Declaration, ParseError> {
        let _var = self.token(TokenKind::Var)?;
        let name = self.identifier()?;
        let ascription = self.type_ascription()?;
        let _equal = self.token(TokenKind::Equal)?;
        let initializer = self.expression()?;
        let semi = self.token(TokenKind::Semicolon)?;

        Ok(Declaration::Variable {
            name,
            ascription,
            initializer,
            semi,
        })
    }

    // declaration := function_declaration | variable_declaration
    #[restore_state_on_err]
    fn declaration(&mut self) -> Result<Declaration, ParseError> {
        let fun = self.function_declaration();
        if fun.is_ok() {
            return fun;
        }

        let var = self.variable_declaration();
        if var.is_ok() {
            return var;
        }

        let errors = vec![
            fun.unwrap_err(), 
            var.unwrap_err()
        ];

        Err(ParseError::combine(errors))
    }

    // assert_statement := "assert" expression ";"
    #[restore_state_on_err]
    fn assert_statement(&mut self) -> Result<Statement, ParseError> {
        let _assert = self.token(TokenKind::Assert)?;
        let expr = self.expression()?;
        let semi = self.token(TokenKind::Semicolon)?;
        Ok(Statement::Assert { expr, semi })
    }

    // expression_statement := expression ";"
    #[restore_state_on_err]
    fn expression_statement(&mut self) -> Result<Statement, ParseError> {
        let expr = self.expression()?;
        let semi = self.token(TokenKind::Semicolon)?;
        Ok(Statement::Expr{ expr, semi })
    }

    // print_statement := "print" expression ";"
    #[restore_state_on_err]
    fn print_statement(&mut self) -> Result<Statement, ParseError> {
        let print = self.token(TokenKind::Print)?;
        let expr = self.expression()?;
        let semi = self.token(TokenKind::Semicolon)?;
        Ok(Statement::Print { print, expr, semi } )
    }

    // statement := assert_statement | declaration | expression_statement | print_statement
    #[restore_state_on_err]
    fn statement(&mut self) -> Result<Statement, ParseError> {
        let assert = self.assert_statement();
        if assert.is_ok() {
            return assert;
        }

        let decl = self.declaration().map(Statement::Decl);
        if decl.is_ok() {
            return decl;
        }

        let expr = self.expression_statement();
        if expr.is_ok() {
            return expr;
        }

        let print = self.print_statement();
        if print.is_ok() {
            return print;
        }

        let errors = vec![
            assert.unwrap_err(),
            decl.unwrap_err(),
            expr.unwrap_err(),
            print.unwrap_err(),
        ];

        Err(ParseError::combine(errors))
    }

    #[restore_state_on_err]
    fn global_statement_or_eof(&mut self) -> Result<Option<Statement>, ParseError> {
        if self.token(TokenKind::Eof).is_ok() {
            return Ok(None);
        }
        Ok(Some(self.statement()?))
    }

    // module := statement* EOF
    #[restore_state_on_err]
    fn module(&mut self) -> Result<Module, ParseError> {
        let mut stmts = Vec::new();

        while let Some(stmt) = self.global_statement_or_eof()? {
            stmts.push(stmt);
        }

        Ok(Module { statements: stmts })
    }
}

pub fn parse_module(tokens: &[Token]) -> Result<Module, ParseError> {
    let mut parser = Parser::new(tokens);
    parser.module()
}

pub fn parse_global_statement_or_eof(tokens: &[Token]) -> Result<Option<Statement>, ParseError> {
    let mut parser = Parser::new(tokens);
    parser.global_statement_or_eof()
}
