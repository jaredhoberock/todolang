use crate::ast::typed::*;
use crate::source_location::SourceSpan;
use crate::token::{Token, TokenKind};
use crate::types::Type;
use super::environment::*;
use std::cell::RefCell;
use std::rc::Rc;
use thiserror::Error;

#[derive(Debug, Error)]
#[error("{message}")]
pub struct Error {
    pub message: String,
    pub location: SourceSpan,
}

pub(crate) trait ErrorLocation<T> {
    fn err_loc(self, location: &SourceSpan) -> Result<T,Error>;
}

impl<T, S: Into<String>> ErrorLocation<T> for Result<T,S> {
    fn err_loc(self, location: &SourceSpan) -> Result<T,Error> {
        self.map_err(|s| Error{ 
            message: s.into(), 
            location: location.clone() 
        })
    }
}


pub struct Interpreter {
    env: Rc<RefCell<Environment>>,
}

impl Interpreter {
    pub fn new() -> Self {
        Self { env: Environment::new_shared_global() }
    }

    // this is pub(crate) for Function::call's use
    pub(crate) fn with_environment<T>(
        &mut self,
        new_env: Rc<RefCell<Environment>>,
        f: impl FnOnce(&mut Self) -> T,
    ) -> T {
        let mut temp_env = new_env;
        std::mem::swap(&mut self.env, &mut temp_env);
        let result = f(self);
        std::mem::swap(&mut self.env, &mut temp_env);
        result
    }

    fn with_enclosed_environment<T>(&mut self, f: impl FnOnce(&mut Self) -> T) -> T {
        let env = Environment::new_enclosed_shared(self.env.clone());
        self.with_environment(env, f)
    }

    pub fn interpret_module(&mut self, module: &Module) -> Result<(),Error> {
        for stmt in &module.statements {
            self.interpret_statement(stmt)?;
        }
        Ok(())
    }

    fn interpret_statement(&mut self, stmt: &Statement) -> Result<(),Error> {
        match stmt {
            Statement::Assert { expr, location, .. } => {
                self.interpret_assert_statement(&expr, &location)
            },
            Statement::Decl(decl)  => {
                self.interpret_declaration(decl.clone())
            },
            Statement::Expr { expr, type_, location } => {
                self.interpret_expression_statement(
                    &expr, 
                    &type_, 
                    &location)
            },
            Statement::Print { expr, location, .. } => {
                self.interpret_print_statement(&expr, &location)
            }
        }
    }

    fn interpret_assert_statement(&mut self, expr: &Expression, loc: &SourceSpan) -> Result<(),Error> {
        let val = self.interpret_expression(&expr)?;
        if !val.as_bool() {
            return Err("assert failed").err_loc(&loc)
        }
        Ok(())
    }

    fn interpret_declaration(&mut self, decl: Rc<Declaration>) -> Result<(),Error> {
        match &*decl {
            Declaration::Function{ name, parameters, body, type_, location } => {
                self.interpret_function_declaration(
                    &decl,
                    &name,
                    &parameters,
                    &body,
                    &type_,
                    &location,
                )
            },
            Declaration::Variable{ initializer, location, .. } => {
                self.interpret_variable_declaration(
                    &decl,
                    &initializer,
                    &location,
                )
            },
            _ => todo!("interpret_declaration")
        }
    }

    fn interpret_function_declaration(
        &mut self,
        decl: &Rc<Declaration>,
        name: &Token,
        _parameters: &Vec<Rc<Declaration>>,
        _body: &Expression,
        _type_: &Type,
        location: &SourceSpan,
    ) -> Result<(), Error> {
        let function = Function::new(decl.clone(), self.env.clone());
        self.env.define(name.lexeme.clone(), Value::Function(function))
            .err_loc(&location)
    }

    fn interpret_variable_declaration(
        &mut self,
        decl: &Rc<Declaration>, 
        initializer: &Expression,
        location: &SourceSpan,
    ) -> Result<(), Error> {
        let value = self.interpret_expression(initializer)?;
        match decl.as_ref() {
            Declaration::Variable { name, .. } => {
                self.env.define(name.lexeme.clone(), value)
                    .err_loc(&location)
            },
            _ => panic!("Internal error: expected variable declaration"),
        }
    }

    fn interpret_expression_statement(&mut self, expr: &Expression, _type: &Type, _location: &SourceSpan) -> Result<(),Error> {
        self.interpret_expression(&expr)?;
        Ok(())
    }

    fn interpret_print_statement(&mut self, expr: &Expression, _location: &SourceSpan) -> Result<(),Error> {
        let val = self.interpret_expression(&expr)?;
        println!("{}", val);
        Ok(())
    }

    // this is pub(crate) for Function::call's use
    pub(crate) fn interpret_expression(&mut self, expr: &Expression) -> Result<Value,Error> {
        match expr {
            Expression::Binary{ lhs, op, rhs, type_, location } => {
                self.interpret_binary_expression(
                    &lhs,
                    &op,
                    &rhs,
                    &type_,
                    &location
                )
            },
            Expression::Block{ statements, last_expr, type_, location } => {
                self.interpret_block_expression(
                    &statements,
                    &last_expr,
                    &type_,
                    &location
                )
            },
            Expression::Call{ callee, arguments, type_, location } => {
                self.interpret_call_expression(
                    &*callee, 
                    &arguments,
                    &type_,
                    &location
                )
            },
            Expression::Literal(lit) => self.interpret_literal_expression(&lit),
            Expression::Unary{ op, operand, type_, location } => {
                self.interpret_unary_expression(
                    &op,
                    &operand,
                    &type_,
                    &location
                )
            },
            Expression::Variable{ name, decl, location, scope_distance } => {
                self.interpret_variable_expression(
                    &name,
                    &decl,
                    *scope_distance,
                    &location
                )
            }
        }
    }

    fn interpret_binary_expression(
        &mut self,
        lhs: &Box<Expression>,
        op: &BinOp,
        rhs: &Box<Expression>,
        _type_: &Type,
        _location: &SourceSpan

    ) -> Result<Value,Error> {
        let lhs_value = self.interpret_expression(&*lhs)?;
        match op.kind {
            // logical operations need to short circuit
            BinOpKind::Or if lhs_value.as_bool() => Ok(lhs_value),
            BinOpKind::And if !lhs_value.as_bool() => Ok(lhs_value),
            BinOpKind::Or | BinOpKind::And => self.interpret_expression(&*rhs),

            // other binary operations
            _ => {
              let rhs_value = self.interpret_expression(&*rhs)?;
              Ok(lhs_value.evaluate_binary_operation(&op, &rhs_value))
            }
        }
    }

    fn interpret_block_expression(
        &mut self,
        statements: &Vec<Statement>,
        last_expr: &Option<Box<Expression>>,
        _type_: &Type,
        _location: &SourceSpan
    ) -> Result<Value,Error> {
        self.with_enclosed_environment(|slf| {
            for stmt in statements {
                slf.interpret_statement(&stmt)?;
            }

            match last_expr {
                Some(expr) => slf.interpret_expression(&*expr),
                None => Ok(Value::Unit),
            }
        })
    }

    fn interpret_call_expression(
        &mut self,
        callee: &Expression, 
        arguments: &Vec<Expression>, 
        _type_: &Type,
        _location: &SourceSpan) -> Result<Value,Error> {
        let callee_value = self.interpret_expression(&callee)?;

        let function = match &callee_value {
            Value::Function(f) => f,
            _ => panic!("Internal error: callee is not a function"),
        };

        let mut argument_values = Vec::new();
        for arg in arguments {
            argument_values.push(self.interpret_expression(&arg)?);
        }

        function.call(self, &argument_values)
    }

    fn interpret_literal_expression(&mut self, literal: &Literal) -> Result<Value,Error> {
        Ok(match &literal.value {
            LiteralValue::Bool(b)   => Value::Bool(*b),
            LiteralValue::Number(n) => Value::Number(*n),
            LiteralValue::String(s) => Value::String(s.clone()),
        })
    }

    fn interpret_unary_expression(
        &mut self, 
        op: &Token, 
        operand: &Box<Expression>, 
        _type_: &Type, 
        _location: &SourceSpan,
    ) -> Result<Value,Error> {
        let operand_value = self.interpret_expression(&*operand)?;
        match op.kind {
            TokenKind::Bang  => Ok(Value::Bool(!operand_value.as_bool())),
            TokenKind::Minus => Ok(Value::Number(-operand_value.as_f64())),
            _ => panic!("Internal error: unknown operator in interpret_unary_expression"),
        }
    }

    fn interpret_variable_expression(
        &mut self,
        name: &Token, 
        _decl: &Rc<Declaration>, 
        scope_distance: usize, 
        location: &SourceSpan
    ) -> Result<Value,Error> {
        self.env.get_at(scope_distance, &name.lexeme)
            .err_loc(&location)
    }
}
