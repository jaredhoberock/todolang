use crate::ast::typed::*;
use crate::source_location::SourceSpan;
use crate::token::Token;
use crate::types::Type;
use super::environment::*;
use std::cell::RefCell;
use std::rc::Rc;

pub struct Interpreter {
    env: Rc<RefCell<Environment>>,
}

impl Interpreter {
    pub fn new() -> Self {
        Self { env: Environment::new_shared_global() }
    }

    pub fn interpret_module(&mut self, module: &Module) -> Result<(),String> {
        for stmt in &module.statements {
            self.interpret_statement(stmt)?;
        }
        Ok(())
    }

    fn interpret_statement(&mut self, stmt: &Statement) -> Result<(), String> {
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

    fn interpret_assert_statement(&mut self, expr: &Expression, _loc: &SourceSpan) -> Result<(), String> {
        let val = self.interpret_expression(&expr)?;
        if !val.as_bool() {
            return Err("assert failed".into())
        }
        Ok(())
    }

    fn interpret_declaration(&mut self, decl: Rc<Declaration>) -> Result<(), String> {
        match &*decl {
            Declaration::Variable{ initializer, .. } => {
                self.interpret_variable_declaration(
                    decl.clone(),
                    &initializer
                )
            },
            _ => todo!("interpret_declaration")
        }
    }

    fn interpret_variable_declaration(
        &mut self,
        decl: Rc<Declaration>, 
        initializer: &Expression
    ) -> Result<(), String> {
        let value = self.interpret_expression(initializer)?;
        match &*decl {
            Declaration::Variable { name, .. } => {
                self.env.define(name.lexeme.clone(), value)
            },
            _ => panic!("Internal error: expected variable declaration"),
        }
    }

    fn interpret_expression_statement(&mut self, expr: &Expression, _type: &Type, _location: &SourceSpan) -> Result<(), String> {
        let expr = self.interpret_expression(&expr)?;
        Ok(())
    }

    fn interpret_print_statement(&mut self, expr: &Expression, _location: &SourceSpan) -> Result<(), String> {
        let val = self.interpret_expression(&expr)?;
        println!("{}", val);
        Ok(())
    }

    fn interpret_expression(&mut self, expr: &Expression) -> Result<Value, String> {
        match expr {
            Expression::Block{ statements, expr, type_, location } => {
                self.interpret_block_expression(
                    &statements,
                    &expr,
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

    fn interpret_block_expression(
        &mut self,
        _statements: &Vec<Statement>,
        _expr: &Option<Box<Expression>>,
        _type_: &Type,
        _location: &SourceSpan
    ) -> Result<Value, String> {
        todo!("interpret_block_expression")
    }

    fn interpret_call_expression(
        &mut self,
        _callee: &Expression, 
        _arguments: &Vec<Expression>, 
        _type_: &Type,
        _location: &SourceSpan) -> Result<Value, String> {
        todo!("interpret_call_expression")
    }

    fn interpret_literal_expression(&mut self, literal: &Literal) -> Result<Value, String> {
        Ok(match &literal.value {
            LiteralValue::Bool(b)   => Value::Bool(*b),
            LiteralValue::Number(n) => Value::Number(*n),
            LiteralValue::String(s) => Value::String(s.clone()),
        })
    }

    fn interpret_variable_expression(
        &mut self,
        _name: &Token, 
        _decl: &Rc<Declaration>, 
        _scope_distance: usize, 
        _location: &SourceSpan
    ) -> Result<Value, String> {
        todo!("interpret_variable_expression")
    }
}
