use crate::ast::typed::*;
use crate::source_location::SourceSpan;
use crate::token::Token;
use crate::types::Type;
use super::environment::*;
use std::ops::ControlFlow;
use std::rc::Rc;

pub struct Interpreter {
}

impl Interpreter {
    pub fn new() -> Self {
        Self {}
    }

    pub fn interpret_module(&mut self, module: &Module) -> Result<(),String> {
        for stmt in &module.statements {
            self.interpret_statement(stmt)?;
        }
        Ok(())
    }

    fn interpret_statement(&mut self, stmt: &Statement) -> Result<ControlFlow<Value>, String> {
        match stmt {
            Statement::Block(block) => self.interpret_block_statement(&block),
            Statement::Decl(decl)  => {
                self.interpret_declaration(decl.clone())
                    .map(|_| ControlFlow::Continue(()))
            },
            Statement::Expr { expr, type_, location } => {
                self.interpret_expression_statement(
                    &expr, 
                    &type_, 
                    &location)
                    .map(|_| ControlFlow::Continue(()))
            },
            Statement::Print { expr, location, .. } => {
                self.interpret_print_statement(&expr, &location)
                    .map(|_| ControlFlow::Continue(()))
            }
        }
    }

    fn interpret_block_statement(&mut self, _block: &BlockStatement) -> Result<ControlFlow<Value>, String> {
        todo!("interpret_block_statement")
    }

    fn interpret_declaration(&mut self, _decl: Rc<Declaration>) -> Result<(), String> {
        todo!("interpret_declaration")
    }

    fn interpret_expression_statement(&mut self, _expr: &Expression, _type: &Type, _location: &SourceSpan) -> Result<(), String> {
        todo!("interpret_expression_statement")
    }

    fn interpret_print_statement(&mut self, expr: &Expression, _location: &SourceSpan) -> Result<(), String> {
        let val = self.interpret_expression(&expr)?;
        println!("{}", val);
        Ok(())
    }

    fn interpret_expression(&mut self, expr: &Expression) -> Result<Value, String> {
        match expr {
            Expression::Call{ callee, arguments, type_, location } => {
                self.interpret_call_expression(
                    &*callee, 
                    &arguments,
                    &type_,
                    &location
                )
            },
            Expression::Literal(lit) => self.interpret_literal_expression(&lit),
            Expression::Variable{ name, decl, location } => {
                self.interpret_variable_expression(
                    &name,
                    &decl,
                    &location
                )
            }
        }
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
            LiteralValue::Number(n) => Value::Number(*n),
            LiteralValue::String(s) => Value::String(s.clone()),
        })
    }

    fn interpret_variable_expression(&mut self, _name: &Token, _decl: &Rc<Declaration>, _location: &SourceSpan) -> Result<Value, String> {
        todo!("interpret_variable_expression")
    }
}
