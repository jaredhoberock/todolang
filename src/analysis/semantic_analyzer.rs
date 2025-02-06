use crate::ast::untyped::*;
use crate::ast::typed::Module as TypedModule;
use crate::ast::typed::BlockStatement as TypedBlockStatement;
use crate::ast::typed::Declaration as TypedDeclaration;
use crate::ast::typed::Expression as TypedExpression;
use crate::ast::typed::LiteralValue as TypedLiteralValue;
use crate::ast::typed::Literal as TypedLiteral;
use crate::ast::typed::Statement as TypedStatement;
use crate::token::Token;
use crate::source_location::SourceSpan;
use crate::types::{unify, Type, TypeEnvironment};
use super::environment::Environment;
use super::environment::Error as NameError;
use std::rc::Rc;
use thiserror::Error;


#[derive(Debug, Error)]
pub enum Error {
    #[error("{0}")]
    Name(NameError, Option<SourceSpan>),

    #[error("{0}")]
    Type(String, Option<SourceSpan>),
}

impl Error {
    fn name(e: NameError, location: &SourceSpan) -> Self {
        Error::Name(e, Some(location.clone()))
    }

    fn type_(msg: impl Into<String>, location: &SourceSpan) -> Self {
        Error::Type(msg.into(), Some(location.clone()))
    }

    pub fn location(&self) -> Option<SourceSpan> {
        match self {
            Error::Name(_, loc) => loc.clone(),
            Error::Type(_, loc) => loc.clone(),
        }
    }
}


struct SemanticAnalyzer {
    env: Environment,
    type_env: TypeEnvironment,
}

impl SemanticAnalyzer {
    pub fn new() -> Self {
        Self { env: Environment::new(), type_env: TypeEnvironment::new(), }
    }

    // this wraps the invocation of f(self) in a new scope
    // returns the result of f(self)
    fn with_new_scope<T>(
        &mut self,
        f: impl FnOnce(&mut Self) -> Result<T,Error>,
    ) -> Result<T,Error> {
        self.env.enter_scope();
        let result = f(self);
        self.env.exit_scope();
        result
    }

    fn analyze_expression(&mut self, expr: &Expression) -> Result<TypedExpression, Error> {
        match expr {
            Expression::Call{ callee, arguments, .. } => {
                self.analyze_call_expression(
                    &callee, 
                    &arguments, 
                    expr.source_span())
            },
            Expression::Literal(lit) => self.analyze_literal_expression(&lit.value, expr.source_span()),
            Expression::Variable{ name } => self.analyze_variable_expression(&name, expr.source_span()),
        }
    }

    fn analyze_call_expression(
        &mut self, 
        untyped_callee: &Expression, 
        untyped_arguments: &Vec<Expression>, 
        location: SourceSpan
    ) -> Result<TypedExpression, Error> {
        let callee = self.analyze_expression(&untyped_callee)?;
        if !callee.type_().is_function() {
            return Err(Error::type_("Cannot call non-function.", &location))
        }

        let mut arguments = Vec::new();
        for arg in untyped_arguments {
            arguments.push(self.analyze_expression(&arg)?);
        }
        let argument_types = arguments.iter()
            .map(|a| a.type_().clone())
            .collect();

        let result_type = callee.type_().function_return_type();
        let call_type = self.type_env.get_function(argument_types, result_type);
        unify(callee.type_(), call_type)
            .map_err(|e| Error::type_(format!("{}", e), &location))?;

        Ok(TypedExpression::Call {
            callee: Box::new(callee),
            arguments,
            type_: result_type,
            location,
        })
    }

    fn analyze_literal_expression(
        &mut self,
        untyped_value: &LiteralValue, 
        location: SourceSpan
    ) -> Result<TypedExpression, Error> {
        let (value, type_) = match untyped_value {
            LiteralValue::Number(n) => (TypedLiteralValue::Number(*n), self.type_env.get_number()),
            LiteralValue::String(s) => (TypedLiteralValue::String(s.clone()), self.type_env.get_string()),
        };

        Ok(TypedExpression::Literal(
            TypedLiteral { value, type_, location }
        ))
    }

    fn analyze_variable_expression(
        &mut self,
        name: &Token, 
        location: SourceSpan
    ) -> Result<TypedExpression, Error> {
        let decl = self.env.get_definition(&name.lexeme)
            .map_err(|e| Error::name(e, &location))?;
        Ok(TypedExpression::Variable {
            name: name.clone(),
            decl,
            location,
        })
    }

    fn analyze_type_expression(&mut self, expr: &TypeExpression) -> Result<Type, Error> {
        self.type_env
            .lookup_type(&expr.identifier.lexeme)
            .ok_or_else(|| {
                Error::type_(format!("Unknown type: '{}'", &expr.identifier.lexeme), &expr.source_span())
            })
    }

    fn analyze_type_ascription(&mut self, ascription: &TypeAscription) -> Result<Type, Error> {
        self.analyze_type_expression(&ascription.expr)
    }

    fn analyze_block_statement(&mut self, stmt: &BlockStatement) -> Result<TypedBlockStatement, Error> {
        self.with_new_scope(|slf| {
            let mut statements = Vec::new();
            for stmt in &stmt.statements {
                statements.push(slf.analyze_statement(&stmt)?);
            }

            // XXX we need to analyze statements to find the actual type of the block

            Ok(TypedBlockStatement{
                statements,
                type_: slf.type_env.get_unit(),
                location: stmt.source_span(),
            })
        })
    }

    fn analyze_declaration(&mut self, decl: &Declaration) -> Result<Rc<TypedDeclaration>, Error> {
        match decl {
            Declaration::Function { name, parameters, body } => {
                self.analyze_function_declaration(
                    name, 
                    parameters, 
                    body,
                    decl.source_span()
                )
            },
            Declaration::Variable { name, ascription, initializer, .. } => {
                self.analyze_variable_declaration(
                    name, 
                    ascription, 
                    initializer, 
                    decl.source_span()
                )
            },
        }
    }

    fn analyze_parameter(&mut self, param: &Parameter) -> Result<Rc<TypedDeclaration>, Error> {
        let type_ = self.analyze_type_ascription(&param.ascription)?;
        self.env.declare(&param.name.lexeme, type_)
            .map_err(|e| Error::name(e, &param.source_span()))?;

        let result = Rc::new(TypedDeclaration::Parameter {
            name: param.name.clone(),
            type_,
            location: param.source_span(),
        });

        self.env.define(&param.name.lexeme, result.clone());
        Ok(result)
    }

    fn analyze_function_declaration(
        &mut self, 
        name: &Token, 
        untyped_parameters: &Vec<Parameter>, 
        untyped_body: &BlockStatement, 
        location: SourceSpan) -> Result<Rc<TypedDeclaration>, Error>
    {
        // declare the function with an unknown type
        self.env.declare(&name.lexeme, self.type_env.get_unknown())
            .map_err(|e| Error::name(e, &location))?;

        // enter function scope
        let result = self.with_new_scope(|slf| {
            // analyze parameters
            let mut parameters = Vec::new();
            for param in untyped_parameters {
                parameters.push(slf.analyze_parameter(&param)?);
            };
            let param_types = parameters.iter().map(|p| p.type_()).collect();

            // XXX TODO analyze return type ascription
            let result_type = slf.type_env.get_unknown();
            let fun_type = slf.type_env.get_function(param_types, result_type);

            // analyze body
            let body = slf.analyze_block_statement(&untyped_body)?;

            // check return type against body
            // XXX we need to do substitution to get the function's inferred type
            unify(result_type, body.type_)
                .map_err(|e| {
                    Error::type_(format!("{}", e), &location)
                })?;

            Ok(Rc::new(TypedDeclaration::Function {
                name: name.clone(),
                parameters,
                body,
                type_: fun_type,
                location,
            }))
        });

        if let Ok(typed_decl) = &result {
          self.env.define(&name.lexeme, typed_decl.clone());
        }

        result
    }

    fn analyze_variable_declaration(
        &mut self, 
        name: &Token, 
        ascription: &TypeAscription,
        initializer: &Expression, 
        location: SourceSpan
    ) -> Result<Rc<TypedDeclaration>, Error> {
        let expected_ty = self.analyze_type_ascription(ascription)?;
        self.env
            .declare(&name.lexeme, expected_ty)
            .map_err(|e| Error::name(e, &location))?;
        let typed_initializer = self.analyze_expression(&initializer)?;

        unify(expected_ty, typed_initializer.type_())
            .map_err(|e| {
                Error::type_(format!("{}", e), &location)
            })?;

        let decl = Rc::new(TypedDeclaration::Variable{
            name: name.clone(),
            initializer: typed_initializer,
            type_: expected_ty,
            location
        });

        self.env.define(&name.lexeme, decl.clone());
        Ok(decl)
    }

    fn analyze_expression_statement(&mut self, expr: &Expression, location: SourceSpan) -> Result<TypedStatement, Error> {
        Ok(TypedStatement::Expr {
            expr: self.analyze_expression(&expr)?,
            type_: self.type_env.get_unit(),
            location
        })
    }

    fn analyze_print_statement(&mut self, untyped_expr: &Expression, location: SourceSpan) -> Result<TypedStatement, Error> {
        let expr = self.analyze_expression(&untyped_expr)?;
        Ok(TypedStatement::Print {
            expr,
            type_: self.type_env.get_unit(),
            location
        })
    }

    fn analyze_statement(&mut self, stmt: &Statement) -> Result<TypedStatement, Error> {
        match stmt {
            Statement::Block(stmt) => self.analyze_block_statement(&stmt).map(TypedStatement::Block),
            Statement::Decl(decl) => self.analyze_declaration(&decl).map(TypedStatement::Decl),
            Statement::Expr { expr, .. } => self.analyze_expression_statement(&expr, stmt.source_span()),
            Statement::Print { expr, .. } => self.analyze_print_statement(&expr, stmt.source_span()),
        }
    }

    fn analyze_module(&mut self, module: &Module) -> Result<TypedModule, Error> {
        let mut statements = Vec::new();
        for stmt in &module.statements {
            statements.push(self.analyze_statement(&stmt)?);
        }
        Ok(TypedModule{ statements })
    }
}

pub fn analyze_module(module: &Module) -> Result<TypedModule, Error> {
    let mut sema = SemanticAnalyzer::new();
    sema.analyze_module(module)
}
