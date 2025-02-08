use crate::ast::untyped::*;
use crate::ast::typed::Module as TypedModule;
use crate::ast::typed::Declaration as TypedDeclaration;
use crate::ast::typed::Expression as TypedExpression;
use crate::ast::typed::LiteralValue as TypedLiteralValue;
use crate::ast::typed::Literal as TypedLiteral;
use crate::ast::typed::Statement as TypedStatement;
use crate::token::{Token, TokenKind};
use crate::source_location::SourceSpan;
use crate::types::Error2 as TypeError;
use crate::types::{Type, TypeEnvironment2};
use super::environment::Environment;
use super::environment::Error as NameError;
use std::rc::Rc;
use thiserror::Error;


#[derive(Debug, Error)]
pub enum Error {
    #[error("{0}")]
    General(String, SourceSpan),

    #[error("{0}")]
    Name(NameError, SourceSpan),

    #[error("{0}")]
    Type(TypeError, SourceSpan),

    #[error("{error}")]
    Mismatch{ error: TypeError, expected_at: SourceSpan, found_at: SourceSpan },
}

impl Error {
    pub fn general(msg: impl Into<String>, location: &SourceSpan) -> Self {
        Self::General(msg.into(), location.clone())
    }

    pub fn location(&self) -> SourceSpan {
        match self {
            Error::General(_, loc) => loc.clone(),
            Error::Name(_, loc) => loc.clone(),
            Error::Mismatch{found_at, ..} => found_at.clone(),
            Error::Type(_, loc) => loc.clone(),
        }
    }
}

trait ErrorLocation<T> {
    fn err_loc(self, location: &SourceSpan) -> Result<T, Error>;
}


impl<T> ErrorLocation<T> for Result<T, NameError> {
    fn err_loc(self, location: &SourceSpan) -> Result<T, Error> {
        self.map_err(|e| Error::Name(e, location.clone()))
    }
}

impl<T> ErrorLocation<T> for Result<T, TypeError> {
    fn err_loc(self, location: &SourceSpan) -> Result<T, Error> {
        self.map_err(|e| Error::Type(e, location.clone()))
    }
}

trait MismatchLocation<T> {
    fn mismatch_loc(self, expected_at: SourceSpan, found_at: SourceSpan) -> Result<T, Error>;
}

impl<T> MismatchLocation<T> for Result<T,TypeError> {
    fn mismatch_loc(self, expected_at: SourceSpan, found_at: SourceSpan) -> Result<T, Error> {
        self.map_err(|e| Error::Mismatch {
            error: e,
            expected_at,
            found_at,
        })
    }
}

struct SemanticAnalyzer {
    env: Environment,
    type_env: TypeEnvironment2,
}

impl SemanticAnalyzer {
    pub fn new() -> Self {
        Self { env: Environment::new(), type_env: TypeEnvironment2::new(), }
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
            Expression::Binary { lhs, op, rhs } => {
                self.analyze_binary_expression(&lhs, &op, &rhs, expr.source_span())
            },
            Expression::Block{ statements, last_expr, .. } => {
                self.analyze_block_expression(
                    &statements,
                    &last_expr,
                    expr.source_span()
                )
            },
            Expression::Call{ callee, arguments, .. } => {
                self.analyze_call_expression(
                    &callee, 
                    &arguments, 
                    expr.source_span()
                )
            },
            Expression::Literal(lit) => self.analyze_literal_expression(&lit.value, expr.source_span()),
            Expression::Unary{ op, operand } => self.analyze_unary_expression(&op, &operand, expr.source_span()),
            Expression::Variable{ name } => self.analyze_variable_expression(&name, expr.source_span()),
        }
    }

    fn analyze_binary_expression(
        &mut self, 
        untyped_lhs: &Box<Expression>,
        op: &BinOp,
        untyped_rhs: &Box<Expression>,
        location: SourceSpan
    ) -> Result<TypedExpression, Error> {
        let lhs = self.analyze_expression(&*untyped_lhs)?;
        let rhs = self.analyze_expression(&*untyped_rhs)?;

        let lhs_ty = lhs.type_();
        let rhs_ty = rhs.type_();
        let bool_ty = self.type_env.get_bool();
        let num_ty  = self.type_env.get_number();
        let str_ty  = self.type_env.get_string();

        // determine input and result types
        use BinOpKind::*;
        let (input_ty, result_ty) = match op.kind {
            And | Or => (bool_ty, bool_ty),
            NotEq | Eq => (lhs_ty, bool_ty),
            Gt | GtEq | Lt | LtEq => (num_ty, bool_ty),
            Sub | Div | Mul => (num_ty, num_ty),
            Add => {
                // plus requires both parameters to be either numbers or strings
                if self.type_env.unify(num_ty, lhs_ty).is_ok() {
                    (num_ty, num_ty)
                } else {
                    (str_ty, str_ty)
                }
            },
        };

        // unify inputs
        self.type_env.unify(input_ty, lhs_ty)
            .err_loc(&lhs.type_defining_location())?;

        self.type_env.unify(input_ty, rhs_ty)
            .err_loc(&rhs.type_defining_location())?;

        Ok(TypedExpression::Binary {
            lhs: Box::new(lhs),
            op: op.clone(),
            rhs: Box::new(rhs),
            type_: result_ty,
            location
        })
    }

    fn analyze_block_expression(
        &mut self,
        untyped_statements: &Vec<Statement>,
        untyped_last_expr: &Option<Box<Expression>>,
        location: SourceSpan
    ) -> Result<TypedExpression, Error> {
        self.with_new_scope(|slf| {
            let mut statements = Vec::new();
            let mut type_ = slf.type_env.get_unit();

            // analyze statements
            for stmt in untyped_statements {
                statements.push(slf.analyze_statement(&stmt)?);
            }

            // analyze a possible final expression
            let last_expr = if let Some(e) = &untyped_last_expr {
                let typed_expr = slf.analyze_expression(&*e)?;
                type_ = typed_expr.type_();
                Some(Box::new(typed_expr))
            } else {
                None
            };

            Ok(TypedExpression::Block {
                statements,
                last_expr,
                type_,
                location,
            })
        })
    }

    fn analyze_call_expression(
        &mut self, 
        untyped_callee: &Expression, 
        untyped_arguments: &Vec<Expression>, 
        location: SourceSpan
    ) -> Result<TypedExpression, Error> {
        let callee = self.analyze_expression(&untyped_callee)?;
        if !callee.type_().is_function() {
            return Err(Error::general("Cannot call non-function", &location))
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
        self.type_env.unify(callee.type_(), call_type)
            .err_loc(&location)?;

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
            LiteralValue::Bool(b)   => (TypedLiteralValue::Bool(*b), self.type_env.get_bool()),
            LiteralValue::Number(n) => (TypedLiteralValue::Number(*n), self.type_env.get_number()),
            LiteralValue::String(s) => (TypedLiteralValue::String(s.clone()), self.type_env.get_string()),
        };

        Ok(TypedExpression::Literal(
            TypedLiteral { value, type_, location }
        ))
    }

    fn analyze_unary_expression(
        &mut self,
        op: &Token,
        untyped_operand: &Box<Expression>,
        location: SourceSpan
    ) -> Result<TypedExpression, Error> {
        let operand = self.analyze_expression(&*untyped_operand)?;
        let expected_type = match op.kind {
            TokenKind::Bang  => self.type_env.get_bool(),
            TokenKind::Minus => self.type_env.get_number(),
            _ => {
                let e = Error::general(format!("Unknown unary operator '{}'", op.lexeme), &location);
                return Err(e)
            }
        };

        // add a unification constraint
        self.type_env.unify(expected_type.clone(), operand.type_().clone())
            .err_loc(&location)?;

        Ok(TypedExpression::Unary {
            op: op.clone(),
            operand: Box::new(operand),
            type_: expected_type,
            location,
        })
    }

    fn analyze_variable_expression(
        &mut self,
        name: &Token, 
        location: SourceSpan
    ) -> Result<TypedExpression, Error> {
        let (decl, scope_distance) = self.env
            .get_definition(&name.lexeme)
            .err_loc(&location)?;
        Ok(TypedExpression::Variable {
            name: name.clone(),
            decl,
            scope_distance,
            location,
        })
    }

    fn analyze_type_expression(&mut self, expr: &TypeExpression) -> Result<Type, Error> {
        self.type_env
            .lookup_type(&expr.identifier.lexeme)
            .ok_or_else(|| { Error::general(
                format!("Unknown type: '{}'", &expr.identifier.lexeme),
                &expr.source_span()
            )})
    }

    fn analyze_type_ascription(&mut self, ascription: &TypeAscription) -> Result<Type, Error> {
        self.analyze_type_expression(&ascription.expr)
    }

    fn analyze_assert_statement(&mut self, untyped_expr: &Expression, location: SourceSpan) -> Result<TypedStatement, Error> {
        let expr = self.analyze_expression(&untyped_expr)?;
        if !expr.type_().is_bool() {
            return Err(Error::general(
                "Argument to 'assert' must be 'bool'",
                &untyped_expr.source_span()
            ))
        }

        Ok(TypedStatement::Assert {
            expr,
            type_: self.type_env.get_unit(),
            location
        })
    }

    fn analyze_declaration(&mut self, decl: &Declaration) -> Result<Rc<TypedDeclaration>, Error> {
        match decl {
            Declaration::Function { name, parameters, return_type, body } => {
                self.analyze_function_declaration(
                    name, 
                    parameters, 
                    return_type,
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
            .err_loc(&param.source_span())?;

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
        untyped_return_type: &TypeExpression,
        untyped_body: &Expression, 
        location: SourceSpan) -> Result<Rc<TypedDeclaration>, Error>
    {
        // analyze parameter type ascriptions
        let mut param_types = Vec::new();
        for p in untyped_parameters {
            let ty = self.analyze_type_ascription(&p.ascription)?;
            param_types.push(ty);
        }

        // analyze return type
        let return_type = self.analyze_type_expression(&untyped_return_type)?;

        // build the function type
        let fun_type = self.type_env.get_function(param_types, return_type);

        // declare the function
        self.env.declare(&name.lexeme, fun_type)
            .err_loc(&location)?;

        // enter function scope
        let result = self.with_new_scope(|slf| {
            // analyze parameters
            let mut parameters = Vec::new();
            for param in untyped_parameters {
                parameters.push(slf.analyze_parameter(&param)?);
            };

            // analyze body
            let body = slf.analyze_expression(&untyped_body)?;

            // check return type against body
            slf.type_env.unify(return_type, body.type_())
                .mismatch_loc(
                    untyped_return_type.source_span(),
                    body.type_defining_location()
                )?;

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
            .err_loc(&location)?;
        let typed_initializer = self.analyze_expression(&initializer)?;

        self.type_env.unify(expected_ty, typed_initializer.type_())
            .err_loc(&location)?;

        let decl = Rc::new(TypedDeclaration::Variable{
            name: name.clone(),
            initializer: typed_initializer,
            type_: expected_ty,
            location
        });

        self.env.define(&name.lexeme, decl.clone());
        Ok(decl)
    }

    fn analyze_statement(&mut self, stmt: &Statement) -> Result<TypedStatement, Error> {
        match stmt {
            Statement::Assert { expr, .. } => self.analyze_assert_statement(&expr, stmt.source_span()),
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
