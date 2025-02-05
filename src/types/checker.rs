use crate::resolver::Resolver;
use crate::declaration_environment::DeclarationEnvironment;
use crate::source_location::Locatable;
use crate::syntax::*;
use std::collections::HashMap;
use std::rc::Rc;
use super::environment::TypeEnvironment;
use super::error::{Error, InternalError};
use super::types::{Kind, Type};

pub struct TypeChecker {
    env: TypeEnvironment,
    error_cache: HashMap<ExprRef, Error>,
    decl_env: Rc<DeclarationEnvironment>,
    resolver: Rc<Resolver>,
}

// type checker either returns the cached type of an expression or computes it
impl TypeChecker {
    pub fn new(decl_env: Rc<DeclarationEnvironment>, resolver: Rc<Resolver>) -> Self {
        Self { 
            env: TypeEnvironment::new(),
            error_cache: HashMap::new(),
            decl_env,
            resolver,
        }
    }

    pub fn check_statement(&mut self, stmt: &Statement) -> Result<(), Error> {
        match stmt {
            Statement::Assert(s) => self.check_assert_statement(s),
            Statement::Block(s)  => self.check_block_statement(s),
            Statement::Decl(s)   => self.check_declaration(s).map(drop),
            Statement::Expr(s)   => self.check_expression_statement(s).map(drop),
            Statement::For(s)    => self.check_for_statement(s).map(drop),
            Statement::If(s)     => self.check_if_statement(s).map(drop),
            Statement::Print(s)  => self.check_print_statement(s).map(drop),
            Statement::Return(s) => self.check_return_statement(s).map(drop),
            Statement::While(s)  => self.check_while_statement(s).map(drop),
        }
    }

    fn unify(&mut self, expected: Type, found: Type) -> Result<(), InternalError> {
        match (&*expected, &*found) {
            // If either is an inference variable, succeed
            (Kind::InferenceVariable(_), _) | (_, Kind::InferenceVariable(_)) => Ok(()),

            // If both types are function types, unify their components
            (Kind::Function(params1, ret1), Kind::Function(params2, ret2)) => {
                if params1.len() != params2.len() {
                    return Err(InternalError::Mismatch { expected, found });
                }
                // unify each parameter type
                for (p1, p2) in params1.iter().zip(params2.iter()) {
                    self.unify(p1.clone(), p2.clone())?;
                }
                // unify return types
                self.unify(ret1.clone(), ret2.clone())
            },

            // If both are concrete, they must be equal
            _ if expected == found => Ok(()),

            // Otherwise, fail
            _ => Err(InternalError::Mismatch{ expected, found }),
        }
    }

    // Looks up the type for the given expression in the cache.
    // If not found, computes it with the provided closure, caches it, and returns it.
    fn memoize<F>(&mut self, expr: ExprRef, compute: F) -> Result<Type, Error>
    where
        F: FnOnce(&mut Self) -> Result<Type, Error>
    {
        // Check the type environment first
        if let Some(ty) = self.env.get_type(expr) {
            return Ok(ty);
        } 

        // Check the error cache
        if let Some(err) = self.error_cache.get(&expr) {
            return Err(err.clone());
        }

        // Otherwise, perform the computation
        let result = compute(self);
        match result {
            Ok(ty) => {
                self.env.set_type(expr, ty);
                Ok(ty)
            }
            Err(err) => {
                self.error_cache.insert(expr, err.clone());
                Err(err)
            }
        }
    }

    fn check_assert_statement(&mut self, stmt: &AssertStatement) -> Result<(), Error> {
        // XXX should check that the expression is bool
        self.check_expression(&stmt.expr)?;
        Ok(())
    }

    fn check_block_statement(&mut self, block: &BlockStatement) -> Result<(), Error> {
        for stmt in &block.statements {
            self.check_statement(stmt)?;
        }
        Ok(())
    }

    fn check_declaration(&mut self, decl: &Declaration) -> Result<Type, Error> {
        let ty = match &decl {
            Declaration::Function(f) => self.check_function_declaration(f),
            Declaration::Variable(v) => self.check_variable_declaration(v),
            _ => Ok(self.env.get_unknown()),
        }?;

        self.decl_env.update_decl(decl.into(), ty);
        Ok(ty)
    }

    fn check_expression_statement(&mut self, stmt: &ExpressionStatement) -> Result<(), Error> {
        self.check_expression(&stmt.expr)?;
        Ok(())
    }

    fn check_for_statement(&mut self, stmt: &ForStatement) -> Result<(), Error> {
        if let Some(init) = &stmt.initializer {
            self.check_statement(init)?;
        }

        if let Some(condition) = &stmt.condition {
            // XXX should check that the type of the condition is bool
            self.check_expression(condition)?;
        }

        if let Some(increment) = &stmt.increment {
            self.check_expression(increment)?;
        }

        self.check_statement(&*stmt.body)
    }

    fn check_if_statement(&mut self, stmt: &IfStatement) -> Result<(), Error> {
        // XXX should check that the condition is bool
        self.check_expression(&stmt.condition)?;
        self.check_statement(&*stmt.then_branch)?;
        if let Some(else_branch) = &stmt.else_branch {
            self.check_statement(&else_branch)?;
        }
        Ok(())
    }

    fn check_print_statement(&mut self, stmt: &PrintStatement) -> Result<(), Error> {
        self.check_expression(&stmt.expr)?;
        Ok(())
    }

    fn check_return_statement(&mut self, stmt: &ReturnStatement) -> Result<(), Error> {
        if let Some(expr) = &stmt.expr {
            self.check_expression(&expr)?;
        }
        Ok(())
    }

    fn check_while_statement(&mut self, stmt: &WhileStatement) -> Result<(), Error> {
        // XXX should check that the condition is bool
        self.check_expression(&stmt.condition)?;
        self.check_statement(&*stmt.body)
    }

    fn check_expression(&mut self, expr: &Expression) -> Result<Type, Error> {
        self.memoize(From::from(expr), |slf| {
            match expr {
                Expression::Assignment(a) => slf.check_assignment_expression(a),
                Expression::Call(c)       => slf.check_call_expression(c),
                Expression::Literal(l)    => slf.check_literal_expression(l),
                Expression::Variable(v)   => slf.check_variable(v),
                _ => Ok(slf.env.get_unknown()),
            }
        })
    }

    fn check_assignment_expression(&mut self, expr: &AssignmentExpression) -> Result<Type, Error> {
        self.memoize(From::from(expr), |slf| {
            let var_ty = slf.check_variable(&expr.var)?;
            let rhs_ty = slf.check_expression(&*expr.expr)?;
            slf.unify(var_ty, rhs_ty)
                .map_err(|e| Error::new(e, expr.source_span()))?;
            Ok(var_ty)
        })
    }

    fn check_call_expression(&mut self, expr: &CallExpression) -> Result<Type, Error> {
        self.memoize(From::from(expr), |slf| {
            let callee_type = slf.check_expression(&*expr.callee)?;
            let mut argument_types = Vec::new();
            for arg in &expr.arguments {
                argument_types.push(slf.check_expression(arg)?);
            }
            let result_type = slf.env.get_unknown();
            let expected_function_type = slf.env.get_function(argument_types, result_type);

            slf.unify(expected_function_type, callee_type)
                .map_err(|e| Error::new(e, expr.source_span()))?;
            Ok(result_type)
        })
    }

    fn check_literal_expression(&mut self, lit: &LiteralExpression) -> Result<Type, Error> {
        self.memoize(From::from(lit), |slf| {
            let ty = match lit.0.value {
                LiteralValue::Number(_) => slf.env.get_number(),
                LiteralValue::String(_) => slf.env.get_string(),
                LiteralValue::Bool(_)   => slf.env.get_bool(),
                _                       => slf.env.get_unknown(),
            };
            Ok(ty)
        })
    }

    fn check_variable(&mut self, var: &Variable) -> Result<Type, Error> {
        self.memoize(From::from(var), |slf| {
            let result = slf.resolver
                .lookup_variable_type(&var)
                .unwrap_or(slf.env.get_unknown());
            Ok(result)
        })
    }

    fn check_type_expression(&mut self, expr: &TypeExpression) -> Result<Type, Error> {
        self.env
            .lookup_type(&expr.identifier.lexeme)
            .ok_or_else(|| Error::new(
                InternalError::UnknownType(expr.identifier.lexeme.clone()),
                expr.source_span()
            ))
    }

    fn check_parameter_declaration(&mut self, decl: &ParameterDeclaration) -> Result<Type, Error> {
        let ty = self.env.get_unknown();
        self.decl_env.update_parameter_decl(decl.into(), ty);
        Ok(ty)
    }

    fn check_variable_declaration(&mut self, decl: &VariableDeclaration) -> Result<Type, Error> {
        let declared = if let Some(ascription) = &decl.ascription {
            self.check_type_expression(&ascription.expr)?
        } else {
            self.env.get_unknown()
        };

        let inferred = if let Some(initializer) = &decl.initializer {
            self.check_expression(initializer)?
        } else {
            self.env.get_unknown()
        };

        self.unify(declared, inferred)
            .map_err(|e| Error::new(e, decl.source_span()))?;

        Ok(declared)
    }

    fn check_function_declaration(&mut self, decl: &FunctionDeclaration) -> Result<Type, Error> {
        let mut param_types = Vec::new();
        for param in &decl.parameters {
            param_types.push(self.check_parameter_declaration(&param)?)
        }

        // XXX TODO optional ascription for return type
        let result_type = self.env.get_unknown();

        // XXX TODO unify result_type and body
        // call check_block_statement here

        Ok(self.env.get_function(param_types, result_type))
    }
}
