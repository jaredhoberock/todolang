use crate::syntax::*;
use std::collections::HashMap;
use super::environment::TypeEnvironment;
use super::types::Type;

pub struct TypeChecker {
    env: TypeEnvironment,
    error_cache: HashMap<ExprRef, String>,
}

// type checker either returns the cached type of an expression or computes it
impl TypeChecker {
    pub fn new() -> Self {
        Self { 
            env: TypeEnvironment::new(),
            error_cache: HashMap::new(),
        }
    }

    // Looks up the type for the given expression in the cache.
    // If not found, computes it with the provided closure, caches it, and returns it.
    fn memoize<F>(&mut self, expr: ExprRef, compute: F) -> Result<Type, String>
    where
        F: FnOnce(&mut Self) -> Result<Type, String>
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

    fn check_literal_expression(&mut self, lit: &LiteralExpression) -> Result<Type, String> {
        self.memoize(From::from(lit), |slf| {
            let ty = match lit.0 {
                Literal::Number(_) => slf.env.get_number(),
                Literal::String(_) => slf.env.get_string(),
                Literal::Bool(_)   => slf.env.get_bool(),
                _                  => slf.env.get_unknown(),
            };
            Ok(ty)
        })
    }

    // the public entry point for checking the types of Expressions
    pub fn check_expression(&mut self, expr: &Expression) -> Result<Type, String> {
        self.memoize(From::from(expr), |slf| {
            match expr {
                Expression::Literal(lit) => slf.check_literal_expression(lit),
                _ => Ok(slf.env.get_unknown()),
            }
        })
    }
}
