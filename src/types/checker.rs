use crate::resolver::Resolver;
use crate::syntax::*;
use std::collections::HashMap;
use std::rc::Rc;
use super::environment::TypeEnvironment;
use super::error::TypeError;
use super::types::{Kind, Type};

pub struct TypeChecker {
    env: TypeEnvironment,
    error_cache: HashMap<ExprRef, TypeError>,
    resolver: Rc<Resolver>,
}

// type checker either returns the cached type of an expression or computes it
impl TypeChecker {
    pub fn new(resolver: Rc<Resolver>) -> Self {
        Self { 
            env: TypeEnvironment::new(),
            error_cache: HashMap::new(),
            resolver,
        }
    }

    // For our simple language:
    // * If either type is an inference variable, we treat them as compatible
    // * Otherwise, the types must be equal
    fn unify(&mut self, t1: Type, t2: Type) -> Result<(), TypeError> {
        match (&*t1, &*t2) {
            // If either is an inference variable, succeed
            (Kind::InferenceVariable(_), _) | (_, Kind::InferenceVariable(_)) => Ok(()),
            // If both are concrete, they must be equal
            _ if t1 == t2 => Ok(()),
            // Otherwise, fail
            _ => Err(TypeError::Mismatch{ 
                expected: t1, 
                found: t2
            }),
        }
    }

    // Looks up the type for the given expression in the cache.
    // If not found, computes it with the provided closure, caches it, and returns it.
    fn memoize<F>(&mut self, expr: ExprRef, compute: F) -> Result<Type, TypeError>
    where
        F: FnOnce(&mut Self) -> Result<Type, TypeError>
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

    pub fn check_expression(&mut self, expr: &Expression) -> Result<Type, TypeError> {
        self.memoize(From::from(expr), |slf| {
            match expr {
                Expression::Assignment(a) => slf.check_assignment_expression(a),
                Expression::Literal(l)    => slf.check_literal_expression(l),
                _ => Ok(slf.env.get_unknown()),
            }
        })
    }

    fn check_assignment_expression(&mut self, expr: &AssignmentExpression) -> Result<Type, TypeError> {
        self.memoize(From::from(expr), |slf| {
            let var_ty = slf.resolver.lookup_variable_type(&expr.var);
            let rhs_ty = slf.check_expression(&*expr.expr)?;
            slf.unify(var_ty, rhs_ty)?;
            Ok(var_ty)
        })
    }

    fn check_literal_expression(&mut self, lit: &LiteralExpression) -> Result<Type, TypeError> {
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

    pub fn check_type_expression(&mut self, expr: &TypeExpression) -> Result<Type, TypeError> {
        self.env.lookup_type(&expr.identifier.lexeme)
            .ok_or_else(|| TypeError::UnknownType(expr.identifier.lexeme.clone()))
    }

    pub fn check_parameter_declaration(&mut self, _: &ParameterDeclaration) -> Result<Type, TypeError> {
        Ok(self.env.get_unknown())
    }

    pub fn check_variable_declaration(&mut self, decl: &VariableDeclaration) -> Result<Type, TypeError> {
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

        self.unify(declared, inferred)?;

        Ok(declared)
    }
}
