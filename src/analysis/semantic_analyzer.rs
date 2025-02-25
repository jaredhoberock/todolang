use crate::ast::untyped::*;
use crate::ast::typed::Declaration as TypedDeclaration;
use crate::ast::typed::DeclRef;
use crate::ast::typed::Module as TypedModule;
use crate::ast::typed::Expression as TypedExpression;
use crate::ast::typed::ExprRef;
use crate::ast::typed::LiteralValue as TypedLiteralValue;
use crate::ast::typed::Literal as TypedLiteral;
use crate::ast::typed::Statement as TypedStatement;
use crate::ast::iterators::*;
use crate::source_location::SourceSpan;
use crate::token::Token;
use crate::types::{TypeEnvironment, TypeScheme};
use super::constraint_set::{ConstraintSet, ConstraintWithProvenance};
use super::environment::Environment;
use super::errors::*;

struct SemanticAnalyzer {
    type_env: TypeEnvironment,
    unresolved_constraints: ConstraintSet,
    env: Environment,
}

impl SemanticAnalyzer {
    pub fn new() -> Self {
        let type_env = TypeEnvironment::new();
        let builtin_types = vec![
            ("Bool", type_env.get_bool()),
            ("Number", type_env.get_number()),
            ("String", type_env.get_string()),
        ];
        Self {
            type_env, 
            unresolved_constraints: ConstraintSet::new(),
            env: Environment::new_with_builtin_types(builtin_types),
        }
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

    fn analyze_expression(&mut self, expr: &Expression) -> Result<ExprRef, Error> {
        match expr {
            Expression::Binary { lhs, op, rhs } => {
                self.analyze_binary_expression(&lhs, &op, &rhs, expr.location())
            },
            Expression::Block{ statements, last_expr, .. } => {
                self.analyze_block_expression(
                    &statements,
                    &last_expr,
                    expr.location()
                )
            },
            Expression::Call{ callee, arguments, .. } => {
                self.analyze_call_expression(
                    &callee, 
                    &arguments, 
                    expr.location()
                )
            },
            Expression::Literal(lit) => self.analyze_literal_expression(&lit.value, expr.location()),
            Expression::Unary{ op, operand } => self.analyze_unary_expression(&op, &operand, expr.location()),
            Expression::Variable{ name } => self.analyze_variable_expression(&name, expr.location()),
        }
    }

    fn analyze_binary_expression(
        &mut self, 
        untyped_lhs: &Box<Expression>,
        op: &BinOp,
        untyped_rhs: &Box<Expression>,
        location: SourceSpan
    ) -> Result<ExprRef, Error> {
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
        // XXX i think the way i'm using unify is not correct
        //     basically all we want to do is introduce constraints for the particular operator
        //     we don't need to use unify at all
        //     after solving constraints, i think we would use unify to get the result type, though
        self.type_env.unify(input_ty, lhs_ty)
            .binop_err_ctx(op.clone(), lhs.type_defining_location())?;

        self.type_env.unify(input_ty, rhs_ty)
            .binop_err_ctx(op.clone(), rhs.type_defining_location())?;

        Ok(ExprRef::new(TypedExpression::Binary {
            lhs,
            op: op.clone(),
            rhs,
            type_: result_ty,
            location
        }))
    }

    fn analyze_block_expression(
        &mut self,
        untyped_statements: &Vec<Statement>,
        untyped_last_expr: &Option<Box<Expression>>,
        location: SourceSpan
    ) -> Result<ExprRef, Error> {
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
                Some(typed_expr)
            } else {
                None
            };

            Ok(ExprRef::new(TypedExpression::Block {
                statements,
                last_expr,
                type_,
                location,
            }))
        })
    }

    fn analyze_call_expression(
        &mut self, 
        untyped_callee: &Expression, 
        untyped_arguments: &Vec<Expression>, 
        location: SourceSpan
    ) -> Result<ExprRef, Error> {
        let callee = self.analyze_expression(&untyped_callee)?;
        if !callee.type_().is_function() {
            return Err(Error::general("Cannot call non-function", &location))
        }

        let mut arguments = Vec::new();
        for arg in untyped_arguments {
            arguments.push(self.analyze_expression(&arg)?);
        }
        let argument_types: Vec<_> = arguments.iter()
            .map(|a| a.type_().clone())
            .collect();

        let result_type = callee.type_().function_return_type();

        // create AST for the call
        let call_expr = ExprRef::new(TypedExpression::Call {
            callee: callee.clone(),
            arguments,
            type_: result_type,
            location: location.clone(),
        });

        // before solving constraints, add additional context for constraints 
        // generated from use of the callee
        self.unresolved_constraints.transform_provenance_for_call(call_expr.clone());

        // XXX introduce an equality constraint for each argument instead of calling unify

        // unify types
        let call_type = self.type_env.get_function(argument_types.clone(), result_type);
        self.type_env.unify(callee.type_(), call_type)
            .err_ctx(&location)?;

        Ok(call_expr)
    }

    fn analyze_literal_expression(
        &mut self,
        untyped_value: &LiteralValue, 
        location: SourceSpan
    ) -> Result<ExprRef, Error> {
        let (value, type_) = match untyped_value {
            LiteralValue::Bool(b)   => (TypedLiteralValue::Bool(*b), self.type_env.get_bool()),
            LiteralValue::Number(n) => (TypedLiteralValue::Number(*n), self.type_env.get_number()),
            LiteralValue::String(s) => (TypedLiteralValue::String(s.clone()), self.type_env.get_string()),
        };

        Ok(ExprRef::new(TypedExpression::Literal(
            TypedLiteral { value, type_, location }
        )))
    }

    fn analyze_unary_expression(
        &mut self,
        op: &UnOp,
        untyped_operand: &Box<Expression>,
        location: SourceSpan
    ) -> Result<ExprRef, Error> {
        let operand = self.analyze_expression(&*untyped_operand)?;
        let expected_type = match op.kind {
            UnOpKind::Neg => self.type_env.get_number(),
            UnOpKind::Not => self.type_env.get_bool(),
        };

        // add a unification constraint
        self.type_env.unify(expected_type.clone(), operand.type_().clone())
            .err_ctx(&location)?;

        Ok(ExprRef::new(TypedExpression::Unary {
            op: op.clone(),
            operand,
            type_: expected_type,
            location,
        }))
    }

    fn analyze_variable_expression(
        &mut self,
        name: &Token, 
        location: SourceSpan
    ) -> Result<ExprRef, Error> {
        let (decl, scope_distance) = self.env
            .get_variable(&name.lexeme)
            .err_ctx(&location)?;

        // instantiate the variable's type and constraints
        let (type_, constraints) = decl.borrow()
            .type_scheme()
            .instantiate(&self.type_env);

        let expr = ExprRef::new(TypedExpression::Variable {
            name: name.clone(),
            decl,
            type_,
            scope_distance,
            location,
        });

        // add constraints to the environment
        for c in constraints {
            let annotated = ConstraintWithProvenance::new_var_use(c, expr.clone());
            self.unresolved_constraints.add_constraint(annotated, self.type_env.substitution_mut())?;
        }

        Ok(expr)
    }

    fn analyze_type_expression(&mut self, expr: &TypeExpression) -> Result<TypeScheme, Error> {
        self.env.get_type_scheme(&expr.identifier.lexeme)
            .err_ctx(&expr.location())
    }

    fn analyze_type_ascription(&mut self, ascription: &TypeAscription) -> Result<TypeScheme, Error> {
        self.analyze_type_expression(&ascription.expr)
    }

    fn analyze_assert_statement(&mut self, untyped_expr: &Expression, location: SourceSpan) -> Result<TypedStatement, Error> {
        let expr = self.analyze_expression(&untyped_expr)?;
        if !expr.type_().is_bool() {
            return Err(Error::general(
                "Argument to 'assert' must be 'bool'",
                &untyped_expr.location()
            ))
        }

        Ok(TypedStatement::Assert {
            expr,
            location
        })
    }

    fn analyze_declaration(&mut self, decl: &Declaration) -> Result<DeclRef, Error> {
        match decl {
            Declaration::Function { name, type_parameters, parameters, return_type, body, } => {
                self.analyze_function_declaration(
                    name, 
                    type_parameters,
                    parameters, 
                    return_type,
                    body,
                    decl.location()
                )
            },
            Declaration::Variable { name, ascription, initializer, .. } => {
                self.analyze_variable_declaration(
                    name, 
                    ascription, 
                    initializer, 
                    decl.location()
                )
            },
        }
    }

    fn analyze_type_parameter(&mut self, param: &TypeParameter) -> Result<DeclRef, Error> {
        // create a generic (universally quantified) variable with constraints
        let type_scheme = TypeScheme::new_generic(
            &self.type_env,
            param.constraint.as_ref().map(|c| c.name.clone())
        );

        let result = DeclRef::new(TypedDeclaration::TypeParameter {
            name: param.name.clone(),
            type_scheme,
            location: param.location(),
        });

        // insert into the name environment
        self.env.insert(&param.name.lexeme, result.clone().into())
            .err_ctx(&param.location())?;

        Ok(result)
    }

    fn analyze_parameter(&mut self, param: &Parameter) -> Result<DeclRef, Error> {
        let type_scheme = self.analyze_type_ascription(&param.ascription)?;

        let result = DeclRef::new(TypedDeclaration::Parameter {
            name: param.name.clone(),
            type_scheme,
            location: param.location(),
        });

        // insert into the name environment
        self.env.insert(&param.name.lexeme, result.clone().into())
            .err_ctx(&param.location())?;

        Ok(result)
    }

    fn analyze_function_declaration(
        &mut self, 
        name: &Token, 
        untyped_type_parameters: &Vec<TypeParameter>,
        untyped_parameters: &Vec<Parameter>, 
        untyped_return_type: &TypeExpression,
        untyped_body: &Expression, 
        location: SourceSpan) -> Result<DeclRef, Error>
    {
        // enter function scope
        self.with_new_scope(|slf| {
            // analyze type parameters
            let mut type_parameters = Vec::new();
            for p in untyped_type_parameters {
                let typed_p = slf.analyze_type_parameter(&p)?;
                type_parameters.push(typed_p);
            }
            
            // analyze parameters
            let mut parameters = Vec::new();
            for param in untyped_parameters {
                parameters.push(slf.analyze_parameter(&param)?);
            }

            // get type scheme of parameters
            let param_type_schemes = parameters.iter()
                .map(|p| p.borrow().type_scheme().clone())
                .collect();

            // analyze return type
            let return_type_scheme = slf.analyze_type_expression(&untyped_return_type)?;

            // build the function's type scheme
            let type_scheme = TypeScheme::new_function(
                &slf.type_env, 
                param_type_schemes,
                return_type_scheme.clone()
            );

            // Create a forward declaration in the enclosing scope
            let result = DeclRef::new(TypedDeclaration::Forward {
                name: name.clone(),
                type_scheme: type_scheme.clone(),
                location: location.clone(),
            });

            // declare the function in the enclosing scope
            slf.env.insert_in_enclosing_scope(&name.lexeme, result.clone().into())
                .err_ctx(&location)?;

            // analyze body
            let body = slf.analyze_expression(&untyped_body)?;

            // instantiate the return type
            let (return_type, _) = return_type_scheme.instantiate(&slf.type_env);

            // check the return type against the body
            slf.type_env.unify(return_type, body.type_())
                .type_mismatch_err_ctx(
                    untyped_return_type.location(),
                    body.type_defining_location()
                )?;

            // define the function
            result.define(TypedDeclaration::Function {
                name: name.clone(),
                type_parameters,
                parameters,
                body,
                type_scheme,
                location
            });

            Ok(result)
        })
    }

    fn analyze_expression_statement(&mut self, expr: &Expression, location: SourceSpan) -> Result<TypedStatement, Error> {
        Ok(TypedStatement::Expr {
            expr: self.analyze_expression(&expr)?,
            location
        })
    }

    fn analyze_print_statement(&mut self, untyped_expr: &Expression, location: SourceSpan) -> Result<TypedStatement, Error> {
        let expr = self.analyze_expression(&untyped_expr)?;
        Ok(TypedStatement::Print {
            expr,
            location
        })
    }

    fn analyze_variable_declaration(
        &mut self, 
        name: &Token, 
        ascription: &TypeAscription,
        initializer: &Expression, 
        location: SourceSpan
    ) -> Result<DeclRef, Error> {
        // first check that the name is unique
        self.env.check_unique_name(&name.lexeme)
            .err_ctx(&location)?;

        let type_scheme = self.analyze_type_ascription(ascription)?;

        let typed_initializer = self.analyze_expression(&initializer)?;

        // instantiate the variable's ascribed type
        let (expected_ty, _) = type_scheme.instantiate(&self.type_env);

        // unify with the intializer's type
        self.type_env.unify(expected_ty, typed_initializer.type_())
            .err_ctx(&location)?;

        let decl = DeclRef::new(TypedDeclaration::Variable{
            name: name.clone(),
            initializer: typed_initializer,
            type_scheme,
            location: location.clone(),
        });

        self.env.insert(&name.lexeme, decl.clone().into())
            .err_ctx(&location)?;

        Ok(decl)
    }

    fn analyze_statement(&mut self, stmt: &Statement) -> Result<TypedStatement, Error> {
        match stmt {
            Statement::Assert { expr, .. } => self.analyze_assert_statement(&expr, stmt.location()),
            Statement::Decl(decl) => self.analyze_declaration(&decl).map(TypedStatement::Decl),
            Statement::Expr { expr, .. } => self.analyze_expression_statement(&expr, stmt.location()),
            Statement::Print { expr, .. } => self.analyze_print_statement(&expr, stmt.location()),
        }
    }

    fn analyze_module(&mut self, module: &Module) -> Result<TypedModule, Error> {
        // infer types
        let mut statements = Vec::new();
        for stmt in &module.statements {
            statements.push(self.analyze_statement(&stmt)?);
        }
        let mut typed_module = TypedModule{ statements };

        // solve constraints
        self.unresolved_constraints.solve_constraints(self.type_env.substitution_mut())?;

        // fully resolve types
        let _ = typed_module.for_each_expression(|expr| {
            expr.apply_to_type(self.type_env.substitution());
        });

        Ok(typed_module)
    }
}

pub fn analyze_module(module: &Module) -> Result<TypedModule, Error> {
    let mut sema = SemanticAnalyzer::new();
    sema.analyze_module(module)
}
