use crate::semantic_analyzer::Error as SemanticError;
use crate::semantic_analyzer::SemanticAnalyzer;
use crate::syntax::*;
use crate::token::*;
use super::environment::*;
use std::collections::HashMap;
use std::ops::ControlFlow;
use thiserror::Error;

#[derive(Debug, Error)]
pub enum Error {
    #[error("Runtime error: {0}")]
    Runtime(String),

    #[error(transparent)]
    Semantic(SemanticError),
}

impl From<String> for Error {
    fn from(s: String) -> Self {
        Error::Runtime(s)
    }
}

impl From<&str> for Error {
    fn from(s: &str) -> Self {
        Error::Runtime(s.to_string())
    }
}

impl From<SemanticError> for Error {
    fn from(e: SemanticError) -> Self {
        Error::Semantic(e)
    }
}

pub struct Interpreter {
    sema: SemanticAnalyzer,
    pub(super) current_environment: Shared<Environment>,
}

impl Interpreter {
    fn initial_global_values() -> HashMap<String, Value> {
        let program_started = std::time::SystemTime::now();

        let clock_function = Callable::new_native_function(
            0, // arity
            move |_, _| -> Result<Value, Error> {
                let elapsed = program_started
                    .elapsed()
                    .map_err(|e| Error::Runtime(e.to_string()))?
                    .as_secs_f64();
                Ok(Value::Number(elapsed))
            },
        );

        let mut map = HashMap::new();
        map.insert("clock".to_string(), Value::Callable(clock_function));
        map
    }

    pub fn new() -> Self {
        let env = Environment::new_shared_with_initial_values(Self::initial_global_values());
        let initial_names: Vec<_> = env
            .borrow()
            .local_names()
            .into_iter()
            .map(|name| name.to_string())
            .collect();
        Self {
            sema: SemanticAnalyzer::new(initial_names.clone()),
            current_environment: env,
        }
    }

    pub(super) fn with_environment<T>(
        &mut self,
        new_env: Shared<Environment>,
        f: impl FnOnce(&mut Self) -> T,
    ) -> T {
        let mut temp_env = new_env;
        std::mem::swap(&mut self.current_environment, &mut temp_env);
        let result = f(self);
        std::mem::swap(&mut self.current_environment, &mut temp_env);
        result
    }

    fn with_enclosed_environment<T>(&mut self, f: impl FnOnce(&mut Self) -> T) -> T {
        let env = Environment::new_shared_with_enclosing(self.current_environment.clone());
        self.with_environment(env, f)
    }

    // if condition is true,
    //     self.with_environment(new_env, f)
    //     where new_env is a new environment with self.current_environment as its enclosing
    //     environment
    // otherwise,
    //     self.with_environment(self.current_environment, f)
    fn with_enclosed_environment_if<T>(
        &mut self,
        condition: bool,
        f: impl FnOnce(&mut Self) -> T,
    ) -> T {
        if condition {
            self.with_enclosed_environment(f)
        } else {
            f(self)
        }
    }

    fn interpret_literal_expression(&self, lit: &LiteralExpression) -> Result<Value, Error> {
        Ok(match &lit.0.value {
            LiteralValue::Bool(b)   => Value::Bool(*b),
            LiteralValue::Nil       => Value::Nil,
            LiteralValue::Number(n) => Value::Number(*n),
            LiteralValue::String(s) => Value::String(s.clone()),
        })
    }

    fn interpret_grouping_expression(
        &mut self,
        expr: &GroupingExpression,
    ) -> Result<Value, Error> {
        self.interpret_expression(&*expr.expr)
    }

    fn interpret_logical_expression(&mut self, expr: &LogicalExpression) -> Result<Value, Error> {
        let left = self.interpret_expression(&*expr.left_expr)?;
        match expr.op.kind {
            TokenKind::Or if left.as_bool() => Ok(left),
            TokenKind::And if !left.as_bool() => Ok(left),
            TokenKind::Or | TokenKind::And => self.interpret_expression(&*expr.right_expr),
            _ => Err(format!("Unexpected '{}' in logical expression", expr.op.lexeme).into()),
        }
    }

    fn matches_pattern(&mut self, pattern: &Pattern, scrutinee: &Value) -> bool {
        match pattern {
            Pattern::Literal(lit) => {
                match &lit.0.value {
                    LiteralValue::Bool(b)   => matches!(scrutinee, Value::Bool(v) if v == b),
                    LiteralValue::Nil       => matches!(scrutinee, Value::Nil),
                    LiteralValue::Number(n) => matches!(scrutinee, Value::Number(v) if v == n),
                    LiteralValue::String(s) => matches!(scrutinee, Value::String(v) if v == s),
                }
            }
            Pattern::Underscore => true
        }
    }

    fn interpret_match_expression(&mut self, expr: &MatchExpression) -> Result<Value, Error> {
        let scrutinee = self.interpret_expression(&expr.scrutinee)?;

        for arm in &expr.arms {
            if self.matches_pattern(&arm.pattern, &scrutinee) {
                return self.interpret_expression(&arm.expr);
            }
        }

        Err("No matching pattern found".into())
    }

    fn interpret_binary_expression(&mut self, expr: &BinaryExpression) -> Result<Value, Error> {
        let lhs = self.interpret_expression(&*expr.left_expr)?;
        let rhs = self.interpret_expression(&*expr.right_expr)?;
        lhs.evaluate_binary_operation(&expr.op.kind, &rhs)
    }

    fn interpret_call_expression(&mut self, call: &CallExpression) -> Result<Value, Error> {
        let value = self.interpret_expression(&*call.callee)?;

        let callee = match &value {
            Value::Callable(callee) => callee,
            _ => return Err("Can only call functions and classes.".into()),
        };

        if call.arguments.len() != callee.arity() {
            return Err(format!(
                "Expected {} arguments but got {}.",
                callee.arity(),
                call.arguments.len()
            ).into());
        }

        let mut arguments = Vec::new();
        for arg in &call.arguments {
            arguments.push(self.interpret_expression(&arg)?);
        }

        callee.call(self, &arguments)
    }

    fn interpret_get_expression(&mut self, expr: &GetExpression) -> Result<Value, Error> {
        let object = self.interpret_expression(&expr.object)?;
        match object {
            Value::Instance(obj) => obj.borrow().get(&expr.name),
            _ => Err("Only instances have properties.".into()),
        }
    }

    fn interpret_set_expression(&mut self, expr: &SetExpression) -> Result<Value, Error> {
        let mut object = self.interpret_expression(&expr.object)?;
        match &mut object {
            Value::Instance(obj) => {
                let value = self.interpret_expression(&expr.value)?;
                obj.borrow_mut().set(&expr.name, &value)?;
                Ok(value)
            }
            _ => Err("Only instances have properties.".into()),
        }
    }

    fn interpret_super_expression(&mut self, expr: &SuperExpression) -> Result<Value, Error> {
        // look up the value of "super" and get which ancestor we found it in
        let ancestor = self.sema.super_scope_distance(expr)?;
        let superclass = match self
            .current_environment
            .borrow()
            .get_from_ancestor(ancestor, "super")
        {
            Ok(Value::Callable(Callable::Class(c))) => c,
            _ => unreachable!("'super' should evaluate to a class."),
        };
        // look up the value of "this" at one scope below where we found "super"
        let instance = match self
            .current_environment
            .borrow()
            .get_from_ancestor(ancestor - 1, "this")?
        {
            Value::Instance(instance) => instance,
            _ => unreachable!("'this' should evaluate to an instance."),
        };
        // look up the method in superclass and bind instance as "this"
        superclass
            .find_method(&expr.method.lexeme)?
            .bind_this(instance.clone())
            .map(|m| Value::Callable(Callable::User(m)))
    }

    fn interpret_this_expression(&mut self, this: &ThisExpression) -> Result<Value, Error> {
        let ancestor = self.sema.this_scope_distance(this)?;
        self.current_environment
            .borrow()
            .get_from_ancestor(ancestor, "this")
    }

    fn interpret_unary_expression(&mut self, expr: &UnaryExpression) -> Result<Value, Error> {
        let value = self.interpret_expression(&*expr.expr)?;
        match expr.op.kind {
            TokenKind::Bang => Ok(Value::Bool(!value.as_bool())),
            TokenKind::Minus => Ok(Value::Number(-value.as_f64()?)),
            _ => Err("Bad operator in unary_expression".into()),
        }
    }

    fn interpret_variable(&mut self, var: &Variable) -> Result<Value, Error> {
        let ancestor = self.sema.variable_scope_distance(var)?;
        self.current_environment
            .borrow()
            .get_from_ancestor(ancestor, &var.name.lexeme)
    }

    fn interpret_assignment_expression(
        &mut self,
        expr: &AssignmentExpression,
    ) -> Result<Value, Error> {
        let value = self.interpret_expression(&*expr.expr)?;
        let ancestor = self.sema.variable_scope_distance(&expr.var)?;
        self.current_environment.borrow_mut().set_in_ancestor(
            ancestor,
            &expr.var.name.lexeme,
            &value,
        )?;
        Ok(value)
    }

    fn interpret_expression(&mut self, expr: &Expression) -> Result<Value, Error> {
        match expr {
            Expression::Assignment(expr) => self.interpret_assignment_expression(expr),
            Expression::Binary(expr) => self.interpret_binary_expression(expr),
            Expression::Call(expr) => self.interpret_call_expression(expr),
            Expression::Get(g) => self.interpret_get_expression(g),
            Expression::Grouping(g) => self.interpret_grouping_expression(g),
            Expression::Literal(l) => self.interpret_literal_expression(l),
            Expression::Logical(expr) => self.interpret_logical_expression(expr),
            Expression::Match(expr) => self.interpret_match_expression(expr),
            Expression::Set(s) => self.interpret_set_expression(s),
            Expression::Super(s) => self.interpret_super_expression(s),
            Expression::This(t) => self.interpret_this_expression(t),
            Expression::Unary(expr) => self.interpret_unary_expression(expr),
            Expression::Variable(var) => self.interpret_variable(var),
        }
    }

    fn interpret_class_declaration(&mut self, decl: &ClassDeclaration) -> Result<(), Error> {
        // get superclass if one is specified
        let superclass = match &decl.superclass {
            Some(superclassname) => {
                let ancestor = self.sema.superclass_scope_distance(decl)?;
                match self
                    .current_environment
                    .borrow()
                    .get_from_ancestor(ancestor, &superclassname.lexeme)?
                {
                    Value::Callable(Callable::Class(c)) => Some(c.clone()),
                    _ => return Err("Superclass must be a class.".into()),
                }
            },
            None => None,
        };

        // if a superclass is specified, interpret the following with an enclosed environment
        let class = self.with_enclosed_environment_if(
            superclass.is_some(),
            |slf| -> Result<Value, Error> {
                if let Some(s) = &superclass {
                    slf.current_environment
                        .borrow_mut()
                        .define("super", &Value::from(s.clone()))?;
                };

                let mut methods = HashMap::new();
                for method_decl in &decl.methods {
                    let callable = UserFunction::new(
                        method_decl,
                        slf.current_environment.clone(),
                        method_decl.name.lexeme == "init",
                    );
                    methods.insert(method_decl.name.lexeme.clone(), callable);
                }

                Ok(Value::from(Class::new_rc(
                    &decl.name.lexeme,
                    &superclass,
                    &methods,
                )))
            },
        )?;

        self.current_environment
            .borrow_mut()
            .define(&decl.name.lexeme, &class)
    }

    fn interpret_function_declaration(&mut self, decl: &FunctionDeclaration) -> Result<(), Error> {
        let function = UserFunction::new(decl, self.current_environment.clone(), false);
        let callable = Callable::User(function);
        self.current_environment
            .borrow_mut()
            .define(&decl.name.lexeme, &Value::Callable(callable))
    }

    fn interpret_variable_declaration(&mut self, decl: &VariableDeclaration) -> Result<(), Error> {
        let value = match &decl.initializer {
            Some(expr) => self.interpret_expression(expr)?,
            None => Value::Nil,
        };
        self.current_environment
            .borrow_mut()
            .define(&decl.name.lexeme, &value)
    }

    fn interpret_declaration(&mut self, decl: &Declaration) -> Result<(), Error> {
        match decl {
            Declaration::Class(c) => self.interpret_class_declaration(c),
            Declaration::Function(f) => self.interpret_function_declaration(f),
            Declaration::Variable(v) => self.interpret_variable_declaration(v),
        }
    }

    fn interpret_assert_statement(&mut self, stmt: &AssertStatement) -> Result<(), Error> {
        let val = self.interpret_expression(&stmt.expr)?;
        if !val.as_bool() {
            return Err("assert failed".into());
        }
        Ok(())
    }

    pub(super) fn interpret_block_statement(
        &mut self,
        block: &BlockStatement,
    ) -> Result<ControlFlow<Value>, Error> {
        self.with_enclosed_environment(|slf| {
            let mut result = Ok(ControlFlow::Continue(()));
            for stmt in &block.statements {
                match slf.interpret_statement(stmt) {
                    Ok(ControlFlow::Continue(())) => continue,
                    Ok(ControlFlow::Break(value)) => {
                        result = Ok(ControlFlow::Break(value));
                        break;
                    }
                    Err(e) => {
                        result = Err(e);
                        break;
                    }
                }
            }
            result
        })
    }

    fn interpret_expression_statement(&mut self, stmt: &ExpressionStatement) -> Result<(), Error> {
        self.interpret_expression(&stmt.expr)?;
        Ok(())
    }

    fn interpret_for_statement(
        &mut self,
        stmt: &ForStatement,
    ) -> Result<ControlFlow<Value>, Error> {
        self.with_enclosed_environment(|slf| {
            let mut result = Ok(ControlFlow::Continue(()));

            if let Some(initializer) = &stmt.initializer {
                slf.interpret_statement(initializer)?;
            }

            while match &stmt.condition {
                Some(condition) => slf.interpret_expression(&condition)?.as_bool(),
                None => true,
            } {
                match slf.interpret_statement(&*stmt.body) {
                    Ok(ControlFlow::Continue(())) => (),
                    Ok(ControlFlow::Break(value)) => {
                        result = Ok(ControlFlow::Break(value));
                        break;
                    }
                    Err(e) => {
                        result = Err(e);
                        break;
                    }
                }

                if let Some(increment) = &stmt.increment {
                    slf.interpret_expression(&increment)?;
                }
            }
            result
        })
    }

    fn interpret_if_statement(&mut self, stmt: &IfStatement) -> Result<ControlFlow<Value>, Error> {
        if self.interpret_expression(&stmt.condition)?.as_bool() {
            self.interpret_statement(&*stmt.then_branch)
        } else {
            match &stmt.else_branch {
                Some(else_branch) => self.interpret_statement(&else_branch),
                None => Ok(ControlFlow::Continue(())),
            }
        }
    }

    fn interpret_print_statement(&mut self, stmt: &PrintStatement) -> Result<(), Error> {
        let val = self.interpret_expression(&stmt.expr)?;
        println!("{}", val);
        Ok(())
    }

    fn interpret_return_statement(
        &mut self,
        stmt: &ReturnStatement,
    ) -> Result<ControlFlow<Value>, Error> {
        let value = match &stmt.expr {
            Some(expr) => self.interpret_expression(expr)?,
            None => Value::Nil,
        };
        Ok(ControlFlow::Break(value))
    }

    fn interpret_while_statement(
        &mut self,
        stmt: &WhileStatement,
    ) -> Result<ControlFlow<Value>, Error> {
        let mut result = Ok(ControlFlow::Continue(()));

        while self.interpret_expression(&stmt.condition)?.as_bool() {
            match self.interpret_statement(&*stmt.body) {
                Ok(ControlFlow::Continue(())) => continue,
                Ok(ControlFlow::Break(value)) => {
                    result = Ok(ControlFlow::Break(value));
                    break;
                }
                Err(e) => {
                    result = Err(e);
                    break;
                }
            }
        }

        result
    }

    fn interpret_statement(&mut self, stmt: &Statement) -> Result<ControlFlow<Value>, Error> {
        match stmt {
            Statement::Block(block) => self.interpret_block_statement(block),
            Statement::For(f) => self.interpret_for_statement(f),
            Statement::If(i) => self.interpret_if_statement(i),
            Statement::Return(ret) => self.interpret_return_statement(ret),
            Statement::While(w) => self.interpret_while_statement(w),

            // map the results of other statements to ControlFlow::Continue
            _ => match stmt {
                Statement::Block(_) => Err("Impossible statement".into()),
                Statement::Assert(assert) => self.interpret_assert_statement(assert),
                Statement::Decl(decl) => self.interpret_declaration(decl),
                Statement::Expr(expr) => self.interpret_expression_statement(expr),
                Statement::For(_) => Err("Impossible statement".into()),
                Statement::If(_) => Err("Impossible statement".into()),
                Statement::Print(print) => self.interpret_print_statement(print),
                Statement::Return(_) => Err("Impossible statement".into()),
                Statement::While(_) => Err("Impossible statement".into()),
            }
            .map(|_| ControlFlow::Continue(())),
        }
    }

    pub fn interpret_program(&mut self, prog: &Program) -> Result<(), Error> {
        self.sema.analyze_program(prog).map_err(|e| Error::Semantic(e))?;
        for stmt in &prog.statements {
            self.interpret_statement(stmt)?;
        }
        Ok(())
    }

    pub fn interpret_global_statement(&mut self, stmt: &Statement) -> Result<(), Error> {
        self.sema.analyze_global_statement(stmt).map_err(|e| Error::Semantic(e))?;
        self.interpret_statement(stmt)?;
        Ok(())
    }
}
