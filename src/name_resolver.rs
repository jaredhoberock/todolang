use std::collections::HashMap;
use std::ptr::NonNull;

use crate::syntax::*;
use crate::token::Token;

#[derive(PartialEq)]
enum ClassKind {
    Normal,
    Subclass,
}

#[derive(PartialEq)]
enum FunctionKind {
    Initializer,
    Normal,
    Method,
}

#[derive(PartialEq)]
enum ScopeKind {
    Block,
    Class(ClassKind),
    Global,
    Function(FunctionKind),
    MatchArm,
    Super,
}

struct Scope {
    pub kind: ScopeKind,

    // maps a name to whether or not it has been defined
    pub names: HashMap<String, bool>,
}

impl Scope {
    fn new(kind: ScopeKind) -> Self {
        Self {
            kind,
            names: HashMap::new(),
        }
    }
}

pub struct NameResolver {
    // maps the address of a Token in the AST
    // (which must be a field of either a Variable or ThisExpression node)
    // to the scope in which its referent is defined
    symbol_table: HashMap<NonNull<Token>, usize>,

    // a scope maps a name to whether or not it has been defined
    scopes: Vec<Scope>,
}

impl NameResolver {
    pub fn new(initial_globals: Vec<String>) -> Self {
        Self {
            symbol_table: HashMap::new(),
            scopes: vec![Scope {
                kind: ScopeKind::Global,
                names: initial_globals
                    .into_iter()
                    .map(|name| (name, true))
                    .collect(),
            }],
        }
    }

    fn innermost_class_scope(&self) -> Option<&Scope> {
        for scope in self.scopes.iter().rev() {
            match scope.kind {
                ScopeKind::Class(_) => return Some(&scope),
                _ => continue,
            }
        }
        None
    }

    fn is_inside_class(&self) -> bool {
        self.innermost_class_scope().is_some()
    }

    fn innermost_function_scope(&self) -> Option<&Scope> {
        for scope in self.scopes.iter().rev() {
            match scope.kind {
                ScopeKind::Function(_) => return Some(&scope),
                _ => continue,
            }
        }
        None
    }

    fn is_inside_initializer(&self) -> bool {
        match &self.innermost_function_scope() {
            None => false,
            Some(scope) => match scope.kind {
                ScopeKind::Function(FunctionKind::Initializer) => true,
                _ => false,
            },
        }
    }

    pub fn lookup(&self, name: &Token) -> Result<usize, String> {
        self.symbol_table
            .get(&NonNull::from(name))
            .copied()
            .ok_or(format!("Internal error: '{}' was not resolved", name.lexeme).to_string())
    }

    // this wraps the invocation of f(self) in a new scope of the given kind if the condition is true
    // f(self) is invoked in either case
    // returns the result of f(self)
    fn with_new_scope_if<T>(
        &mut self,
        condition: bool,
        kind: ScopeKind,
        f: impl FnOnce(&mut Self) -> Result<T, String>,
    ) -> Result<T, String> {
        if condition {
            self.scopes.push(Scope::new(kind));
        }
        let result = f(self);
        if condition {
            self.scopes.pop();
        }
        result
    }

    // this wraps the invocation of f(self) in a new scope of the given kind
    // returns the result of f(self)
    fn with_new_scope<T>(
        &mut self,
        kind: ScopeKind,
        f: impl FnOnce(&mut Self) -> Result<T, String>,
    ) -> Result<T, String> {
        self.with_new_scope_if(true, kind, f)
    }

    fn declare(&mut self, name: &str) -> Result<(), String> {
        let scope = self
            .scopes
            .last_mut()
            .ok_or("Cannot declare name outside of a scope")?;

        if scope.names.contains_key(name) {
            return Err(format!("'{}' is already declared in this scope", name));
        }

        scope.names.insert(name.to_string(), false);
        Ok(())
    }

    fn define(&mut self, name: &str) -> Result<(), String> {
        let scope = self
            .scopes
            .last_mut()
            .ok_or("Cannot define name outside of a scope")?;

        if !scope.names.contains_key(name) {
            return Err(format!("Cannot define '{}' before declaration", name));
        }

        scope.names.insert(name.to_string(), true);
        Ok(())
    }

    fn declare_defined(&mut self, name: &str) -> Result<(), String> {
        self.declare(name)?;
        self.define(name)
    }

    fn resolve_name(&mut self, name: &Token) -> Result<(), String> {
        if self.scopes.is_empty() {
            return Err("Cannot resolve name outside of a scope".to_string());
        }

        // look through each scope, starting at the top of
        // the stack for a declaration for name, and note
        // the location of its scope if found
        for i in (0..self.scopes.len()).rev() {
            if self.scopes[i].names.contains_key(&name.lexeme) {
                // we record the distance that we need to "climb" from
                // the innermost scope to find the name's referent
                self.symbol_table
                    .insert(NonNull::from(name), self.scopes.len() - 1 - i);
                return Ok(());
            }
        }

        Err(format!(
            "Name resolution error: Name '{}' is undefined",
            name.lexeme
        ))
    }

    fn resolve_variable(&mut self, variable: &Variable) -> Result<(), String> {
        self.resolve_name(&variable.name)
    }

    fn resolve_literal(&mut self, _lit: &Literal) -> Result<(), String> {
        Ok(())
    }

    fn resolve_unary_expression(&mut self, expr: &UnaryExpression) -> Result<(), String> {
        self.resolve_expression(&expr.expr)
    }

    fn resolve_binary_expression(&mut self, expr: &BinaryExpression) -> Result<(), String> {
        self.resolve_expression(&expr.left_expr)?;
        self.resolve_expression(&expr.right_expr)
    }

    fn resolve_get_expression(&mut self, expr: &GetExpression) -> Result<(), String> {
        self.resolve_expression(&expr.object)
    }

    fn resolve_grouping_expression(&mut self, expr: &GroupingExpression) -> Result<(), String> {
        self.resolve_expression(&expr.expr)
    }

    fn resolve_assignment_expression(&mut self, expr: &AssignmentExpression) -> Result<(), String> {
        self.resolve_expression(&expr.expr)?;
        self.resolve_variable(&expr.var)
    }

    fn resolve_logical_expression(&mut self, expr: &LogicalExpression) -> Result<(), String> {
        self.resolve_expression(&expr.left_expr)?;
        self.resolve_expression(&expr.right_expr)
    }

    fn resolve_match_arm(&mut self, arm: &MatchArm) -> Result<(), String> {
        self.with_new_scope(ScopeKind::MatchArm, |slf| {
            // XXX TODO: handle pattern bindings here
            slf.resolve_expression(&arm.expr)
        })
    }

    fn resolve_match_expression(&mut self, match_expr: &MatchExpression) -> Result<(), String> {
        self.resolve_expression(&match_expr.scrutinee)?;

        for arm in &match_expr.arms {
            self.resolve_match_arm(&arm)?;
        }

        Ok(())
    }

    fn resolve_call_expression(&mut self, call: &CallExpression) -> Result<(), String> {
        self.resolve_expression(&call.callee)?;
        for arg in &call.arguments {
            self.resolve_expression(arg)?;
        }
        Ok(())
    }

    fn resolve_set_expression(&mut self, expr: &SetExpression) -> Result<(), String> {
        self.resolve_expression(&expr.value)?;
        self.resolve_expression(&*expr.object)
    }

    fn resolve_super_expression(&mut self, expr: &SuperExpression) -> Result<(), String> {
        match self.innermost_class_scope() {
            Some(scope) if matches!(scope.kind, ScopeKind::Class(ClassKind::Subclass)) => {
                self.resolve_name(&expr.keyword)
            }
            Some(_) => Err("Can't use 'super' in a class with no superclass.".to_string()),
            None => Err("Can't use 'super' outside of a class.".to_string()),
        }
    }

    fn resolve_this_expression(&mut self, expr: &ThisExpression) -> Result<(), String> {
        if !self.is_inside_class() {
            return Err("Can't use 'this' outside of a class".to_string());
        }
        self.resolve_name(&expr.keyword)
    }

    fn resolve_expression(&mut self, expr: &Expression) -> Result<(), String> {
        match expr {
            Expression::Assignment(a) => self.resolve_assignment_expression(a),
            Expression::Binary(b) => self.resolve_binary_expression(b),
            Expression::Call(c) => self.resolve_call_expression(c),
            Expression::Get(g) => self.resolve_get_expression(g),
            Expression::Grouping(g) => self.resolve_grouping_expression(g),
            Expression::Literal(l) => self.resolve_literal(l),
            Expression::Logical(l) => self.resolve_logical_expression(l),
            Expression::Match(m) => self.resolve_match_expression(m),
            Expression::Set(s) => self.resolve_set_expression(s),
            Expression::Super(s) => self.resolve_super_expression(s),
            Expression::This(t) => self.resolve_this_expression(t),
            Expression::Unary(u) => self.resolve_unary_expression(u),
            Expression::Variable(v) => self.resolve_variable(v),
        }
    }

    fn resolve_declaration(&mut self, decl: &Declaration) -> Result<(), String> {
        match decl {
            Declaration::Class(c) => self.resolve_class_declaration(c),
            Declaration::Function(f) => self.resolve_function_declaration(f, FunctionKind::Normal),
            Declaration::Variable(v) => self.resolve_variable_declaration(v),
        }
    }

    fn resolve_assert_statement(&mut self, stmt: &AssertStatement) -> Result<(), String> {
        self.resolve_expression(&stmt.expr)
    }

    fn resolve_return_statement(&mut self, stmt: &ReturnStatement) -> Result<(), String> {
        if self.scopes.is_empty() {
            return Err("Cannot resolve return outside of a scope".to_string());
        }

        if self.is_inside_initializer() {
            if stmt.expr.is_some() {
                return Err("Can't return a value from an initializer.".to_string());
            }
        }

        let scope = self.scopes.last().unwrap();
        match scope.kind {
            ScopeKind::Global => return Err("Can't return from top-level code.".to_string()),
            _ => (),
        }

        if let Some(expr) = &stmt.expr {
            self.resolve_expression(expr)
        } else {
            Ok(())
        }
    }

    fn resolve_print_statement(&mut self, stmt: &PrintStatement) -> Result<(), String> {
        self.resolve_expression(&stmt.expr)
    }

    fn resolve_expression_statement(&mut self, stmt: &ExpressionStatement) -> Result<(), String> {
        self.resolve_expression(&stmt.expr)
    }

    fn resolve_block_statement(&mut self, block: &BlockStatement) -> Result<(), String> {
        self.with_new_scope(ScopeKind::Block, |slf| {
            for stmt in &block.statements {
                slf.resolve_statement(stmt)?;
            }
            Ok(())
        })
    }

    fn resolve_class_declaration(&mut self, class: &ClassDeclaration) -> Result<(), String> {
        self.declare(&class.name.lexeme)?;
        if let Some(superclass_name) = &class.superclass {
            if superclass_name.lexeme == class.name.lexeme {
                return Err("A class can't inherit from itself.".to_string());
            }
            self.resolve_name(&superclass_name)?;
        }
        self.with_new_scope_if(class.superclass.is_some(), ScopeKind::Super, |slf| {
            if class.superclass.is_some() {
                slf.declare_defined("super")?;
            }
            let class_scope_kind = if class.superclass.is_some() {
                ScopeKind::Class(ClassKind::Subclass)
            } else {
                ScopeKind::Class(ClassKind::Normal)
            };
            slf.with_new_scope(class_scope_kind, |slf| {
                slf.declare_defined("this")?;
                for method in &class.methods {
                    let kind = if method.name.lexeme == "init" {
                        FunctionKind::Initializer
                    } else {
                        FunctionKind::Method
                    };
                    slf.resolve_function_declaration(&method, kind)?;
                }
                Ok(())
            })
        })?;
        self.define(&class.name.lexeme)
    }

    fn resolve_for_statement(&mut self, stmt: &ForStatement) -> Result<(), String> {
        if let Some(initializer) = &stmt.initializer {
            self.resolve_statement(&*initializer)?;
        }
        if let Some(condition) = &stmt.condition {
            self.resolve_expression(&*condition)?;
        }
        if let Some(increment) = &stmt.increment {
            self.resolve_expression(&*increment)?;
        }
        self.resolve_statement(&*stmt.body)
    }

    fn resolve_function_declaration(
        &mut self,
        decl: &FunctionDeclaration,
        kind: FunctionKind,
    ) -> Result<(), String> {
        self.declare(&decl.name.lexeme)?;
        self.define(&decl.name.lexeme)?;

        self.with_new_scope(ScopeKind::Function(kind), |slf| {
            for param in &decl.parameters {
                slf.declare(&param.lexeme)?;
                slf.define(&param.lexeme)?;
            }
            slf.resolve_block_statement(&decl.body)
        })
    }

    fn resolve_if_statement(&mut self, stmt: &IfStatement) -> Result<(), String> {
        self.resolve_expression(&stmt.condition)?;
        self.resolve_statement(&*stmt.then_branch)?;
        if let Some(else_branch) = &stmt.else_branch {
            self.resolve_statement(&else_branch)?;
        }
        Ok(())
    }

    fn resolve_variable_declaration(&mut self, decl: &VariableDeclaration) -> Result<(), String> {
        self.declare(&decl.name.lexeme)?;
        if let Some(init) = &decl.initializer {
            self.resolve_expression(init)?;
        }
        self.define(&decl.name.lexeme)
    }

    fn resolve_while_statement(&mut self, stmt: &WhileStatement) -> Result<(), String> {
        self.resolve_expression(&stmt.condition)?;
        self.resolve_statement(&*stmt.body)
    }

    fn resolve_statement(&mut self, stmt: &Statement) -> Result<(), String> {
        match stmt {
            Statement::Assert(a) => self.resolve_assert_statement(a),
            Statement::Block(b) => self.resolve_block_statement(b),
            Statement::Expr(e) => self.resolve_expression_statement(e),
            Statement::Decl(d) => self.resolve_declaration(d),
            Statement::For(f) => self.resolve_for_statement(f),
            Statement::If(i) => self.resolve_if_statement(i),
            Statement::Print(p) => self.resolve_print_statement(p),
            Statement::Return(r) => self.resolve_return_statement(r),
            Statement::While(w) => self.resolve_while_statement(w),
        }
    }

    pub fn resolve_program(&mut self, prog: &Program) -> Result<(), String> {
        if !(self.scopes.len() == 1 && self.scopes[0].kind == ScopeKind::Global) {
            return Err(
                "Internal error: expected single global scope at start of program.".to_string(),
            );
        }

        for stmt in &prog.statements {
            self.resolve_statement(stmt)?;
        }

        if self.scopes.len() == 1 && self.scopes[0].kind == ScopeKind::Global {
            Ok(())
        } else {
            Err("Internal error: expected single global scope at end of program.".to_string())
        }
    }
}
