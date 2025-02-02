use crate::symbol_table::{NamedEntity, SymbolTable};
use crate::syntax::*;
use crate::types::{TypeChecker,TypeError};
use std::collections::HashMap;
use thiserror::Error;

#[derive(Debug, Error)]
pub enum SemanticError {
    #[error("{0}")]
    Class(String),

    #[error("{0}")]
    ControlFlow(String),

    #[error("Name resolution error: {0}")]
    Name(String),

    #[error(transparent)]
    Type(#[from] TypeError),
}

impl SemanticError {
    fn class(msg: impl Into<String>) -> Self {
        SemanticError::Class(msg.into())
    }

    fn control_flow(msg: impl Into<String>) -> Self {
        SemanticError::ControlFlow(msg.into())
    }

    fn name(msg: impl Into<String>) -> Self {
        SemanticError::Name(msg.into())
    }
}

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
enum LexicalScopeKind {
    Block,
    Class(ClassKind),
    Global,
    Function(FunctionKind),
    MatchArm,
    Super,
}

struct LexicalScopeEntry {
    is_defined: bool,
    entity: NamedEntity,
}

// a LexicalScope is a collection of declarations
// it provides methods for introducing different kinds of declarations
struct LexicalScope {
    pub kind: LexicalScopeKind,

    // maps a name in this scope to metadata about its declaration
    pub declarations: HashMap<String, LexicalScopeEntry>,
}

impl LexicalScope {
    fn new(kind: LexicalScopeKind) -> Self {
        Self {
            kind,
            declarations: HashMap::new(),
        }
    }

    fn new_global(global_builtin_functions: Vec<String>) -> Self {
        let mut result = Self::new(LexicalScopeKind::Global);
        for name in &global_builtin_functions {
            result.declare_builtin_function(name).ok();
        }
        result
    }

    fn assert_name_is_unique(&self, name: &str) -> Result<(), SemanticError> {
        if self.declarations.contains_key(name) {
            return Err(SemanticError::name(format!("'{}' is already declared in this scope", name)));
        }
        Ok(())
    }

    fn insert(&mut self, name: &str, is_defined: bool, entity: NamedEntity) -> Result<(),SemanticError> {
        self.assert_name_is_unique(name)?;
        self.declarations.insert(name.to_string(), LexicalScopeEntry { is_defined, entity });
        Ok(())
    }

    fn define(&mut self, name: &str) {
        match self.declarations.get_mut(name) {
            Some(entry) if entry.is_defined => {
                panic!("Internal compiler error: cannot redefine '{}'", name);
            }
            Some(entry) => {
                entry.is_defined = true;
            }
            None => {
                panic!("Internal compiler error: cannot define '{}' before declaration", name);
            }
        }
    }
    
    fn declare_builtin_function(&mut self, name: &str) -> Result<(),SemanticError> {
        self.insert(&name, false, NamedEntity::BuiltinFunction)
    }

    fn declare_class(&mut self, decl: &ClassDeclaration) -> Result<(),SemanticError> {
        self.insert(&decl.name.lexeme, false, NamedEntity::Declaration(decl.into()))
    }

    fn declare_and_define_function(&mut self, decl: &FunctionDeclaration) -> Result<(),SemanticError> {
        self.insert(&decl.name.lexeme, true, NamedEntity::Declaration(decl.into()))
    }

    fn declare_and_define_parameter(&mut self, decl: &ParameterDeclaration) -> Result<(),SemanticError> {
        self.insert(&decl.name.lexeme, true, NamedEntity::Parameter)
    }

    fn declare_and_define_super(&mut self, decl: &ClassDeclaration) -> Result<(),SemanticError> {
        self.insert("super", true, NamedEntity::Declaration(decl.into()))
    }

    fn declare_and_define_this(&mut self, decl: &ClassDeclaration) -> Result<(),SemanticError> {
        self.insert("this", true, NamedEntity::Declaration(decl.into()))
    }

    fn declare_variable(&mut self, decl: &VariableDeclaration) -> Result<(),SemanticError> {
        self.insert(&decl.name.lexeme, false, NamedEntity::Declaration(decl.into()))
    }
}


pub struct SemanticAnalyzer {
    scopes: Vec<LexicalScope>,
    symbol_table: SymbolTable,
    type_checker: TypeChecker,
}


impl SemanticAnalyzer {
    pub fn new(builtin_functions: Vec<String>) -> Self {
        Self {
            scopes: vec![LexicalScope::new_global(builtin_functions)],
            symbol_table: SymbolTable::new(),
            type_checker: TypeChecker::new(),
        }
    }

    // this wraps the invocation of f(self) in a new scope of the given kind if the condition is true
    // f(self) is invoked in either case
    // returns the result of f(self)
    fn with_new_scope_if<T>(
        &mut self,
        condition: bool,
        kind: LexicalScopeKind,
        f: impl FnOnce(&mut Self) -> Result<T,SemanticError>,
    ) -> Result<T,SemanticError> {
        if condition {
            self.scopes.push(LexicalScope::new(kind));
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
        kind: LexicalScopeKind,
        f: impl FnOnce(&mut Self) -> Result<T,SemanticError>,
    ) -> Result<T,SemanticError> {
        self.with_new_scope_if(true, kind, f)
    }

    fn innermost_class_scope(&self) -> Option<&LexicalScope> {
        for scope in self.scopes.iter().rev() {
            match scope.kind {
                LexicalScopeKind::Class(_) => return Some(&scope),
                _ => continue,
            }
        }
        None
    }

    fn is_inside_class(&self) -> bool {
        self.innermost_class_scope().is_some()
    }

    fn innermost_function_scope(&self) -> Option<&LexicalScope> {
        for scope in self.scopes.iter().rev() {
            match scope.kind {
                LexicalScopeKind::Function(_) => return Some(&scope),
                _ => continue,
            }
        }
        None
    }

    fn is_inside_function(&self) -> bool {
        self.innermost_function_scope().is_some()
    }

    fn is_inside_initializer(&self) -> bool {
        match &self.innermost_function_scope() {
            None => false,
            Some(scope) => match scope.kind {
                LexicalScopeKind::Function(FunctionKind::Initializer) => true,
                _ => false,
            },
        }
    }

    fn current_scope_mut(&mut self) -> &mut LexicalScope {
        self.scopes
            .last_mut()
            .expect("Internal compiler error: called current_scope_mut outside of a scope")
    }

    // name resolution searches the stack of scopes for a binding of the given name to some entity
    // if such a binding exists, it returns the pair
    // 1. A reference to the entity, and
    // 2. the distance to the scope containing that entity
    fn resolve_name(&self, name: &str) -> Result<(NamedEntity, usize), SemanticError> {
        if self.scopes.is_empty() {
            panic!("Internal compiler error: cannot resolve name '{}' outside of a scope", name);
        }

        // look through each scope, starting at the top of
        // the stack for a binding for name, and note
        // the location of its scope if found
        for (i, scope) in self.scopes.iter().enumerate().rev() {
            if let Some(entry) = scope.declarations.get(name) {
                return Ok((entry.entity, self.scopes.len() - 1 - i))
            }
        }

        Err(SemanticError::name(format!("Name '{}' is undefined", name)))
    }

    fn resolve_super(&mut self, super_: &SuperExpression) -> Result<(), SemanticError> {
        let (binding, scope_distance) = self.resolve_name("super")?;
        let class = binding.into_class_decl()
            .expect("Internal compiler error: 'super' resolved to something other than a ClassDeclaration");
        self.symbol_table.bind_super(super_, class, scope_distance);
        Ok(())
    }

    fn resolve_this(&mut self, this: &ThisExpression) -> Result<(), SemanticError> {
        let (binding, scope_distance) = self.resolve_name("this")?;
        let class = binding.into_class_decl()
            .expect("Internal compiler error: 'this' resolved to something other than a ClassDeclaration");
        self.symbol_table.bind_this(this, class, scope_distance);
        Ok(())
    }

    fn resolve_variable(&mut self, var: &Variable) -> Result<(), SemanticError> {
        let (binding, scope_distance) = self.resolve_name(&var.name.lexeme)?;
        self.symbol_table.bind_variable(var, binding, scope_distance);
        Ok(())
    }

    fn analyze_literal_expression(&mut self, _: &LiteralExpression) -> Result<(), SemanticError> {
        Ok(())
    }

    fn analyze_expression(&mut self, expr: &Expression) -> Result<(), SemanticError> {
        match expr {
            Expression::Assignment(a) => self.analyze_assignment_expression(a),
            Expression::Binary(b) => self.analyze_binary_expression(b),
            Expression::Call(c) => self.analyze_call_expression(c),
            Expression::Get(g) => self.analyze_get_expression(g),
            Expression::Grouping(g) => self.analyze_grouping_expression(g),
            Expression::Literal(l) => self.analyze_literal_expression(l),
            Expression::Logical(l) => self.analyze_logical_expression(l),
            Expression::Match(m) => self.analyze_match_expression(m),
            Expression::Set(s) => self.analyze_set_expression(s),
            Expression::Super(s) => self.analyze_super_expression(s),
            Expression::This(t) => self.analyze_this_expression(t),
            Expression::Unary(u) => self.analyze_unary_expression(u),
            Expression::Variable(v) => self.analyze_variable(v),
        }?;

        // check the type of the expression
        self.type_checker.check_expression(expr)
            .map(drop)
            .map_err(Into::into)
    }

    fn analyze_assignment_expression(&mut self, expr: &AssignmentExpression) -> Result<(), SemanticError> {
        // XXX TODO type check 
        self.analyze_expression(&expr.expr)?;
        self.analyze_variable(&expr.var)
    }

    fn analyze_binary_expression(&mut self, expr: &BinaryExpression) -> Result<(), SemanticError> {
        self.analyze_expression(&expr.left_expr)?;
        self.analyze_expression(&expr.right_expr)
    }

    fn analyze_call_expression(&mut self, call: &CallExpression) -> Result<(), SemanticError> {
        // XXX TODO check that callee is a function
        self.analyze_expression(&call.callee)?;
        for arg in &call.arguments {
            self.analyze_expression(arg)?;
        }
        Ok(())
    }

    fn analyze_get_expression(&mut self, expr: &GetExpression) -> Result<(), SemanticError> {
        self.analyze_expression(&expr.object)
    }

    fn analyze_grouping_expression(&mut self, expr: &GroupingExpression) -> Result<(), SemanticError> {
        self.analyze_expression(&expr.expr)
    }

    fn analyze_logical_expression(&mut self, expr: &LogicalExpression) -> Result<(), SemanticError> {
        self.analyze_expression(&expr.left_expr)?;
        self.analyze_expression(&expr.right_expr)
    }

    fn analyze_match_expression(&mut self, expr: &MatchExpression) -> Result<(), SemanticError> {
        self.analyze_expression(&expr.scrutinee)?;
        for arm in &expr.arms {
            self.analyze_match_arm(arm)?;
        }
        Ok(())
    }

    fn analyze_match_arm(&mut self, arm: &MatchArm) -> Result<(), SemanticError> {
        self.with_new_scope(LexicalScopeKind::MatchArm, |slf| {
          slf.analyze_pattern(&arm.pattern)?;
          slf.analyze_expression(&arm.expr)
        })
    }

    fn analyze_pattern(&mut self, pattern: &Pattern) -> Result<(), SemanticError> {
        match pattern {
            Pattern::Literal(l) => self.analyze_literal_pattern(l),
            Pattern::Underscore => Ok(())
        }
    }

    fn analyze_literal_pattern(&mut self, _lit: &LiteralPattern) -> Result<(), SemanticError> {
      Ok(())
    }

    fn analyze_set_expression(&mut self, set: &SetExpression) -> Result<(), SemanticError> {
        // XXX TODO type check
        self.analyze_expression(&set.value)?;
        self.analyze_expression(&*set.object)
    }

    fn analyze_super_expression(&mut self, super_: &SuperExpression) -> Result<(), SemanticError> {
        match self.innermost_class_scope() {
            Some(scope) if matches!(scope.kind, LexicalScopeKind::Class(ClassKind::Subclass)) => {
                self.resolve_super(&super_).map_err(Into::into)
            }
            Some(_) => Err(SemanticError::name("Can't use 'super' in a class with no superclass.")),
            None    => Err(SemanticError::name("Can't use 'super' outside of a class.")),
        }
    }

    fn analyze_this_expression(&mut self, this: &ThisExpression) -> Result<(), SemanticError> {
        if !self.is_inside_class() {
            return Err(SemanticError::name("Can't use 'this' outside of a class"));
        }
        self.resolve_this(&this).map_err(Into::into)
    }

    fn analyze_unary_expression(&mut self, expr: &UnaryExpression) -> Result<(), SemanticError> {
        self.analyze_expression(&expr.expr)
    }

    fn analyze_variable(&mut self, var: &Variable) -> Result<(), SemanticError> {
        self.resolve_variable(&var)
    }

    fn analyze_type_expression(&mut self, expr: &TypeExpression) -> Result<(), SemanticError> {
        self.type_checker
            .check_type_expression(expr)
            .map(drop)
            .map_err(Into::into)
    }

    fn analyze_type_ascription(&mut self, ascription: &TypeAscription) -> Result<(), SemanticError> {
        self.analyze_type_expression(&ascription.expr)
    }

    fn analyze_declaration(&mut self, decl: &Declaration) -> Result<(), SemanticError> {
        match decl {
            Declaration::Class(c) => self.analyze_class_declaration(c),
            Declaration::Function(f) => self.analyze_function_declaration(f, FunctionKind::Normal),
            Declaration::Variable(v) => self.analyze_variable_declaration(v),
        }
    }

    fn analyze_class_declaration(&mut self, class: &ClassDeclaration) -> Result<(), SemanticError> {
        self.current_scope_mut().declare_class(class)?;

        // Handle superclass if present
        let superdecl = match &class.superclass {
            Some(superclass_name) if superclass_name.lexeme == class.name.lexeme => {
                return Err(SemanticError::class("A class can't inherit from itself."));
            }
            Some(superclass_name) => {
                let (decl, scope_distance) = self.resolve_name(&superclass_name.lexeme)?;
                let superdecl = decl.into_class_decl()
                    .ok_or(SemanticError::class("Superclass must be a class."))?;
                self.symbol_table.bind_superclass(class, superdecl, scope_distance);
                Some(superdecl)
            }
            None => None
        };

        // Handle class body and methods
        self.with_new_scope_if(superdecl.is_some(), LexicalScopeKind::Super, |slf| {
            let class_kind = if let Some(superdecl) = superdecl {
                slf.current_scope_mut().declare_and_define_super(superdecl)?;
                ClassKind::Subclass
            } else {
                ClassKind::Normal
            };

            slf.with_new_scope(LexicalScopeKind::Class(class_kind), |slf| {
                slf.current_scope_mut().declare_and_define_this(class)?;

                for method in &class.methods {
                    let kind = if method.name.lexeme == "init" {
                        FunctionKind::Initializer
                    } else {
                        FunctionKind::Method
                    };
                    slf.analyze_function_declaration(&method, kind)?;
                }
                Ok(())
            })
        })?;

        self.current_scope_mut().define(&class.name.lexeme);
        Ok(())
    }

    fn analyze_function_declaration(
        &mut self, 
        decl: &FunctionDeclaration,
        kind: FunctionKind) -> Result<(), SemanticError> {
        self.current_scope_mut().declare_and_define_function(&decl)?;
        self.with_new_scope(LexicalScopeKind::Function(kind), |slf| {
            for param in &decl.parameters {
                slf.analyze_parameter_declaration(&param)?;
            };
            slf.analyze_block_statement(&decl.body)
        })
    }

    fn analyze_parameter_declaration(&mut self, decl: &ParameterDeclaration) -> Result<(), SemanticError> {
        self.current_scope_mut().declare_and_define_parameter(decl)
    }

    fn analyze_variable_declaration(&mut self, decl: &VariableDeclaration) -> Result<(), SemanticError> {
        self.current_scope_mut().declare_variable(decl)?;

        if let Some(ascription) = &decl.ascription {
            self.analyze_type_ascription(ascription)?;
        }

        if let Some(init) = &decl.initializer {
            self.analyze_expression(init)?;
        }
        self.current_scope_mut().define(&decl.name.lexeme);

        self.type_checker.check_variable_declaration(decl)
            .map(drop)
            .map_err(Into::into)
    }

    fn analyze_assert_statement(&mut self, stmt: &AssertStatement) -> Result<(), SemanticError> {
        self.analyze_expression(&stmt.expr)
    }

    fn analyze_block_statement(&mut self, block: &BlockStatement) -> Result<(), SemanticError> {
        self.with_new_scope(LexicalScopeKind::Block, |slf| {
            for stmt in &block.statements {
                slf.analyze_statement(stmt)?;
            }
            Ok(())
        })
    }

    fn analyze_expression_statement(&mut self, stmt: &ExpressionStatement) -> Result<(), SemanticError> {
        self.analyze_expression(&stmt.expr)
    }

    fn analyze_for_statement(&mut self, stmt: &ForStatement) -> Result<(), SemanticError> {
        if let Some(initializer) = &stmt.initializer {
            self.analyze_statement(&*initializer)?;
        }
        if let Some(condition) = &stmt.condition {
            self.analyze_expression(&*condition)?;
        }
        if let Some(increment) = &stmt.increment {
            self.analyze_expression(&*increment)?;
        }
        self.analyze_statement(&*stmt.body)
    }

    fn analyze_if_statement(&mut self, stmt: &IfStatement) -> Result<(), SemanticError> {
        self.analyze_expression(&stmt.condition)?;
        self.analyze_statement(&*stmt.then_branch)?;
        if let Some(else_branch) = &stmt.else_branch {
            self.analyze_statement(&else_branch)?;
        }
        Ok(())
    }

    fn analyze_print_statement(&mut self, stmt: &PrintStatement) -> Result<(), SemanticError> {
        self.analyze_expression(&stmt.expr)
    }

    fn analyze_return_statement(&mut self, stmt: &ReturnStatement) -> Result<(), SemanticError> {
        if !self.is_inside_function() {
            return Err(SemanticError::control_flow("Cannot return from outside of a function."));
        }

        if self.is_inside_initializer() {
            if stmt.expr.is_some() {
                return Err(SemanticError::control_flow("Can't return a value from an initializer."));
            }
        }

        if let Some(expr) = &stmt.expr {
            self.analyze_expression(expr)
        } else {
            Ok(())
        }
    }

    fn analyze_while_statement(&mut self, stmt: &WhileStatement) -> Result<(), SemanticError> {
        self.analyze_expression(&stmt.condition)?;
        self.analyze_statement(&*stmt.body)
    }

    fn analyze_statement(&mut self, stmt: &Statement) -> Result<(), SemanticError> {
        match stmt {
            Statement::Assert(a) => self.analyze_assert_statement(a),
            Statement::Block(b) => self.analyze_block_statement(b),
            Statement::Expr(e) => self.analyze_expression_statement(e),
            Statement::Decl(d) => self.analyze_declaration(d),
            Statement::For(f) => self.analyze_for_statement(f),
            Statement::If(i) => self.analyze_if_statement(i),
            Statement::Print(p) => self.analyze_print_statement(p),
            Statement::Return(r) => self.analyze_return_statement(r),
            Statement::While(w) => self.analyze_while_statement(w),
        }
    }

    pub fn analyze_global_statement(&mut self, stmt: &Statement) -> Result<(), SemanticError> {
        if self.scopes.len() != 1 || self.scopes[0].kind != LexicalScopeKind::Global {
            panic!("Internal compiler error: expected single global scope before global statement.");
        }

        self.analyze_statement(stmt)?;

        if self.scopes.len() != 1 || self.scopes[0].kind != LexicalScopeKind::Global {
            panic!("Internal compiler error: expected single global scope after global statement.");
        }

        Ok(())
    }

    pub fn analyze_program(&mut self, prog: &Program) -> Result<(), SemanticError> {
        for stmt in &prog.statements {
            self.analyze_global_statement(stmt)?;
        }
        Ok(())
    }

    // XXX these functions are used by the Interpreter, and haven't been ported to use SemanticError yet
    pub fn superclass_scope_distance(&self, subclass: &ClassDeclaration) -> Result<usize,String> {
        self.symbol_table.lookup_superclass(subclass).map(|(_,result)| result)
    }

    pub fn super_scope_distance(&self, super_: &SuperExpression) -> Result<usize,String> {
        self.symbol_table.lookup_super(super_).map(|(_,result)| result)
    }

    pub fn this_scope_distance(&self, var: &ThisExpression) -> Result<usize,String> {
        self.symbol_table.lookup_this(var).map(|(_,result)| result)
    }

    pub fn variable_scope_distance(&self, var: &Variable) -> Result<usize,String> {
        self.symbol_table.lookup_variable(var).map(|(_,result)| result)
    }
}
