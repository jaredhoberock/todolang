use crate::declaration_environment::DeclarationEnvironment;
use crate::symbol_table::{NamedEntity, SymbolTable};
use crate::syntax::*;
use std::collections::HashMap;
use std::rc::Rc;
use thiserror::Error;

#[derive(Debug, Error)]
pub enum Error {
    #[error("{0}")]
    Class(String),

    #[error("{0}")]
    ControlFlow(String),

    #[error("Name resolution error: {0}")]
    Name(String),
}

impl Error {
    fn class(msg: impl Into<String>) -> Self {
        Error::Class(msg.into())
    }

    fn control_flow(msg: impl Into<String>) -> Self {
        Error::ControlFlow(msg.into())
    }

    fn name(msg: impl Into<String>) -> Self {
        Error::Name(msg.into())
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

    fn assert_name_is_unique(&self, name: &str) -> Result<(), Error> {
        if self.declarations.contains_key(name) {
            return Err(Error::name(format!("'{}' is already declared in this scope", name)));
        }
        Ok(())
    }

    fn insert(&mut self, name: &str, is_defined: bool, entity: NamedEntity) -> Result<(),Error> {
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
    
    fn declare_builtin_function(&mut self, name: &str) -> Result<(),Error> {
        self.insert(&name, false, NamedEntity::BuiltinFunction)
    }

    fn declare_class(&mut self, decl: &ClassDeclaration) -> Result<(),Error> {
        self.insert(&decl.name.lexeme, false, NamedEntity::Declaration(decl.into()))
    }

    fn declare_and_define_function(&mut self, decl: &FunctionDeclaration) -> Result<(),Error> {
        self.insert(&decl.name.lexeme, true, NamedEntity::Declaration(decl.into()))
    }

    fn declare_and_define_parameter(&mut self, decl: &ParameterDeclaration) -> Result<(),Error> {
        self.insert(&decl.name.lexeme, true, NamedEntity::Parameter(decl.into()))
    }

    fn declare_and_define_super(&mut self, decl: &ClassDeclaration) -> Result<(),Error> {
        self.insert("super", true, NamedEntity::Declaration(decl.into()))
    }

    fn declare_and_define_this(&mut self, decl: &ClassDeclaration) -> Result<(),Error> {
        self.insert("this", true, NamedEntity::Declaration(decl.into()))
    }

    fn declare_variable(&mut self, decl: &VariableDeclaration) -> Result<(),Error> {
        self.insert(&decl.name.lexeme, false, NamedEntity::Declaration(decl.into()))
    }
}


pub struct NameResolver {
    scopes: Vec<LexicalScope>,
    symbol_table: Rc<SymbolTable>,
    decl_env: Rc<DeclarationEnvironment>,
}


impl NameResolver {
    pub fn new(
        symbol_table: Rc<SymbolTable>,
        decl_env: Rc<DeclarationEnvironment>,
        builtin_functions: Vec<String>) -> Self 
    {
        Self {
            scopes: vec![LexicalScope::new_global(builtin_functions)],
            symbol_table,
            decl_env,
        }
    }

    // this wraps the invocation of f(self) in a new scope of the given kind if the condition is true
    // f(self) is invoked in either case
    // returns the result of f(self)
    fn with_new_scope_if<T>(
        &mut self,
        condition: bool,
        kind: LexicalScopeKind,
        f: impl FnOnce(&mut Self) -> Result<T,Error>,
    ) -> Result<T,Error> {
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
        f: impl FnOnce(&mut Self) -> Result<T,Error>,
    ) -> Result<T,Error> {
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
    fn resolve_name(&self, name: &str) -> Result<(NamedEntity, usize), Error> {
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

        Err(Error::name(format!("Name '{}' is undefined", name)))
    }

    fn bind_super(&mut self, super_: &SuperExpression) -> Result<(), Error> {
        let (binding, scope_distance) = self.resolve_name("super")?;
        let class = binding.into_class_decl()
            .expect("Internal compiler error: 'super' resolved to something other than a ClassDeclaration");
        self.symbol_table.bind_super(super_, class, scope_distance);
        Ok(())
    }

    fn bind_this(&mut self, this: &ThisExpression) -> Result<(), Error> {
        let (binding, scope_distance) = self.resolve_name("this")?;
        let class = binding.into_class_decl()
            .expect("Internal compiler error: 'this' resolved to something other than a ClassDeclaration");
        self.symbol_table.bind_this(this, class, scope_distance);
        Ok(())
    }

    fn bind_variable(&mut self, var: &Variable) -> Result<(), Error> {
        let (binding, scope_distance) = self.resolve_name(&var.name.lexeme)?;
        self.symbol_table.bind_variable(var, binding, scope_distance);
        Ok(())
    }

    fn resolve_expression(&mut self, expr: &Expression) -> Result<(), Error> {
        match expr {
            Expression::Assignment(a) => self.resolve_assignment_expression(a),
            Expression::Binary(b) => self.resolve_binary_expression(b),
            Expression::Call(c) => self.resolve_call_expression(c),
            Expression::Get(g) => self.resolve_get_expression(g),
            Expression::Grouping(g) => self.resolve_grouping_expression(g),
            Expression::Literal(_) => Ok(()),
            Expression::Logical(l) => self.resolve_logical_expression(l),
            Expression::Match(m) => self.resolve_match_expression(m),
            Expression::Set(s) => self.resolve_set_expression(s),
            Expression::Super(s) => self.resolve_super_expression(s),
            Expression::This(t) => self.resolve_this_expression(t),
            Expression::Unary(u) => self.resolve_unary_expression(u),
            Expression::Variable(v) => self.resolve_variable(v),
        }
    }

    fn resolve_assignment_expression(&mut self, expr: &AssignmentExpression) -> Result<(), Error> {
        self.resolve_expression(&expr.expr)?;
        self.resolve_variable(&expr.var)
    }

    fn resolve_binary_expression(&mut self, expr: &BinaryExpression) -> Result<(), Error> {
        self.resolve_expression(&expr.left_expr)?;
        self.resolve_expression(&expr.right_expr)
    }

    fn resolve_call_expression(&mut self, call: &CallExpression) -> Result<(), Error> {
        self.resolve_expression(&call.callee)?;
        for arg in &call.arguments {
            self.resolve_expression(arg)?;
        }
        Ok(())
    }

    fn resolve_get_expression(&mut self, expr: &GetExpression) -> Result<(), Error> {
        self.resolve_expression(&expr.object)
    }

    fn resolve_grouping_expression(&mut self, expr: &GroupingExpression) -> Result<(), Error> {
        self.resolve_expression(&expr.expr)
    }

    fn resolve_logical_expression(&mut self, expr: &LogicalExpression) -> Result<(), Error> {
        self.resolve_expression(&expr.left_expr)?;
        self.resolve_expression(&expr.right_expr)
    }

    fn resolve_match_expression(&mut self, expr: &MatchExpression) -> Result<(), Error> {
        self.resolve_expression(&expr.scrutinee)?;
        for arm in &expr.arms {
            self.resolve_match_arm(arm)?;
        }
        Ok(())
    }

    fn resolve_match_arm(&mut self, arm: &MatchArm) -> Result<(), Error> {
        self.with_new_scope(LexicalScopeKind::MatchArm, |slf| {
            slf.resolve_pattern(&arm.pattern)?;
            slf.resolve_expression(&arm.expr)
        })
    }

    fn resolve_pattern(&mut self, pattern: &Pattern) -> Result<(), Error> {
        match pattern {
            Pattern::Literal(_) => Ok(()),
            Pattern::Underscore => Ok(())
        }
    }

    fn resolve_set_expression(&mut self, set: &SetExpression) -> Result<(), Error> {
        self.resolve_expression(&set.value)?;
        self.resolve_expression(&*set.object)
    }

    fn resolve_super_expression(&mut self, super_: &SuperExpression) -> Result<(), Error> {
        match self.innermost_class_scope() {
            Some(scope) if matches!(scope.kind, LexicalScopeKind::Class(ClassKind::Subclass)) => {
                self.bind_super(&super_).map_err(Into::into)
            }
            Some(_) => Err(Error::name("Can't use 'super' in a class with no superclass.")),
            None    => Err(Error::name("Can't use 'super' outside of a class.")),
        }
    }

    fn resolve_this_expression(&mut self, this: &ThisExpression) -> Result<(), Error> {
        if !self.is_inside_class() {
            return Err(Error::name("Can't use 'this' outside of a class"));
        }
        self.bind_this(&this).map_err(Into::into)
    }

    fn resolve_unary_expression(&mut self, expr: &UnaryExpression) -> Result<(), Error> {
        self.resolve_expression(&expr.expr)
    }

    fn resolve_variable(&mut self, var: &Variable) -> Result<(), Error> {
        self.bind_variable(&var)
    }

    fn resolve_type_expression(&mut self, _: &TypeExpression) -> Result<(), Error> {
        Ok(())
    }

    fn resolve_type_ascription(&mut self, ascription: &TypeAscription) -> Result<(), Error> {
        self.resolve_type_expression(&ascription.expr)
    }

    fn resolve_declaration(&mut self, decl: &Declaration) -> Result<(), Error> {
        // create a new declaration in the environment
        self.decl_env.new_decl(decl.into());

        match decl {
            Declaration::Class(c)    => self.resolve_class_declaration(c),
            Declaration::Function(f) => self.resolve_function_declaration(f, FunctionKind::Normal),
            Declaration::Variable(v) => self.resolve_variable_declaration(v),
        }
    }

    fn resolve_class_declaration(&mut self, class: &ClassDeclaration) -> Result<(), Error> {
        self.current_scope_mut().declare_class(class)?;

        // Handle superclass if present
        let superdecl = match &class.superclass {
            Some(superclass_name) if superclass_name.lexeme == class.name.lexeme => {
                return Err(Error::class("A class can't inherit from itself."));
            }
            Some(superclass_name) => {
                let (decl, scope_distance) = self.resolve_name(&superclass_name.lexeme)?;
                let superdecl = decl.into_class_decl()
                    .ok_or(Error::class("Superclass must be a class."))?;
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
                    slf.resolve_function_declaration(&method, kind)?;
                }
                Ok(())
            })
        })?;

        self.current_scope_mut().define(&class.name.lexeme);
        Ok(())
    }

    fn resolve_function_declaration(
        &mut self, 
        decl: &FunctionDeclaration,
        kind: FunctionKind) -> Result<(), Error> {
        self.current_scope_mut().declare_and_define_function(&decl)?;
        self.with_new_scope(LexicalScopeKind::Function(kind), |slf| {
            for param in &decl.parameters {
                slf.resolve_parameter_declaration(&param)?;
            };
            slf.resolve_block_statement(&decl.body)
        })
    }

    fn resolve_parameter_declaration(&mut self, decl: &ParameterDeclaration) -> Result<(), Error> {
        // create a new declaration in the environment
        self.decl_env.new_parameter_decl(decl.into());

        // declare the parameter in the current scope
        self.current_scope_mut().declare_and_define_parameter(decl)
    }

    fn resolve_variable_declaration(&mut self, decl: &VariableDeclaration) -> Result<(), Error> {
        self.current_scope_mut().declare_variable(decl)?;

        if let Some(ascription) = &decl.ascription {
            self.resolve_type_ascription(ascription)?;
        }

        if let Some(init) = &decl.initializer {
            self.resolve_expression(init)?;
        }
        self.current_scope_mut().define(&decl.name.lexeme);

        Ok(())
    }

    fn resolve_assert_statement(&mut self, stmt: &AssertStatement) -> Result<(), Error> {
        self.resolve_expression(&stmt.expr)
    }

    fn resolve_block_statement(&mut self, block: &BlockStatement) -> Result<(), Error> {
        self.with_new_scope(LexicalScopeKind::Block, |slf| {
            for stmt in &block.statements {
                slf.resolve_statement(stmt)?;
            }
            Ok(())
        })
    }

    fn resolve_expression_statement(&mut self, stmt: &ExpressionStatement) -> Result<(), Error> {
        self.resolve_expression(&stmt.expr)
    }

    fn resolve_for_statement(&mut self, stmt: &ForStatement) -> Result<(), Error> {
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

    fn resolve_if_statement(&mut self, stmt: &IfStatement) -> Result<(), Error> {
        self.resolve_expression(&stmt.condition)?;
        self.resolve_statement(&*stmt.then_branch)?;
        if let Some(else_branch) = &stmt.else_branch {
            self.resolve_statement(&else_branch)?;
        }
        Ok(())
    }

    fn resolve_print_statement(&mut self, stmt: &PrintStatement) -> Result<(), Error> {
        self.resolve_expression(&stmt.expr)
    }

    fn resolve_return_statement(&mut self, stmt: &ReturnStatement) -> Result<(), Error> {
        if !self.is_inside_function() {
            return Err(Error::control_flow("Cannot return from outside of a function."));
        }

        if self.is_inside_initializer() {
            if stmt.expr.is_some() {
                return Err(Error::control_flow("Can't return a value from an initializer."));
            }
        }

        if let Some(expr) = &stmt.expr {
            self.resolve_expression(expr)
        } else {
            Ok(())
        }
    }

    fn resolve_while_statement(&mut self, stmt: &WhileStatement) -> Result<(), Error> {
        self.resolve_expression(&stmt.condition)?;
        self.resolve_statement(&*stmt.body)
    }

    fn resolve_statement(&mut self, stmt: &Statement) -> Result<(), Error> {
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

    pub fn resolve_global_statement(&mut self, stmt: &Statement) -> Result<(), Error> {
        if self.scopes.len() != 1 || self.scopes[0].kind != LexicalScopeKind::Global {
            panic!("Internal compiler error: expected single global scope before global statement.");
        }

        self.resolve_statement(stmt)?;

        if self.scopes.len() != 1 || self.scopes[0].kind != LexicalScopeKind::Global {
            panic!("Internal compiler error: expected single global scope after global statement.");
        }

        Ok(())
    }
}
