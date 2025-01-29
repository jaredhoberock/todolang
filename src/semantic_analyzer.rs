use std::collections::HashMap;
use std::ptr::NonNull;

use crate::syntax::*;

// XXX we need to scrub the ast lifetime from this file

#[derive(Copy, Clone)]
enum DeclRef<'ast> {
    BuiltinFunction,
    Class(&'ast ClassDeclaration),
    Function,
    Parameter,
    Variable,
}

struct SymbolTableEntry<'ast> {
    decl: DeclRef<'ast>,
    scope_distance: usize,
}

#[derive(Eq, Hash, PartialEq)]
enum Symbol {
    Subclass(NonNull<ClassDeclaration>),
    Super(NonNull<SuperExpression>),
    This(NonNull<ThisExpression>),
    Variable(NonNull<Variable>),
}

impl Symbol {
    pub fn from_super(super_: &SuperExpression) -> Self {
        Self::Super(NonNull::from(super_))
    }

    pub fn from_subclass(subclass: &ClassDeclaration) -> Self {
        Self::Subclass(NonNull::from(subclass))
    }

    pub fn from_this(this: &ThisExpression) -> Self {
        Self::This(NonNull::from(this))
    }

    pub fn from_variable(var: &Variable) -> Self {
        Self::Variable(NonNull::from(var))
    }

    pub fn name(&self) -> &str {
        match self {
            Symbol::Subclass(subclass) => unsafe { &subclass.as_ref().name.lexeme},
            Symbol::Super(_) => "super",
            Symbol::This(_) => "this",
            Symbol::Variable(var) => unsafe { &var.as_ref().name.lexeme},
        }
    }
}

struct SymbolTable<'ast> {
    symbols: HashMap<Symbol, SymbolTableEntry<'ast>>,
}

impl<'ast> SymbolTable<'ast> {
    pub fn new() -> Self {
        Self { 
            symbols: HashMap::new(),
        }
    }

    pub fn insert_superclass(&mut self, subclass: &'ast ClassDeclaration, superclass: &'ast ClassDeclaration, scope_distance: usize) {
        let entry = SymbolTableEntry{ decl: DeclRef::Class(superclass), scope_distance };
        self.symbols.insert(Symbol::Subclass(NonNull::from(subclass)), entry);
    }

    pub fn insert_super(&mut self, super_: &'ast SuperExpression, decl: &'ast ClassDeclaration, scope_distance: usize) {
        let entry = SymbolTableEntry{ decl: DeclRef::Class(decl), scope_distance };
        self.symbols.insert(Symbol::Super(NonNull::from(super_)), entry);
    }

    pub fn insert_this(&mut self, this: &'ast ThisExpression, decl: &'ast ClassDeclaration, scope_distance: usize) {
        let entry = SymbolTableEntry { decl: DeclRef::Class(decl), scope_distance };
        self.symbols.insert(Symbol::This(NonNull::from(this)), entry);
    }

    pub fn insert_variable(&mut self, var: &'ast Variable, decl: DeclRef<'ast>, scope_distance: usize) {
        let entry = SymbolTableEntry { decl, scope_distance };
        self.symbols.insert(Symbol::Variable(NonNull::from(var)), entry);
    }

    fn get(&self, symbol: Symbol) -> Result<&SymbolTableEntry<'ast>, String> {
        self.symbols
            .get(&symbol)
            .ok_or(format!("Internal error: '{}' was not found in symbol table", symbol.name()))
    }

    pub fn get_superclass(&self, subclass: &'ast ClassDeclaration) -> Result<(&'ast ClassDeclaration, usize), String> {
        let entry = self.get(Symbol::from_subclass(subclass))?;
        match entry.decl {
            DeclRef::Class(superclass) => Ok((superclass, entry.scope_distance)),
            _ => Err("Internal error: super class mapped to non-class declaration".to_string()),
        }
    }

    pub fn get_super(&self, super_: &'ast SuperExpression) -> Result<(&'ast ClassDeclaration, usize), String> {
        let entry = self.get(Symbol::from_super(super_))?;
        match entry.decl {
            DeclRef::Class(class) => Ok((class, entry.scope_distance)),
            _ => Err("Internal error: super expression mapped to non-class declaration".to_string()),
        }
    }

    pub fn get_this(&self, this: &'ast ThisExpression) -> Result<(&'ast ClassDeclaration, usize), String> {
        let entry = self.get(Symbol::from_this(this))?;
        match entry.decl {
            DeclRef::Class(class) => Ok((class, entry.scope_distance)),
            _ => Err("Internal error: this expression mapped to non-class declaration".to_string()),
        }
    }

    pub fn get_variable(&self, var: &'ast Variable) -> Result<(DeclRef<'ast>, usize), String> {
        let entry = self.get(Symbol::from_variable(var))?;
        Ok((entry.decl, entry.scope_distance))
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

struct LexicalScopeEntry<'ast> {
    is_defined: bool,
    decl: DeclRef<'ast>,
}

// a LexicalScope is a collection of declarations
// it provides methods for introducing different kinds of declarations
struct LexicalScope<'ast> {
    pub kind: LexicalScopeKind,

    // maps a name in this scope to metadata about its declaration
    pub declarations: HashMap<String, LexicalScopeEntry<'ast>>,
}

impl<'ast> LexicalScope<'ast> {
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

    fn assert_name_is_unique(&self, name: &str) -> Result<(), String> {
        if self.declarations.contains_key(name) {
            return Err(format!("'{}' is already declared in this scope", name));
        }
        Ok(())
    }

    fn insert(&mut self, name: &str, is_defined: bool, decl: DeclRef<'ast>) -> Result<(),String> {
        self.assert_name_is_unique(name)?;
        self.declarations.insert(name.to_string(), LexicalScopeEntry { is_defined, decl });
        Ok(())
    }

    fn define(&mut self, name: &str) -> Result<(),String> {
        if let Some(entry) = self.declarations.get_mut(name) {
            if entry.is_defined {
                return Err(format!("Internal error: cannot redefine '{}'", name))
            }
            entry.is_defined = true;
            Ok(())
        } else {
            Err(format!("Internal error: cannot define '{}' before declaration", name))
        }
    }

    fn declare_builtin_function(&mut self, name: &str) -> Result<(),String> {
        self.insert(&name, false, DeclRef::BuiltinFunction)
    }

    fn declare_class(&mut self, decl: &'ast ClassDeclaration) -> Result<(),String> {
        self.insert(&decl.name.lexeme, false, DeclRef::Class(decl))
    }

    fn declare_and_define_function(&mut self, decl: &'ast FunctionDeclaration) -> Result<(), String> {
        self.insert(&decl.name.lexeme, true, DeclRef::Function)
    }

    fn declare_and_define_parameter(&mut self, decl: &'ast ParameterDeclaration) -> Result<(), String> {
        self.insert(&decl.name.lexeme, true, DeclRef::Parameter)
    }

    fn declare_and_define_super(&mut self, decl: &'ast ClassDeclaration) -> Result<(), String> {
        self.insert("super", true, DeclRef::Class(decl))
    }

    fn declare_and_define_this(&mut self, decl: &'ast ClassDeclaration) -> Result<(), String> {
        self.insert("this", true, DeclRef::Class(decl))
    }

    fn declare_variable(&mut self, decl: &'ast VariableDeclaration) -> Result<(), String> {
        self.insert(&decl.name.lexeme, false, DeclRef::Variable)
    }
}


pub struct SemanticAnalyzer<'ast> {
    symbol_table: SymbolTable<'ast>,
    scopes: Vec<LexicalScope<'ast>>,
}


impl<'ast> SemanticAnalyzer<'ast> {
    pub fn new(builtin_functions: Vec<String>) -> Self {
        Self {
            symbol_table: SymbolTable::new(),
            scopes: vec![LexicalScope::new_global(builtin_functions)],
        }
    }

    // this wraps the invocation of f(self) in a new scope of the given kind if the condition is true
    // f(self) is invoked in either case
    // returns the result of f(self)
    fn with_new_scope_if<T>(
        &mut self,
        condition: bool,
        kind: LexicalScopeKind,
        f: impl FnOnce(&mut Self) -> Result<T, String>,
    ) -> Result<T, String> {
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
        f: impl FnOnce(&mut Self) -> Result<T, String>,
    ) -> Result<T, String> {
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

    fn current_scope_mut(&mut self) -> Result<&mut LexicalScope<'ast>, String> {
        self.scopes.last_mut().ok_or("Internal error: outside of a scope".to_string())
    }

    // name resolution searches the stack of scopes for a declaration of the given name
    // if such a declaration exists, it returns a pair of
    // 1. A reference to the declaration, and
    // 2. the distance to the scope containing that declaration
    fn resolve_name(&self, name: &str) -> Result<(DeclRef<'ast>, usize), String> {
        if self.scopes.is_empty() {
            return Err("Internal error: cannot resolve name outside of a scope".to_string());
        }

        // look through each scope, starting at the top of
        // the stack for a declaration for name, and note
        // the location of its scope if found
        for (i, scope) in self.scopes.iter().enumerate().rev() {
            if let Some(entry) = scope.declarations.get(name) {
                return Ok((entry.decl, self.scopes.len() - 1 - i))
            }
        }

        Err(format!(
            "Name resolution error: Name '{}' is undefined",
            name
        ))
    }

    fn resolve_super(&mut self, super_: &'ast SuperExpression) -> Result<(), String> {
        let (decl, scope_distance) = self.resolve_name("super")?;
        match decl {
            DeclRef::Class(c) => {
                self.symbol_table.insert_super(super_, c, scope_distance);
                Ok(())
            },
            _ => Err("Internal error: 'super' resolved to something other than a ClassDeclaration".to_string()),
        }
    }

    fn resolve_this(&mut self, this: &'ast ThisExpression) -> Result<(), String> {
        let (decl, scope_distance) = self.resolve_name("this")?;
        match decl {
            DeclRef::Class(c) => {
                self.symbol_table.insert_this(this, c, scope_distance);
                Ok(())
            },
            _ => Err("Internal error: 'this' resolved to something other than a ClassDeclaration".to_string()),
        }
    }

    fn resolve_variable(&mut self, var: &'ast Variable) -> Result<(), String> {
        let (decl, scope_distance) = self.resolve_name(&var.name.lexeme)?;
        self.symbol_table.insert_variable(var, decl, scope_distance);
        Ok(())
    }

    fn analyze_literal(&mut self, _lit: &'ast Literal) -> Result<(), String> {
        // XXX TODO we would return the type of literal here
        Ok(())
    }

    fn analyze_expression(&mut self, expr: &'ast Expression) -> Result<(), String> {
        match expr {
            Expression::Assignment(a) => self.analyze_assignment_expression(a),
            Expression::Binary(b) => self.analyze_binary_expression(b),
            Expression::Call(c) => self.analyze_call_expression(c),
            Expression::Get(g) => self.analyze_get_expression(g),
            Expression::Grouping(g) => self.analyze_grouping_expression(g),
            Expression::Literal(l) => self.analyze_literal(l),
            Expression::Logical(l) => self.analyze_logical_expression(l),
            Expression::Match(m) => self.analyze_match_expression(m),
            Expression::Set(s) => self.analyze_set_expression(s),
            Expression::Super(s) => self.analyze_super_expression(s),
            Expression::This(t) => self.analyze_this_expression(t),
            Expression::Unary(u) => self.analyze_unary_expression(u),
            Expression::Variable(v) => self.analyze_variable(v),
        }
    }

    fn analyze_assignment_expression(&mut self, expr: &'ast AssignmentExpression) -> Result<(), String> {
        // XXX TODO type check 
        self.analyze_expression(&expr.expr)?;
        self.analyze_variable(&expr.var)
    }

    fn analyze_binary_expression(&mut self, expr: &'ast BinaryExpression) -> Result<(), String> {
        self.analyze_expression(&expr.left_expr)?;
        self.analyze_expression(&expr.right_expr)
    }

    fn analyze_call_expression(&mut self, call: &'ast CallExpression) -> Result<(), String> {
        // XXX TODO check that callee is a function
        self.analyze_expression(&call.callee)?;
        for arg in &call.arguments {
            self.analyze_expression(arg)?;
        }
        Ok(())
    }

    fn analyze_get_expression(&mut self, expr: &'ast GetExpression) -> Result<(), String> {
        self.analyze_expression(&expr.object)
    }

    fn analyze_grouping_expression(&mut self, expr: &'ast GroupingExpression) -> Result<(), String> {
        self.analyze_expression(&expr.expr)
    }

    fn analyze_logical_expression(&mut self, expr: &'ast LogicalExpression) -> Result<(), String> {
        self.analyze_expression(&expr.left_expr)?;
        self.analyze_expression(&expr.right_expr)
    }

    fn analyze_match_expression(&mut self, expr: &'ast MatchExpression) -> Result<(), String> {
        self.analyze_expression(&expr.scrutinee)?;
        for arm in &expr.arms {
            self.analyze_match_arm(arm)?;
        }
        Ok(())
    }

    fn analyze_match_arm(&mut self, arm: &'ast MatchArm) -> Result<(), String> {
        self.with_new_scope(LexicalScopeKind::MatchArm, |slf| {
          slf.analyze_pattern(&arm.pattern)?;
          slf.analyze_expression(&arm.expr)
        })
    }

    fn analyze_pattern(&mut self, pattern: &'ast Pattern) -> Result<(), String> {
        match pattern {
            Pattern::Literal(l) => self.analyze_literal(l),
            Pattern::Underscore => Ok(())
        }
    }

    fn analyze_set_expression(&mut self, set: &'ast SetExpression) -> Result<(), String> {
        // XXX TODO type check
        self.analyze_expression(&set.value)?;
        self.analyze_expression(&*set.object)
    }

    fn analyze_super_expression(&mut self, super_: &'ast SuperExpression) -> Result<(), String> {
        match self.innermost_class_scope() {
            Some(scope) if matches!(scope.kind, LexicalScopeKind::Class(ClassKind::Subclass)) => {
                self.resolve_super(&super_)
            }
            Some(_) => Err("Can't user 'super' in a class with no superclass.".to_string()),
            None => Err("Can't use 'super' outside of a class.".to_string()),
        }
    }

    fn analyze_this_expression(&mut self, this: &'ast ThisExpression) -> Result<(), String> {
        if !self.is_inside_class() {
            return Err("Can't use 'this' outside of a class".to_string());
        }
        self.resolve_this(&this)
    }

    fn analyze_unary_expression(&mut self, expr: &'ast UnaryExpression) -> Result<(), String> {
        self.analyze_expression(&expr.expr)
    }

    fn analyze_variable(&mut self, var: &'ast Variable) -> Result<(), String> {
        self.resolve_variable(&var)
    }

    fn analyze_declaration(&mut self, decl: &'ast Declaration) -> Result<(), String> {
        match decl {
            Declaration::Class(c) => self.analyze_class_declaration(c),
            Declaration::Function(f) => self.analyze_function_declaration(f, FunctionKind::Normal),
            Declaration::Variable(v) => self.analyze_variable_declaration(v),
        }
    }

    fn analyze_class_declaration(&mut self, class: &'ast ClassDeclaration) -> Result<(), String> {
        self.current_scope_mut()?.declare_class(class)?;

        // Handle superclass if present
        let superdecl = match &class.superclass {
            Some(superclass_name) if superclass_name.lexeme == class.name.lexeme => {
                return Err("A class can't inherit from itself.".to_string());
            }
            Some(superclass_name) => {
                let (decl, scope_distance) = self.resolve_name(&superclass_name.lexeme)?;
                match decl {
                    DeclRef::Class(superdecl) => {
                        self.symbol_table.insert_superclass(class, superdecl, scope_distance);
                        Some(superdecl)
                    }
                    _ => return Err("Superclass must be a class.".to_string())
                }
            }
            None => None
        };

        // Handle class body and methods
        self.with_new_scope_if(superdecl.is_some(), LexicalScopeKind::Super, |slf| {
            let class_kind = if let Some(superdecl) = superdecl {
                slf.current_scope_mut()?.declare_and_define_super(superdecl)?;
                ClassKind::Subclass
            } else {
                ClassKind::Normal
            };

            slf.with_new_scope(LexicalScopeKind::Class(class_kind), |slf| {
                slf.current_scope_mut()?.declare_and_define_this(class)?;

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

        self.current_scope_mut()?.define(&class.name.lexeme)
    }

    fn analyze_function_declaration(
        &mut self, 
        decl: &'ast FunctionDeclaration,
        kind: FunctionKind) -> Result<(), String> {
        self.current_scope_mut()?.declare_and_define_function(&decl)?;
        self.with_new_scope(LexicalScopeKind::Function(kind), |slf| {
            for param in &decl.parameters {
                slf.analyze_parameter_declaration(&param)?;
            };
            slf.analyze_block_statement(&decl.body)
        })
    }

    fn analyze_parameter_declaration(&mut self, decl: &'ast ParameterDeclaration) -> Result<(), String> {
        self.current_scope_mut()?.declare_and_define_parameter(decl)
    }

    fn analyze_variable_declaration(&mut self, decl: &'ast VariableDeclaration) -> Result<(), String> {
        self.current_scope_mut()?.declare_variable(decl)?;
        if let Some(init) = &decl.initializer {
            // XXX TODO check that the type of the variable's
            //     declaration matches the type of the initializer expression
            self.analyze_expression(init)?;
        }
        self.current_scope_mut()?.define(&decl.name.lexeme)
    }

    fn analyze_assert_statement(&mut self, stmt: &'ast AssertStatement) -> Result<(), String> {
        self.analyze_expression(&stmt.expr)
    }

    fn analyze_block_statement(&mut self, block: &'ast BlockStatement) -> Result<(), String> {
        self.with_new_scope(LexicalScopeKind::Block, |slf| {
            for stmt in &block.statements {
                slf.analyze_statement(stmt)?;
            }
            Ok(())
        })
    }

    fn analyze_expression_statement(&mut self, stmt: &'ast ExpressionStatement) -> Result<(), String> {
        self.analyze_expression(&stmt.expr)
    }

    fn analyze_for_statement(&mut self, stmt: &'ast ForStatement) -> Result<(), String> {
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

    fn analyze_if_statement(&mut self, stmt: &'ast IfStatement) -> Result<(), String> {
        self.analyze_expression(&stmt.condition)?;
        self.analyze_statement(&*stmt.then_branch)?;
        if let Some(else_branch) = &stmt.else_branch {
            self.analyze_statement(&else_branch)?;
        }
        Ok(())
    }

    fn analyze_print_statement(&mut self, stmt: &'ast PrintStatement) -> Result<(), String> {
        self.analyze_expression(&stmt.expr)
    }

    fn analyze_return_statement(&mut self, stmt: &'ast ReturnStatement) -> Result<(), String> {
        if !self.is_inside_function() {
            return Err("Cannot return from outside of a function.".to_string());
        }

        if self.is_inside_initializer() {
            if stmt.expr.is_some() {
                return Err("Can't return a value from an initializer.".to_string());
            }
        }

        if let Some(expr) = &stmt.expr {
            self.analyze_expression(expr)
        } else {
            Ok(())
        }
    }

    fn analyze_while_statement(&mut self, stmt: &'ast WhileStatement) -> Result<(), String> {
        self.analyze_expression(&stmt.condition)?;
        self.analyze_statement(&*stmt.body)
    }

    fn analyze_statement(&mut self, stmt: &'ast Statement) -> Result<(), String> {
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

    pub fn analyze_global_statement(&mut self, stmt: &'ast Statement) -> Result<(), String> {
        if !(self.scopes.len() == 1 && self.scopes[0].kind == LexicalScopeKind::Global) {
            return Err(
                "Internal error: expected single global scope before global statement.".to_string(),
            );
        }

        self.analyze_statement(stmt)?;

        if self.scopes.len() == 1 && self.scopes[0].kind == LexicalScopeKind::Global {
            Ok(())
        } else {
            Err("Internal error: expected single global scope after global statement.".to_string())
        }
    }

    pub fn analyze_program(&mut self, prog: &'ast Program) -> Result<(), String> {
        for stmt in &prog.statements {
            self.analyze_global_statement(stmt)?;
        }
        Ok(())
    }

    pub fn superclass_scope_distance(&self, subclass: &'ast ClassDeclaration) -> Result<usize,String> {
        self.symbol_table.get_superclass(subclass).map(|(_,result)| result)
    }

    pub fn super_scope_distance(&self, super_: &'ast SuperExpression) -> Result<usize,String> {
        self.symbol_table.get_super(super_).map(|(_,result)| result)
    }

    pub fn this_scope_distance(&self, var: &'ast ThisExpression) -> Result<usize,String> {
        self.symbol_table.get_this(var).map(|(_,result)| result)
    }

    pub fn variable_scope_distance(&self, var: &'ast Variable) -> Result<usize,String> {
        self.symbol_table.get_variable(var).map(|(_,result)| result)
    }
}
