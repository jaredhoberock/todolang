use crate::ast::typed::Declaration;
use crate::types::{Type, TypeScheme};
use std::collections::HashMap;
use std::rc::Rc;
use thiserror::Error;

#[derive(Debug, Clone, Error)]
pub enum Error {
    #[error("Name resolution error: {0}")]
    Name(String),
}

impl Error {
    fn name(msg: impl Into<String>) -> Self {
        Error::Name(msg.into())
    }
}

#[derive(Debug, Clone)]
pub enum Definition {
    // An program-defined entity with a declaration in the AST
    Ast(Rc<Declaration>),
    // A built-in entity such as a primitive type
    Builtin(TypeScheme),
}

impl From<Rc<Declaration>> for Definition {
    fn from(decl: Rc<Declaration>) -> Self {
        Definition::Ast(decl)
    }
}

impl From<TypeScheme> for Definition {
    fn from(ty: TypeScheme) -> Self {
        Definition::Builtin(ty)
    }
}

impl Definition {
    pub fn as_type_scheme(&self) -> Option<TypeScheme> {
        match self {
            Definition::Builtin(ty) => Some(ty.clone()),
            Definition::Ast(decl) => match decl.as_ref() {
                Declaration::TypeParameter { type_scheme, .. } => Some(type_scheme.clone()),
                _ => None,
            }
        }
    }

    pub fn as_variable(&self) -> Option<Rc<Declaration>> {
        match self {
            Definition::Ast(decl) => match decl.as_ref() {
                Declaration::Function { .. }
                | Declaration::Parameter { .. }
                | Declaration::Variable { .. } => Some(decl.clone()),
                _ => None,
            },
            // For now, the only kinds of builtin definitions are TypeSchemes, not values
            Definition::Builtin(_) => None,
        }
    }
}

#[derive(Debug, Clone)]
pub enum Entry {
    // an entry in the environment is either a declaration with a (possibly-tentative) TypeScheme,
    Declared(TypeScheme),
    // or an entry is defined with a definition
    Defined(Definition),
}

#[derive(Debug, Clone)]
pub struct Environment {
    scopes: Vec<HashMap<String, Entry>>,
}

impl Environment {
    pub fn new() -> Self {
        Self{ scopes: vec![HashMap::new()], }
    }

    pub fn new_with_builtin_types(types: Vec<(&str, Type)>) -> Self {
        let mut env = Environment::new();
        for (name, ty) in types {
            let ts = TypeScheme::new_unconstrained(ty);
            // XXX the declaration for name should actually be a type type
            //     i.e., the type of the `String` type is `type`, not `String`
            env.declare(name, ts.clone()).ok();
            env.define(name, Definition::Builtin(ts));
        }
        env
    }

    pub fn enter_scope(&mut self) {
        self.scopes.push(HashMap::new())
    }

    pub fn exit_scope(&mut self) {
        self.scopes.pop();
    }

    pub fn check_unique_name(&self, name: &str) -> Result<(), Error> {
        if let Some(current_scope) = self.scopes.last() {
            if current_scope.contains_key(name) {
                return Err(Error::name(format!("'{}' is already declared in this scope", name)));
            }
        }
        Ok(())
    }

    pub fn declare(&mut self, name: &str, ty: TypeScheme) -> Result<(), Error> {
        self.check_unique_name(&name)?;
        self.scopes
            .last_mut()
            .unwrap()
            .insert(name.into(), Entry::Declared(ty));
        Ok(())
    }

    pub fn declare_in_enclosing_scope(&mut self, name: &str, ty: TypeScheme) -> Result<(), Error> {
        let enclosing_scope_idx = self.scopes.len() - 2;
        let enclosing_scope = self.scopes.get_mut(enclosing_scope_idx)
            .expect("Internal compiler error: no enclosing scope exists");
        if enclosing_scope.contains_key(name) {
            return Err(Error::name(format!("'{}' is already declared in this scope", name)));
        }
        enclosing_scope.insert(name.into(), Entry::Declared(ty));
        Ok(())
    }
    
    pub fn define(&mut self, name: &str, decl: Definition) {
        let scope = self.scopes.last_mut().unwrap();

        match scope.get(name) {
            Some(Entry::Declared(_)) => {
                scope.insert(name.to_string(), Entry::Defined(decl));
            },
            Some(Entry::Defined(_)) => {
                panic!("Internal compiler error: cannot redefine '{}'", name);
            },
            None => {
                panic!("Internal compiler error: cannot define '{}' before declaration", name);
            }
        }
    }

    /// Looks up a name in the environment and returns the matching Entry
    /// along with the number of scopes between the current (innermost) scope
    /// and the scope where the name is found (0 means the innermost scope)
    fn lookup_with_distance(&self, name: &str) -> Result<(Entry, usize), Error> {
        if self.scopes.is_empty() {
            panic!("Internal compiler error: cannot resolve name '{}' outside of a scope", name);
        }

        for (distance, scope) in self.scopes.iter().rev().enumerate() {
            if let Some(entry) = scope.get(name) {
                return Ok((entry.clone(), distance))
            }
        }

        Err(Error::name(format!("cannot find '{}' in this scope", name)))
    }

    /// Returns the definition for a name along with the scope distance.
    /// The returned tuple contains the defined Declaration and the scope distance.
    fn get_definition(&self, name: &str) -> Result<(Definition, usize), Error> {
        match self.lookup_with_distance(name)? {
            (Entry::Defined(def), distance) => Ok((def.clone(), distance)),
            (Entry::Declared(_), _) => {
                panic!("Internal compiler error: declaration '{}' has no definition", name);
            },
        }
    }

    /// Returns the definition of a named variable along with the scope distance.
    pub fn get_variable(&self, name: &str) -> Result<(Rc<Declaration>, usize), Error> {
        let (def, scope_distance) = self.get_definition(name)?;
        def.as_variable()
            .map(|decl| (decl, scope_distance))
            .ok_or_else(|| Error::name(format!("'{}' does not name a variable", name)))
    }

    /// Looks up a type scheme by name
    pub fn get_type_scheme(&self, name: &str) -> Result<TypeScheme, Error> {
        let (def, _scope_distance) = self.get_definition(name)?;
        def.as_type_scheme().ok_or_else(|| Error::name(format!("'{}' does not name a type", name)))
    }
}
