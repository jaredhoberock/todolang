use crate::ast::typed::{Declaration, DeclRef};
use crate::types::{Type, TypeScheme};
use std::collections::HashMap;
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
pub enum Entry {
    // An program-defined entity with a declaration in the AST
    Ast(DeclRef),
    // A built-in entity such as a primitive type
    Builtin(TypeScheme),
}

impl Entry {
    pub fn as_type_scheme(&self) -> Option<TypeScheme> {
        match self {
            Self::Builtin(ty) => Some(ty.clone()),
            Self::Ast(decl) => match &*decl.borrow() {
                Declaration::TypeParameter { type_scheme, .. } => Some(type_scheme.clone()),
                _ => None,
            }
        }
    }

    pub fn as_variable(&self) -> Option<DeclRef> {
        match self {
            Self::Ast(decl) => match *decl.borrow() {
                Declaration::Forward { .. }
                | Declaration::Function { .. }
                | Declaration::Parameter { .. }
                | Declaration::Variable { .. } => Some(decl.clone()),
                _ => None,
            },
            // For now, the only kinds of builtin definitions are TypeSchemes, not values
            Self::Builtin(_) => None,
        }
    }
}

impl From<DeclRef> for Entry {
    fn from(decl: DeclRef) -> Self {
        Entry::Ast(decl)
    }
}

impl From<TypeScheme> for Entry {
    fn from(ty: TypeScheme) -> Self {
        Entry::Builtin(ty)
    }
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
            let ts = TypeScheme::new_unbounded(ty);
            // XXX the entry for name should actually be a type type
            //     i.e., the type of the `String` type is `type`, not `String`
            env.insert(name, ts.into()).ok();
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

    pub fn insert(&mut self, name: &str, entry: Entry) -> Result<(), Error> {
        self.check_unique_name(&name)?;
        self.scopes
            .last_mut()
            .unwrap()
            .insert(name.into(), entry);
        Ok(())
    }

    pub fn insert_in_enclosing_scope(&mut self, name: &str, entry: Entry) -> Result<(), Error> {
        let enclosing_scope_idx = self.scopes.len() - 2;
        let enclosing_scope = self.scopes.get_mut(enclosing_scope_idx)
            .expect("Internal compiler error: no enclosing scope exists");
        if enclosing_scope.contains_key(name) {
            return Err(Error::name(format!("'{}' is already declared in this scope", name)));
        }
        enclosing_scope.insert(name.into(), entry);
        Ok(())
    }
    
    /// Looks up a name in the environment and returns the matching Entry
    /// along with the number of scopes between the current (innermost) scope
    /// and the scope where the name is found (0 means the innermost scope)
    fn get(&self, name: &str) -> Result<(Entry, usize), Error> {
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

    /// Returns the definition of a named variable along with the scope distance.
    pub fn get_variable(&self, name: &str) -> Result<(DeclRef, usize), Error> {
        let (entry, scope_distance) = self.get(name)?;
        entry.as_variable()
            .map(|decl| (decl, scope_distance))
            .ok_or_else(|| Error::name(format!("'{}' does not name a variable", name)))
    }

    /// Looks up a type scheme by name
    pub fn get_type_scheme(&self, name: &str) -> Result<TypeScheme, Error> {
        let (entry, _scope_distance) = self.get(name)?;
        entry.as_type_scheme().ok_or_else(|| Error::name(format!("'{}' does not name a type", name)))
    }
}
