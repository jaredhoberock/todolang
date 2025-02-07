use crate::ast::typed::Declaration;
use crate::types::Type;
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
pub enum Entry {
    Declared(Type),
    Defined(Rc<Declaration>),
}

#[derive(Debug, Clone)]
pub struct Environment {
    scopes: Vec<HashMap<String, Entry>>,
}

impl Environment {
    pub fn new() -> Self {
        Self{ scopes: vec![HashMap::new()], }
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

    pub fn declare(&mut self, name: &str, ty: Type) -> Result<(), Error> {
        self.check_unique_name(&name)?;
        self.scopes
            .last_mut()
            .unwrap()
            .insert(name.into(), Entry::Declared(ty));
        Ok(())
    }
    
    pub fn define(&mut self, name: &str, decl: Rc<Declaration>) {
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
    pub fn get_definition(&self, name: &str) -> Result<(Rc<Declaration>, usize), Error> {
        match self.lookup_with_distance(name)? {
            (Entry::Defined(def), distance) => Ok((def.clone(), distance)),
            (Entry::Declared(_), _) => {
                panic!("Internal compiler error: declaration '{}' has no definition", name);
            },
        }
    }
}
