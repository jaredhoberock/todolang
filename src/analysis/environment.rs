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

    fn lookup(&self, name: &str) -> Result<Entry, Error> {
        if self.scopes.is_empty() {
            panic!("Internal compiler error: cannot resolve name '{}' outside of a scope", name);
        }

        for scope in self.scopes.iter().rev() {
            if let Some(entry) = scope.get(name) {
                return Ok(entry.clone())
            }
        }

        Err(Error::name(format!("cannot find '{}' in this scope", name)))
    }

    pub fn get_definition(&self, name: &str) -> Result<Rc<Declaration>, Error> {
        match self.lookup(name) {
            Ok(Entry::Defined(def)) => Ok(def.clone()),
            Ok(Entry::Declared(_)) => {
                panic!("Internal compiler error: declaration '{}' has no definition", name);
            },
            Err(e) => Err(e),
        }
    }
}
