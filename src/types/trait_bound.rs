use crate::token::Token;
use derive_more::Display;
use std::hash::{Hash, Hasher};
use super::{Kind, Substitution, TypeEnvironment, TypeVar};

#[derive(Clone, Debug, Display)]
#[display(fmt = "{type_var}: {trait_}")]
pub struct TraitBound {
    pub type_var: TypeVar,
    pub trait_: Token,
}

impl PartialEq for TraitBound {
    fn eq(&self, other: &Self) -> bool {
        self.type_var == other.type_var && self.trait_.lexeme == other.trait_.lexeme
    }
}

impl Eq for TraitBound {}

impl Hash for TraitBound {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.type_var.hash(state);
        self.trait_.lexeme.hash(state);
    }
}

impl TraitBound {
    fn new(type_var: TypeVar, trait_: Token) -> Self {
        Self {
            type_var,
            trait_,
        }
    }

    pub fn new_generic(env: &TypeEnvironment, trait_: Token) -> Self {
        Self::new(env.generic().as_type_var().cloned().unwrap(), trait_)
    }

    pub fn apply(&self, mapping: &Substitution) -> Self {
        let new_tv = self.type_var
            .apply(mapping)
            .as_type_var()
            .cloned()
            .unwrap_or(self.type_var.clone());
        Self {
            type_var: new_tv,
            trait_: self.trait_.clone()
        }
    }

    pub fn try_solve(&self, mapping: &mut Substitution) -> Option<bool> {
        let type_ = self.type_var.apply(mapping);
        match (&*type_, self.trait_.lexeme.as_str()) {
            (Kind::Number, "Add") => Some(true),
            (Kind::InferenceVariable(_), _) => None,
            _ => Some(false),
        }
    }
}
