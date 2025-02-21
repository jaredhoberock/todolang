use crate::token::Token;
use derive_more::Display;
use std::hash::{Hash, Hasher};
use super::{Kind, Substitution, TypeVar};

#[derive(Debug, PartialEq, Eq)]
pub enum ConstraintResolution {
    Satisfied,
    Unresolved,
    Failure,
}

#[derive(Clone, Debug, Display)]
#[display(fmt = "{type_var}: {trait_}")]
pub struct Constraint {
    pub type_var: TypeVar,
    pub trait_: Token,
}

impl PartialEq for Constraint {
    fn eq(&self, other: &Self) -> bool {
        self.type_var == other.type_var && self.trait_.lexeme == other.trait_.lexeme
    }
}

impl Eq for Constraint {}

impl Hash for Constraint {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.type_var.hash(state);
        self.trait_.lexeme.hash(state);
    }
}

impl Constraint {
    pub fn new(type_var: TypeVar, trait_: Token) -> Self {
        Self { type_var, trait_ }
    }

    pub fn apply(&self, mapping: &Substitution) -> Self {
        let new_tv = self.type_var
            .apply(mapping)
            .as_type_var()
            .cloned()
            .unwrap_or(self.type_var.clone());
        Self::new(new_tv, self.trait_.clone())
    }

    pub fn try_solve(&self, mapping: &Substitution) -> ConstraintResolution {
        let type_ = self.type_var.apply(mapping);
        match (&*type_, self.trait_.lexeme.as_str()) {
            (Kind::Number, "Add") => ConstraintResolution::Satisfied,
            (Kind::InferenceVariable(_), _) => ConstraintResolution::Unresolved,
            _ => ConstraintResolution::Failure,
        }
    }
}
