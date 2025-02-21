use derive_more::Display;
use super::{Kind, Substitution, TypeVar};

#[derive(Debug, PartialEq, Eq)]
pub enum ConstraintResolution {
    Satisfied,
    Unresolved,
    Failure,
}

#[derive(Clone, Debug, Hash, Eq, PartialEq, Display)]
#[display(fmt = "{type_var}: {trait_}")]
pub struct Constraint {
    pub type_var: TypeVar,
    pub trait_: String,
}

impl Constraint {
    pub fn new(type_var: TypeVar, trait_: String) -> Self {
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
        match (&*type_, self.trait_.as_str()) {
            (Kind::Number, "Add") => ConstraintResolution::Satisfied,
            (Kind::InferenceVariable(_), _) => ConstraintResolution::Unresolved,
            _ => ConstraintResolution::Failure,
        }
    }
}
