use crate::token::Token;
use derive_more::Display;
use std::hash::Hash;
use super::{Substitution, TraitBound, Type, TypeVar, unify};

#[derive(Debug, PartialEq, Eq)]
pub enum ConstraintResolution {
    Satisfied,
    Unresolved,
    Failure,
}

#[derive(Clone, Debug, Eq, PartialEq, Hash, Display)]
#[display(fmt = "{expected} = {found}")]
pub struct TypeEquality {
    pub expected: Type,
    pub found: Type,
}

impl TypeEquality {
    pub fn apply(&self, mapping: &Substitution) -> Self {
        Self {
            expected: self.expected.apply(&mapping),
            found: self.found.apply(&mapping),
        }
    }

    pub fn try_solve(&self, mapping: &mut Substitution) -> ConstraintResolution {
        if unify(self.expected, self.found, mapping).is_ok() {
            ConstraintResolution::Satisfied
        } else {
            ConstraintResolution::Failure
        }
    }
}

#[derive(Clone, Debug, Eq, PartialEq, Hash, Display)]
pub enum Constraint {
    Eq(TypeEquality),
    Trait(TraitBound),
}

impl Constraint {
    pub fn new_trait_bound(type_var: TypeVar, trait_: Token) -> Self {
        Self::Trait(TraitBound { type_var, trait_ })
    }

    pub fn new_equality(expected: Type, found: Type) -> Self {
        Self::Eq(TypeEquality { expected, found })
    }

    pub fn apply(&self, mapping: &Substitution) -> Self {
        match self {
            Self::Eq(eq) => eq.apply(mapping).into(),
            Self::Trait(t) => t.apply(mapping).into(),
        }
    }

    pub fn try_solve(&self, mapping: &mut Substitution) -> ConstraintResolution {
        match self {
            Self::Eq(eq) => eq.try_solve(mapping),
            Self::Trait(t) => match t.try_solve(mapping) {
                Some(true)  => ConstraintResolution::Satisfied,
                Some(false) => ConstraintResolution::Failure,
                None        => ConstraintResolution::Unresolved,
            },
        }
    }

    pub fn as_trait_bound(self) -> Option<TraitBound> {
        match self {
            Self::Trait(tb) => Some(tb),
            _ => None,
        }
    }
}

impl From<TypeEquality> for Constraint {
    fn from(eq: TypeEquality) -> Self {
        Self::Eq(eq)
    }
}

impl From<TraitBound> for Constraint {
    fn from(tb: TraitBound) -> Self {
        Self::Trait(tb)
    }
}
