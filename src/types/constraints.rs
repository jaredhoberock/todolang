use crate::token::Token;
use derive_more::Display;
use std::hash::{Hash, Hasher};
use super::{Kind, Substitution, Type, TypeVar, unify};

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

    pub fn try_solve(&self, mapping: &mut Substitution) -> ConstraintResolution {
        let type_ = self.type_var.apply(mapping);
        match (&*type_, self.trait_.lexeme.as_str()) {
            (Kind::Number, "Add") => ConstraintResolution::Satisfied,
            (Kind::InferenceVariable(_), _) => ConstraintResolution::Unresolved,
            _ => ConstraintResolution::Failure,
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
            Self::Trait(t) => t.try_solve(mapping),
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
