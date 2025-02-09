use super::types::*;
use thiserror::Error;

#[derive(Clone, Debug, PartialEq, Eq, Error)]
#[error("Type mismatch: expected '{expected}', found '{found}'")]
pub struct Error {
    pub expected: Type, 
    pub found: Type,
}

fn unify(expected: Type, found: Type, subst: &mut Substitution) -> Result<(), Error> {
    // apply the current substitution to both types
    let expected = expected.apply(subst);
    let found = found.apply(subst);

    match (&*expected, &*found) {
        // If the expected type is an inference variable, record a binding.
        (Kind::InferenceVariable(id), _) => {
            subst.insert(*id, found);
            Ok(())
        },
        // If the found type is an inference variable, record a binding.
        (_, Kind::InferenceVariable(id)) => {
            subst.insert(*id, expected);
            Ok(())
        },
        // For function types, both must be functions with the same number of parameters
        (Kind::Function(params1, ret1), Kind::Function(params2, ret2)) => {
            if params1.len() != params2.len() {
                return Err(Error { expected, found });
            }
            // Unify each parameter pair
            for (p1, p2) in params1.iter().zip(params2.iter()) {
                unify(p1.clone(), p2.clone(), subst)?;
            }
            // Unify the return types
            unify(ret1.clone(), ret2.clone(), subst)
        },
        // If both types are concrete and equal, unification succeeds.
        _ if expected == found => Ok(()),
        // Otherwise, the types do not unify
        _ => Err(Error { expected, found }),
    }
}

pub struct TypeEnvironment {
    arena: TypeArena,
    substitution: Substitution,
}

impl TypeEnvironment {
    pub fn new() -> Self {
        Self { 
            arena: TypeArena::new(),
            substitution: Substitution::new(),
        }
    }

    pub fn fresh(&self) -> Type {
        self.arena.fresh()
    }

    pub fn get_bool(&self) -> Type {
        self.arena.bool()
    }

    pub fn get_function(&self, parameters: Vec<Type>, result: Type) -> Type {
        self.arena.function(parameters, result)
    }

    pub fn get_number(&self) -> Type {
        self.arena.number()
    }

    pub fn get_string(&self) -> Type {
        self.arena.string()
    }

    pub fn get_unit(&self) -> Type {
        self.arena.unit()
    }

    pub fn lookup_type(&self, name: &str) -> Option<Type> {
        match name {
            "Number" => Some(self.get_number()),
            "String" => Some(self.get_string()),
            "Bool"   => Some(self.get_bool()),
            _        => None, // no user-defined types yet
        }
    }

    pub fn unify(&mut self, t1: Type, t2: Type) -> Result<(), Error> {
        unify(t1, t2, &mut self.substitution)
    }

    pub fn apply(&self, t: &Type) -> Type {
        t.apply(&self.substitution)
    }
}
