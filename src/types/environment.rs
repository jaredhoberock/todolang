use std::collections::HashMap;
use super::types::*;
use thiserror::Error;

#[derive(Clone, Debug, PartialEq, Eq, Error)]
#[error("Type mismatch: expected '{expected}', found '{found}'")]
pub struct Error {
    pub expected: Type, 
    pub found: Type,
}

// Helper function to try unifying when one side is an inference variable.
// If `var_type` is an inference variable, it handles the three cases:
// - Unbound: bind it to the other type.
// - Generic: succeed only if the other type is the same generic variable.
// - Link: follow the link and unify recursively.
// Returns Some(Ok(())) or Some(Err(...)) if `var_type` is an inference variable;
// otherwise, returns None.
fn try_unify_inference_variable(var_type: &Type, other: &Type, subst: &mut Substitution) -> Option<Result<(), Error>> {
    if let Kind::InferenceVariable(ref tv) = **var_type {
        match tv {
            TypeVar::Unbound(id) => {
                // Optionally, you might perform an occurs-check here.
                subst.insert(*id, other.clone());
                return Some(Ok(()));
            },
            TypeVar::Generic(id1) => {
                // Only allow unification if the other type is an identical generic.
                if let Kind::InferenceVariable(TypeVar::Generic(id2)) = &**other {
                    if id1 == id2 {
                        return Some(Ok(()));
                    }
                }
                return Some(Err(Error { expected: var_type.clone(), found: other.clone() }));
            },
        }
    }
    None
}

fn unify(expected: Type, found: Type, subst: &mut Substitution) -> Result<(), Error> {
    // Normalize both types by applying the current substitution.
    let expected = expected.apply(subst);
    let found = found.apply(subst);

    // Early check: if the normalized types are equal, unification succeeds.
    if expected == found {
        return Ok(());
    }

    // Try handling if either side is an inference variable.
    if let Some(result) = try_unify_inference_variable(&expected, &found, subst) {
        return result;
    }
    if let Some(result) = try_unify_inference_variable(&found, &expected, subst) {
        return result;
    }

    // Handle function types.
    match (&*expected, &*found) {
        (Kind::Function(params1, ret1), Kind::Function(params2, ret2)) => {
            if params1.len() != params2.len() {
                return Err(Error { expected, found });
            }
            for (p1, p2) in params1.iter().zip(params2.iter()) {
                unify(p1.clone(), p2.clone(), subst)?;
            }
            unify(ret1.clone(), ret2.clone(), subst)
        },
        // Catch-all: if no other pattern matched, the types don't unify.
        _ => Err(Error { expected, found }),
    }
}

/// Instantiates a polymorphic type by replacing each generic type variable with a fresh inference variable.
///
/// Traverses the type `t` and for each `TypeVar::Generic`, generates a fresh variable via `env.fresh()`
/// (wrapped as a `Type`) and records the mapping from the generic ID to the fresh type.
/// 
/// # Parameters
/// - `env`: The type environment used for generating fresh variables.
/// - `t`: The polymorphic type to instantiate.
/// 
/// # Returns
/// - The monomorphic type of `t` with generics replaced with fresh inference variables.
/// - `mapping` is a `HashMap<usize, Type>` mapping each generic variable's ID to its fresh inference variable.
fn instantiate(
    env: &TypeEnvironment, 
    t: Type,
    mapping: &mut HashMap<usize,Type>
) -> Type {
    match &*t {
        Kind::InferenceVariable(ref var) => match var {
            // If t is an inference variable and it's generic,
            // replace it with a fresh (unbound) variable
            TypeVar::Generic(id) => {
                if let Some(mapped_type) = mapping.get(id) {
                    mapped_type.clone()
                } else {
                    let fresh_type = env.fresh();
                    mapping.insert(*id, fresh_type.clone());
                    fresh_type
                }
            }
            // For non-generic inference variables, just return t
            _ => t,
        },
        // For function types, recursively instantiate the parameters and return type
        Kind::Function(ref params, ref ret) => {
            let new_params = params
                .iter()
                .map(|p| instantiate(env, *p, mapping))
                .collect();
            let new_ret = instantiate(env, *ret, mapping);
            env.get_function(new_params, new_ret)
        },
        // For all other types, just return t
        _ => t,
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

    pub fn generic(&self) -> Type {
        self.arena.generic()
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

    pub fn unify(&mut self, t1: Type, t2: Type) -> Result<(), Error> {
        unify(t1, t2, &mut self.substitution)
    }

    pub fn instantiate_and_unify(&mut self, polymorphic_t1: Type, t2: Type) -> Result<(), Error> {
        let t1 = self.instantiate(polymorphic_t1);
        return self.unify(t1, t2)
    }

    pub fn apply(&self, t: &Type) -> Type {
        t.apply(&self.substitution)
    }

    pub fn instantiate(&self, t: Type) -> Type {
        let mut mapping = HashMap::new();
        instantiate(self, t, &mut mapping)
    }
}
