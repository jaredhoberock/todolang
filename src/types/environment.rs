use std::collections::HashMap;
use super::impl_environment::ImplEnvironment;
use super::types::*;
use thiserror::Error;

#[derive(Clone, Debug, PartialEq, Eq, Error)]
#[error("Type mismatch: expected '{expected}', found '{found}'")]
pub struct Error {
    pub expected: Type, 
    pub found: Type,
}

fn unify_inference_variable(var: &TypeVar, other: &Type, subst: &mut Substitution) -> Result<(), Error> {
    match var {
        TypeVar::Monotype { id, .. } => {
            // consider an occurs-check here
            // XXX this would be a recursive unification, right?
            subst.insert(*id, other.clone());
            Ok(())
        },
        TypeVar::Generic(id1) => {
            // only allow unification if the other type is an identical generic.
            if let Kind::InferenceVariable(TypeVar::Generic(id2)) = &**other {
                if id1 == id2 {
                    return Ok(());
                } 
            }
            Err(Error { 
                expected: var.as_type(), 
                found: other.clone(),
            })
        }
    }
}

pub fn unify(expected: Type, found: Type, subst: &mut Substitution) -> Result<(), Error> {
    // Normalize both types by applying the current substitution.
    let expected = expected.apply(subst);
    let found = found.apply(subst);

    // Early check: if the normalized types are equal, unification succeeds.
    if expected == found {
        return Ok(());
    }

    match (&*expected, &*found) {
        // handle inference variables
        (Kind::InferenceVariable(tv), _) => unify_inference_variable(tv, &found, subst),
        (_, Kind::InferenceVariable(tv)) => unify_inference_variable(tv, &expected, subst),

        // handle function types
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
/// Traverses the type `t` and for each `TypeVar::Generic`, generates a fresh variable via `env.fresh_from()`
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
                    let fresh_type = env.fresh_from(*id);
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
    impls: ImplEnvironment,
}

impl TypeEnvironment {
    pub fn new() -> Self {
        Self { 
            arena: TypeArena::new(),
            substitution: Substitution::new(),
            impls: ImplEnvironment::new(),
        }
    }

    pub fn substitution(&self) -> &Substitution {
        &self.substitution
    }

    pub fn substitution_mut(&mut self) -> &mut Substitution {
        &mut self.substitution
    }

    fn fresh_from(&self, origin: usize) -> Type {
        self.arena.fresh_from(origin)
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

    pub fn apply(&self, t: &Type) -> Type {
        t.apply(&self.substitution)
    }

    pub fn instantiate(&self, polytype: Type) -> (Type, Substitution) {
        let mut mapping = HashMap::new();
        let result = instantiate(self, polytype, &mut mapping);
        (result, mapping)
    }

    /// This is the dual operation of `instantiate`
    pub fn generalize(&self, monotype: Type) -> (Type, Substitution) {
        // find free type variables in the monotype
        let free_vars = monotype.free_type_vars(&self.substitution);

        // build a mapping from each free variable to a new generic type
        let mut mapping = HashMap::new();
        for tv in free_vars {
            if let TypeVar::Monotype {id, .. } = tv {
                // generate a new generic type
                let generic_type = self.generic();
                mapping.insert(id, generic_type);
            }
        }

        let generalized = monotype.apply(&mapping);
        (generalized, mapping)
    }

    pub fn impl_env_mut(&mut self) -> &mut ImplEnvironment {
        &mut self.impls
    }

    pub fn impl_env(&self) -> &ImplEnvironment {
        &self.impls
    }
}
