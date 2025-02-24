use crate::token::Token;
use std::collections::HashSet;
use super::{Constraint, Kind, Type, TypeEnvironment};

#[derive(Clone, Debug)]
pub struct TypeScheme {
  pub type_: Type,
  pub constraints: HashSet<Constraint>,
}

impl TypeScheme {
    pub fn new(type_: Type, constraints: HashSet<Constraint>) -> Self {
        Self {
            type_,
            constraints,
        }
    }

    pub fn new_unconstrained(type_: Type) -> Self {
        Self::new(type_, HashSet::new())
    }

    pub fn new_generic(env: &TypeEnvironment, trait_name: Option<Token>) -> Self {
        let type_ = env.generic();
        let mut constraints = HashSet::new();
        match (trait_name, &*type_) {
            (Some(trait_name), Kind::InferenceVariable(type_var)) => {
                let constraint = Constraint::new_trait_bound(type_var.clone(), trait_name);
                constraints.insert(constraint);
            },
            _ => (),
        }
        Self::new(type_, constraints)
    }
  
    pub fn new_function(env: &TypeEnvironment, parameters: Vec<TypeScheme>, result: TypeScheme) -> Self {
        // extract the underlying types from each parameter
        let parameter_types: Vec<Type> = parameters
            .iter()
            .map(|scheme| scheme.type_)
            .collect();
  
        // build the function type
        let function_type = env.get_function(parameter_types, result.type_);
  
        // combine constraints from all parameters and the result
        let mut constraints: HashSet<Constraint> = HashSet::new();
        for scheme in parameters {
            constraints.extend(scheme.constraints);
        }
        constraints.extend(result.constraints);
  
        Self::new(function_type, constraints)
    }

    pub fn instantiate(&self, env: &TypeEnvironment) -> (Type, HashSet<Constraint>) {
        // first instantiate the type
        let (ty, mapping) = env.instantiate(self.type_);
  
        // apply the mapping to each constraint
        let instantiated_constraints: HashSet<Constraint> = self.constraints
            .clone()
            .into_iter()
            .map(|constraint| constraint.apply(&mapping))
            .collect();

        (ty, instantiated_constraints)
    }

    /// Given a monotype and its associated constraints, produces a generalized
    /// type scheme by replacing free inference variables with generic ones.
    ///
    /// This is essentially the inverse of `instantiate`.
    ///
    /// `env` is the type environment used for generalization.
    pub fn generalized_from(env: &TypeEnvironment, monotype: Type, constraints: HashSet<Constraint>) -> Self {
        // generalize the monotype
        let (polytype, mapping) = env.generalize(monotype);

        // apply the mapping to each constraint
        let generalized_constraints: HashSet<Constraint> = constraints
            .into_iter()
            .map(|constraint| constraint.apply(&mapping))
            .collect();

        Self::new(polytype, generalized_constraints)
    }
}
