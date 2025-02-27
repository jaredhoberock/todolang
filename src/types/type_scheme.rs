use std::collections::HashSet;
use super::{TraitBound, Type, TypeEnvironment};

#[derive(Clone, Debug)]
pub struct TypeScheme {
  pub type_: Type,
  pub bounds: HashSet<TraitBound>,
}

impl TypeScheme {
    fn new(type_: Type, bounds: HashSet<TraitBound>) -> Self {
        Self {
            type_,
            bounds,
        }
    }

    pub fn new_unbounded(type_: Type) -> Self {
        Self::new(type_, HashSet::new())
    }

    pub fn new_bounded(env: &TypeEnvironment, trait_bound: Option<TraitBound>) -> Self {
        match trait_bound {
            Some(trait_bound) => {
                // the TraitBound's type var as our generic polytype
                let type_ = trait_bound.type_var.as_type();

                // Create a HashSet with a single TraitBound
                let mut bounds = HashSet::new();
                bounds.insert(trait_bound);
                Self::new(type_, bounds)
            },
            // otherwise, create an unbounded type scheme with a fresh generic type
            _ => Self::new_unbounded(env.generic())
        }
    }
  
    pub fn new_function(env: &TypeEnvironment, parameters: Vec<TypeScheme>, result: TypeScheme) -> Self {
        // extract the underlying types from each parameter
        let parameter_types: Vec<Type> = parameters
            .iter()
            .map(|scheme| scheme.type_)
            .collect();
  
        // build the function type
        let function_type = env.get_function(parameter_types, result.type_);
  
        // combine bounds from all parameters and the result
        let mut bounds: HashSet<TraitBound> = HashSet::new();
        for scheme in parameters {
            bounds.extend(scheme.bounds);
        }
        bounds.extend(result.bounds);
  
        Self::new(function_type, bounds)
    }

    pub fn instantiate(&self, env: &mut TypeEnvironment) -> (Type, HashSet<TraitBound>) {
        // first instantiate the type
        let (ty, mapping) = env.instantiate(self.type_);
  
        // apply the mapping to each bound
        let instantiated_bounds: HashSet<TraitBound> = self.bounds
            .clone()
            .into_iter()
            .map(|bound| bound.apply(&mapping))
            .collect();

        (ty, instantiated_bounds)
    }

    /// Given a monotype and its associated constraints, produces a generalized
    /// type scheme by replacing free inference variables with generic ones.
    ///
    /// This is essentially the inverse of `instantiate`.
    ///
    /// `env` is the type environment used for generalization.
    pub fn generalized_from(env: &TypeEnvironment, monotype: Type, bounds: HashSet<TraitBound>) -> Self {
        // generalize the monotype
        let (polytype, mapping) = env.generalize(monotype);

        // apply the mapping to each bound
        let generalized_bounds: HashSet<TraitBound> = bounds
            .into_iter()
            .map(|bound| bound.apply(&mapping))
            .collect();

        Self::new(polytype, generalized_bounds)
    }
}
