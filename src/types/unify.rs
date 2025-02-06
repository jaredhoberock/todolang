use super::error::InternalError;
use super::types::{Kind, Type};

pub fn unify(expected: Type, found: Type) -> Result<(), InternalError> {
    match (&*expected, &*found) {
        // If either is an inference variable, succeed
        (Kind::InferenceVariable(_), _) | (_, Kind::InferenceVariable(_)) => Ok(()),

        // If both types are function types, unify their components
        (Kind::Function(params1, ret1), Kind::Function(params2, ret2)) => {
            if params1.len() != params2.len() {
                return Err(InternalError::Mismatch { expected, found });
            }
            // unify each parameter type
            for (p1, p2) in params1.iter().zip(params2.iter()) {
                unify(p1.clone(), p2.clone())?;
            }
            // unify return types
            unify(ret1.clone(), ret2.clone())
        },

        // If both are concrete, they must be equal
        _ if expected == found => Ok(()),

        // Otherwise, fail
        _ => Err(InternalError::Mismatch{ expected, found }),
    }
}
