use crate::ast::typed::{Expression, ExprRef};
use crate::source_location::SourceSpan;
use crate::types::*;
use derive_more::Display;
use miette::Diagnostic;
use std::collections::HashSet;
use std::collections::VecDeque;
use std::hash::Hash;
use thiserror::Error;

// Provenance records the reason a particular Constraint was instantiated
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
enum Provenance {
    // all trait bound constraints originate in some use of a variable
    VarUse { expr: ExprRef },

    // some variables are used in call expressions. when this happens,
    // we promote VarUse into Call to capture additional context for diagnostics
    Call { call_expr: ExprRef },
}

impl Provenance {
    fn promote_use_into_call(self, call_expr: ExprRef) -> Provenance {
        match (&self, &*call_expr) {
            (Self::VarUse { expr }, Expression::Call { callee, .. }) if expr == callee => {
                Self::Call { call_expr }
            }
            _ => self
        }
    }

    fn into_error(self, constraint: Constraint, mapping: &Substitution) -> Error {
        let trait_bound = constraint.as_trait_bound().expect("Type equality case unimplemented");
        let failing_type = trait_bound.type_var.apply(mapping);

        match self {
            Self::VarUse { expr } => Error::bound(
                failing_type,
                trait_bound.trait_.lexeme.clone(),
                expr.location(),
            ),
            Self::Call { call_expr } => {
                let callee = call_expr.callee().unwrap();
                let callee_name = callee.variable_decl()
                    .unwrap()
                    .borrow()
                    .name()
                    .lexeme
                    .clone();
                let callee_type = callee.type_();

                let argument_locations: Vec<_> = call_expr.arguments()
                    .unwrap()
                    .iter()
                    .map(|a| a.location())
                    .collect();

                // figure out which parameter produced the type variable that failed the constraint
                let failing_type_var = trait_bound.type_var.clone();

                let mut failing_param_loc = None;
                for (i, param_ty) in callee_type.parameter_types().enumerate() {
                    if let Kind::InferenceVariable(ref tv) = **param_ty {
                        if *tv == failing_type_var && i < argument_locations.len() {
                            failing_param_loc = argument_locations.get(i).cloned();
                            break;
                        }
                    }
                }

                Error::call(
                    failing_type,
                    trait_bound.trait_.lexeme.to_string(),
                    failing_param_loc.unwrap(),
                    callee.location(),
                    callee_name,
                    trait_bound.trait_.location.clone()
                )
            },
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash, Display)]
#[display(fmt = "{constraint}")]
pub struct ConstraintWithProvenance {
    constraint: Constraint,
    provenance: Provenance,
}

impl ConstraintWithProvenance {
    pub fn new_var_use(constraint: Constraint, expr: ExprRef) -> Self {
        Self {
            constraint,
            provenance: Provenance::VarUse { expr },
        }
    }

    fn transform_provenance_for_call(self, call_expr: ExprRef) -> Self {
        Self {
            constraint: self.constraint,
            provenance: self.provenance.promote_use_into_call(call_expr),
        }
    }

    fn into_error(&self, mapping: &Substitution) -> Error {
        self.provenance.clone()
            .into_error(self.constraint.clone(), mapping)
    }

    fn try_solve(&self, mapping: &mut Substitution) -> Result<ConstraintResolution,Error> {
        match self.constraint.try_solve(mapping) {
            ConstraintResolution::Failure => Err(self.into_error(mapping)),
            resolution => Ok(resolution),
        }
    }

    fn solve(&self, mapping: &mut Substitution) -> Result<(),Error> {
        match self.try_solve(mapping) {
            Ok(ConstraintResolution::Satisfied) => Ok(()),
            _ => Err(self.into_error(mapping)),
        }
    }
}

pub struct ConstraintSet {
    constraints: HashSet<ConstraintWithProvenance>,
}

impl ConstraintSet {
    pub fn new() -> Self {
        Self { constraints: HashSet::new() }
    }

    pub fn transform_provenance_for_call(&mut self, call_expr: ExprRef) {
        let old_set = std::mem::take(&mut self.constraints);
        self.constraints = old_set.into_iter()
            .map(|c| c.transform_provenance_for_call(call_expr.clone()))
            .collect();
    }

    pub fn add_constraint(
        &mut self, 
        constraint: ConstraintWithProvenance, 
        mapping: &mut Substitution) -> Result<(), Error>
    {
        // XXX first check for contradictions
        // XXX recursively add any implied constraints

        let resolution = constraint.try_solve(mapping)?;
        if resolution == ConstraintResolution::Unresolved {
            self.constraints.insert(constraint);
        }

        Ok(())
    }

    /// Attempts to solve all constraints in the constraint set
    ///
    /// # Errors
    ///
    /// If any constraint cannot be solved (i.e. it fails the check), it is returned as an error immediately.
    pub fn solve_constraints(&mut self, mapping: &mut Substitution) -> Result<(), Error> {
        // solve constraints using a worklist and detect contradictions
        let mut queue: VecDeque<_> = self.constraints.drain().collect();
        let mut resolved = HashSet::new();

        while let Some(constraint) = queue.pop_front() {
            // skip if we've already processed this constraint
            if resolved.contains(&constraint) {
                continue;
            }

            // insist that the constraint is satisfied
            constraint.solve(mapping)?;
            resolved.insert(constraint);

            // XXX TODO contradictions
            //// check for any contradictions with already solved constraints
            //for existing in &resolved {
            //    for contradiction in constraint.contradicts() {
            //        if existing.eq(&contradiction.as_ref()) {
            //            // XXX should return Error::Contradiction
            //            return Err(format!(
            //                "Constraint {:?} contradicts resolved constraint {:?}",
            //                constraint, contradiction
            //            ));
            //        }
            //    }
            //}
        }

        Ok(())
    }
}

#[derive(Debug, Error, Diagnostic)]
#[error("The trait bound '{0}: {1}' is not satisfied", failing_type, trait_name)]
pub struct BoundError {
    pub failing_type: Type,
    pub trait_name: String,
    // the location of the use that generated the constraint
    #[label]
    pub use_location: SourceSpan,
}

#[derive(Debug, Error, Diagnostic)]
#[error("The trait bound '{0}: {1}' is not satisfied'", failing_param_type, failing_trait)]
pub struct CallError {
    pub failing_param_type: Type,
    pub failing_trait: String,
    #[label(primary, "the trait '{0}' is not implemented for '{1}'", failing_trait, failing_param_type)]
    pub failing_param_loc: SourceSpan,
    #[label("required by a bound introduced by this call")]
    pub call_location: SourceSpan,
    pub fn_name: String,
    #[label("required by a bound in '{fn_name}'")]
    pub fn_loc: SourceSpan,
}

#[derive(Debug, Error, Diagnostic)]
#[error(transparent)]
#[diagnostic(transparent)]
pub enum Error {
    Bound(BoundError),
    Call(CallError),
}

impl Error {
    fn bound(
        failing_type: Type,
        trait_name: String,
        use_location: SourceSpan, 
    ) -> Self {
        Self::Bound(BoundError {
            failing_type,
            trait_name,
            use_location,
        })
    }

    fn call(
        failing_param_type: Type,
        failing_trait: String,
        failing_param_loc: SourceSpan,
        call_location: SourceSpan,
        fn_name: String,
        fn_loc: SourceSpan,
    ) -> Self {
        Self::Call(CallError {
            failing_param_type,
            failing_trait,
            failing_param_loc,
            call_location,
            fn_name,
            fn_loc,
        })
    }
}
