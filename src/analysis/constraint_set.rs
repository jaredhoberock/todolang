use crate::ast::typed::ExprRef;
use crate::source_location::SourceSpan;
use crate::types::*;
use derive_more::Display;
use miette::Diagnostic;
use std::collections::HashSet;
use std::collections::VecDeque;
use std::hash::Hash;
use thiserror::Error;

#[derive(Clone, Debug, Eq, PartialEq, Hash, Display)]
#[display(fmt = "{expected_ty} = {found_ty}")]
pub struct TypeEquality {
    pub expected_ty: Type,
    pub expected_at: Option<SourceSpan>,
    pub found_ty: Type,
    pub found_at: SourceSpan,
}

impl TypeEquality {
    fn try_solve(&self, mapping: &mut Substitution) -> Option<bool> {
        if unify(self.expected_ty, self.found_ty, mapping).is_ok() {
            Some(true)
        } else {
            Some(false)
        }
    }

    fn into_error(self, mapping: &Substitution) -> TypeEqualityError {
        TypeEqualityError {
            expected_ty: self.expected_ty.apply(mapping),
            expected_at: self.expected_at,
            found_ty: self.found_ty.apply(mapping),
            found_at: self.found_at,
        }
    }
}

#[derive(Debug, Error, Diagnostic)]
#[error("Type mismatch: expected '{expected_ty}', found '{found_ty}'")]
pub struct TypeEqualityError {
    pub expected_ty: Type,
    #[label("expected type '{expected_ty}' found here")]
    pub expected_at: Option<SourceSpan>,
    pub found_ty: Type,
    #[label(primary "expected '{expected_ty}', found '{found_ty}'")]
    pub found_at: SourceSpan,
}

#[derive(Clone, Debug, Eq, PartialEq, Hash, Display)]
enum Constraint {
    Eq(TypeEquality),
    Trait(TraitBound),
}

impl Into<Constraint> for TypeEquality {
    fn into(self) -> Constraint {
        Constraint::Eq(self)
    }
}

impl Into<Constraint> for TraitBound {
    fn into(self) -> Constraint {
        Constraint::Trait(self)
    }
}

impl Constraint {
    fn try_solve(&self, env: &mut TypeEnvironment) -> Option<bool> {
        match self {
            Self::Eq(eq) => eq.try_solve(env.substitution_mut()),
            Self::Trait(trait_bound) => trait_bound.try_solve(env.impl_env(), env.substitution()),
        }
    }

    fn into_error(self, mapping: &Substitution) -> Error {
        match self {
            Self::Eq(eq) => eq.into_error(mapping).into(),
            Self::Trait(tb) => tb.into_error(mapping).into(),
        }
    }

    fn transform_provenance_for_call(self, call_expr: ExprRef) -> Self {
        match self {
            Self::Trait(tb) => tb.try_from_call(call_expr).into(),
            _ => self,
        }
    }
}

#[derive(Debug, Error, Diagnostic)]
pub enum Error {
    #[error(transparent)]
    #[diagnostic(transparent)]
    Eq(TypeEqualityError),
    #[error(transparent)]
    #[diagnostic(transparent)]
    Trait(TraitBoundError),
    #[error("Unresolvable constraints")]
    Unresolvable,
}

impl Into<Error> for TypeEqualityError {
    fn into(self) -> Error {
        Error::Eq(self)
    }
}

impl Into<Error> for TraitBoundError {
    fn into(self) -> Error {
        Error::Trait(self)
    }
}

pub struct ConstraintSet {
    constraints: HashSet<Constraint>,
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

    fn add_constraint(
        &mut self, 
        env: &mut TypeEnvironment,
        constraint: Constraint
    ) -> Result<(), Error> {
        // XXX first check for contradictions
        // XXX recursively add any implied constraints

        let result = match constraint.try_solve(env) {
            Some(true) => Ok(()),
            Some(false) => Err(constraint.into_error(env.substitution())),
            None => {
                self.constraints.insert(constraint);
                Ok(())
            },
        };

        result
    }

    pub fn add_trait_bound_constraint(
        &mut self,
        env: &mut TypeEnvironment,
        trait_bound: TraitBound,
        provenance: ExprRef,
    ) -> Result<(), Error> {
        let constraint = Constraint::Trait(trait_bound.from_var_use(provenance));
        self.add_constraint(env, constraint)
    }

    pub fn add_type_equality_constraint(
        &mut self,
        env: &mut TypeEnvironment,
        expected_ty: Type,
        expected_at: Option<SourceSpan>,
        found_ty: Type,
        found_at: SourceSpan
    ) -> Result<(), Error> {
        let constraint = Constraint::Eq(TypeEquality { 
            expected_ty, 
            expected_at, 
            found_ty, 
            found_at
        });
        self.add_constraint(env, constraint)
    }

    pub fn solve_constraints(&mut self, env: &mut TypeEnvironment) -> Result<(), Error> {
        let mut worklist: VecDeque<Constraint> = self.constraints.drain().collect();

        loop {
            let mut new_worklist = VecDeque::new();
            let mut progress = false;

            while let Some(constraint) = worklist.pop_front() {
                match constraint.try_solve(env) {
                    Some(true) => {
                        // XXX enqueue any new constraints generated by this resolution
                        progress = true;
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
                    None => {
                        // constraint isn't ready yet; re-enqueue it
                        new_worklist.push_back(constraint);
                    }
                    Some(false) => {
                        return Err(constraint.into_error(env.substitution()));
                    }
                }
            }

            if new_worklist.is_empty() {
                // all constraints have been resolved
                return Ok(())
            }

            if !progress {
                // if no constraint was resolved in this pass, we're stuck
                // return the first unresolved constraint as an error
                // XXX TODO return a collection of unresolved constraint errors
                let constraint = new_worklist.pop_front().unwrap(); 
                return Err(constraint.into_error(env.substitution()));
            }

            worklist = new_worklist;
        }
    }
}
