use crate::ast::typed::DeclRef;
use crate::source_location::SourceSpan;
use crate::types::*;
use derive_more::Display;
use miette::Diagnostic;
use std::collections::HashSet;
use std::collections::VecDeque;
use std::hash::Hash;
use thiserror::Error;

#[derive(Clone, Debug, PartialEq, Eq, Hash, Display)]
#[display(fmt = "{constraint}")]
pub struct AnnotatedConstraint {
    constraint: Constraint,
    // the use site that generated the constraint
    use_location: SourceSpan,
}

impl AnnotatedConstraint {
    pub fn new(constraint: Constraint, use_location: SourceSpan) -> Self {
        Self {
            constraint,
            use_location,
        }
    }

    fn solve(&self, mapping: &Substitution) -> Result<(),Error> {
        match self.constraint.try_solve(&mapping) {
            ConstraintResolution::Satisfied => Ok(()),
            _ => Err(Error::basic(
                self.constraint.clone(),
                self.constraint.type_var.apply(&mapping),
                self.use_location.clone(),
            ))
        }
    }
}

pub struct ConstraintSet {
    constraints: HashSet<AnnotatedConstraint>,
}

impl ConstraintSet {
    fn new() -> Self {
        Self { constraints: HashSet::new() }
    }

    fn add_constraint(&mut self, constraint: AnnotatedConstraint) {
        self.constraints.insert(constraint);

        // XXX check for contradictions before adding
        // XXX recursively add any implied constraints
        // XXX solve as we add constraints?
    }

    /// Attempts to solve all constraints in the constraint set
    ///
    /// # Errors
    ///
    /// If any constraint cannot be solved (i.e. it fails the check), it is returned as an error immediately.
    fn solve_constraints(&mut self, mapping: &Substitution) -> Result<(), Error> {
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

pub struct ConstraintEnvironment {
    scopes: Vec<ConstraintSet>,
}

impl ConstraintEnvironment {
    pub fn new() -> Self {
        Self { 
            scopes: vec![ConstraintSet::new()],
        }
    }

    pub fn enter_scope(&mut self) {
        self.scopes.push(ConstraintSet::new());
    }

    pub fn exit_scope(&mut self) {
        let scope = self.scopes
            .pop()
            .expect("Internal compiler error: constraints stack is empty");

        if !scope.constraints.is_empty() {
            panic!("Internal compiler error: unresolved constraints at end of scope");
        }
    }

    pub fn add_constraint(&mut self, constraint: AnnotatedConstraint) {
        self.scopes
            .last_mut()
            .expect("Internal compiler error: constraints stack is empty")
            .add_constraint(constraint)
    }

    /// Attempts to solve all constraints in the current scope
    /// If any constraint does not resolve to ConstraintResolution::Satisfied,
    /// an error is returned
    pub fn solve_constraints(&mut self, type_env: &TypeEnvironment) -> Result<(), Error> {
        self.scopes
            .last_mut()
            .expect("Internal compiler error: constraints stack is empty")
            .solve_constraints(type_env.substitution())
    }
}


#[derive(Debug, Error, Diagnostic)]
#[error("The trait bound '{0}: {1}' is not satisfied", failing_type, constraint.trait_.lexeme)]
pub struct BasicError {
    pub constraint: Constraint,
    pub failing_type: Type,
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
    Basic(BasicError),
    Call(CallError),
}

impl Error {
    fn basic(
        constraint: Constraint,
        failing_type: Type,
        use_location: SourceSpan, 
    ) -> Self {
        Self::Basic(BasicError {
            constraint,
            failing_type,
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

    fn as_basic(self) -> Option<BasicError> {
        match self {
            Self::Basic(e) => Some(e),
            _ => None,
        }
    }
}

pub trait ConstrainedCallErrorContext<T> {
    fn constrained_call_err_ctx(
        self,
        callee_decl: DeclRef,
        callee_type: Type,
        argument_locations: Vec<SourceSpan>,
    ) -> Result<T,Error>;
}

impl<T> ConstrainedCallErrorContext<T> for Result<T,Error> {
    // this embellishes a basic constraint error with additional diagnostic context
    // related to a function call that failed
    fn constrained_call_err_ctx(
        self,
        callee_decl: DeclRef,
        callee_type: Type,
        argument_locations: Vec<SourceSpan>,
    ) -> Result<T,Error> {
        self.map_err(|error| {
            let basic_error = error.as_basic().unwrap();

            // figure out which parameter has the type which failed the constraint
            let failing_type_var = basic_error.constraint.type_var.clone();
            let failing_param_type = basic_error.failing_type.clone();

            let mut failing_param_loc = None;
            for (i, param_ty) in callee_type.parameter_types().enumerate() {
                if let Kind::InferenceVariable(ref tv) = **param_ty {
                    if *tv == failing_type_var && i < argument_locations.len() {
                        failing_param_loc = argument_locations.get(i).cloned();
                        break;
                    }
                }
            }

            // find the 

            // XXX instead of printing the failing_param_type, we really need to print the name of
            // the parameter's type as it would appear in the source text
            match failing_param_loc {
                Some(failing_param_loc) => Error::call(
                    failing_param_type,
                    basic_error.constraint.trait_.lexeme.to_string(),
                    failing_param_loc, 
                    basic_error.use_location.clone(),
                    callee_decl.borrow().name().lexeme.clone(),
                    basic_error.constraint.trait_.location.clone(),
                ),
                None => Error::Basic(basic_error),
            }
        })
    }
}
