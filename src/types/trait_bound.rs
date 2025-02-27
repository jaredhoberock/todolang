use crate::token::Token;
use crate::ast::typed::{Expression, ExprRef};
use crate::source_location::SourceSpan;
use derive_more::Display;
use miette::Diagnostic;
use std::hash::{Hash, Hasher};
use super::{Kind, Substitution, Type, TypeEnvironment, TypeVar};
use thiserror::Error;

#[derive(Clone, Debug, Display)]
#[display(fmt = "{type_var}: {trait_}")]
pub struct TraitBound {
    // XXX none of these should be pub
    pub type_var: TypeVar,
    pub trait_: Token,
    provenance: Option<Provenance>,
}

impl PartialEq for TraitBound {
    fn eq(&self, other: &Self) -> bool {
        self.type_var == other.type_var
            && self.trait_.lexeme == other.trait_.lexeme
            && self.provenance == other.provenance
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
    fn new(type_var: TypeVar, trait_: Token, provenance: Option<Provenance>) -> Self {
        Self {
            type_var,
            trait_,
            provenance,
        }
    }

    pub fn new_generic(env: &TypeEnvironment, trait_: Token) -> Self {
        Self::new(
            env.generic().as_type_var().cloned().unwrap(), 
            trait_,
            None
        )
    }

    pub fn from_var_use(self, expr: ExprRef) -> Self {
        Self::new(self.type_var, self.trait_, Some(Provenance::VarUse { expr }))
    }

    pub fn try_from_call(self, call_expr: ExprRef) -> Self {
        let provenance = self.provenance
            .map(|p| p.promote_use_into_call(call_expr));
        Self::new(self.type_var, self.trait_, provenance)
    }

    pub fn apply(&self, mapping: &Substitution) -> Self {
        let new_tv = self.type_var
            .apply(mapping)
            .as_type_var()
            .cloned()
            .unwrap_or(self.type_var.clone());
        Self::new(new_tv, self.trait_.clone(), self.provenance.clone())
    }

    pub fn try_solve(&self, mapping: &mut Substitution) -> Option<bool> {
        let type_ = self.type_var.apply(mapping);
        match (&*type_, self.trait_.lexeme.as_str()) {
            (Kind::Number, "Add") => Some(true),
            (Kind::InferenceVariable(_), _) => None,
            _ => Some(false),
        }
    }

    pub fn into_error(self, mapping: &mut Substitution) -> TraitBoundError {
        let failing_type = self.type_var.apply(mapping);

        match self.provenance {
            Some(Provenance::VarUse { expr }) => TraitBoundError::var_use(
                failing_type,
                self.trait_.lexeme.clone(),
                expr.location(),
            ),
            Some(Provenance::Call { call_expr }) => {
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
                let failing_type_var = self.type_var.clone();

                let mut failing_param_loc = None;
                for (i, param_ty) in callee_type.parameter_types().enumerate() {
                    if let Kind::InferenceVariable(ref tv) = **param_ty {
                        if *tv == failing_type_var && i < argument_locations.len() {
                            failing_param_loc = argument_locations.get(i).cloned();
                            break;
                        }
                    }
                }

                TraitBoundError::call(
                    failing_type,
                    self.trait_.lexeme.to_string(),
                    failing_param_loc.unwrap(),
                    callee.location(),
                    callee_name,
                    self.trait_.location.clone()
                )
            },
            _ => panic!("Internal compiler error: can't convert TraitBound without Provenance into Error"),
        }
    }
}

// all trait bound constraints originate in some use of a variable
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
enum Provenance {
    // the use of some variable (i.e., an Expression::Variable) originated the trait bound
    VarUse { expr: ExprRef },

    // some variables are used in call expressions. when this happens,
    // we promote VarUse into Call to capture additional context for diagnostics
    Call { call_expr: ExprRef },
}

impl Provenance {
    fn promote_use_into_call(self, call_expr: ExprRef) -> Self {
        match (&self, &*call_expr) {
            (Self::VarUse { expr }, Expression::Call { callee, .. }) if expr == callee => {
                Self::Call { call_expr }
            }
            _ => self
        }
    }
}

#[derive(Debug, Error, Diagnostic)]
#[error("The trait bound '{0}: {1}' is not satisfied", failing_type, trait_name)]
pub struct VarUseError {
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
pub enum TraitBoundError {
    VarUse(VarUseError),
    Call(CallError),
}

impl TraitBoundError {
    fn var_use(
        failing_type: Type,
        trait_name: String,
        use_location: SourceSpan,
    ) -> Self {
        Self::VarUse(VarUseError {
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
