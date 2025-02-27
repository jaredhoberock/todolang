use crate::ast::typed::BinOp;
use crate::source_location::SourceSpan;
use crate::types::Error as UnifyError;
use crate::types::Type;
use miette::Diagnostic;
use super::constraint_set::Error as ConstraintError;
use super::environment::Error as NameError;
use thiserror::Error;

#[derive(Debug, Error, Diagnostic)]
#[error("Type mismatch: expected '{expected_ty}', found '{found_ty}'")]
pub struct TypeMismatchError {
    pub expected_ty: Type,
    #[label("expected type '{expected_ty}' found here")]
    pub expected_at: SourceSpan,
    pub found_ty: Type,
    #[label(primary, "expected '{expected_ty}', found '{found_ty}'")]
    pub found_at: SourceSpan,
}

#[derive(Debug, Error, Diagnostic)]
#[error("Type mismatch: operator '{0}' expected '{expected_ty}', found '{found_ty}'", op.kind)]
pub struct BinOpError {
    pub op: BinOp,
    pub expected_ty: Type,
    pub found_ty: Type,
    #[label]
    pub found_at: SourceSpan,
}

#[derive(Debug, Error, Diagnostic)]
#[error("{message}")]
pub struct GeneralError {
    pub message: String,
    #[label]
    pub location: SourceSpan,
}


/// This is the type used by SemanticAnalyzer to report errors
/// Error::General is the variant for which no additional context is needed for diagnosis
/// If additional context is needed, introduce a new ErrorType above and a corresponding
/// variant for Error.
#[derive(Debug, Error, Diagnostic)]
#[error(transparent)]
#[diagnostic(transparent)]
pub enum Error {
    BinOp(BinOpError),
    Constraint(ConstraintError),
    General(GeneralError),
    TypeMismatch(TypeMismatchError),
}

impl Error {
    pub fn general(msg: impl Into<String>, location: &SourceSpan) -> Self {
        Self::General(GeneralError {
            message: msg.into(), 
            location: location.clone(),
        })
    }
}


// the traits below allow us to chain additional context such as source location
// onto errors originating from lower-level systems such as the name resolver or type checker


pub trait ErrorContext<T> {
    fn err_ctx(self, location: &SourceSpan) -> Result<T, Error>;
}

// XXX why can't all of these be generic?
impl<T> ErrorContext<T> for Result<T, NameError> {
    fn err_ctx(self, location: &SourceSpan) -> Result<T, Error> {
        self.map_err(|e| Error::general(format!("{}", e), location))
    }
}

impl<T> ErrorContext<T> for Result<T, UnifyError> {
    fn err_ctx(self, location: &SourceSpan) -> Result<T, Error> {
        self.map_err(|e| Error::general(format!("{}", e), location))
    }
}

impl<T> ErrorContext<T> for Result<T, ConstraintError> {
    fn err_ctx(self, location: &SourceSpan) -> Result<T, Error> {
        self.map_err(|e| Error::general(format!("{}", e), location))
    }
}

impl From<ConstraintError> for Error {
    fn from(e: ConstraintError) -> Error {
        Error::Constraint(e)
    }
}

pub trait TypeMismatchContext<T> {
    fn type_mismatch_err_ctx(self, expected_at: SourceSpan, found_at: SourceSpan) -> Result<T, Error>;
}

impl<T> TypeMismatchContext<T> for Result<T,UnifyError> {
    fn type_mismatch_err_ctx(self, expected_at: SourceSpan, found_at: SourceSpan) -> Result<T, Error> {
        self.map_err(|e| {
            let wrapped_e = TypeMismatchError {
                expected_ty: e.expected,
                expected_at,
                found_ty: e.found,
                found_at,
            };
            Error::TypeMismatch(wrapped_e)
        })
    }
}

pub trait BinOpErrorContext<T> {
    fn binop_err_ctx(self, op: BinOp, found_at: SourceSpan) -> Result<T,Error>;
}

impl<T> BinOpErrorContext<T> for Result<T,UnifyError> {
    fn binop_err_ctx(self, op: BinOp, found_at: SourceSpan) -> Result<T,Error> {
        self.map_err(|e| {
            let wrapped_e = BinOpError {
                op,
                expected_ty: e.expected,
                found_ty: e.found,
                found_at,
            };
            Error::BinOp(wrapped_e)
        })
    }
}
