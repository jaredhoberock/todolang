use crate::ast::typed::BinOp;
use crate::source_location::SourceSpan;
use crate::types::Error as UnifyError;
use crate::types::Type;
use super::environment::Error as NameError;
use thiserror::Error;

#[derive(Debug, Error)]
#[error("Type mismatch: expected '{0}', found '{1}'", expected.0, found.0)]
pub struct TypeMismatchError {
    pub expected: (Type, SourceSpan),
    pub found: (Type, SourceSpan),
}

impl TypeMismatchError {
    pub fn location(&self) -> SourceSpan {
        self.found.1.clone()
    }
}

#[derive(Debug, Error)]
#[error("Type mismatch: operator '{0}' expected '{expected_ty}', found '{1}'", op.kind, found.0)]
pub struct BinOpError {
    pub op: BinOp,
    pub expected_ty: Type,
    pub found: (Type, SourceSpan),
}

impl BinOpError {
    pub fn location(&self) -> SourceSpan {
        self.found.1.clone()
    }
}


/// This is the type used by SemanticAnalyzer to report errors
/// Error::General is the variant for which no additional context is needed for diagnosis
/// If additional context is needed, introduce a new ErrorType above and a corresponding
/// variant for Error.
#[derive(Debug, Error)]
pub enum Error {
    #[error(transparent)]
    BinOp(BinOpError),

    #[error("{0}")]
    General(String, SourceSpan),

    #[error(transparent)]
    TypeMismatch(TypeMismatchError),
}

impl Error {
    pub fn general(msg: impl Into<String>, location: &SourceSpan) -> Self {
        Self::General(msg.into(), location.clone())
    }

    pub fn location(&self) -> SourceSpan {
        match self {
            Error::BinOp(e) => e.location(),
            Error::General(_, loc) => loc.clone(),
            Error::TypeMismatch(e) => e.location(),
        }
    }
}



// the traits below allow us to chain additional context such as source location
// onto errors originating from lower-level systems such as the name resolver or type checker


pub trait ErrorContext<T> {
    fn err_ctx(self, location: &SourceSpan) -> Result<T, Error>;
}

impl<T> ErrorContext<T> for Result<T, NameError> {
    fn err_ctx(self, location: &SourceSpan) -> Result<T, Error> {
        self.map_err(|e| Error::General(format!("{}", e), location.clone()))
    }
}

impl<T> ErrorContext<T> for Result<T, UnifyError> {
    fn err_ctx(self, location: &SourceSpan) -> Result<T, Error> {
        self.map_err(|e| Error::General(format!("{}", e), location.clone()))
    }
}

pub trait TypeMismatchContext<T> {
    fn type_mismatch_err_ctx(self, expected_at: SourceSpan, found_at: SourceSpan) -> Result<T, Error>;
}

impl<T> TypeMismatchContext<T> for Result<T,UnifyError> {
    fn type_mismatch_err_ctx(self, expected_at: SourceSpan, found_at: SourceSpan) -> Result<T, Error> {
        self.map_err(|e| {
            let wrapped_e = TypeMismatchError {
                expected: (e.expected, expected_at),
                found: (e.found, found_at),
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
                found: (e.found, found_at),
            };
            Error::BinOp(wrapped_e)
        })
    }
}
