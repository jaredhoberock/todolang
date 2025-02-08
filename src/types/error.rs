use crate::source_location::SourceSpan;
use super::types::Type;
use thiserror::Error;

#[derive(Clone, Debug, PartialEq, Eq, Error)]
pub enum InternalError {
    #[error("Unknown type: '{0}'")]
    UnknownType(String),

    #[error("Type mismatch: expected '{expected}', found '{found}'")]
    Mismatch { expected: Type, found: Type, },
}

#[derive(Clone, Debug, Error)]
#[error("{details}")]
pub struct Error {
    details: InternalError,
    pub location: SourceSpan,
}

impl Error {
    pub fn new(details: InternalError, location: SourceSpan) -> Self {
        Self { details, location, }
    }
}
