use super::types::Type;
use thiserror::Error;

#[derive(Clone, Debug, PartialEq, Eq, Error)]
pub enum Error {
    #[error("Unknown type: '{0}'")]
    UnknownType(String),

    #[error("Type mismatch: expected '{expected}', found '{found}'")]
    Mismatch { expected: Type, found: Type },
}
