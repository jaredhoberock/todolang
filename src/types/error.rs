use std::fmt;
use super::types::Type; // Import `Type` from the `types` module

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum TypeError {
    UnknownType(String),
    Mismatch { expected: Type, found: Type },
}

impl fmt::Display for TypeError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            TypeError::UnknownType(name) => write!(f, "Unknown type: '{}'", name),
            TypeError::Mismatch { expected, found } => {
                write!(f, "Type mismatch: expected '{}', found '{}'", expected, found)
            }
        }
    }
}
