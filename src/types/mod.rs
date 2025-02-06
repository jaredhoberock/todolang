mod checker;
mod environment;
mod error;
mod types;
mod unify;

pub use checker::*;
pub use environment::TypeEnvironment;
pub use error::Error;
pub use types::Type;
pub use unify::unify;
