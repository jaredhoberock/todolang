mod checker;
mod environment;
mod environment2;
mod error;
mod types;
mod unify;

pub use checker::*;
pub use environment::TypeEnvironment;
pub use environment2::TypeEnvironment as TypeEnvironment2;
pub use environment2::Error as Error2;
pub use error::Error;
pub use types::Type;
pub use unify::unify;
