use internment::Intern;
use std::ops::Deref;

#[derive(Debug, PartialEq, Eq, Hash)]
pub enum Kind {
    Number,
    Unknown,
}

// Public reference type that hides Intern<Type> from users of this module
#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub struct Type(Intern<Kind>);

impl Deref for Type {
    type Target = Kind;

    fn deref(&self) -> &Self::Target {
        self.0.as_ref()
    }
}

pub struct TypeArena;

impl TypeArena {
    pub fn new() -> Self {
      Self
    }

    pub fn number(&self) -> Type {
        Type(Intern::new(Kind::Number))
    }

    pub fn unknown(&self) -> Type {
        Type(Intern::new(Kind::Unknown))
    }
}
