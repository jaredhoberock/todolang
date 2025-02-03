use internment::Intern;
use std::cell::Cell;
use std::fmt;
use std::ops::Deref;

#[derive(Debug, PartialEq, Eq, Hash)]
pub enum Kind {
    Number,
    String,
    Bool,
    Function(Vec<Type>, Type),
    InferenceVariable(usize),
}

// Public reference type that hides Intern<Type> from users of this module
#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub struct Type(Intern<Kind>);

impl Type {
    pub fn is_number(&self) -> bool {
        matches!(**self, Kind::Number)
    }
}

impl Deref for Type {
    type Target = Kind;

    fn deref(&self) -> &Self::Target {
        self.0.as_ref()
    }
}

impl fmt::Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match &**self {
            Kind::Number        => write!(f, "Number"),
            Kind::String        => write!(f, "String"),
            Kind::Bool          => write!(f, "Bool"),
            Kind::Function(params,result) => {
                write!(f, "fn(")?;
                for (i, param) in params.iter().enumerate() {
                    if i > 0 { write!(f, ", ")?; }
                    write!(f, "{}", param)?;
                }
                write!(f, ") -> {}", result)
            },
            Kind::InferenceVariable(id) => write!(f, "T{}", id),
        }
    }
}

pub struct TypeArena {
    counter: Cell<usize>,
}

impl TypeArena {
    pub fn new() -> Self {
      Self { counter: Cell::new(0) }
    }

    pub fn fresh(&self) -> Type {
        let id = self.counter.get();
        self.counter.set(id + 1);
        Type(Intern::new(Kind::InferenceVariable(id)))
    }

    pub fn bool(&self) -> Type {
        Type(Intern::new(Kind::Bool))
    }

    pub fn function(&self, parameters: Vec<Type>, result: Type) -> Type {
        Type(Intern::new(Kind::Function(parameters, result)))
    }

    pub fn number(&self) -> Type {
        Type(Intern::new(Kind::Number))
    }

    pub fn string(&self) -> Type {
        Type(Intern::new(Kind::String))
    }

    pub fn unknown(&self) -> Type {
        self.fresh()
    }
}
