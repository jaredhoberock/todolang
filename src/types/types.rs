use internment::Intern;
use std::cell::Cell;
use std::collections::HashMap;
use std::fmt;
use std::ops::Deref;

#[derive(Debug, PartialEq, Eq, Hash)]
pub enum Kind {
    Bool,
    Function(Vec<Type>, Type),
    InferenceVariable(usize),
    Number,
    String,
    Unit,
}

// Public reference type that hides Intern<Type> from users of this module
#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub struct Type(Intern<Kind>);

/// A Substitution maps inference variable IDs to the types that have been inferred
pub type Substitution = HashMap<usize, Type>;

impl Type {
    /// Recursively replaces any inference variable with their bindings from the substitution.
    pub fn apply(&self, subst: &Substitution) -> Type {
        match **self {
            Kind::InferenceVariable(id) => {
                if let Some(t) = subst.get(&id) {
                    // recursively apply when substitution maps to another variable
                    t.apply(subst)
                } else {
                    self.clone()
                }
            },
            Kind::Function(ref params, ref ret) => {
                let new_params: Vec<Type> = params
                    .iter()
                    .map(|t| t.apply(subst))
                    .collect();
                let new_ret = ret.apply(subst);
                Type(Intern::new(Kind::Function(new_params, new_ret)))
            },
            // for concrete types, no substitution is needed
            _ => self.clone(),
        }
    }

    pub fn is_bool(&self) -> bool {
        matches!(**self, Kind::Bool)
    }

    pub fn is_function(&self) -> bool {
        matches!(**self, Kind::Function(_,_))
    }

    pub fn is_number(&self) -> bool {
        matches!(**self, Kind::Number)
    }

    pub fn function_return_type(&self) -> Type {
        match **self {
            Kind::Function(_, result) => result.clone(),
            _ => panic!("Internal compiler error: not a function type"),
        }
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
            Kind::Unit => write!(f, "()"),
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

    pub fn unit(&self) -> Type {
        Type(Intern::new(Kind::Unit))
    }

    pub fn unknown(&self) -> Type {
        self.fresh()
    }
}
