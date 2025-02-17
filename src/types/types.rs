use internment::Intern;
use std::cell::Cell;
use std::collections::{HashMap, HashSet};
use std::fmt;
use std::ops::Deref;

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum TypeVar {
    /// An inference variable that can later be unified.
    Monotype(usize),
    /// A generic (rigid) variable that represents a universally quantified type.
    Generic(usize),
}

impl TypeVar {
    pub fn id(&self) -> usize {
        match &self {
            Self::Monotype(id) => *id,
            Self::Generic(id) => *id,
        }
    }

    /// Wraps the type variable into a Type as an inference variable
    pub fn as_type(&self) -> Type {
        Type(Intern::new(Kind::InferenceVariable(self.clone())))
    }
    
    /// Applies the substitution to this type variable.
    pub fn apply(&self, subst: &Substitution) -> Type {
        match self {
            Self::Monotype(id) => {
                if let Some(t) = subst.get(id) {
                    // Recursively apply substitution to the bound type
                    t.apply(subst)
                } else {
                    // No binding found; wrap the variable
                    self.as_type()
                }
            },
            Self::Generic(_) => {
                // Generic (rigid) variables are not substituted; wrap them
                self.as_type()
            },
        }
    }
}

#[derive(Debug, PartialEq, Eq, Hash)]
pub enum Kind {
    Bool,
    Function(Vec<Type>, Type),
    InferenceVariable(TypeVar),
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
            Kind::InferenceVariable(ref var) => {
                var.apply(&subst)
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

    pub fn as_type_var(&self) -> Option<&TypeVar> {
        match **self {
            Kind::InferenceVariable(ref tv) => Some(tv),
            _ => None,
        }
    }

    pub fn is_bool(&self) -> bool {
        matches!(**self, Kind::Bool)
    }

    pub fn is_concrete(&self) -> bool {
        match **self {
            Kind::InferenceVariable(_) => false,
            Kind::Function(ref params, ref ret) => {
                let params_are_concrete = params
                    .iter()
                    .all(|p| p.is_concrete());
                params_are_concrete && ret.is_concrete()
            },
            _ => true,
        }
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

    /// Returns `true` if the given type variable `var` occurs anywhere in `self`,
    /// taking into account any bindings in the substitution `subst`.
    pub fn occurs(&self, var: &TypeVar, subst: &Substitution) -> bool {
        match **self {
            Kind::InferenceVariable(ref v) => {
                // If it's the same variable, we've found an occurrence.
                if v == var {
                    return true;
                }
                // For monotypes, check their binding in the substitution.
                if let TypeVar::Monotype(id) = v {
                    if let Some(bound_type) = subst.get(id) {
                        return bound_type.occurs(var, subst);
                    }
                }
                // For Generic variables (or if no binding is found), no occurrence.
                false
            },
            Kind::Function(ref params, ref ret) => {
                // Check if the variable occurs in any parameter or in the return type.
                params.iter().any(|p| p.occurs(var, subst)) || ret.occurs(var, subst)
            },
            // For all concrete types (Bool, Number, String, Unit), the variable doesn't occur.
            _ => false,
        }
    }

    /// Returns a set of free type variables in this type,
    /// taking into account the current substitution.
    pub fn free_type_vars(&self, subst: &Substitution) -> HashSet<TypeVar> {
        // first apply the substitution to get the "resolved" type
        let applied = self.apply(subst);
        let mut free_vars = HashSet::new();
        applied.collect_free_vars_internal(&mut free_vars);
        free_vars
    }

    fn collect_free_vars_internal(&self, free_vars: &mut HashSet<TypeVar>) {
        match **self {
            Kind::InferenceVariable(ref tv) => {
                if let TypeVar::Monotype(_) = tv {
                    free_vars.insert(tv.clone());
                }
                // we ignore TypeVar::Generic because they are already quantified
            },
            Kind::Function(ref params, ref ret) => {
                for param in params {
                    param.collect_free_vars_internal(free_vars);
                }
                ret.collect_free_vars_internal(free_vars);
            },
            // Concrete types contain no type variables
            _ => {}
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
            Kind::InferenceVariable(var) => match var {
                TypeVar::Monotype(id) => write!(f, "T{}", id),
                TypeVar::Generic(id) => write!(f, "G{}", id),
            },
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
        Type(Intern::new(Kind::InferenceVariable(TypeVar::Monotype(id))))
    }

    pub fn generic(&self) -> Type {
        let id = self.counter.get();
        self.counter.set(id + 1);
        Type(Intern::new(Kind::InferenceVariable(TypeVar::Generic(id))))
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
}
