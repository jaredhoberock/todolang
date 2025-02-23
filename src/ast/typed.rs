use crate::source_location::SourceSpan;
use crate::token::Token;
use crate::types::{Type, TypeScheme};
pub use super::untyped::BinOp;
pub use super::untyped::BinOpKind;
pub use super::untyped::Constraint;
pub use super::untyped::UnOp;
pub use super::untyped::UnOpKind;
use std::cell::{Ref,RefCell};
use std::hash::{Hash,Hasher};
use std::rc::Rc;

#[derive(Debug)]
pub enum LiteralValue {
    Bool(bool),
    Number(f64),
    String(String),
}

#[derive(Debug)]
pub struct Literal {
    pub value: LiteralValue,
    pub type_: Type,
    pub location: SourceSpan,
}

#[derive(Debug)]
pub enum Expression {
    Binary {
        lhs: Box<Self>,
        op: BinOp,
        rhs: Box<Self>,
        type_: Type,
        location: SourceSpan,
    },
    Block {
        statements: Vec<Statement>,
        last_expr: Option<Box<Self>>,
        type_: Type,
        location: SourceSpan,
    },
    Call {
        callee: Box<Self>,
        arguments: Vec<Self>,
        type_: Type,
        location: SourceSpan,
    },
    Literal(Literal),
    Unary {
        op: UnOp,
        operand: Box<Self>,
        type_: Type,
        location: SourceSpan,
    },
    Variable {
        name: Token,
        decl: DeclRef,
        type_: Type,
        scope_distance: usize,
        location: SourceSpan,
    },
}

impl Expression {
    pub fn type_(&self) -> Type {
        match self {
            Self::Binary{ type_, .. } => type_.clone(),
            Self::Block { type_, .. } => type_.clone(),
            Self::Call{ type_, .. } => type_.clone(),
            Self::Literal(lit) => lit.type_.clone(),
            Self::Unary {type_, .. } => type_.clone(),
            Self::Variable{ type_, .. } => type_.clone(),
        }
    }

    pub fn location(&self) -> SourceSpan {
        match &self {
            Self::Binary{ location, .. }
            | Self::Block { location, .. }
            | Self::Call { location, .. }
            | Self::Unary { location, .. }
            | Self::Variable { location, .. }
            => location.clone(),
            Self::Literal(l) => l.location.clone(),
        }
    }

    pub fn type_defining_location(&self) -> SourceSpan {
        match &self {
            Self::Block { statements, last_expr, .. } => {
                if let Some(expr) = last_expr {
                    // If there's a trailing expression, use its location
                    expr.type_defining_location()
                } else if !statements.is_empty() {
                    // Otherwise, use the last statement's location
                    statements.last().unwrap().location()
                } else {
                    // If the block is empty, return the block's overall location
                    self.location()
                }
            },
            _ => self.location(),
        }
    }

    pub fn variable_decl(&self) -> Option<DeclRef> {
        match &self {
            Self::Block { last_expr, .. } => last_expr.as_ref()
                .and_then(|e| e.variable_decl()),
            Self::Variable { decl, .. } => Some(decl.clone()),
            _ => None,
        }
    }
}

// Forward declarations (which are themselves caused by recursive functions)
// require Declarations to be in a RefCell
// When a declaration's definition becomes available, we mutate the node
#[derive(Debug, Clone)]
pub struct DeclRef(Rc<RefCell<Declaration>>);

impl DeclRef {
    pub fn new(decl: Declaration) -> Self {
        DeclRef(Rc::new(RefCell::new(decl)))
    }

    pub fn borrow(&self) -> Ref<Declaration> {
        self.0.borrow()
    }

    // provides the definition for a Declaration::Forward by mutating
    // the stored Declaration
    pub fn define(&self, def: Declaration) {
        match *self.borrow() {
            Declaration::Forward { name: ref old_name, .. } => {
                // extract the name and type scheme from the new declaration
                let new_name = def.name();

                if old_name != new_name {
                    panic!("Internal compiler error: definition mismatch: name changed");
                }
            }
            _ => panic!("Internal compiler error: cannot redefine '{}'", self.borrow().name().lexeme)
        }

        match def {
            Declaration::Forward { .. } => {
                panic!("Internal compiler error: cannot define a forward declaration with another forward declaration");
            },
            _ => (),
        }

        *self.0.borrow_mut() = def;
    }
}

impl PartialEq for DeclRef {
    fn eq(&self, other: &Self) -> bool {
        Rc::ptr_eq(&self.0, &other.0)
    }
}

impl Eq for DeclRef {}

impl Hash for DeclRef {
    fn hash<H: Hasher>(&self, state: &mut H) {
        // Hash the pointer address of the Rc
        Rc::as_ptr(&self.0).hash(state);
    }
}

#[derive(Debug)]
pub enum Declaration {
    Forward {
        name: Token,
        type_scheme: TypeScheme,
        location: SourceSpan,
    },
    Function {
        name: Token,
        type_parameters: Vec<DeclRef>,
        parameters: Vec<DeclRef>,
        body: Expression,
        type_scheme: TypeScheme,
        location: SourceSpan,
    },
    Parameter {
        name: Token,
        type_scheme: TypeScheme,
        location: SourceSpan,
    },
    TypeParameter {
        name: Token,
        type_scheme: TypeScheme,
        location: SourceSpan,
    },
    Variable {
        name: Token,
        initializer: Expression,
        type_scheme: TypeScheme,
        location: SourceSpan,
    },
}

impl Declaration {
    pub fn location(&self) -> SourceSpan {
        match &self {
            | Self::Forward { location, .. }
            | Self::Function { location, .. }
            | Self::Parameter { location, .. }
            | Self::TypeParameter { location, .. }
            | Self::Variable { location, .. }
            => location.clone()
        }
    }

    pub fn name(&self) -> &Token {
        match self {
            Self::Forward { name, .. }
            | Self::Function { name, .. }
            | Self::Parameter { name, .. }
            | Self::TypeParameter { name, .. }
            | Self::Variable { name, .. }
            => &name
        }
    }

    pub fn type_scheme(&self) -> &TypeScheme {
        match self {
            Declaration::Forward { type_scheme, .. }
            | Declaration::Function { type_scheme, .. }
            | Declaration::Parameter { type_scheme, .. }
            | Declaration::TypeParameter { type_scheme, .. }
            | Declaration::Variable { type_scheme, .. }
            => type_scheme
        }
    }
}

#[derive(Debug)]
pub enum Statement {
    Assert {
        expr: Expression,
        type_: Type,
        location: SourceSpan,
    },
    Decl(DeclRef),
    Expr {
        expr: Expression,
        type_: Type,
        location: SourceSpan,
    },
    Print {
        expr: Expression,
        type_: Type,
        location: SourceSpan,
    },
}

impl Statement {
    pub fn location(&self) -> SourceSpan {
        match &self {
            Self::Assert{ location, .. }
            | Self::Expr { location, .. }
            | Self::Print { location, .. }
            => location.clone(),
            Self::Decl(decl) => decl.borrow().location(),
        }
    }
}

#[derive(Debug)]
pub struct Module {
    pub statements: Vec<Statement>,
}
