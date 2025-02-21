use crate::source_location::SourceSpan;
use crate::token::Token;
use crate::types::{Type, TypeScheme};
pub use super::untyped::BinOp;
pub use super::untyped::BinOpKind;
pub use super::untyped::Constraint;
pub use super::untyped::UnOp;
pub use super::untyped::UnOpKind;
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
        decl: Rc<Declaration>,
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
            Expression::Block { statements, last_expr, .. } => {
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
}

#[derive(Debug)]
pub enum Declaration {
    Function {
        name: Token,
        type_parameters: Vec<Rc<Self>>,
        parameters: Vec<Rc<Self>>,
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
            Self::Function { location, .. }
            | Self::Parameter { location, .. }
            | Self::TypeParameter { location, .. }
            | Self::Variable { location, .. }
            => location.clone()
        }
    }

    pub fn name(&self) -> &Token {
        match self {
            Self::Function { name, .. } => &name,
            Self::Parameter { name, .. } => &name,
            Self::TypeParameter { name, .. } => &name,
            Self::Variable { name, .. } => &name,
        }
    }

    pub fn type_scheme(&self) -> &TypeScheme {
        match self {
            Declaration::Function { type_scheme, .. }
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
    Decl(Rc<Declaration>),
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
            Self::Decl(decl) => decl.as_ref().location(),
        }
    }
}

#[derive(Debug)]
pub struct Module {
    pub statements: Vec<Statement>,
}
