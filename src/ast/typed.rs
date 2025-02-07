use crate::source_location::SourceSpan;
use crate::token::Token;
use crate::types::Type;
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
    Block {
        statements: Vec<Statement>,
        expr: Option<Box<Self>>,
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
    Variable {
        name: Token,
        decl: Rc<Declaration>,
        scope_distance: usize,
        location: SourceSpan,
    },
}

impl Expression {
    pub fn type_(&self) -> Type {
        match self {
            Self::Block { type_, .. } => type_.clone(),
            Self::Call{ type_, .. } => type_.clone(),
            Self::Literal(lit) => lit.type_.clone(),
            Self::Variable{ decl, .. } => decl.type_(),
        }
    }
}

#[derive(Debug)]
pub enum Declaration {
    Function {
        name: Token,
        parameters: Vec<Rc<Self>>,
        body: Expression,
        type_: Type,
        location: SourceSpan,
    },
    Parameter {
        name: Token,
        type_: Type,
        location: SourceSpan,
    },
    Variable {
        name: Token,
        initializer: Expression,
        type_: Type,
        location: SourceSpan,
    },
}

impl Declaration {
    pub fn name(&self) -> &Token {
        match self {
            Self::Function { name, .. } => &name,
            Self::Parameter { name, .. } => &name,
            Self::Variable { name, .. } => &name,
        }
    }

    pub fn type_(&self) -> Type {
        match self {
            Declaration::Function { type_, .. }  => type_.clone(),
            Declaration::Parameter { type_, .. } => type_.clone(),
            Declaration::Variable { type_, .. }  => type_.clone(),
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

#[derive(Debug)]
pub struct Module {
    pub statements: Vec<Statement>,
}
