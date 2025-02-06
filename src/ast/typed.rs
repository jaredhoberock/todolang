use crate::source_location::SourceSpan;
use crate::token::Token;
use crate::types::Type;
use std::rc::Rc;

#[derive(Debug)]
pub enum LiteralValue {
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
        location: SourceSpan,
    },
}

impl Expression {
    pub fn type_(&self) -> Type {
        match self {
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
        body: BlockStatement,
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
pub struct BlockStatement {
    pub statements: Vec<Statement>,
    pub type_: Type,
    pub location: SourceSpan,
}

#[derive(Debug)]
pub enum Statement {
    Block(BlockStatement),
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
