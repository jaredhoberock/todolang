use crate::source_location::SourceSpan;
use crate::token::Token;

#[derive(Debug)]
pub enum LiteralValue {
    Bool(bool),
    Number(f64),
    String(String),
}

#[derive(Debug)]
pub struct Literal {
    pub value: LiteralValue,
    pub span: SourceSpan,
}

#[derive(Debug)]
pub enum Expression {
    Block {
        lbrace: Token,
        statements: Vec<Statement>,
        expr: Option<Box<Self>>,
        rbrace: Token,
    },
    Call {
        callee: Box<Self>,
        arguments: Vec<Self>,
        closing_paren: Token,
    },
    Literal(Literal),
    Variable {
        name: Token,
    },
}

impl Expression {
    pub fn source_span(&self) -> SourceSpan {
        match &self {
            Self::Block{ lbrace, rbrace, .. } => {
                lbrace.span.merge(&rbrace.span)
            },
            Self::Call{ callee, closing_paren, .. } => {
                callee.source_span()
                    .merge(&closing_paren.span)
            },
            Self::Literal(lit) => {
                lit.span.clone()
            },
            Self::Variable{ name } => {
                name.span.clone()
            }
        }
    }
}

#[derive(Debug)]
pub struct TypeExpression {
    pub identifier: Token,
}

impl TypeExpression {
    pub fn source_span(&self) -> SourceSpan {
        self.identifier.span.clone()
    }
}

#[derive(Debug)]
pub struct TypeAscription {
    pub colon: Token,
    pub expr: TypeExpression,
}

impl TypeAscription {
    pub fn source_span(&self) -> SourceSpan {
        self.colon.span.merge(&self.expr.source_span())
    }
}

#[derive(Debug)]
pub struct Parameter {
    pub name: Token,
    pub ascription: TypeAscription,
}

impl Parameter {
    pub fn source_span(&self) -> SourceSpan {
        self.name.span.merge(&self.ascription.source_span())
    }
}

#[derive(Debug)]
pub enum Declaration {
    Function {
        name: Token,
        parameters: Vec<Parameter>,
        body: Expression,
    },
    Variable {
        name: Token,
        ascription: TypeAscription,
        initializer: Expression,
        semi: Token,
    },
}

impl Declaration {
    pub fn source_span(&self) -> SourceSpan {
        match &self {
            Self::Function{ name, body, .. } => {
                name.span.merge(&body.source_span())
            },
            Self::Variable{ name, semi, .. } => {
                name.span.merge(&semi.span)
            },
        }
    }
}

#[derive(Debug)]
pub enum Statement {
    Assert {
        expr: Expression, 
        semi: Token,
    },
    Decl(Declaration),
    Expr {
        expr: Expression,
        semi: Token,
    },
    Print {
        print: Token,
        expr: Expression,
        semi: Token,
    },
}

impl Statement {
    pub fn source_span(&self) -> SourceSpan {
        match &self {
            Self::Assert{ expr, semi } => {
                expr.source_span().merge(&semi.span)
            },
            Self::Decl(d) => d.source_span(),
            Self::Expr{ expr, semi } => {
                expr.source_span().merge(&semi.span)
            },
            Self::Print{ print, semi, .. } => {
                print.span.merge(&semi.span)
            }
        }
    }
}

#[derive(Debug)]
pub struct Module {
    pub statements: Vec<Statement>,
}
