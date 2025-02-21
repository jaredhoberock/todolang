use crate::source_location::SourceSpan;
use crate::token::{Token, TokenKind};
use derive_more::Display;

#[derive(Debug)]
pub enum LiteralValue {
    Bool(bool),
    Number(f64),
    String(String),
}

#[derive(Debug)]
pub struct Literal {
    pub value: LiteralValue,
    pub location: SourceSpan,
}

#[derive(Debug, Clone, Display)]
pub enum BinOpKind {
    #[display(fmt = "+")]
    Add,
    #[display(fmt = "and")]
    And,
    #[display(fmt = "/")]
    Div,
    #[display(fmt = "==")]
    Eq,
    #[display(fmt = ">")]
    Gt,
    #[display(fmt = ">=")]
    GtEq,
    #[display(fmt = "<")]
    Lt,
    #[display(fmt = "<=")]
    LtEq,
    #[display(fmt = "*")]
    Mul,
    #[display(fmt = "!=")]
    NotEq,
    #[display(fmt = "or")]
    Or,
    #[display(fmt = "-")]
    Sub,
}

#[derive(Debug, Clone)]
pub struct BinOp {
    pub kind: BinOpKind,
    pub location: SourceSpan,
}

impl BinOp {
    pub fn from_token(op: Token) -> Self {
        let kind = match op.kind {
            TokenKind::Plus => BinOpKind::Add,
            TokenKind::And => BinOpKind::And,
            TokenKind::Slash => BinOpKind::Div,
            TokenKind::EqualEqual => BinOpKind::Eq,
            TokenKind::Greater => BinOpKind::Gt,
            TokenKind::GreaterEqual => BinOpKind::GtEq,
            TokenKind::Less => BinOpKind::Lt,
            TokenKind::LessEqual => BinOpKind::LtEq,
            TokenKind::Star => BinOpKind::Mul,
            TokenKind::BangEqual => BinOpKind::NotEq,
            TokenKind::Or => BinOpKind::Or,
            TokenKind::Minus => BinOpKind::Sub,
            _ => panic!("Internal error: unknown binary operator '{}'", op.lexeme),
        };
        Self { kind, location: op.location }
    }
}

#[derive(Debug, Clone)]
pub enum UnOpKind {
    Neg, // -expr
    Not, // !expr
}

#[derive(Debug, Clone)]
pub struct UnOp {
    pub kind: UnOpKind,
    pub location: SourceSpan,
}

impl UnOp {
    pub fn from_token(op: Token) -> Self {
        let kind = match op.kind {
            TokenKind::Bang => UnOpKind::Not,
            TokenKind::Minus => UnOpKind::Neg,
            _ => panic!("Internal error: unknown unary operator '{}'", op.lexeme),
        };
        Self { kind, location: op.location }
    }
}

#[derive(Debug)]
pub enum Expression {
    Binary {
        lhs: Box<Self>,
        op: BinOp,
        rhs: Box<Self>,
    },
    Block {
        lbrace: Token,
        statements: Vec<Statement>,
        last_expr: Option<Box<Self>>,
        rbrace: Token,
    },
    Call {
        callee: Box<Self>,
        arguments: Vec<Self>,
        closing_paren: Token,
    },
    Literal(Literal),
    Unary {
        op: UnOp,
        operand: Box<Self>,
    },
    Variable {
        name: Token,
    },
}

impl Expression {
    pub fn location(&self) -> SourceSpan {
        match &self {
            Self::Binary{ lhs, rhs, .. } => {
                lhs.location()
                    .merge(&rhs.location())
            },
            Self::Block{ lbrace, rbrace, .. } => {
                lbrace.location.merge(&rbrace.location)
            },
            Self::Call{ callee, closing_paren, .. } => {
                callee.location()
                    .merge(&closing_paren.location)
            },
            Self::Literal(lit) => {
                lit.location.clone()
            },
            Self::Unary{ op, operand } => {
                op.location.merge(&operand.location())
            },
            Self::Variable{ name } => {
                name.location.clone()
            }
        }
    }
}

#[derive(Debug)]
pub struct TypeExpression {
    pub identifier: Token,
}

impl TypeExpression {
    pub fn location(&self) -> SourceSpan {
        self.identifier.location.clone()
    }
}

#[derive(Debug)]
pub struct TypeAscription {
    pub colon: Token,
    pub expr: TypeExpression,
}

impl TypeAscription {
    pub fn location(&self) -> SourceSpan {
        self.colon.location.merge(&self.expr.location())
    }
}

#[derive(Debug, Clone)]
pub struct Constraint {
    pub name: Token,
}

#[derive(Debug)]
pub struct TypeParameter {
    pub name: Token,
    pub constraint: Option<Constraint>,
}

impl TypeParameter {
    pub fn location(&self) -> SourceSpan {
        self.name.location.clone()
    }
}

#[derive(Debug)]
pub struct Parameter {
    pub name: Token,
    pub ascription: TypeAscription,
}

impl Parameter {
    pub fn location(&self) -> SourceSpan {
        self.name.location.merge(&self.ascription.location())
    }
}

#[derive(Debug)]
pub enum Declaration {
    Function {
        name: Token,
        type_parameters: Vec<TypeParameter>,
        parameters: Vec<Parameter>,
        return_type: TypeExpression,
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
    pub fn location(&self) -> SourceSpan {
        match &self {
            Self::Function{ name, body, .. } => {
                name.location.merge(&body.location())
            },
            Self::Variable{ name, semi, .. } => {
                name.location.merge(&semi.location)
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
    pub fn location(&self) -> SourceSpan {
        match &self {
            Self::Assert{ expr, semi } => {
                expr.location().merge(&semi.location)
            },
            Self::Decl(d) => d.location(),
            Self::Expr{ expr, semi } => {
                expr.location().merge(&semi.location)
            },
            Self::Print{ print, semi, .. } => {
                print.location.merge(&semi.location)
            }
        }
    }
}

#[derive(Debug)]
pub struct Module {
    pub statements: Vec<Statement>,
}
