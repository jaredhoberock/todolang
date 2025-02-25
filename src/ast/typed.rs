use crate::source_location::SourceSpan;
use crate::token::Token;
use crate::types::{Substitution, Type, TypeScheme};
pub use super::untyped::BinOp;
pub use super::untyped::BinOpKind;
pub use super::untyped::Constraint;
pub use super::untyped::UnOp;
pub use super::untyped::UnOpKind;
use std::cell::{Ref,RefCell,RefMut};
use std::hash::{Hash,Hasher};
use std::ops::Deref;
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

// Constraints originate in Expressions
// Therefore, provenance tracking for constraints requires
// storing references to expression nodes
// However, each ExprRef in the AST is unique
#[derive(Debug, Clone)]
pub struct ExprRef(Rc<Expression>);

impl ExprRef {
    pub fn new(expr: Expression) -> Self {
        Self(Rc::new(expr))
    }

    pub fn get_mut(&mut self) -> &mut Expression {
        Rc::get_mut(&mut self.0)
            .expect("Internal compiler error: ExprRef is unexpectedly shared at mutation time")
    }
}

impl Deref for ExprRef {
    type Target = Expression;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl PartialEq for ExprRef {
    fn eq(&self, other: &Self) -> bool {
        Rc::ptr_eq(&self.0, &other.0)
    }
}

impl Eq for ExprRef {}

impl Hash for ExprRef {
    fn hash<H: Hasher>(&self, state: &mut H) {
        Rc::as_ptr(&self.0).hash(state)
    }
}

#[derive(Debug)]
pub enum Expression {
    Binary {
        lhs: ExprRef,
        op: BinOp,
        rhs: ExprRef,
        type_: Type,
        location: SourceSpan,
    },
    Block {
        statements: Vec<Statement>,
        last_expr: Option<ExprRef>,
        type_: Type,
        location: SourceSpan,
    },
    Call {
        callee: ExprRef,
        arguments: Vec<ExprRef>,
        type_: Type,
        location: SourceSpan,
    },
    Literal(Literal),
    Unary {
        op: UnOp,
        operand: ExprRef,
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
    // mutates self.type_ by applying the given mapping to self.type_
    // Note that this does not recurse into subexpressions, it simply
    // mutates self.type_
    pub fn apply_to_type(&mut self, mapping: &Substitution) {
        match self {
            Self::Binary { type_, .. }
            | Self::Block { type_, .. }
            | Self::Call { type_, .. }
            | Self::Unary { type_, .. }
            | Self::Variable { type_, .. }
            => *type_ = type_.apply(mapping),
            Self::Literal(_) => {},
        }
    }

    pub fn callee(&self) -> Option<ExprRef> {
        match self {
            Self::Call { callee, .. } => Some(callee.clone()),
            _ => None,
        }
    }

    pub fn arguments(&self) -> Option<&Vec<ExprRef>> {
        match self {
            Self::Call { arguments, .. } => Some(arguments),
            _ => None,
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

    pub fn borrow_mut(&mut self) -> RefMut<Declaration> {
        self.0.borrow_mut()
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
        body: ExprRef,
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
        initializer: ExprRef,
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
        expr: ExprRef,
        location: SourceSpan,
    },
    Decl(DeclRef),
    Expr {
        expr: ExprRef,
        location: SourceSpan,
    },
    Print {
        expr: ExprRef,
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
