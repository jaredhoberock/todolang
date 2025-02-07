use crate::ast::typed::*;
use std::cell::RefCell;
use std::rc::Rc;
use std::rc::Weak;

#[derive(Clone, Debug)]
pub struct Function {
    decl: Rc<Declaration>
}

impl Function {
    fn new(decl: Rc<Declaration>) -> Self {
        Function { decl }
    }

    fn to_string(&self) -> String {
        let decl = &*self.decl;
        format!("<fn {}>", &decl.name().lexeme)
    }
}

#[derive(Clone, Debug)]
pub enum Value {
    Bool(bool),
    Function(Function),
    Number(f64),
    String(String),
    Unit,
}

impl Value {
    pub fn unit() -> Self {
        Self::Unit
    }

    pub fn as_bool(&self) -> bool {
        match self {
            Value::Bool(b) => *b,
            _ => panic!("Value is not a 'bool'"),
        }
    }
}

impl std::fmt::Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        let s = match self {
            Value::Bool(b) => b.to_string(),
            Value::Function(f) => f.to_string(),
            Value::Number(n) => {
                let s = n.to_string();
                if s.ends_with(".0") {
                    s[..s.len() - 2].to_string()
                } else {
                    s
                }
            }
            Value::String(s) => s.clone(),
            Value::Unit => "()".to_string(),
        };

        write!(f, "{}", s)
    }
}
