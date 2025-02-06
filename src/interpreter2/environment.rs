use crate::ast::typed::*;
use std::collections::HashMap;
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
    Function(Function),
    Number(f64),
    String(String),
    Unit,
}

impl Value {
    pub fn as_string(&self) -> String {
        match self {
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
        }
    }
}

impl std::fmt::Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{}", self.as_string())
    }
}
