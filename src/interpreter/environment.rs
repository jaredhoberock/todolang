use crate::ast::typed::*;
use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;
use super::interpreter::*;

#[derive(Clone, Debug)]
pub struct Function {
    decl: DeclRef,
    closure: Rc<RefCell<Environment>>,
}

impl Function {
    pub fn new(decl: DeclRef, closure: Rc<RefCell<Environment>>) -> Self {
        Function { decl, closure }
    }

    pub fn call(&self, interp: &mut Interpreter, args: &Vec<Value>) -> Result<Value,Error> {
        // create a new environment for the function call
        let env = Environment::new_enclosed_shared(self.closure.clone());

        // get the parameter names and body
        let decl_borrow = self.decl.borrow();
        let (parameter_names, body) = match &*decl_borrow {
            Declaration::Function{ parameters, body, .. } => {
                let parameter_names: Vec<String> = parameters.iter()
                    .map(|p| p.borrow().name().lexeme.clone())
                    .collect();
                (parameter_names, body)
            },
            _ => panic!("Function has bad declaration"),
        };

        // bind arguments to parameters
        for (name, arg) in parameter_names.iter().zip(args.iter()) {
            env.define(name.clone(), arg.clone())
                .expect("Internal error: defining function parameter failed");
        }

        // interpret the call
        interp.with_environment(env, |interpreter| {
            interpreter.interpret_expression(&body)
        })
    }

    fn to_string(&self) -> String {
        format!("<fn {}>", &self.decl.borrow().name().lexeme)
    }
}

impl PartialEq for Function {
    fn eq(&self, other: &Self) -> bool {
        self.decl == other.decl
    }
}

impl Eq for Function {}

#[derive(Clone, Debug, PartialEq)]
pub enum Value {
    Bool(bool),
    Function(Function),
    Number(f64),
    String(String),
    Unit,
}

impl Value {
    pub fn as_bool(&self) -> bool {
        match self {
            Value::Bool(b) => *b,
            _ => panic!("Value is not a 'Bool'"),
        }
    }

    pub fn as_f64(&self) -> f64 {
        match self {
            Value::Number(n) => *n,
            _ => panic!("Value is not a 'Number'"),
        }
    }

    pub fn evaluate_binary_operation(&self, op: &BinOp, rhs: &Self) -> Self {
        use Value::*;
        use BinOpKind::*;

        match op.kind {
            Add   => match (self, rhs) {
                (Number(n1), Number(n2)) => Number(n1 + n2),
                (String(s1), String(s2)) => String(s1.clone() + &s2),
                (_, _) => panic!("Operands must be two numbers or two strings."),
            },
            And   => Bool(self.as_bool() && rhs.as_bool()),
            Div => match (self, rhs) {
                (Number(n1), Number(n2)) => Number(n1 / n2),
                (_, _) => panic!("Operands must be two numbers."),
            },
            Eq    => Bool(self == rhs),
            Gt    => Bool(self.as_f64() > rhs.as_f64()),
            GtEq  => Bool(self.as_f64() >= rhs.as_f64()),
            Lt    => Bool(self.as_f64() <  rhs.as_f64()),
            LtEq  => Bool(self.as_f64() <= rhs.as_f64()),
            Mul => match (self, rhs) {
                (Number(n1), Number(n2)) => Number(n1 * n2),
                (_, _) => panic!("Operands must be two numbers."),
            },
            Or    => Bool(self.as_bool() || self.as_bool()),
            NotEq => Bool(self != rhs),
            Sub   => Number(self.as_f64() - rhs.as_f64()),
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

#[derive(Debug)]
pub struct Environment {
    enclosing: Option<Rc<RefCell<Environment>>>,
    values: HashMap<String, Value>,
}

impl Environment {
    pub fn new_shared_global() -> Rc<RefCell<Self>> {
        Rc::new(RefCell::new(Self {
            enclosing: None,
            values: HashMap::new(),
        }))
    }

    pub fn new_enclosed_shared(enclosing: Rc<RefCell<Self>>) -> Rc<RefCell<Self>> {
        Rc::new(RefCell::new(Self {
            enclosing: Some(enclosing),
            values: HashMap::new(),
        }))
    }

    // Define a new variable in the current scope
    pub fn define(&mut self, name: String, value: Value) -> Result<(), String> {
        if self.values.contains_key(&name) {
            Err(format!("Variable '{}' is already defined.", name).into())
        } else {
            self.values.insert(name, value.clone());
            Ok(())
        }
    }
}

/// An extension trait to add instance methods to Rc<RefCell<Environment>>
pub trait EnvironmentMethods {
    /// Define a new variable in the current environment.
    /// Returns an error if the variable is already defined.
    fn define(&self, name: String, value: Value) -> Result<(), String>;

    /// Traverse the environment chain by `distance` scopes.
    /// A distance of 0 means the current scope; 1 means the immediate enclosing scope; etc.
    fn ancestor(&self, distance: usize) -> Rc<RefCell<Environment>>;

    /// Look up a variable by name using its precomputed scope distance.
    fn get_at(&self, distance: usize, name: &str) -> Result<Value, String>;

    /// Assign a value to a variable at the specified scope distance.
    fn _assign_at(&self, distance: usize, name: &str, value: Value) -> Result<(), String>;
}

impl EnvironmentMethods for Rc<RefCell<Environment>> {
    fn define(&self, name: String, value: Value) -> Result<(), String> {
        self.borrow_mut().define(name, value)
    }

    fn ancestor(&self, distance: usize) -> Rc<RefCell<Environment>> {
        let mut environment = self.clone();
        for _ in 0..distance {
            // Since resolution ensures the distance is valid, this unwrap is acceptable.
            let enclosing = environment
                .borrow()
                .enclosing
                .clone()
                .expect("Internal error: No enclosing environment at the given distance.");
            environment = enclosing;
        }
        environment
    }

    fn get_at(&self, distance: usize, name: &str) -> Result<Value, String> {
        let target_env = self.ancestor(distance);
        // Create a new scope to limit the lifetime of the borrow.
        let result = {
            let borrowed = target_env.borrow();
            borrowed.values.get(name).cloned()
        };
        result.ok_or_else(|| format!("Undefined variable '{}'", name))
    }

    fn _assign_at(&self, distance: usize, name: &str, value: Value) -> Result<(), String> {
        let target_env = self.ancestor(distance);
        let mut env_ref = target_env.borrow_mut();
        if env_ref.values.contains_key(name) {
            env_ref.values.insert(name.to_string(), value);
            Ok(())
        } else {
            Err(format!("Undefined variable '{}'", name))
        }
    }
}
