use crate::token::*;
use crate::syntax::FunctionDeclaration;
use super::interpreter::{Error, Interpreter};
use std::cell::RefCell;
use std::collections::HashMap;
use std::ops::ControlFlow;
use std::rc::Rc;
use std::rc::Weak;

pub type Shared<T> = Rc<RefCell<T>>;

#[derive(Clone, Debug, PartialEq)]
pub enum Value {
    Callable(Callable),
    Bool(bool),
    Instance(Shared<Instance>),
    Nil,
    Number(f64),
    String(String),
}

impl From<Rc<Class>> for Value {
    fn from(class: Rc<Class>) -> Self {
        Value::Callable(Callable::Class(class))
    }
}

impl Value {
    pub fn as_bool(&self) -> bool {
        match self {
            Value::Bool(b) => *b,
            Value::Nil => false,
            _ => true,
        }
    }

    fn as_string(&self) -> String {
        match self {
            Value::Callable(c) => c.to_string(),
            Value::Instance(i) => i.borrow().to_string(),
            Value::Number(n) => {
                let s = n.to_string();
                if s.ends_with(".0") {
                    s[..s.len() - 2].to_string()
                } else {
                    s
                }
            }
            Value::String(s) => s.clone(),
            Value::Bool(b) => b.to_string(),
            Value::Nil => "nil".to_string(),
        }
    }

    pub fn as_f64(&self) -> Result<f64, String> {
        match self {
            Value::Number(n) => Ok(*n),
            _ => Err("Value is not a number".to_string()),
        }
    }

    pub fn evaluate_binary_operation(&self, op: &TokenKind, rhs: &Self) -> Result<Self, Error> {
        use Value::*;

        match op {
            TokenKind::BangEqual => Ok(Bool(self != rhs)),
            TokenKind::EqualEqual => Ok(Bool(self == rhs)),
            TokenKind::Greater => Ok(Bool(self.as_f64()? > rhs.as_f64()?)),
            TokenKind::GreaterEqual => Ok(Bool(self.as_f64()? >= rhs.as_f64()?)),
            TokenKind::Less => Ok(Bool(self.as_f64()? < rhs.as_f64()?)),
            TokenKind::LessEqual => Ok(Bool(self.as_f64()? <= rhs.as_f64()?)),
            TokenKind::Minus => Ok(Number(self.as_f64()? - rhs.as_f64()?)),
            TokenKind::Plus => match (self, rhs) {
                (Number(n1), Number(n2)) => Ok(Number(n1 + n2)),
                (String(s1), String(s2)) => Ok(String(s1.clone() + &s2)),
                (_, _) => Err("Operands must be two numbers or two strings.".into()),
            },
            TokenKind::Slash => match (self, rhs) {
                (Number(n1), Number(n2)) => Ok(Number(n1 / n2)),
                (_, _) => Err("Operands must be two numbers.".into()),
            },
            TokenKind::Star => match (self, rhs) {
                (Number(n1), Number(n2)) => Ok(Number(n1 * n2)),
                (_, _) => Err("Operands must be two numbers.".into()),
            },
            _ => Err("Unexpected operator in binary operation".into()),
        }
    }
}

impl std::fmt::Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{}", self.as_string())
    }
}

#[derive(Clone)]
pub enum Callable {
    Class(Rc<Class>),
    Native(
        usize,
        Rc<dyn Fn(&mut Interpreter, &Vec<Value>) -> Result<Value, Error>>,
    ),
    User(UserFunction),
}

impl Callable {
    pub fn new_native_function<F>(arity: usize, func: F) -> Self
    where
        F: Fn(&mut Interpreter, &Vec<Value>) -> Result<Value, Error> + 'static,
    {
        Self::Native(arity, Rc::new(func))
    }

    pub fn call(&self, interp: &mut Interpreter, arguments: &Vec<Value>) -> Result<Value, Error> {
        match &self {
            Callable::Class(c) => c.call(interp, arguments),
            Callable::Native(_arity, f) => f(interp, arguments),
            Callable::User(f) => f.call(interp, arguments),
        }
    }

    pub fn arity(&self) -> usize {
        match &self {
            Callable::Class(c) => c.arity(),
            Callable::Native(arity, _) => *arity,
            Callable::User(f) => f.arity(),
        }
    }

    fn to_string(&self) -> String {
        match &self {
            Callable::Class(class) => class.to_string(),
            Callable::Native(_, _) => "<native fn>".to_string(),
            Callable::User(fun) => fun.to_string(),
        }
    }
}

impl PartialEq for Callable {
    fn eq(&self, other: &Self) -> bool {
        match (&self, &other) {
            (Callable::User(a), Callable::User(b)) => {
                // compare raw pointers to FunctionDecl
                a.decl_ptr == b.decl_ptr
            }
            (Callable::Native(_, a), Callable::Native(_, b)) => Rc::ptr_eq(a, b),
            _ => false,
        }
    }
}

impl std::fmt::Debug for Callable {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{}", self.to_string())
    }
}

#[derive(Clone)]
pub struct Instance {
    class: Rc<Class>,
    fields: HashMap<String, Value>,
    self_rc: Weak<RefCell<Self>>,
}

impl Instance {
    fn new_shared(class: Rc<Class>) -> Shared<Self> {
        Rc::new_cyclic(|weak| {
            RefCell::new(Self {
                class: class.clone(),
                fields: HashMap::new(),
                self_rc: weak.clone(),
            })
        })
    }

    fn to_string(&self) -> String {
        self.class.to_string() + " instance"
    }

    pub fn get(&self, name: &Token) -> Result<Value, Error> {
        if let Some(value) = self.fields.get(&name.lexeme) {
            return Ok(value.clone());
        }
        self.class
            .find_method(&name.lexeme)
            .and_then(|m| m.bind_this(self.self_rc.upgrade().unwrap()))
            .map(|m| m.as_value())
            .map_err(|_| format!("Undefined property '{}'.", name.lexeme).into())
    }

    pub fn set(&mut self, name: &Token, value: &Value) -> Result<(), String> {
        self.fields.insert(name.lexeme.clone(), value.clone());
        Ok(())
    }
}

impl PartialEq for Instance {
    fn eq(&self, other: &Self) -> bool {
        std::ptr::eq(self, other)
    }
}

impl std::fmt::Debug for Instance {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{}", self.to_string())
    }
}

#[derive(Clone)]
pub struct Class {
    name: String,
    superclass: Option<Rc<Self>>,
    methods: HashMap<String, UserFunction>,
    self_rc: Weak<Self>,
}

impl Class {
    pub fn new_rc(
        name: &String,
        superclass: &Option<Rc<Self>>,
        methods: &HashMap<String, UserFunction>,
    ) -> Rc<Self> {
        Rc::new_cyclic(|weak| Self {
            name: name.clone(),
            superclass: superclass.clone(),
            methods: methods.clone(),
            self_rc: weak.clone(),
        })
    }

    pub fn call(&self, interpreter: &mut Interpreter, arguments: &Vec<Value>) -> Result<Value, Error> {
        let class_rc = self.self_rc.upgrade().unwrap();
        let instance = Instance::new_shared(class_rc);

        // look for an "init" method and call it if found
        self.find_method(&"init".to_string())
            .and_then(|m| m.bind_this(instance.clone()))
            .and_then(|m| m.call(interpreter, arguments))
            .ok();

        Ok(Value::Instance(instance.clone()))
    }

    pub fn find_method(&self, name: &String) -> Result<UserFunction, Error> {
        match (self.methods.get(name), &self.superclass) {
            (Some(method), _) => Ok(method.clone()),
            (None, Some(superclass)) => superclass.find_method(name),
            (None, None) => Err(format!("Undefined method '{}'", name).into()),
        }
    }

    fn arity(&self) -> usize {
        self.find_method(&"init".to_string())
            .map(|m| m.arity())
            .unwrap_or(0)
    }

    fn to_string(&self) -> String {
        format!("<class {}>", self.name)
    }
}

impl PartialEq for Class {
    fn eq(&self, _other: &Self) -> bool {
        false
    }
}

#[derive(Clone)]
pub struct Environment {
    enclosing: Option<Shared<Environment>>,
    values: HashMap<String, Value>,
}

impl Environment {
    pub fn new_shared_with_initial_values(values: HashMap<String, Value>) -> Shared<Self> {
        Rc::new(RefCell::new(Self {
            enclosing: None,
            values,
        }))
    }

    pub fn new_shared_with_enclosing(enclosing: Shared<Environment>) -> Shared<Self> {
        Rc::new(RefCell::new(Environment {
            enclosing: Some(enclosing),
            values: HashMap::new(),
        }))
    }

    pub fn local_names(&self) -> Vec<&str> {
        self.values.keys().map(|s| s.as_str()).collect()
    }

    pub fn get_from_ancestor(&self, distance: usize, name: &str) -> Result<Value, Error> {
        if distance == 0 {
            self.get(name)
        } else {
            match &self.enclosing {
                Some(enclosing) => enclosing.borrow().get_from_ancestor(distance - 1, name),
                None => Err("Internal error: ancestor not found.".into()),
            }
        }
    }

    pub fn set_in_ancestor(
        &mut self,
        distance: usize,
        name: &str,
        value: &Value,
    ) -> Result<(), Error> {
        if distance == 0 {
            self.set(name, &value)
        } else {
            match &self.enclosing {
                Some(enclosing) => {
                    enclosing
                        .borrow_mut()
                        .set_in_ancestor(distance - 1, name, value)
                }
                None => Err("Internal error: ancestor not found.".into()),
            }
        }
    }

    fn get(&self, name: &str) -> Result<Value, Error> {
        if let Some(value) = self.values.get(name) {
            Ok(value.clone())
        } else if let Some(enclosing) = &self.enclosing {
            enclosing.borrow().get(name)
        } else {
            Err(format!("Undefined variable '{}'.", name).into())
        }
    }

    fn set(&mut self, name: &str, value: &Value) -> Result<(), Error> {
        if self.values.contains_key(name) {
            self.values.insert(name.to_string(), value.clone());
            Ok(())
        } else if let Some(enclosing) = &mut self.enclosing {
            enclosing.borrow_mut().set(name, value)
        } else {
            Err(format!("Undefined variable '{}'.", name).into())
        }
    }

    pub fn define(&mut self, name: &str, value: &Value) -> Result<(), Error> {
        if self.values.contains_key(name) {
            Err(format!("Variable '{}' is already defined.", name).into())
        } else {
            self.values.insert(name.to_string(), value.clone());
            Ok(())
        }
    }
}

impl std::fmt::Debug for Environment {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        if let Some(enclosing) = &self.enclosing {
            write!(f, "{:?}", enclosing.borrow())?;
        }

        for (name, value) in &self.values {
            writeln!(f, "{} : {}", name, value)?;
        }

        Ok(())
    }
}

#[derive(Debug, Clone)]
pub struct UserFunction {
    decl_ptr: *const FunctionDeclaration,
    closure: Shared<Environment>,
    is_initializer: bool,
}

impl UserFunction {
    pub fn new(decl: &FunctionDeclaration, closure: Shared<Environment>, is_initializer: bool) -> Self {
        UserFunction {
            decl_ptr: decl,
            closure,
            is_initializer,
        }
    }

    fn decl(&self) -> &FunctionDeclaration {
        // SAFETY: we know the FunctionDeclaration outlives the Interpreter
        unsafe { &*self.decl_ptr }
    }

    fn as_value(&self) -> Value {
        let callable = Callable::User(self.clone());
        Value::Callable(callable)
    }

    pub fn call(&self, interpreter: &mut Interpreter, args: &Vec<Value>) -> Result<Value, Error> {
        // create a new environment for the function call
        let env = Environment::new_shared_with_enclosing(self.closure.clone());

        // bind arguments to parameters
        for (param, arg) in self.decl().parameters.iter().zip(args.iter()) {
            env.borrow_mut().define(&param.name.lexeme, arg)?;
        }

        interpreter.with_environment(env, |interpreter| {
            // interpret the call
            let result = match interpreter.interpret_block_statement(&self.decl().body) {
                Ok(ControlFlow::Break(return_value)) => Ok(return_value),
                Ok(ControlFlow::Continue(())) => Ok(Value::Nil),
                Err(e) => Err(e),
            };

            // special case the init method to return this
            if let Ok(value) = result {
                if self.is_initializer {
                    Ok(interpreter.current_environment.borrow().get("this")?)
                } else {
                    Ok(value)
                }
            } else {
                result
            }
        })
    }

    pub fn bind_this(&self, this: Shared<Instance>) -> Result<Self, Error> {
        // create a new environment for the bound method
        let env = Environment::new_shared_with_enclosing(self.closure.clone());

        // point "this" at the instance
        env.borrow_mut()
            .define(&"this".to_string(), &Value::Instance(this))?;

        // return a function with the new environment
        Ok(Self::new(self.decl(), env, self.is_initializer))
    }

    fn arity(&self) -> usize {
        self.decl().parameters.len()
    }

    fn to_string(&self) -> String {
        format!("<fn {}>", self.decl().name.lexeme)
    }
}
