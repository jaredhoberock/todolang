use std::cell::RefCell;
use std::collections::HashMap;
use std::ops::ControlFlow;
use std::rc::Rc;
use std::rc::Weak;

use crate::semantic_analyzer::SemanticAnalyzer;
use crate::syntax::*;
use crate::token::*;

type Shared<T> = Rc<RefCell<T>>;

#[derive(Clone, Debug, PartialEq)]
enum Value {
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
    fn as_bool(&self) -> bool {
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

    fn as_f64(&self) -> Result<f64, String> {
        match self {
            Value::Number(n) => Ok(*n),
            _ => Err("Value is not a number".to_string()),
        }
    }

    fn evaluate_binary_operation(&self, op: &TokenKind, rhs: &Self) -> Result<Self, String> {
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
                (_, _) => Err("Operands must be two numbers or two strings.".to_string()),
            },
            TokenKind::Slash => match (self, rhs) {
                (Number(n1), Number(n2)) => Ok(Number(n1 / n2)),
                (_, _) => Err("Operands must be two numbers.".to_string()),
            },
            TokenKind::Star => match (self, rhs) {
                (Number(n1), Number(n2)) => Ok(Number(n1 * n2)),
                (_, _) => Err("Operands must be two numbers.".to_string()),
            },
            _ => Err("Unexpected operator in binary operation".to_string()),
        }
    }
}

impl std::fmt::Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{}", self.as_string())
    }
}

#[derive(Clone)]
enum Callable {
    Class(Rc<Class>),
    Native(
        usize,
        Rc<dyn Fn(&mut Interpreter, &Vec<Value>) -> Result<Value, String>>,
    ),
    User(UserFunction),
}

impl Callable {
    fn new_native_function<F>(arity: usize, func: F) -> Self
    where
        F: Fn(&mut Interpreter, &Vec<Value>) -> Result<Value, String> + 'static,
    {
        Self::Native(arity, Rc::new(func))
    }

    fn call(&self, interp: &mut Interpreter, arguments: &Vec<Value>) -> Result<Value, String> {
        match &self {
            Callable::Class(c) => c.call(interp, arguments),
            Callable::Native(_arity, f) => f(interp, arguments),
            Callable::User(f) => f.call(interp, arguments),
        }
    }

    fn arity(&self) -> usize {
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
struct Instance {
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

    fn get(&self, name: &Token) -> Result<Value, String> {
        if let Some(value) = self.fields.get(&name.lexeme) {
            return Ok(value.clone());
        }
        self.class
            .find_method(&name.lexeme)
            .and_then(|m| m.bind_this(self.self_rc.upgrade().unwrap()))
            .map(|m| m.as_value())
            .map_err(|_| format!("Undefined property '{}'.", name.lexeme))
    }

    fn set(&mut self, name: &Token, value: &Value) -> Result<(), String> {
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
struct Class {
    name: String,
    superclass: Option<Rc<Self>>,
    methods: HashMap<String, UserFunction>,
    self_rc: Weak<Self>,
}

impl Class {
    fn new_rc(
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

    fn call(&self, interpreter: &mut Interpreter, arguments: &Vec<Value>) -> Result<Value, String> {
        let class_rc = self.self_rc.upgrade().unwrap();
        let instance = Instance::new_shared(class_rc);

        // look for an "init" method and call it if found
        self.find_method(&"init".to_string())
            .and_then(|m| m.bind_this(instance.clone()))
            .and_then(|m| m.call(interpreter, arguments))
            .ok();

        Ok(Value::Instance(instance.clone()))
    }

    fn find_method(&self, name: &String) -> Result<UserFunction, String> {
        match (self.methods.get(name), &self.superclass) {
            (Some(method), _) => Ok(method.clone()),
            (None, Some(superclass)) => superclass.find_method(name),
            (None, None) => Err(format!("Undefined method '{}'", name)),
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
struct Environment {
    enclosing: Option<Shared<Environment>>,
    values: HashMap<String, Value>,
}

impl Environment {
    fn new_shared_with_initial_values(values: HashMap<String, Value>) -> Shared<Self> {
        Rc::new(RefCell::new(Self {
            enclosing: None,
            values,
        }))
    }

    fn new_shared_with_enclosing(enclosing: Shared<Environment>) -> Shared<Self> {
        Rc::new(RefCell::new(Environment {
            enclosing: Some(enclosing),
            values: HashMap::new(),
        }))
    }

    fn local_names(&self) -> Vec<&str> {
        self.values.keys().map(|s| s.as_str()).collect()
    }

    fn get_from_ancestor(&self, distance: usize, name: &str) -> Result<Value, String> {
        if distance == 0 {
            self.get(name)
        } else {
            match &self.enclosing {
                Some(enclosing) => enclosing.borrow().get_from_ancestor(distance - 1, name),
                None => Err("Internal error: ancestor not found.".to_string()),
            }
        }
    }

    fn set_in_ancestor(
        &mut self,
        distance: usize,
        name: &str,
        value: &Value,
    ) -> Result<(), String> {
        if distance == 0 {
            self.set(name, &value)
        } else {
            match &self.enclosing {
                Some(enclosing) => {
                    enclosing
                        .borrow_mut()
                        .set_in_ancestor(distance - 1, name, value)
                }
                None => Err("Internal error: ancestor not found.".to_string()),
            }
        }
    }

    fn get(&self, name: &str) -> Result<Value, String> {
        if let Some(value) = self.values.get(name) {
            Ok(value.clone())
        } else if let Some(enclosing) = &self.enclosing {
            enclosing.borrow().get(name)
        } else {
            Err(format!("Undefined variable '{}'.", name))
        }
    }

    fn set(&mut self, name: &str, value: &Value) -> Result<(), String> {
        if self.values.contains_key(name) {
            self.values.insert(name.to_string(), value.clone());
            Ok(())
        } else if let Some(enclosing) = &mut self.enclosing {
            enclosing.borrow_mut().set(name, value)
        } else {
            Err(format!("Undefined variable '{}'.", name))
        }
    }

    fn define(&mut self, name: &str, value: &Value) -> Result<(), String> {
        if self.values.contains_key(name) {
            Err(format!("Variable '{}' is already defined.", name))
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
struct UserFunction {
    decl_ptr: *const FunctionDeclaration,
    closure: Shared<Environment>,
    is_initializer: bool,
}

impl UserFunction {
    fn new(decl: &FunctionDeclaration, closure: Shared<Environment>, is_initializer: bool) -> Self {
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

    fn call(&self, interpreter: &mut Interpreter, args: &Vec<Value>) -> Result<Value, String> {
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

    fn bind_this(&self, this: Shared<Instance>) -> Result<Self, String> {
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

pub struct Interpreter {
    sema: SemanticAnalyzer,
    current_environment: Shared<Environment>,
}

impl Interpreter {
    fn initial_global_values() -> HashMap<String, Value> {
        let program_started = std::time::SystemTime::now();

        let clock_function = Callable::new_native_function(
            0, // arity
            move |_, _| -> Result<Value, String> {
                let elapsed = program_started
                    .elapsed()
                    .map_err(|e| e.to_string())?
                    .as_secs_f64();
                Ok(Value::Number(elapsed))
            },
        );

        let mut map = HashMap::new();
        map.insert("clock".to_string(), Value::Callable(clock_function));
        map
    }

    pub fn new() -> Self {
        let env = Environment::new_shared_with_initial_values(Self::initial_global_values());
        let initial_names: Vec<_> = env
            .borrow()
            .local_names()
            .into_iter()
            .map(|name| name.to_string())
            .collect();
        Self {
            sema: SemanticAnalyzer::new(initial_names.clone()),
            current_environment: env,
        }
    }

    fn with_environment<T>(
        &mut self,
        new_env: Shared<Environment>,
        f: impl FnOnce(&mut Self) -> T,
    ) -> T {
        let mut temp_env = new_env;
        std::mem::swap(&mut self.current_environment, &mut temp_env);
        let result = f(self);
        std::mem::swap(&mut self.current_environment, &mut temp_env);
        result
    }

    fn with_enclosed_environment<T>(&mut self, f: impl FnOnce(&mut Self) -> T) -> T {
        let env = Environment::new_shared_with_enclosing(self.current_environment.clone());
        self.with_environment(env, f)
    }

    // if condition is true,
    //     self.with_environment(new_env, f)
    //     where new_env is a new environment with self.current_environment as its enclosing
    //     environment
    // otherwise,
    //     self.with_environment(self.current_environment, f)
    fn with_enclosed_environment_if<T>(
        &mut self,
        condition: bool,
        f: impl FnOnce(&mut Self) -> T,
    ) -> T {
        if condition {
            self.with_enclosed_environment(f)
        } else {
            f(self)
        }
    }

    fn interpret_literal_expression(&self, lit: &LiteralExpression) -> Result<Value, String> {
        Ok(match &lit.0 {
            Literal::Bool(b) => Value::Bool(*b),
            Literal::Nil => Value::Nil,
            Literal::Number(n) => Value::Number(*n),
            Literal::String(s) => Value::String(s.clone()),
        })
    }

    fn interpret_grouping_expression(
        &mut self,
        expr: &GroupingExpression,
    ) -> Result<Value, String> {
        self.interpret_expression(&*expr.expr)
    }

    fn interpret_logical_expression(&mut self, expr: &LogicalExpression) -> Result<Value, String> {
        let left = self.interpret_expression(&*expr.left_expr)?;
        match expr.op.kind {
            TokenKind::Or => {
                if left.as_bool() {
                    return Ok(left);
                }
            }
            TokenKind::And => {
                if !left.as_bool() {
                    return Ok(left);
                }
            }
            _ => {
                return Err(format!(
                    "Unexpected '{}' in logical expression",
                    expr.op.lexeme
                ))
            }
        };
        return self.interpret_expression(&*expr.right_expr);
    }

    fn matches_pattern(&mut self, pattern: &Pattern, scrutinee: &Value) -> bool {
        match pattern {
            Pattern::Literal(lit) => {
                match &lit.0 {
                    Literal::Bool(b) => matches!(scrutinee, Value::Bool(v) if v == b),
                    Literal::Nil => matches!(scrutinee, Value::Nil),
                    Literal::Number(n) => matches!(scrutinee, Value::Number(v) if v == n),
                    Literal::String(s) => matches!(scrutinee, Value::String(v) if v == s),
                }
            }
            Pattern::Underscore => true
        }
    }

    fn interpret_match_expression(&mut self, expr: &MatchExpression) -> Result<Value, String> {
        let scrutinee = self.interpret_expression(&expr.scrutinee)?;

        for arm in &expr.arms {
            if self.matches_pattern(&arm.pattern, &scrutinee) {
                return self.interpret_expression(&arm.expr);
            }
        }

        Err("No matching pattern found".to_string())
    }

    fn interpret_binary_expression(&mut self, expr: &BinaryExpression) -> Result<Value, String> {
        let lhs = self.interpret_expression(&*expr.left_expr)?;
        let rhs = self.interpret_expression(&*expr.right_expr)?;
        lhs.evaluate_binary_operation(&expr.op.kind, &rhs)
    }

    fn interpret_call_expression(&mut self, call: &CallExpression) -> Result<Value, String> {
        let value = self.interpret_expression(&*call.callee)?;

        let callee = match &value {
            Value::Callable(callee) => callee,
            _ => return Err("Can only call functions and classes.".to_string()),
        };

        if call.arguments.len() != callee.arity() {
            return Err(format!(
                "Expected {} arguments but got {}.",
                callee.arity(),
                call.arguments.len()
            ));
        }

        let mut arguments = Vec::new();
        for arg in &call.arguments {
            arguments.push(self.interpret_expression(&arg)?);
        }

        callee.call(self, &arguments)
    }

    fn interpret_get_expression(&mut self, expr: &GetExpression) -> Result<Value, String> {
        let object = self.interpret_expression(&expr.object)?;
        match object {
            Value::Instance(obj) => obj.borrow().get(&expr.name),
            _ => Err("Only instances have properties.".to_string()),
        }
    }

    fn interpret_set_expression(&mut self, expr: &SetExpression) -> Result<Value, String> {
        let mut object = self.interpret_expression(&expr.object)?;
        match &mut object {
            Value::Instance(obj) => {
                let value = self.interpret_expression(&expr.value)?;
                obj.borrow_mut().set(&expr.name, &value)?;
                Ok(value)
            }
            _ => Err("Only instances have properties.".to_string()),
        }
    }

    fn interpret_super_expression(&mut self, expr: &SuperExpression) -> Result<Value, String> {
        // look up the value of "super" and get which ancestor we found it in
        let ancestor = self.sema.super_scope_distance(expr)?;
        let superclass = match self
            .current_environment
            .borrow()
            .get_from_ancestor(ancestor, "super")
        {
            Ok(Value::Callable(Callable::Class(c))) => c,
            _ => unreachable!("'super' should evaluate to a class."),
        };
        // look up the value of "this" at one scope below where we found "super"
        let instance = match self
            .current_environment
            .borrow()
            .get_from_ancestor(ancestor - 1, "this")?
        {
            Value::Instance(instance) => instance,
            _ => unreachable!("'this' should evaluate to an instance."),
        };
        // look up the method in superclass and bind instance as "this"
        superclass
            .find_method(&expr.method.lexeme)?
            .bind_this(instance.clone())
            .map(|m| Value::Callable(Callable::User(m)))
    }

    fn interpret_this_expression(&mut self, this: &ThisExpression) -> Result<Value, String> {
        let ancestor = self.sema.this_scope_distance(this)?;
        self.current_environment
            .borrow()
            .get_from_ancestor(ancestor, "this")
    }

    fn interpret_unary_expression(&mut self, expr: &UnaryExpression) -> Result<Value, String> {
        let value = self.interpret_expression(&*expr.expr)?;
        match expr.op.kind {
            TokenKind::Bang => Ok(Value::Bool(!value.as_bool())),
            TokenKind::Minus => Ok(Value::Number(-value.as_f64()?)),
            _ => Err("Bad operator in unary_expression".to_string()),
        }
    }

    fn interpret_variable(&mut self, var: &Variable) -> Result<Value, String> {
        let ancestor = self.sema.variable_scope_distance(var)?;
        self.current_environment
            .borrow()
            .get_from_ancestor(ancestor, &var.name.lexeme)
    }

    fn interpret_assignment_expression(
        &mut self,
        expr: &AssignmentExpression,
    ) -> Result<Value, String> {
        let value = self.interpret_expression(&*expr.expr)?;
        let ancestor = self.sema.variable_scope_distance(&expr.var)?;
        self.current_environment.borrow_mut().set_in_ancestor(
            ancestor,
            &expr.var.name.lexeme,
            &value,
        )?;
        Ok(value)
    }

    fn interpret_expression(&mut self, expr: &Expression) -> Result<Value, String> {
        match expr {
            Expression::Assignment(expr) => self.interpret_assignment_expression(expr),
            Expression::Binary(expr) => self.interpret_binary_expression(expr),
            Expression::Call(expr) => self.interpret_call_expression(expr),
            Expression::Get(g) => self.interpret_get_expression(g),
            Expression::Grouping(g) => self.interpret_grouping_expression(g),
            Expression::Literal(l) => self.interpret_literal_expression(l),
            Expression::Logical(expr) => self.interpret_logical_expression(expr),
            Expression::Match(expr) => self.interpret_match_expression(expr),
            Expression::Set(s) => self.interpret_set_expression(s),
            Expression::Super(s) => self.interpret_super_expression(s),
            Expression::This(t) => self.interpret_this_expression(t),
            Expression::Unary(expr) => self.interpret_unary_expression(expr),
            Expression::Variable(var) => self.interpret_variable(var),
        }
    }

    fn interpret_class_declaration(&mut self, decl: &ClassDeclaration) -> Result<(), String> {
        // get superclass if one is specified
        let superclass = match &decl.superclass {
            Some(superclassname) => {
                let ancestor = self.sema.superclass_scope_distance(decl)?;
                match self
                    .current_environment
                    .borrow()
                    .get_from_ancestor(ancestor, &superclassname.lexeme)?
                {
                    Value::Callable(Callable::Class(c)) => Some(c.clone()),
                    _ => return Err("Superclass must be a class.".to_string()),
                }
            },
            None => None,
        };

        // if a superclass is specified, interpret the following with an enclosed environment
        let class = self.with_enclosed_environment_if(
            superclass.is_some(),
            |slf| -> Result<Value, String> {
                if let Some(s) = &superclass {
                    slf.current_environment
                        .borrow_mut()
                        .define("super", &Value::from(s.clone()))?;
                };

                let mut methods = HashMap::new();
                for method_decl in &decl.methods {
                    let callable = UserFunction::new(
                        method_decl,
                        slf.current_environment.clone(),
                        method_decl.name.lexeme == "init",
                    );
                    methods.insert(method_decl.name.lexeme.clone(), callable);
                }

                Ok(Value::from(Class::new_rc(
                    &decl.name.lexeme,
                    &superclass,
                    &methods,
                )))
            },
        )?;

        self.current_environment
            .borrow_mut()
            .define(&decl.name.lexeme, &class)
    }

    fn interpret_function_declaration(&mut self, decl: &FunctionDeclaration) -> Result<(), String> {
        let function = UserFunction::new(decl, self.current_environment.clone(), false);
        let callable = Callable::User(function);
        self.current_environment
            .borrow_mut()
            .define(&decl.name.lexeme, &Value::Callable(callable))
    }

    fn interpret_variable_declaration(&mut self, decl: &VariableDeclaration) -> Result<(), String> {
        let value = match &decl.initializer {
            Some(expr) => self.interpret_expression(expr)?,
            None => Value::Nil,
        };
        self.current_environment
            .borrow_mut()
            .define(&decl.name.lexeme, &value)
    }

    fn interpret_declaration(&mut self, decl: &Declaration) -> Result<(), String> {
        match decl {
            Declaration::Class(c) => self.interpret_class_declaration(c),
            Declaration::Function(f) => self.interpret_function_declaration(f),
            Declaration::Variable(v) => self.interpret_variable_declaration(v),
        }
    }

    fn interpret_assert_statement(&mut self, stmt: &AssertStatement) -> Result<(), String> {
        let val = self.interpret_expression(&stmt.expr)?;
        if !val.as_bool() {
            return Err("assert failed".to_string());
        }
        Ok(())
    }

    fn interpret_block_statement(
        &mut self,
        block: &BlockStatement,
    ) -> Result<ControlFlow<Value>, String> {
        self.with_enclosed_environment(|slf| {
            let mut result = Ok(ControlFlow::Continue(()));
            for stmt in &block.statements {
                match slf.interpret_statement(stmt) {
                    Ok(ControlFlow::Continue(())) => continue,
                    Ok(ControlFlow::Break(value)) => {
                        result = Ok(ControlFlow::Break(value));
                        break;
                    }
                    Err(e) => {
                        result = Err(e);
                        break;
                    }
                }
            }
            result
        })
    }

    fn interpret_expression_statement(&mut self, stmt: &ExpressionStatement) -> Result<(), String> {
        self.interpret_expression(&stmt.expr)?;
        Ok(())
    }

    fn interpret_for_statement(
        &mut self,
        stmt: &ForStatement,
    ) -> Result<ControlFlow<Value>, String> {
        self.with_enclosed_environment(|slf| {
            let mut result = Ok(ControlFlow::Continue(()));

            if let Some(initializer) = &stmt.initializer {
                slf.interpret_statement(initializer)?;
            }

            while match &stmt.condition {
                Some(condition) => slf.interpret_expression(&condition)?.as_bool(),
                None => true,
            } {
                match slf.interpret_statement(&*stmt.body) {
                    Ok(ControlFlow::Continue(())) => (),
                    Ok(ControlFlow::Break(value)) => {
                        result = Ok(ControlFlow::Break(value));
                        break;
                    }
                    Err(e) => {
                        result = Err(e);
                        break;
                    }
                }

                if let Some(increment) = &stmt.increment {
                    slf.interpret_expression(&increment)?;
                }
            }
            result
        })
    }

    fn interpret_if_statement(&mut self, stmt: &IfStatement) -> Result<ControlFlow<Value>, String> {
        if self.interpret_expression(&stmt.condition)?.as_bool() {
            self.interpret_statement(&*stmt.then_branch)
        } else {
            match &stmt.else_branch {
                Some(else_branch) => self.interpret_statement(&else_branch),
                None => Ok(ControlFlow::Continue(())),
            }
        }
    }

    fn interpret_print_statement(&mut self, stmt: &PrintStatement) -> Result<(), String> {
        let val = self.interpret_expression(&stmt.expr)?;
        println!("{}", val);
        Ok(())
    }

    fn interpret_return_statement(
        &mut self,
        stmt: &ReturnStatement,
    ) -> Result<ControlFlow<Value>, String> {
        let value = match &stmt.expr {
            Some(expr) => self.interpret_expression(expr)?,
            None => Value::Nil,
        };
        Ok(ControlFlow::Break(value))
    }

    fn interpret_while_statement(
        &mut self,
        stmt: &WhileStatement,
    ) -> Result<ControlFlow<Value>, String> {
        let mut result = Ok(ControlFlow::Continue(()));

        while self.interpret_expression(&stmt.condition)?.as_bool() {
            match self.interpret_statement(&*stmt.body) {
                Ok(ControlFlow::Continue(())) => continue,
                Ok(ControlFlow::Break(value)) => {
                    result = Ok(ControlFlow::Break(value));
                    break;
                }
                Err(e) => {
                    result = Err(e);
                    break;
                }
            }
        }

        result
    }

    fn interpret_statement(&mut self, stmt: &Statement) -> Result<ControlFlow<Value>, String> {
        match stmt {
            Statement::Block(block) => self.interpret_block_statement(block),
            Statement::For(f) => self.interpret_for_statement(f),
            Statement::If(i) => self.interpret_if_statement(i),
            Statement::Return(ret) => self.interpret_return_statement(ret),
            Statement::While(w) => self.interpret_while_statement(w),

            // map the results of other statements to ControlFlow::Continue
            _ => match stmt {
                Statement::Block(_) => Err("Impossible statement".to_string()),
                Statement::Assert(assert) => self.interpret_assert_statement(assert),
                Statement::Decl(decl) => self.interpret_declaration(decl),
                Statement::Expr(expr) => self.interpret_expression_statement(expr),
                Statement::For(_) => Err("Impossible statement".to_string()),
                Statement::If(_) => Err("Impossible statement".to_string()),
                Statement::Print(print) => self.interpret_print_statement(print),
                Statement::Return(_) => Err("Impossible statement".to_string()),
                Statement::While(_) => Err("Impossible statement".to_string()),
            }
            .map(|_| ControlFlow::Continue(())),
        }
    }

    pub fn interpret_program(&mut self, prog: &Program) -> Result<(), String> {
        self.sema.analyze_program(prog).map_err(|e| format!("{}", e))?;
        for stmt in &prog.statements {
            self.interpret_statement(stmt)?;
        }
        Ok(())
    }

    pub fn interpret_global_statement(&mut self, stmt: &Statement) -> Result<(), String> {
        self.sema.analyze_global_statement(stmt).map_err(|e| format!("{}", e))?;
        self.interpret_statement(stmt)?;
        Ok(())
    }
}
