use std::collections::HashMap;
use crate::syntax::ExprRef;
use super::types::*;

pub struct TypeEnvironment {
    arena: TypeArena,
    table: HashMap<ExprRef, Type>,
}

impl TypeEnvironment {
    pub fn new() -> Self {
        Self { 
            arena: TypeArena::new(),
            table: HashMap::new(),
        }
    }

    // Get type information for an expression
    pub fn get_type(&self, expr: ExprRef) -> Option<Type> {
        self.table.get(&expr).copied()
    }

    // Associate a type with an expression
    pub fn set_type(&mut self, expr: ExprRef, ty: Type) {
        self.table.insert(expr, ty);
    }

    // Get types through the arena
    pub fn get_bool(&self) -> Type {
        self.arena.bool()
    }

    pub fn get_number(&self) -> Type {
        self.arena.number()
    }

    pub fn get_string(&self) -> Type {
        self.arena.string()
    }

    pub fn get_unknown(&self) -> Type {
        self.arena.unknown()
    }
}
