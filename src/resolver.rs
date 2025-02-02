use crate::symbol_table::{NamedEntity, SymbolTable};
use crate::declaration_environment::DeclarationEnvironment;
use crate::syntax::*;
use crate::types::Type;
use std::rc::Rc;

pub struct Resolver {
    symbol_table: Rc<SymbolTable>,
    decl_env: Rc<DeclarationEnvironment>,
}

impl Resolver {
    pub fn new(symbol_table: Rc<SymbolTable>, decl_env: Rc<DeclarationEnvironment>) -> Self {
        Self { symbol_table, decl_env }
    }

    pub fn lookup_variable_type(&self, var: &Variable) -> Type {
        let entry = self.symbol_table
            .lookup_variable(var)
            .expect(&format!("Internal compiler error: variable '{}' not found in symbol table.", var.name.lexeme));
        match entry.0 {
            NamedEntity::Declaration(d) => self.decl_env.lookup_type_for_decl(d),
            NamedEntity::Parameter(p) => self.decl_env.lookup_type_for_parameter_decl(p),
            _ => panic!("Internal compiler error: variable '{}' does not refer to a Declaration.", var.name.lexeme),
        }
    }
}
