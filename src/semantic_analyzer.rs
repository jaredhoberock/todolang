use crate::declaration_environment::DeclarationEnvironment;
use crate::name_resolver::NameResolver;
use crate::name_resolver::Error as NameError;
use crate::resolver::Resolver;
use crate::symbol_table::SymbolTable;
use crate::syntax::*;
use crate::types::TypeChecker;
use crate::types::Error as TypeError;
use std::rc::Rc;
use thiserror::Error;

#[derive(Debug, Error)]
pub enum Error {
    #[error(transparent)]
    Name(#[from] NameError),

    #[error(transparent)]
    Type(#[from] TypeError),
}

pub struct SemanticAnalyzer {
    symbol_table: Rc<SymbolTable>,
    decl_env: Rc<DeclarationEnvironment>,
    name_resolver: NameResolver,
    type_checker: TypeChecker,
}


impl SemanticAnalyzer {
    pub fn new(builtin_functions: Vec<String>) -> Self {
        let symbol_table = Rc::new(SymbolTable::new());
        let decl_env = Rc::new(DeclarationEnvironment::new());
        let resolver = Rc::new(Resolver::new(symbol_table.clone(), decl_env.clone()));
        Self {
            symbol_table: symbol_table.clone(),
            decl_env: decl_env.clone(),
            name_resolver: NameResolver::new(symbol_table.clone(), decl_env.clone(), builtin_functions),
            type_checker: TypeChecker::new(decl_env.clone(), resolver),
        }
    }

    pub fn analyze_global_statement(&mut self, stmt: &Statement) -> Result<(), Error> {
        self.name_resolver
            .resolve_global_statement(&stmt)
            .map_err(Error::from)?;

        self.type_checker
            .check_statement(&stmt)
            .map_err(Error::from)
    }

    pub fn analyze_program(&mut self, prog: &Program) -> Result<(), Error> {
        for stmt in &prog.statements {
            self.analyze_global_statement(stmt)?;
        }
        Ok(())
    }

    // XXX these functions are used by the Interpreter, and haven't been ported to use Error yet
    // XXX these should probably be infallible anyway. the symbol table should panic if it gets a
    //     nonsensical lookup request
    pub fn superclass_scope_distance(&self, subclass: &ClassDeclaration) -> Result<usize,String> {
        self.symbol_table.lookup_superclass(subclass).map(|(_,result)| result)
    }

    pub fn super_scope_distance(&self, super_: &SuperExpression) -> Result<usize,String> {
        self.symbol_table.lookup_super(super_).map(|(_,result)| result)
    }

    pub fn this_scope_distance(&self, var: &ThisExpression) -> Result<usize,String> {
        self.symbol_table.lookup_this(var).map(|(_,result)| result)
    }

    pub fn variable_scope_distance(&self, var: &Variable) -> Result<usize,String> {
        self.symbol_table.lookup_variable(var).map(|(_,result)| result)
    }
}
