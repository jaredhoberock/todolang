use crate::syntax::*;
use std::cell::RefCell;
use std::collections::HashMap;
use std::ptr::NonNull;

#[derive(Copy, Clone)]
pub enum NamedEntity {
    BuiltinFunction,
    Declaration(DeclRef),
    Parameter(NonNull<ParameterDeclaration>),
}

impl NamedEntity {
    pub fn into_decl(&self) -> Option<DeclRef> {
        match &self {
            NamedEntity::Declaration(d) => Some(d.clone()),
            _ => None,
        }
    }

    pub fn into_class_decl<'a>(&self) -> Option<&'a ClassDeclaration> {
        match self.into_decl() {
            // SAFETY: the AST outlives the SymbolTable
            Some(DeclRef::Class(ptr)) => Some(unsafe { &ptr.as_ref() }),
            _ => None,
        }
    }
}

#[derive(Clone)]
struct SymbolTableEntry {
    entity: NamedEntity,
    scope_distance: usize,
}

#[derive(Eq, Hash, PartialEq)]
enum Symbol {
    Subclass(NonNull<ClassDeclaration>),
    Super(NonNull<SuperExpression>),
    This(NonNull<ThisExpression>),
    Variable(NonNull<Variable>),
}

impl Symbol {
    pub fn from_super(super_: &SuperExpression) -> Self {
        Self::Super(NonNull::from(super_))
    }

    pub fn from_subclass(subclass: &ClassDeclaration) -> Self {
        Self::Subclass(NonNull::from(subclass))
    }

    pub fn from_this(this: &ThisExpression) -> Self {
        Self::This(NonNull::from(this))
    }

    pub fn from_variable(var: &Variable) -> Self {
        Self::Variable(NonNull::from(var))
    }

    pub fn name(&self) -> &str {
        match self {
            Symbol::Subclass(subclass) => unsafe { &subclass.as_ref().name.lexeme},
            Symbol::Super(_) => "super",
            Symbol::This(_) => "this",
            Symbol::Variable(var) => unsafe { &var.as_ref().name.lexeme},
        }
    }
}

pub struct SymbolTable {
    bindings: RefCell<HashMap<Symbol, SymbolTableEntry>>,
}

impl SymbolTable {
    pub fn new() -> Self {
        Self { 
            bindings: RefCell::new(HashMap::new()),
        }
    }

    pub fn bind_superclass(&self, subclass: &ClassDeclaration, superclass: &ClassDeclaration, scope_distance: usize) {
        let entity = NamedEntity::Declaration(superclass.into());
        let entry = SymbolTableEntry{ entity, scope_distance };
        self.bindings
            .borrow_mut()
            .insert(Symbol::Subclass(NonNull::from(subclass)), entry);
    }

    pub fn bind_super(&self, super_: &SuperExpression, decl: &ClassDeclaration, scope_distance: usize) {
        let entity = NamedEntity::Declaration(decl.into());
        let entry = SymbolTableEntry{ entity, scope_distance };
        self.bindings
            .borrow_mut()
            .insert(Symbol::Super(NonNull::from(super_)), entry);
    }

    pub fn bind_this(&self, this: &ThisExpression, decl: &ClassDeclaration, scope_distance: usize) {
        let entity = NamedEntity::Declaration(decl.into());
        let entry = SymbolTableEntry { entity, scope_distance };
        self.bindings
            .borrow_mut()
            .insert(Symbol::This(NonNull::from(this)), entry);
    }

    pub fn bind_variable(&self, var: &Variable, entity: NamedEntity, scope_distance: usize) {
        let entry = SymbolTableEntry { entity, scope_distance };
        self.bindings
            .borrow_mut()
            .insert(Symbol::Variable(NonNull::from(var)), entry);
    }

    fn lookup(&self, symbol: Symbol) -> Result<SymbolTableEntry, String> {
        self.bindings
            .borrow()
            .get(&symbol)
            .cloned()
            .ok_or(format!("Internal error: '{}' was not found in symbol table", symbol.name()))
    }

    pub fn lookup_superclass(&self, subclass: &ClassDeclaration) -> Result<(&ClassDeclaration, usize), String> {
        let entry = self.lookup(Symbol::from_subclass(subclass))?;
        let class_decl = entry.entity.into_class_decl()
            .ok_or("Internal error: super class mapped to non-class declaration")?;
        Ok((class_decl, entry.scope_distance))
    }

    pub fn lookup_super(&self, super_: &SuperExpression) -> Result<(&ClassDeclaration, usize), String> {
        let entry = self.lookup(Symbol::from_super(super_))?;
        let class_decl = entry.entity.into_class_decl()
            .ok_or("Internal error: super expression mapped to non-class declaration")?;
        Ok((class_decl, entry.scope_distance))
    }

    pub fn lookup_this(&self, this: &ThisExpression) -> Result<(&ClassDeclaration, usize), String> {
        let entry = self.lookup(Symbol::from_this(this))?;
        let class_decl = entry.entity.into_class_decl()
            .ok_or("Internal error: this expression mapped to non-class declaration")?;
        Ok((class_decl, entry.scope_distance))
    }

    pub fn lookup_variable(&self, var: &Variable) -> Result<(NamedEntity, usize), String> {
        let entry = self.lookup(Symbol::from_variable(var))?;
        Ok((entry.entity, entry.scope_distance))
    }
}
