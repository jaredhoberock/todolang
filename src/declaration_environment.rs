use crate::types::Type;
use crate::syntax::{DeclRef, ParameterDeclaration};
use std::cell::RefCell;
use std::collections::HashMap;
use std::ptr::NonNull;

#[derive(Eq, Hash, PartialEq)]
enum Key {
    Decl(DeclRef),
    ParmDecl(NonNull<ParameterDeclaration>),
}

pub struct DeclarationEnvironment {
    env: RefCell<HashMap<Key, Option<Type>>>,
}

impl DeclarationEnvironment {
    pub fn new() -> Self {
        Self {
            env: RefCell::new(HashMap::new()),
        }
    }

    fn new_entry(&self, key: Key) {
        self.env.borrow_mut().insert(key, None);
    }

    fn update_entry(&self, key: Key, ty: Type) {
        let mut env = self.env.borrow_mut();
        env.get_mut(&key)
            .expect(&format!("Attempted to update non-existent key"))
            .replace(ty);
    }

    pub fn new_decl(&self, decl: DeclRef) {
        self.new_entry(Key::Decl(decl))
    }

    pub fn update_decl(&self, decl: DeclRef, ty: Type) {
        self.update_entry(Key::Decl(decl), ty)
    }

    pub fn new_parameter_decl(&self, decl: NonNull<ParameterDeclaration>) {
        self.new_entry(Key::ParmDecl(decl))
    }

    pub fn update_parameter_decl(&self, decl: NonNull<ParameterDeclaration>, ty: Type) {
        self.update_entry(Key::ParmDecl(decl), ty)
    }

    pub fn lookup_decl(&self, decl: DeclRef) -> Option<Option<Type>> {
        self.env.borrow().get(&Key::Decl(decl)).cloned()
    }

    pub fn lookup_parameter_decl(&self, decl: NonNull<ParameterDeclaration>) -> Option<Option<Type>> {
        self.env.borrow().get(&Key::ParmDecl(decl)).cloned()
    }

    pub fn lookup_type_for_decl(&self, decl: DeclRef) -> Option<Type> {
        self.lookup_decl(decl)
            .expect("Internal compiler error: declaration not found in environment.")
    }

    pub fn lookup_type_for_parameter_decl(&self, decl: NonNull<ParameterDeclaration>) -> Option<Type> {
        self.lookup_parameter_decl(decl)
            .expect("Internal compiler error: parameter declaration not found in environment.")
    }
}
