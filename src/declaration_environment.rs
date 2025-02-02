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

    fn insert(&self, key: Key, ty: Option<Type>) {
        self.env.borrow_mut().insert(key, ty);
    }

    pub fn insert_decl(&self, decl: DeclRef, ty: Option<Type>) {
        self.insert(Key::Decl(decl), ty)
    }

    pub fn insert_parameter_decl(&self, decl: NonNull<ParameterDeclaration>, ty: Option<Type>) {
        self.insert(Key::ParmDecl(decl), ty)
    }

    pub fn lookup_decl(&self, decl: DeclRef) -> Option<Option<Type>> {
        self.env.borrow().get(&Key::Decl(decl)).cloned()
    }

    pub fn lookup_parameter_decl(&self, decl: NonNull<ParameterDeclaration>) -> Option<Option<Type>> {
        self.env.borrow().get(&Key::ParmDecl(decl)).cloned()
    }

    pub fn lookup_type_for_decl(&self, decl: DeclRef) -> Type {
        self.lookup_decl(decl)
            .expect("Internal compiler error: declaration not found in environment.")
            .expect("Internal compiler error: Type not computed for declaration.")
    }

    pub fn lookup_type_for_parameter_decl(&self, decl: NonNull<ParameterDeclaration>) -> Type {
        self.lookup_parameter_decl(decl)
            .expect("Internal compiler error: parameter declaration not found in environment.")
            .expect("Internal compiler error: Type not computed for declaration.")
    }
}
