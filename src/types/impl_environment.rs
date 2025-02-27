use std::collections::{HashMap, HashSet};
use super::Type;

pub struct ImplEnvironment {
    impls: HashMap<String, HashSet<Type>>,
}

impl ImplEnvironment {
    pub fn new() -> Self {
        Self { impls: HashMap::new() }
    }

    pub fn add_impl(&mut self, trait_name: String, ty: Type) {
        self.impls
            .entry(trait_name)
            .or_insert_with(HashSet::new)
            .insert(ty);
    }

    pub fn has_impl(&self, trait_name: String, ty: Type) -> bool {
        self.impls
            .get(&trait_name)
            .map_or(false, |set| set.contains(&ty))
    }
}
