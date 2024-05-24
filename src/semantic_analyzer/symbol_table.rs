use std::collections::HashMap;

use crate::ast::{Identifier, Type};

#[derive(Debug)]
pub(crate) struct SymbolTable {
    variables: HashMap<Identifier, Type>,
}

impl SymbolTable {
    pub(crate) fn new() -> Self {
        SymbolTable {
            variables: HashMap::new(),
        }
    }

    pub(crate) fn insert(&mut self, name: Identifier, var_type: Type) {
        self.variables.insert(name, var_type);
    }

    pub(crate) fn get(&self, name: &Identifier) -> Option<&Type> {
        self.variables.get(name)
    }
}
