use std::collections::HashMap;

use crate::ast::Type;

#[derive(Debug)]
pub(crate) struct SymbolTable {
    variables: HashMap<String, Type>,
}

impl SymbolTable {
    pub(crate) fn new() -> Self {
        SymbolTable {
            variables: HashMap::new(),
        }
    }

    pub(crate) fn insert(&mut self, name: String, var_type: Type) {
        self.variables.insert(name, var_type);
    }

    pub(crate) fn get(&self, name: &str) -> Option<&Type> {
        self.variables.get(name)
    }
}
