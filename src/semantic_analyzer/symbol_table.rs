use std::collections::HashMap;

use crate::{
    ast::{
        EnumDef, FunctionItem, Identifier, ModuleItem, StructDef, TraitDef, TupleStructDef, Type,
    },
    error::SemanticErrorKind,
};

#[derive(Debug)]
pub(crate) enum Symbol {
    Variable(Type),
    Struct(StructDef),
    TupleStruct(TupleStructDef),
    Enum(EnumDef),
    Trait(TraitDef),
    Function(FunctionItem),
    Module(ModuleItem),
}

#[derive(Debug)]
pub(crate) struct SymbolTable {
    symbols: HashMap<Identifier, Symbol>,
    parent: Option<Box<SymbolTable>>, // For nested scopes
}

impl SymbolTable {
    pub(crate) fn new() -> Self {
        SymbolTable {
            symbols: HashMap::new(),
            parent: None,
        }
    }

    pub(crate) fn with_parent(parent: SymbolTable) -> Self {
        SymbolTable {
            symbols: HashMap::new(),
            parent: Some(Box::new(parent)),
        }
    }

    pub(crate) fn insert(
        &mut self,
        name: Identifier,
        symbol: Symbol,
    ) -> Result<(), SemanticErrorKind> {
        if self.symbols.contains_key(&name) {
            Err(SemanticErrorKind::DuplicateVariable { name }) // Position info should be added here
        } else {
            self.symbols.insert(name, symbol);
            Ok(())
        }
    }

    pub(crate) fn get(&self, name: &Identifier) -> Option<&Symbol> {
        self.symbols
            .get(name)
            .or_else(|| self.parent.as_ref()?.get(name))
    }
}
