use core::fmt;
use std::collections::HashMap;

use crate::{
    ast::{
        EnumDef, FunctionItem, Identifier, ModuleItem, PathType, StructDef, TraitDef,
        TupleStructDef, Type,
    },
    error::SemanticErrorKind,
};

#[derive(Debug, Clone)]
pub(crate) enum Symbol {
    Variable(Type),
    Struct(StructDef),
    TupleStruct(TupleStructDef),
    Enum(EnumDef),
    Trait(TraitDef),
    Function(FunctionItem),
    Module(ModuleItem),
    Import(Vec<PathType>),
}

impl fmt::Display for Symbol {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Symbol::Variable(v) => write!(f, "{}", v),
            Symbol::Struct(s) => write!(f, "{:?}", s),
            Symbol::TupleStruct(ts) => write!(f, "{:?}", ts),
            Symbol::Enum(e) => write!(f, "{:?}", e),
            Symbol::Trait(t) => write!(f, "{:?}", t),
            Symbol::Function(func) => write!(f, "{:?}", func),
            Symbol::Module(m) => write!(f, "{:?}", m),
            Symbol::Import(i) => {
                let mut segment_strings: Vec<String> = Vec::new();

                for pt in i {
                    segment_strings.push(format!("{:?}", pt.path_root));
                }

                let full_path = segment_strings.join("::");

                write!(f, "{}", full_path)
            }
        }
    }
}

#[derive(Debug, Clone)]
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
