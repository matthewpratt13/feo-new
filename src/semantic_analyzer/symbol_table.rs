use core::fmt;
use std::collections::HashMap;

use crate::ast::{
    EnumDef, FunctionItem, Identifier, PathType, StructDef, TraitDef, TupleStructDef, Type,
};

pub(crate) type SymbolTable = HashMap<Identifier, Symbol>;

#[derive(Debug, Clone, PartialEq)]
pub(crate) enum Symbol {
    Variable(Type),
    Struct(StructDef),
    TupleStruct(TupleStructDef),
    Enum(EnumDef),
    Trait(TraitDef),
    Function {
        associated_type_opt: Option<PathType>,
        function: FunctionItem,
    },
    Module(SymbolTable),
}

impl fmt::Display for Symbol {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Symbol::Variable(v) => write!(f, "{}", v),
            Symbol::Struct(s) => write!(f, "{:?}", s),
            Symbol::TupleStruct(ts) => write!(f, "{:?}", ts),
            Symbol::Enum(e) => write!(f, "{:?}", e),
            Symbol::Trait(t) => write!(f, "{:?}", t),
            Symbol::Function {
                associated_type_opt: associated_type,
                ..
            } => write!(f, "{:?}", associated_type),
            Symbol::Module(m) => write!(f, "{:?}", m),
        }
    }
}
