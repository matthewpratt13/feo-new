use core::fmt;
use std::collections::HashMap;

use crate::ast::{
    EnumDef, FunctionItem, Identifier, ModuleItem, PathType, StructDef, TraitDef, TupleStructDef,
    Type,
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
    Module {
        module: ModuleItem,
        symbols: SymbolTable,
    },
}

impl Symbol {
    pub(crate) fn symbol_type(&self) -> Identifier {
        match self.clone() {
            Symbol::Variable(t) => Identifier::from(&t.to_string()),
            Symbol::Struct(s) => s.struct_name,
            Symbol::TupleStruct(ts) => ts.struct_name,
            Symbol::Enum(e) => e.enum_name,
            Symbol::Trait(t) => t.trait_name,
            Symbol::Function {
                associated_type_opt,
                function,
            } => match associated_type_opt {
                Some(t) => {
                    let assoc_type_name = t.type_name;
                    let func_name = function.function_name;

                    let func_full_path = format!("{}::{}", assoc_type_name, func_name);
                    Identifier(func_full_path)
                }
                None => Identifier::from(""),
            },
            Symbol::Module { module, .. } => module.module_name,
        }
    }
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
            Symbol::Module { module, .. } => write!(f, "{:?}", module),
        }
    }
}
