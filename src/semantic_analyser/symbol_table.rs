use core::fmt;
use std::collections::HashMap;

use crate::ast::{
    EnumDef, FunctionItem, Identifier, ModuleItem, PathType, StructDef, TraitDef, TupleStructDef,
    Type,
};

pub(crate) type SymbolTable = HashMap<Identifier, Symbol>;

#[derive(Debug, Clone)]
pub(crate) enum ScopeKind {
    Global,
    LocalBlock,
    MatchExpr,
    ForInLoop,
    Function,
    Module,
}

// TODO: add `path` fields to `Struct`, `TupleStruct`, `Enum` and `Trait`
#[derive(Debug, Clone, PartialEq)]
pub(crate) enum Symbol {
    Variable(Type),
    Struct(StructDef),
    TupleStruct(TupleStructDef),
    Enum(EnumDef),
    Trait(TraitDef),
    Function {
        path_opt: Option<PathType>,
        function: FunctionItem,
    },
    Module {
        path_opt: Option<PathType>,
        module: ModuleItem,
        symbols: SymbolTable,
    },
    Constant {
        path: PathType,
        ty: Type,
    },
}

#[derive(Debug, Clone)]
pub(crate) struct Scope {
    pub(crate) scope_kind: ScopeKind,
    pub(crate) symbols: SymbolTable,
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
                path_opt: associated_type_opt,
                function,
            } => match associated_type_opt {
                Some(t) => {
                    let assoc_type_name = t.type_name;
                    let func_name = function.function_name;

                    let func_full_path = format!("{}::{}", assoc_type_name, func_name);
                    Identifier::from(&func_full_path)
                }
                None => Identifier::from(""),
            },
            Symbol::Module { module, .. } => module.module_name,
            Symbol::Constant { ty, .. } => Identifier::from(&ty.to_string()),
        }
    }
}

impl fmt::Display for Symbol {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Symbol::Variable(v) => write!(f, "{}", v),
            Symbol::Struct(s) => write!(f, "{}", s.struct_name),
            Symbol::TupleStruct(ts) => write!(f, "{}", ts.struct_name),
            Symbol::Enum(e) => write!(f, "{}", e.enum_name),
            Symbol::Trait(t) => write!(f, "{}", t.trait_name),
            Symbol::Function { function, .. } => write!(f, "{}", function.function_name),
            Symbol::Module { module, .. } => write!(f, "{}", module.module_name),
            Symbol::Constant { path, .. } => write!(f, "{}", path.type_name),
        }
    }
}
