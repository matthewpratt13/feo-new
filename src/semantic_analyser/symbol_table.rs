use core::fmt;
use std::collections::HashMap;

use crate::ast::{
    AliasDecl, EnumDef, FunctionItem, Identifier, ModuleItem, PathType, StructDef, TraitDef,
    TupleStructDef, Type,
};

pub(crate) type SymbolTable = HashMap<PathType, Symbol>;

#[derive(Debug, Clone)]
pub(crate) enum ScopeKind {
    LocalBlock,
    MatchExpr,
    ForInLoop,
    Function,
    TraitImpl,
    Impl,
    Module,
    Global,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) enum Symbol {
    Variable(Type),
    Struct {
        path: PathType,
        struct_def: StructDef,
    },
    TupleStruct {
        path: PathType,
        tuple_struct_def: TupleStructDef,
    },
    Enum {
        path: PathType,
        enum_def: EnumDef,
    },
    Trait {
        path: PathType,
        trait_def: TraitDef,
    },
    Alias {
        path: PathType,
        alias_decl: AliasDecl,
    },
    Constant {
        path: PathType,
        constant_name: Identifier,
        constant_type: Type,
    },
    Function {
        path: PathType,
        function: FunctionItem,
    },
    Module {
        path: PathType,
        module: ModuleItem,
        symbols: SymbolTable,
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
            Symbol::Struct { struct_def, .. } => struct_def.struct_name,
            Symbol::TupleStruct {
                tuple_struct_def, ..
            } => tuple_struct_def.struct_name,
            Symbol::Enum { enum_def, .. } => enum_def.enum_name,
            Symbol::Trait { trait_def, .. } => trait_def.trait_name,
            Symbol::Alias { alias_decl, .. } => alias_decl.alias_name,
            Symbol::Constant { constant_name, .. } => constant_name,
            Symbol::Function { function, .. } => function.function_name,
            Symbol::Module { module, .. } => module.module_name,
        }
    }
}

impl fmt::Display for Symbol {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Symbol::Variable(v) => write!(f, "{}", v),
            Symbol::Struct { path, .. } => write!(f, "{}", path),
            Symbol::TupleStruct { path, .. } => write!(f, "{}", path),
            Symbol::Enum { path, .. } => write!(f, "{}", path),
            Symbol::Trait { path, .. } => write!(f, "{}", path),
            Symbol::Alias { path, .. } => write!(f, "{}", path),
            Symbol::Constant { path, .. } => write!(f, "{}", path),
            Symbol::Function { path, .. } => write!(f, "{}", path),
            Symbol::Module { path, .. } => write!(f, "{}", path),
        }
    }
}
