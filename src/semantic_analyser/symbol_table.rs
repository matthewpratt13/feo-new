use core::fmt;
use std::collections::HashMap;

use crate::ast::{
    EnumDef, FunctionItem, Identifier, ModuleItem, PathType, StructDef, TraitDef, TupleStructDef,
    Type, Visibility,
};

pub(crate) type SymbolTable = HashMap<PathType, Symbol>;

#[derive(Debug, Clone, PartialEq, PartialOrd)]
pub(crate) enum ScopeKind {
    LocalBlock,
    MatchExpr,
    ForInLoop,
    Function(String),
    TraitImpl(String),
    Impl(String),
    Module(String),
    Package,
    Public,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) enum Symbol {
    Variable {
        name: Identifier,
        var_type: Type,
    },
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
        visibility: Visibility,
        alias_name: Identifier,
        original_type_opt: Option<Type>,
    },
    Constant {
        path: PathType,
        visibility: Visibility,
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
            Symbol::Variable { var_type, .. } => Identifier::from(&var_type.to_string()),
            Symbol::Struct { struct_def, .. } => struct_def.struct_name,
            Symbol::TupleStruct {
                tuple_struct_def, ..
            } => tuple_struct_def.struct_name,
            Symbol::Enum { enum_def, .. } => enum_def.enum_name,
            Symbol::Trait { trait_def, .. } => trait_def.trait_name,
            Symbol::Alias { alias_name, .. } => alias_name,
            Symbol::Constant { constant_type, .. } => Identifier::from(&constant_type.to_string()),
            Symbol::Function { function, .. } => function.function_name,
            Symbol::Module { module, .. } => module.module_name,
        }
    }

    pub(crate) fn visibility(&self) -> Visibility {
        match self.clone() {
            Symbol::Variable { .. } => Visibility::Private,
            Symbol::Struct { struct_def, .. } => struct_def.visibility,
            Symbol::TupleStruct {
                tuple_struct_def, ..
            } => tuple_struct_def.visibility,
            Symbol::Enum { enum_def, .. } => enum_def.visibility,
            Symbol::Trait { trait_def, .. } => trait_def.visibility,
            Symbol::Alias { visibility, .. } => visibility,
            Symbol::Constant { visibility, .. } => visibility,
            Symbol::Function { function, .. } => function.visibility,
            Symbol::Module { .. } => Visibility::Private,
        }
    }
}

impl fmt::Display for Symbol {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Symbol::Variable { name, .. } => write!(f, "{}", name),
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
