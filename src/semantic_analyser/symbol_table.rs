use core::fmt;
use std::collections::HashMap;

use crate::ast::{
    EnumDef, Expression, FunctionItem, Identifier, ModuleItem, StructDef, TraitDef, TupleStructDef,
    Type, TypePath, Unit, Visibility,
};

pub(crate) type SymbolTable = HashMap<TypePath, Symbol>;

#[derive(Debug, Clone, PartialEq, PartialOrd)]
pub(crate) enum ScopeKind {
    LocalBlock,
    MatchExpr,
    ForInLoop,
    Function(String),
    Module(String),
    RootModule(String),
    Lib,
    Public,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) enum Symbol {
    Variable {
        name: Identifier,
        var_type: Type,
        // data: Option<Expression>,
    },
    Struct {
        path: TypePath,
        struct_def: StructDef,
    },
    TupleStruct {
        path: TypePath,
        tuple_struct_def: TupleStructDef,
    },
    Enum {
        path: TypePath,
        enum_def: EnumDef,
    },
    Trait {
        path: TypePath,
        trait_def: TraitDef,
    },
    Alias {
        path: TypePath,
        visibility: Visibility,
        alias_name: Identifier,
        original_type_opt: Option<Type>,
    },
    Constant {
        path: TypePath,
        visibility: Visibility,
        constant_name: Identifier,
        constant_type: Type,
    },
    Function {
        path: TypePath,
        function: FunctionItem,
    },
    Module {
        path: TypePath,
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
    pub(crate) fn symbol_type(&self) -> Type {
        match self.clone() {
            Symbol::Variable { var_type, .. } => var_type,
            Symbol::Struct { path, .. } => Type::UserDefined(path),
            Symbol::TupleStruct { path, .. } => Type::UserDefined(path),
            Symbol::Enum { path, .. } => Type::UserDefined(path),
            Symbol::Trait { path, .. } => Type::UserDefined(path),
            Symbol::Alias { path, .. } => Type::UserDefined(path),
            Symbol::Constant { constant_type, .. } => constant_type,
            Symbol::Function { function, .. } => match function.return_type_opt {
                Some(t) => *t.clone(),
                None => Type::UnitType(Unit),
            },
            Symbol::Module { .. } => Type::UnitType(Unit),
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
            Symbol::Variable { name, .. } => write!(f, "Variable(\"{name}\")"),
            Symbol::Struct { path, .. } => write!(f, "Struct({path})"),
            Symbol::TupleStruct { path, .. } => write!(f, "TupleStruct({path})"),
            Symbol::Enum { path, .. } => write!(f, "Enum({path})"),
            Symbol::Trait { path, .. } => write!(f, "Trait({path})"),
            Symbol::Alias { path, .. } => write!(f, "Alias({path})"),
            Symbol::Constant { path, .. } => write!(f, "Constant({path})"),
            Symbol::Function { path, .. } => write!(f, "Function({path})"),
            Symbol::Module { path, .. } => write!(f, "Module({path})"),
        }
    }
}
