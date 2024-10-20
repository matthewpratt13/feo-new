use core::fmt;
use std::{collections::HashMap, rc::Rc};

use crate::{
    ast::{
        EnumDef, FunctionItem, Identifier, InherentImplItem, ModuleItem, StructDef, TraitDef,
        TraitImplItem, TupleStructDef, Type, TypePath, Visibility,
    },
    error::SemanticErrorKind,
    semantic_analyser::utils::ToIdentifier,
};

use super::FormatItem;

/// Type alias representing a symbol table that maps `TypePath` to `Symbol`.
pub(crate) type SymbolTable = HashMap<TypePath, Symbol>;

/// Enumeration of the different kinds of scopes that can be encountered during semantic analysis.
#[derive(Debug, Clone, PartialEq, PartialOrd)]
pub(crate) enum ScopeKind {
    LocalBlock,
    MatchExpr,
    ForInLoop,
    FunctionBody(TypePath),
    FunctionDef(TypePath),
    TraitImpl {
        implemented_trait_path: TypePath,
        implementing_type_path: TypePath,
    },
    Impl(TypePath),
    // TraitDef(TypePath),
    Module(TypePath),
    ProgramRoot,
    Public,
}

impl fmt::Display for ScopeKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ScopeKind::LocalBlock => write!(f, "LocalBlock"),
            ScopeKind::MatchExpr => write!(f, "MatchExpr"),
            ScopeKind::ForInLoop => write!(f, "ForInLoop"),
            ScopeKind::FunctionBody(type_path) => {
                write!(f, "FunctionBody(\"{}\")", type_path)
            }
            ScopeKind::FunctionDef(type_path) => {
                write!(f, "FunctionDef(\"{}\")", type_path)
            }
            ScopeKind::TraitImpl {
                implemented_trait_path,
                implementing_type_path,
            } => write!(
                f,
                "TraitImpl(\"{} for {}\")",
                implemented_trait_path, implementing_type_path
            ),

            ScopeKind::Impl(type_path) => write!(f, "Impl(\"{}\")", type_path),
            // ScopeKind::TraitDef(type_path) => write!(f, "TraitDef(\"{}\")", type_path),
            ScopeKind::Module(type_path) => write!(f, "Module(\"{}\")", type_path),
            ScopeKind::ProgramRoot => write!(f, "ProgramRoot"),
            ScopeKind::Public => write!(f, "Public"),
        }
    }
}

/// Enum representing different types of symbols that can be encountered during semantic analysis.
#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) enum Symbol {
    Variable {
        name: Identifier,
        var_type: Type,
    },
    Struct {
        path: TypePath,
        struct_def: Rc<StructDef>,
        associated_items_inherent: Vec<InherentImplItem>,
        associated_items_trait: Vec<TraitImplItem>,
    },
    TupleStruct {
        path: TypePath,
        tuple_struct_def: Rc<TupleStructDef>,
        associated_items_inherent: Vec<InherentImplItem>,
        associated_items_trait: Vec<TraitImplItem>,
    },
    Enum {
        path: TypePath,
        enum_def: Rc<EnumDef>,
        associated_items_inherent: Vec<InherentImplItem>,
        associated_items_trait: Vec<TraitImplItem>,
    },
    Trait {
        path: TypePath,
        trait_def: Rc<TraitDef>,
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
        function: Rc<FunctionItem>,
    },
    Module {
        path: TypePath,
        module: Rc<ModuleItem>,
        symbols: SymbolTable,
    },
}

impl Symbol {
    pub(crate) fn add_associated_items(
        &mut self,
        inherent_item: Option<InherentImplItem>,
        trait_item: Option<TraitImplItem>,
    ) -> Result<(), SemanticErrorKind> {
        match self {
            Symbol::Struct {
                associated_items_inherent,
                associated_items_trait,
                ..
            }
            | Symbol::TupleStruct {
                associated_items_inherent,
                associated_items_trait,
                ..
            }
            | Symbol::Enum {
                associated_items_inherent,
                associated_items_trait,
                ..
            } => {
                if let Some(item) = inherent_item {
                    associated_items_inherent.push(item);
                }

                if let Some(item) = trait_item {
                    associated_items_trait.push(item);
                }

                Ok(())
            }

            sym => Err(SemanticErrorKind::UnexpectedSymbol {
                name: sym.type_path().to_identifier(),
                expected: "struct or enum".to_string(),
                found: sym.symbol_type().to_backtick_string(),
            }),
        }
    }

    pub(crate) fn symbol_type(&self) -> Type {
        match self.clone() {
            Symbol::Variable { var_type, .. } => var_type,
            Symbol::Struct { path, .. } => Type::UserDefined(path),
            Symbol::TupleStruct { path, .. } => Type::UserDefined(path),
            Symbol::Enum { path, .. } => Type::UserDefined(path),
            Symbol::Trait { path, .. } => Type::UserDefined(path),
            Symbol::Alias { path, .. } => Type::UserDefined(path),
            Symbol::Constant { constant_type, .. } => constant_type,
            Symbol::Function { function, .. } => match &function.return_type_opt {
                Some(t) => *t.clone(),
                None => Type::UNIT_TYPE,
            },
            Symbol::Module { .. } => Type::UNIT_TYPE,
        }
    }

    pub(crate) fn type_path(&self) -> TypePath {
        match self.clone() {
            Symbol::Variable { name, .. } => name.to_type_path(),
            Symbol::Struct { path, .. }
            | Symbol::TupleStruct { path, .. }
            | Symbol::Enum { path, .. }
            | Symbol::Trait { path, .. }
            | Symbol::Alias { path, .. }
            | Symbol::Constant { path, .. }
            | Symbol::Function { path, .. }
            | Symbol::Module { path, .. } => path,
        }
    }
}

impl FormatItem for Symbol {}

impl fmt::Display for Symbol {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Symbol::Variable { name, .. } => write!(f, "Variable(\"{name}\")"),
            Symbol::Struct { path, .. } => write!(f, "Struct(\"{path}\")"),
            Symbol::TupleStruct { path, .. } => write!(f, "TupleStruct(\"{path}\")"),
            Symbol::Enum { path, .. } => write!(f, "Enum(\"{path}\")"),
            Symbol::Trait { path, .. } => write!(f, "Trait(\"{path}\")"),
            Symbol::Alias { path, .. } => write!(f, "Alias(\"{path}\")"),
            Symbol::Constant { path, .. } => write!(f, "Constant(\"{path}\")"),
            Symbol::Function { path, .. } => write!(f, "Function(\"{path}\")"),
            Symbol::Module { path, .. } => write!(f, "Module(\"{path}\")"),
        }
    }
}

/// Struct that represents a single scope in the semantic analyser.
#[derive(Debug, Clone)]
pub(crate) struct Scope {
    pub(crate) scope_kind: ScopeKind,
    pub(crate) symbols: SymbolTable,
}

// TODO: add docs
#[derive(Debug, Clone, PartialEq)]
pub(crate) struct Module {
    pub(crate) name: Identifier,
    pub(crate) table: SymbolTable,
}
