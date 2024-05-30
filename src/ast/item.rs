use core::fmt;

use crate::span::Span;

use super::{
    AssigneeExpr, BlockExpr, Identifier, IdentifierPatt, InnerAttr, Item, Keyword, OuterAttr,
    PathType, ReferenceOp, Separator, Type, ValueExpr,
};

///////////////////////////////////////////////////////////////////////////
// HELPER TYPES
///////////////////////////////////////////////////////////////////////////

#[derive(Debug, Clone, PartialEq)]
pub(crate) enum EnumVariantType {
    Struct(EnumVariantStruct),
    Tuple(EnumVariantTuple),
}

#[derive(Debug, Clone, PartialEq)]
pub(crate) enum FunctionOrMethodParam {
    FunctionParam(FunctionParam),
    MethodParam(SelfParam),
}

impl fmt::Display for FunctionOrMethodParam {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            FunctionOrMethodParam::FunctionParam(t) => write!(f, "{}", t),
            FunctionOrMethodParam::MethodParam(t) => write!(f, "{}", t),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub(crate) enum InherentImplItem {
    ConstantDecl(ConstantDecl),
    FunctionItem(FunctionItem),
}

#[derive(Debug, Clone, PartialEq)]
pub(crate) enum TraitDefItem {
    AliasDecl(AliasDecl),
    ConstantDecl(ConstantDecl),
    FunctionItem(FunctionItem),
}

#[derive(Debug, Clone, PartialEq)]
pub(crate) enum TraitImplItem {
    AliasDecl(AliasDecl),
    ConstantDecl(ConstantDecl),
    FunctionItem(FunctionItem),
}

#[derive(Debug, Clone, PartialEq)]
pub(crate) enum Visibility {
    Private,                   // default
    PubPackage(PubPackageVis), // `pub(package)`
    Pub,                       // accessible everywhere
}

#[derive(Debug, Clone, PartialEq)]
pub(crate) struct EnumVariantStruct {
    pub(crate) struct_fields: Vec<StructDefField>,
}

#[derive(Debug, Clone, PartialEq)]
pub(crate) struct EnumVariantTuple {
    pub(crate) element_types: Vec<Type>,
}

#[derive(Debug, Clone, PartialEq)]
pub(crate) struct EnumVariant {
    pub(crate) attributes_opt: Option<Vec<OuterAttr>>,
    pub(crate) visibility: Visibility,
    pub(crate) variant_name: Identifier,
    pub(crate) variant_type_opt: Option<EnumVariantType>,
}

#[derive(Debug, Clone, PartialEq)]
pub(crate) struct FunctionParam {
    pub(crate) param_name: IdentifierPatt,
    pub(crate) param_type: Box<Type>,
}

impl fmt::Display for FunctionParam {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}: {}", self.param_name, self.param_type)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub(crate) struct ImportTree {
    pub(crate) path_segments: Vec<PathSegment>,
    pub(crate) wildcard_opt: Option<Separator>, // trailing `::*`
    pub(crate) as_clause_opt: Option<(Keyword, Identifier)>, // (`as`, `new_name`)
}

#[derive(Debug, Clone, PartialEq)]
pub(crate) struct PathSegment {
    pub(crate) root: PathType, // e.g., `PathRoot::Identifier(_)`, `package::module::Object`
    pub(crate) subset_opt: Option<PathSubset>, // e.g., `::{ Foo, Bar, .. }` (basic)
}

#[derive(Debug, Clone, PartialEq)]
pub(crate) struct PathSubset {
    pub(crate) nested_trees: Vec<ImportTree>, // e.g., `::{ Foo, bar::baz{ FooBar, BAZ, .. }, .. }`
}

#[derive(Debug, Clone, PartialEq)]
pub(crate) struct PubPackageVis {
    pub(crate) kw_pub: Keyword,
    pub(crate) kw_package: Keyword,
}

#[derive(Debug, Clone, PartialEq)]
pub(crate) struct SelfParam {
    pub(crate) reference_op_opt: Option<ReferenceOp>,
    pub(crate) kw_self: Keyword,
}

impl fmt::Display for SelfParam {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:?}{}", self.reference_op_opt, self.kw_self)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub(crate) struct StructDefField {
    pub(crate) attributes_opt: Option<Vec<OuterAttr>>,
    pub(crate) visibility: Visibility,
    pub(crate) field_name: Identifier,
    pub(crate) field_type: Box<Type>,
}

#[derive(Debug, Clone, PartialEq)]
pub(crate) struct TupleStructDefElement {
    pub(crate) attributes_opt: Option<Vec<OuterAttr>>,
    pub(crate) visibility: Visibility,
    pub(crate) element_type: Box<Type>,
}

///////////////////////////////////////////////////////////////////////////
// AST NODE STRUCTURES
///////////////////////////////////////////////////////////////////////////

#[derive(Debug, Clone, PartialEq)]
pub struct AliasDecl {
    pub(crate) attributes_opt: Option<Vec<OuterAttr>>,
    pub(crate) visibility: Visibility,
    pub(crate) kw_alias: Keyword,
    pub(crate) alias_name: Identifier,
    pub(crate) original_type_opt: Option<Type>,
    pub(crate) span: Span,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ConstantDecl {
    pub(crate) attributes_opt: Option<Vec<OuterAttr>>,
    pub(crate) visibility: Visibility,
    pub(crate) kw_const: Keyword,
    pub(crate) constant_name: Identifier,
    pub(crate) constant_type: Box<Type>,
    pub(crate) value_opt: Option<ValueExpr>,
    pub(crate) span: Span,
}

#[derive(Debug, Clone, PartialEq)]
pub struct EnumDef {
    pub(crate) attributes_opt: Option<Vec<OuterAttr>>,
    pub(crate) visibility: Visibility,
    pub(crate) kw_enum: Keyword,
    pub(crate) enum_name: Identifier,
    pub(crate) variants: Vec<EnumVariant>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct FunctionItem {
    pub(crate) attributes_opt: Option<Vec<OuterAttr>>,
    pub(crate) visibility: Visibility,
    pub(crate) kw_func: Keyword,
    pub(crate) function_name: Identifier,
    pub(crate) params_opt: Option<Vec<FunctionOrMethodParam>>,
    pub(crate) return_type_opt: Option<Box<Type>>,
    pub(crate) block_opt: Option<BlockExpr>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ImportDecl {
    pub(crate) attributes_opt: Option<Vec<OuterAttr>>,
    pub(crate) visibility: Visibility,
    pub(crate) kw_import: Keyword,
    pub(crate) import_tree: ImportTree,
}

#[derive(Debug, Clone, PartialEq)]
pub struct InherentImplDef {
    pub(crate) attributes_opt: Option<Vec<OuterAttr>>,
    pub(crate) kw_impl: Keyword,
    pub(crate) nominal_type: PathType,
    pub(crate) associated_items_opt: Option<Vec<InherentImplItem>>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ModuleItem {
    pub(crate) outer_attributes_opt: Option<Vec<OuterAttr>>,
    pub(crate) visibility: Visibility,
    pub(crate) kw_module: Keyword,
    pub(crate) module_name: Identifier,
    pub(crate) inner_attributes_opt: Option<Vec<InnerAttr>>,
    pub(crate) items_opt: Option<Vec<Item>>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct StaticVarDecl {
    pub(crate) attributes_opt: Option<Vec<OuterAttr>>,
    pub(crate) visibility: Visibility,
    pub(crate) kw_static: Keyword,
    pub(crate) kw_mut_opt: Option<Keyword>,
    pub(crate) var_name: Identifier,
    pub(crate) var_type: Type,
    pub(crate) assignee_opt: Option<Box<AssigneeExpr>>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct StructDef {
    pub(crate) attributes_opt: Option<Vec<OuterAttr>>,
    pub(crate) visibility: Visibility,
    pub(crate) kw_struct: Keyword,
    pub(crate) struct_name: Identifier,
    pub(crate) fields_opt: Option<Vec<StructDefField>>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct TupleStructDef {
    pub(crate) attributes_opt: Option<Vec<OuterAttr>>,
    pub(crate) visibility: Visibility,
    pub(crate) kw_struct: Keyword,
    pub(crate) struct_name: Identifier,
    pub(crate) elements_opt: Option<Vec<TupleStructDefElement>>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct TraitDef {
    pub(crate) outer_attributes_opt: Option<Vec<OuterAttr>>,
    pub(crate) visibility: Visibility,
    pub(crate) kw_trait: Keyword,
    pub(crate) trait_name: Identifier,
    pub(crate) inner_attributes_opt: Option<Vec<InnerAttr>>,
    pub(crate) trait_items_opt: Option<Vec<TraitDefItem>>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct TraitImplDef {
    pub(crate) attributes_opt: Option<Vec<OuterAttr>>,
    pub(crate) kw_impl: Keyword,
    pub(crate) implemented_trait_path: PathType,
    pub(crate) kw_for: Keyword,
    pub(crate) implementing_type: Type,
    pub(crate) associated_items_opt: Option<Vec<TraitImplItem>>,
}
