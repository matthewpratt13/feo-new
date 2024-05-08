///////////////////////////////////////////////////////////////////////////
// HELPER TYPES
///////////////////////////////////////////////////////////////////////////
use super::{
    AssigneeExpr, Delimiter, Expression, Identifier, InnerAttr, Item, Keyword, OuterAttr, Pattern,
    ReferenceOp, Separator, Type, ValueExpr,
};

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

#[derive(Debug, Clone, PartialEq)]
pub(crate) enum InherentImplItem {
    ConstantDecl(ConstantDecl),
    FunctionDef(FunctionItem),
}

#[derive(Debug, Clone, PartialEq)]
pub(crate) enum TraitDefItem {
    AliasDecl(AliasDecl),
    ConstantDecl(ConstantDecl),
    FunctionDef(FunctionItem),
}

#[derive(Debug, Clone, PartialEq)]
pub(crate) enum TraitImplItem {
    AliasDecl(AliasDecl),
    ConstantDecl(ConstantDecl),
    FunctionDef(FunctionItem),
}

#[derive(Debug, Clone, PartialEq)]
pub(crate) enum Visibility {
    Private,                   // default
    PubPackage(PubPackageVis), // `pub(package)`
    Pub,                       // `pub`
}

/// Type alias representing a path to an `Item` or local variable.
pub(crate) type PathType = Expression;

#[derive(Debug, Clone, PartialEq)]
pub(crate) struct EnumVariantStruct {
    pub(crate) open_brace: Delimiter,
    pub(crate) fields: Vec<StructDefField>,
    pub(crate) close_brace: Delimiter,
}

#[derive(Debug, Clone, PartialEq)]
pub(crate) struct EnumVariantTuple {
    pub(crate) open_paren: Delimiter,
    pub(crate) element_types: Vec<Type>,
    pub(crate) close_paren: Delimiter,
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
    pub(crate) param_name: Pattern,
    pub(crate) param_type: Box<Type>,
}

#[derive(Debug, Clone, PartialEq)]
pub(crate) struct ImportTree {
    pub(crate) path_segments: Vec<PathSegment>,
    pub(crate) wildcard_opt: Option<Separator>,
    pub(crate) as_clause_opt: Option<(Keyword, Identifier)>,
}

#[derive(Debug, Clone, PartialEq)]
pub(crate) struct PathSegment {
    pub(crate) root: Expression,
    pub(crate) subset_opt: Option<PathSubset>,
}

#[derive(Debug, Clone, PartialEq)]
pub(crate) struct PathSubset {
    pub(crate) open_brace: Delimiter,
    pub(crate) trees: Vec<ImportTree>,
    pub(crate) close_brace: Delimiter,
}

#[derive(Debug, Clone, PartialEq)]
pub(crate) struct PubPackageVis {
    pub(crate) kw_pub: Keyword,
    pub(crate) open_paren: Delimiter,
    pub(crate) kw_package: Keyword,
    pub(crate) close_paren: Delimiter,
}

#[derive(Debug, Clone, PartialEq)]
pub(crate) struct SelfParam {
    pub(crate) prefix_opt: Option<ReferenceOp>,
    pub(crate) kw_self: Keyword,
}

#[derive(Debug, Clone, PartialEq)]
pub(crate) struct StructDefField {
    pub(crate) attributes_opt: Option<Vec<OuterAttr>>,
    pub(crate) visibility: Visibility,
    pub(crate) field_name: Identifier,
    pub(crate) field_type: Box<Type>,
}

#[derive(Debug, Clone, PartialEq)]
pub(crate) struct TupleStructDefField {
    pub(crate) attributes_opt: Option<Vec<OuterAttr>>,
    pub(crate) visibility: Visibility,
    pub(crate) field_type: Box<Type>,
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
}

#[derive(Debug, Clone, PartialEq)]
pub struct ConstantDecl {
    pub(crate) attributes_opt: Option<Vec<OuterAttr>>,
    pub(crate) visibility: Visibility,
    pub(crate) kw_const: Keyword,
    pub(crate) item_name: Identifier,
    pub(crate) item_type: Box<Type>,
    pub(crate) value_opt: Option<ValueExpr>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct EnumDef {
    pub(crate) attributes_opt: Option<Vec<OuterAttr>>,
    pub(crate) visibility: Visibility,
    pub(crate) kw_enum: Keyword,
    pub(crate) enum_name: Identifier,
    pub(crate) open_brace: Delimiter,
    pub(crate) variants: Vec<EnumVariant>,
    pub(crate) close_brace: Delimiter,
}

#[derive(Debug, Clone, PartialEq)]
pub struct FunctionItem {
    pub(crate) attributes_opt: Option<Vec<OuterAttr>>,
    pub(crate) visibility: Visibility,
    pub(crate) kw_func: Keyword,
    pub(crate) function_name: Identifier,
    pub(crate) open_paren: Delimiter,
    pub(crate) params_opt: Option<Vec<FunctionOrMethodParam>>,
    pub(crate) close_paren: Delimiter,
    pub(crate) return_type_opt: Option<Box<Type>>,
    pub(crate) block_opt: Option<Expression>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ImportDecl {
    pub(crate) attributes_opt: Option<Vec<OuterAttr>>,
    pub(crate) visibility: Visibility,
    pub(crate) kw_import: Keyword,
    pub(crate) tree: ImportTree,
}

#[derive(Debug, Clone, PartialEq)]
pub struct InherentImplDef {
    pub(crate) attributes_opt: Option<Vec<OuterAttr>>,
    pub(crate) kw_impl: Keyword,
    pub(crate) nominal_type: Type,
    pub(crate) open_brace: Delimiter,
    pub(crate) associated_items_opt: Option<Vec<InherentImplItem>>,
    pub(crate) close_brace: Delimiter,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ModuleItem {
    pub(crate) outer_attributes_opt: Option<Vec<OuterAttr>>,
    pub(crate) visibility: Visibility,
    pub(crate) kw_module: Keyword,
    pub(crate) module_name: Identifier,
    pub(crate) open_brace: Delimiter,
    pub(crate) inner_attributes_opt: Option<Vec<InnerAttr>>,
    pub(crate) items_opt: Option<Vec<Item>>,
    pub(crate) close_brace: Delimiter,
}

#[derive(Debug, Clone, PartialEq)]
pub struct StaticItemDecl {
    pub(crate) attributes_opt: Option<Vec<OuterAttr>>,
    pub(crate) visibility: Visibility,
    pub(crate) kw_static: Keyword,
    pub(crate) kw_mut_opt: Option<Keyword>,
    pub(crate) item_name: Identifier,
    pub(crate) item_type: Type,
    pub(crate) assignee_opt: Option<Box<AssigneeExpr>>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct StructDef {
    pub(crate) attributes_opt: Option<Vec<OuterAttr>>,
    pub(crate) visibility: Visibility,
    pub(crate) kw_struct: Keyword,
    pub(crate) struct_name: Identifier,
    pub(crate) open_brace: Delimiter,
    pub(crate) fields_opt: Option<Vec<StructDefField>>,
    pub(crate) close_brace: Delimiter,
}

#[derive(Debug, Clone, PartialEq)]
pub struct TupleStructDef {
    pub(crate) attributes_opt: Option<Vec<OuterAttr>>,
    pub(crate) visibility: Visibility,
    pub(crate) kw_struct: Keyword,
    pub(crate) struct_name: Identifier,
    pub(crate) open_paren: Delimiter,
    pub(crate) fields_opt: Option<Vec<TupleStructDefField>>,
    pub(crate) close_paren: Delimiter,
}

#[derive(Debug, Clone, PartialEq)]
pub struct TraitDef {
    pub(crate) outer_attributes_opt: Option<Vec<OuterAttr>>,
    pub(crate) visibility: Visibility,
    pub(crate) kw_trait: Keyword,
    pub(crate) trait_name: Identifier,
    pub(crate) open_brace: Delimiter,
    pub(crate) inner_attributes_opt: Option<Vec<InnerAttr>>,
    pub(crate) trait_items_opt: Option<Vec<TraitDefItem>>,
    pub(crate) close_brace: Delimiter,
}

#[derive(Debug, Clone, PartialEq)]
pub struct TraitImplDef {
    pub(crate) attributes_opt: Option<Vec<OuterAttr>>,
    pub(crate) kw_impl: Keyword,
    pub(crate) implemented_trait_path: PathType,
    pub(crate) kw_for: Keyword,
    pub(crate) implementing_type: Type,
    pub(crate) open_brace: Delimiter,
    pub(crate) associated_items_opt: Option<Vec<TraitImplItem>>,
    pub(crate) close_brace: Delimiter,
}
