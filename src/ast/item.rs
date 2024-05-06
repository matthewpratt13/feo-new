///////////////////////////////////////////////////////////////////////////
// HELPER TYPES
///////////////////////////////////////////////////////////////////////////
use super::{
    Delimiter, Expression, Identifier, InnerAttr, Item, Keyword, OuterAttr, PathType, Pattern, ReferenceOp, Separator, Type, ValueExpr
};

#[derive(Debug, Clone, PartialEq)]
pub enum EnumVariantType {
    Struct(EnumVariantStruct),
    Tuple(EnumVariantTuple),
}

#[derive(Debug, Clone, PartialEq)]
pub enum FunctionOrMethodParam {
    FunctionParam(FunctionParam),
    MethodParam(SelfParam),
}

#[derive(Debug, Clone, PartialEq)]
pub enum InherentImplItem {
    ConstantDecl(ConstantDecl),
    FunctionDef(FunctionItem),
}

#[derive(Debug, Clone, PartialEq)]
pub enum TraitDefItem {
    AliasDecl(AliasDecl),
    ConstantDecl(ConstantDecl),
    FunctionDef(FunctionItem),
}

#[derive(Debug, Clone, PartialEq)]
pub enum TraitImplItem {
    AliasDecl(AliasDecl),
    ConstantDecl(ConstantDecl),
    FunctionDef(FunctionItem),
}

#[derive(Debug, Clone, PartialEq)]
pub enum Visibility {
    Private,                   // default
    PubPackage(PubPackageVis), // `pub(package)`
    Pub,                       // `pub`
}

#[derive(Debug, Clone, PartialEq)]
pub struct EnumVariantStruct {
    pub open_brace: Delimiter,
    pub fields: Vec<StructDefField>,
    pub close_brace: Delimiter,
}

#[derive(Debug, Clone, PartialEq)]
pub struct EnumVariantTuple {
    pub open_paren: Delimiter,
    pub element_types: Vec<Type>,
    pub close_paren: Delimiter,
}

#[derive(Debug, Clone, PartialEq)]
pub struct EnumVariant {
    pub attributes_opt: Option<Vec<OuterAttr>>,
    pub visibility: Visibility,
    pub variant_name: Identifier,
    pub variant_type_opt: Option<EnumVariantType>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct FunctionParam {
    pub param_name: Pattern,
    pub param_type: Box<Type>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ImportTree {
    pub path_segments: Vec<PathSegment>,
    pub wildcard_opt: Option<Separator>,
    pub as_clause_opt: Option<(Keyword, Identifier)>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct PathSegment {
    pub root: Expression,
    pub subset_opt: Option<PathSubset>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct PathSubset {
    pub open_brace: Delimiter,
    pub trees: Vec<ImportTree>,
    pub close_brace: Delimiter,
}

#[derive(Debug, Clone, PartialEq)]
pub struct PubPackageVis {
    pub kw_pub: Keyword,
    pub open_paren: Delimiter,
    pub kw_package: Keyword,
    pub close_paren: Delimiter,
}

#[derive(Debug, Clone, PartialEq)]
pub struct SelfParam {
    pub prefix_opt: Option<ReferenceOp>,
    pub kw_self: Keyword,
}

#[derive(Debug, Clone, PartialEq)]
pub struct StructDefField {
    pub attributes_opt: Option<Vec<OuterAttr>>,
    pub visibility: Visibility,
    pub field_name: Identifier,
    pub field_type: Box<Type>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct TupleStructDefField {
    pub attributes_opt: Option<Vec<OuterAttr>>,
    pub visibility: Visibility,
    pub field_type: Box<Type>,
}

///////////////////////////////////////////////////////////////////////////
// AST NODE STRUCTURES
///////////////////////////////////////////////////////////////////////////

#[derive(Debug, Clone, PartialEq)]
pub struct AliasDecl {
    pub attributes_opt: Option<Vec<OuterAttr>>,
    pub visibility: Visibility,
    pub kw_alias: Keyword,
    pub alias_name: Identifier,
    pub original_type_opt: Option<Type>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ConstantDecl {
    pub attributes_opt: Option<Vec<OuterAttr>>,
    pub visibility: Visibility,
    pub kw_const: Keyword,
    pub item_name: Identifier,
    pub item_type: Box<Type>,
    pub value_opt: Option<ValueExpr>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct EnumDef {
    pub attributes_opt: Option<Vec<OuterAttr>>,
    pub visibility: Visibility,
    pub kw_enum: Keyword,
    pub enum_name: Identifier,
    pub open_brace: Delimiter,
    pub variants: Vec<EnumVariant>,
    pub close_brace: Delimiter,
}

#[derive(Debug, Clone, PartialEq)]
pub struct FunctionItem {
    pub attributes_opt: Option<Vec<OuterAttr>>,
    pub visibility: Visibility,
    pub kw_func: Keyword,
    pub function_name: Identifier,
    pub open_paren: Delimiter,
    pub params_opt: Option<Vec<FunctionOrMethodParam>>,
    pub close_paren: Delimiter,
    pub return_type_opt: Option<Box<Type>>,
    pub block_opt: Option<Expression>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ImportDecl {
    pub attributes_opt: Option<Vec<OuterAttr>>,
    pub visibility: Visibility,
    pub kw_import: Keyword,
    pub tree: ImportTree,
}

#[derive(Debug, Clone, PartialEq)]
pub struct InherentImplDef {
    pub attributes_opt: Option<Vec<OuterAttr>>,
    pub kw_impl: Keyword,
    pub nominal_type: Type,
    pub open_brace: Delimiter,
    pub associated_items_opt: Option<Vec<InherentImplItem>>,
    pub close_brace: Delimiter,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ModuleItem {
    pub outer_attributes_opt: Option<Vec<OuterAttr>>,
    pub visibility: Visibility,
    pub kw_module: Keyword,
    pub module_name: Identifier,
    pub open_brace: Delimiter,
    pub inner_attributes_opt: Option<Vec<InnerAttr>>,
    pub items_opt: Option<Vec<Item>>,
    pub close_brace: Delimiter,
}

#[derive(Debug, Clone, PartialEq)]
pub struct StaticItemDecl {
    pub attributes_opt: Option<Vec<OuterAttr>>,
    pub visibility: Visibility,
    pub kw_static: Keyword,
    pub kw_mut_opt: Option<Keyword>,
    pub item_name: Identifier,
    pub item_type: Type,
    pub value_opt: Option<Box<Expression>>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct StructDef {
    pub attributes_opt: Option<Vec<OuterAttr>>,
    pub visibility: Visibility,
    pub kw_struct: Keyword,
    pub struct_name: Identifier,
    pub open_brace: Delimiter,
    pub fields_opt: Option<Vec<StructDefField>>,
    pub close_brace: Delimiter,
}

#[derive(Debug, Clone, PartialEq)]
pub struct TupleStructDef {
    pub attributes_opt: Option<Vec<OuterAttr>>,
    pub visibility: Visibility,
    pub kw_struct: Keyword,
    pub struct_name: Identifier,
    pub open_paren: Delimiter,
    pub fields_opt: Option<Vec<TupleStructDefField>>,
    pub close_paren: Delimiter,
}

#[derive(Debug, Clone, PartialEq)]
pub struct TraitDef {
    pub outer_attributes_opt: Option<Vec<OuterAttr>>,
    pub visibility: Visibility,
    pub kw_trait: Keyword,
    pub trait_name: Identifier,
    pub open_brace: Delimiter,
    pub inner_attributes_opt: Option<Vec<InnerAttr>>,
    pub trait_items_opt: Option<Vec<TraitDefItem>>,
    pub close_brace: Delimiter,
}

#[derive(Debug, Clone, PartialEq)]
pub struct TraitImplDef {
    pub attributes_opt: Option<Vec<OuterAttr>>,
    pub kw_impl: Keyword,
    pub implemented_trait_path: PathType,
    pub kw_for: Keyword,
    pub implementing_type: Type,
    pub open_brace: Delimiter,
    pub associated_items_opt: Option<Vec<TraitImplItem>>,
    pub close_brace: Delimiter,
}
