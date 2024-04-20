///////////////////////////////////////////////////////////////////////////
/// HELPER TYPES
///////////////////////////////////////////////////////////////////////////
use super::{
    BlockExpr, Delimiter, Expression, Identifier, InnerAttr, Item, Keyword, OuterAttr, PathExpr,
    Separator, Type, UnaryOp,
};

#[derive(Debug, Clone)]
pub enum EnumVariantType {
    Struct(EnumVariantStruct),
    Tuple(EnumVariantTuple),
}

#[derive(Debug, Clone)]
pub enum FunctionOrMethodParam {
    FunctionParam(FunctionParam),
    MethodParam(SelfParam),
}

#[derive(Debug, Clone)]
pub enum InherentImplItem {
    ConstantDecl(ConstantDecl),
    FunctionItem(FunctionItem),
}

#[derive(Debug, Clone)]
pub enum TraitItem {
    AliasDecl(AliasDecl),
    ConstantDecl(ConstantDecl),
    FunctionItem(FunctionItem),
}

#[derive(Debug, Clone)]
pub enum Visibility {
    Private,                   // default
    PubPackage(PubPackageVis), // `pub(package)`
    Pub,                       // `pub`
}

#[derive(Debug, Clone)]
pub struct EnumVariantStruct {
    pub open_brace: Delimiter,
    pub fields_opt: Option<Vec<(Identifier, Type)>>,
    pub close_brace: Delimiter,
}

#[derive(Debug, Clone)]
pub struct EnumVariantTuple {
    pub open_paren: Delimiter,
    pub element_types: Vec<Type>,
    pub close_paren: Delimiter,
}

#[derive(Debug, Clone)]
pub struct EnumVariant {
    pub attributes_opt: Option<Vec<OuterAttr>>,
    pub variant_name: Identifier,
    pub variant_type_opt: Option<EnumVariantType>,
}

#[derive(Debug, Clone)]
pub struct FunctionParam {
    pub param: Identifier,
    pub param_type: Box<Type>,
}

#[derive(Debug, Clone)]
pub struct ImportTree {
    pub segments: Vec<PathSegment>,
}

#[derive(Debug, Clone)]
pub struct PathSegment {
    pub root: PathExpr,
    pub subset_opt: Option<PathSubset>,
}

#[derive(Debug, Clone)]
pub struct PathSubset {
    pub open_brace: Delimiter,
    pub trees: Vec<ImportTree>,
    pub close_brace: Delimiter,
}

#[derive(Debug, Clone)]
pub struct PubPackageVis {
    pub kw_pub: Keyword,
    pub open_paren: Delimiter,
    pub kw_package: Keyword,
    pub close_paren: Delimiter,
}

#[derive(Debug, Clone)]
pub struct SelfParam {
    pub prefix_opt: Option<UnaryOp>,
    pub kw_self: Keyword,
}

#[derive(Debug, Clone)]
pub struct StructDefField {
    pub attributes_opt: Option<Vec<OuterAttr>>,
    pub visibility: Visibility,
    pub field_name: Identifier,
    pub field_type: Box<Type>,
}

#[derive(Debug, Clone)]
pub struct TupleStructDefField {
    pub visibility: Visibility,
    pub field_type: Box<Type>,
}

///////////////////////////////////////////////////////////////////////////
/// NODES
///////////////////////////////////////////////////////////////////////////

#[derive(Debug, Clone)]
pub struct AliasDecl {
    pub attributes_opt: Option<Vec<OuterAttr>>,
    pub visibility: Visibility,
    pub kw_alias: Keyword,
    pub alias_name: Identifier,
    pub original_type_opt: Option<Type>,
    pub semicolon: Separator,
}

#[derive(Debug, Clone)]
pub struct ConstantDecl {
    pub attributes_opt: Option<Vec<OuterAttr>>,
    pub visibility: Visibility,
    pub kw_const: Keyword,
    pub item_name: Identifier,
    pub item_type: Box<Type>,
    pub value_opt: Option<Box<Expression>>,
}

#[derive(Debug, Clone)]
pub struct EnumDef {
    pub attributes_opt: Option<Vec<OuterAttr>>,
    pub visibility: Visibility,
    pub kw_enum: Keyword,
    pub enum_name: Identifier,
    pub open_brace: Delimiter,
    pub variants: Vec<EnumVariant>,
    pub close_brace: Delimiter,
}

#[derive(Debug, Clone)]
pub struct FunctionItem {
    pub attributes_opt: Option<Vec<OuterAttr>>,
    pub visibility: Visibility,
    pub kw_func: Keyword,
    pub function_name: Identifier,
    pub open_paren: Delimiter,
    pub params_opt: Option<Vec<FunctionOrMethodParam>>,
    pub close_paren: Delimiter,
    pub return_type_opt: Option<Box<Type>>,
    pub block_opt: Option<BlockExpr>,
}

#[derive(Debug, Clone)]
pub struct ImportDecl {
    pub attributes_opt: Option<Vec<OuterAttr>>,
    pub visibility: Visibility,
    pub kw_import: Keyword,
    pub tree: ImportTree,
}

#[derive(Debug, Clone)]
pub struct InherentImplDef {
    pub attributes_opt: Option<Vec<OuterAttr>>,
    pub kw_impl: Keyword,
    pub nominal_type: Type,
    pub open_brace: Delimiter,
    pub associated_items_opt: Option<Vec<InherentImplItem>>,
    pub close_brace: Delimiter,
}

#[derive(Debug, Clone)]
pub struct ModuleItem {
    pub attributes_opt: Option<Vec<InnerAttr>>,
    pub visibility: Visibility,
    pub kw_mod: Keyword,
    pub module_name: Identifier,
    pub open_brace_opt: Option<Delimiter>,
    pub items_opt: Option<Vec<Item>>,
    pub close_brace_opt: Option<Delimiter>,
}

#[derive(Debug, Clone)]
pub struct StaticItemDecl {
    pub attributes_opt: Option<Vec<OuterAttr>>,
    pub visibility: Visibility,
    pub kw_static: Keyword,
    pub kw_mut_opt: Option<Keyword>,
    pub item_name: Identifier,
    pub item_type: Type,
    pub value_opt: Option<Box<Expression>>,
}

#[derive(Debug, Clone)]
pub struct StructDef {
    pub attributes_opt: Option<Vec<OuterAttr>>,
    pub visibility: Visibility,
    pub kw_struct: Keyword,
    pub struct_name: Identifier,
    pub open_brace: Delimiter,
    pub fields: Vec<StructDefField>,
    pub close_brace: Delimiter,
}

#[derive(Debug, Clone)]
pub struct TupleStructDef {
    pub attributes_opt: Option<Vec<OuterAttr>>,
    pub visibility: Visibility,
    pub kw_struct: Keyword,
    pub struct_name: Identifier,
    pub open_paren: Delimiter,
    pub fields_opt: Option<Vec<TupleStructDefField>>,
    pub close_paren: Delimiter,
    pub semicolon: Separator,
}

#[derive(Debug, Clone)]
pub struct TraitDef {
    pub attributes_opt: Option<Vec<OuterAttr>>,
    pub visibility: Visibility,
    pub kw_trait: Keyword,
    pub trait_name: Identifier,
    pub open_brace: Delimiter,
    pub associated_items_opt: Option<Vec<TraitItem>>,
    pub close_brace: Delimiter,
}

#[derive(Debug, Clone)]
pub struct TraitImplDef {
    pub attributes_opt: Option<Vec<OuterAttr>>,
    pub kw_impl: Keyword,
    pub implemented_trait_path: PathExpr,
    pub kw_for: Keyword,
    pub implementing_type: Type,
    pub open_brace_opt: Option<Delimiter>,
    pub associated_items_opt: Option<Vec<TraitItem>>,
    pub close_brace_opt: Option<Delimiter>,
}
