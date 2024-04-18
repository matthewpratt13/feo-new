///////////////////////////////////////////////////////////////////////////
/// HELPER TYPES
///////////////////////////////////////////////////////////////////////////

#[derive(Debug, Clone)]
pub enum ImportTree {
    SimplePath(PathExpr),           // `package::module::Foo`
    PathSubset(PathSubset),         // `::{ Foo, Bar }`
    PathRecursive(Box<ImportTree>), // package::module::{ Foo, bar::{ Baz } }
}

#[derive(Debug, Clone)]
pub enum Visibility {
    Private,                   // default
    PubPackage(PubPackageVis), // `pub(package)`
    Pub,                       // `pub`
}

#[derive(Debug, Clone)]
pub struct PathSubset {
    pub dbl_colon: Separator,
    pub open_brace: Delimiter,
    pub trees: Vec<PathExpr>,
    pub close_brace: Delimiter,
}

#[derive(Debug, Clone)]
pub struct PubPackageVis {
    pub kw_pub: Keyword,
    pub open_paren: Delimiter,
    pub kw_package: Keyword,
    pub close_paren: Delimiter,
}

///////////////////////////////////////////////////////////////////////////
/// NODES
///////////////////////////////////////////////////////////////////////////
use super::{Delimiter, Expression, Identifier, Keyword, OuterAttr, PathExpr, Separator, Type};

#[derive(Debug, Clone)]
pub struct AliasDecl {
    pub attributes_opt: Option<Vec<OuterAttr>>,
    pub visibility: Visibility,
    pub kw_alias: Keyword,
    pub alias_name: Identifier,
    pub assignment_opt: Option<(Separator, Type)>, // `= Type`
    pub semicolon: Separator,
}

#[derive(Debug, Clone)]
pub struct ConstantDecl {
    pub attributes_opt: Option<Vec<OuterAttr>>,
    pub visibility: Visibility,
    pub kw_const: Keyword,
    pub item_name: Identifier,
    pub item_type: Type,
    pub assignment_opt: Option<Box<Expression>>,
    pub semicolon: Separator,
}

#[derive(Debug, Clone)]
pub struct EnumDef {}

#[derive(Debug, Clone)]
pub struct FunctionDef {}

#[derive(Debug, Clone)]
pub struct ImportDecl {
    pub attributes_opt: Option<Vec<OuterAttr>>,
    pub visibility: Visibility,
    pub kw_import: Keyword,
    pub import_trees: Vec<ImportTree>,
    pub semicolon: Separator,
}

#[derive(Debug, Clone)]
pub struct InherentImplDef {}

#[derive(Debug, Clone)]
pub struct ModuleDef {}

#[derive(Debug, Clone)]
pub struct StaticItemDecl {
    pub attributes_opt: Option<Vec<OuterAttr>>,
    pub visibility: Visibility,
    pub kw_static: Keyword,
    pub kw_mut_opt: Option<Keyword>,
    pub item_name: Identifier,
    pub item_type: Type,
    pub assignment_opt: Option<Box<Expression>>,
    pub semicolon: Separator,
}

#[derive(Debug, Clone)]
pub struct StructDef {}

#[derive(Debug, Clone)]
pub struct TraitDef {}

#[derive(Debug, Clone)]
pub struct TraitImplDef {}
