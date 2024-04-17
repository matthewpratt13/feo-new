///////////////////////////////////////////////////////////////////////////
/// NODES
///////////////////////////////////////////////////////////////////////////
use super::{Expression, Identifier, Keyword, Separator, Type};

#[derive(Debug, Clone)]
pub struct AliasDecl {
    // pub attributes_opt: Option<Vec<VariableAttr>>,
    // pub visibility_opt: Option<Visibility>,
    pub kw_alias: Keyword,
    pub alias_name: Identifier,
    pub type_ann_opt: Option<(Separator, Type)>, // `= Type`
    pub semicolon: Separator,
}

#[derive(Debug, Clone)]
pub struct ConstantDecl {
    // pub attributes_opt: Option<Vec<VariableAttr>>,
    // pub visibility_opt: Option<Visibility>,
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
pub struct ImportDecl {}

#[derive(Debug, Clone)]
pub struct InherentImplDef {}

#[derive(Debug, Clone)]
pub struct ModuleDef {}

#[derive(Debug, Clone)]
pub struct StaticItemDecl {
    // pub attributes_opt: Option<Vec<VariableAttr>>,
    // pub visibility_opt: Option<Visibility>,
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
