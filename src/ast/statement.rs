use super::{
    AssigneeExpr, Expression, Identifier, IdentifierPatt, InferredType, Item, Keyword, NoneExpr,
    Statement, Type, UnitType, ValueExpr,
};

use crate::span::{Span, Spanned};

use core::fmt;

///////////////////////////////////////////////////////////////////////////
// AST NODE STRUCTURES
///////////////////////////////////////////////////////////////////////////

#[derive(Clone, PartialEq, Eq)]
pub struct LetStmt {
    pub(crate) kw_let: Keyword,
    pub(crate) assignee: IdentifierPatt,
    pub(crate) type_ann_opt: Option<Type>,
    pub(crate) value_opt: Option<Expression>,
    pub(crate) span: Span,
}

impl Spanned for Statement {
    fn span(&self) -> Span {
        match self.clone() {
            Statement::Let(l) => l.span,
            Statement::Item(i) => i.span(),
            Statement::Expression(e) => e.span(),
        }
    }
}

impl fmt::Display for Statement {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self.clone() {
            Statement::Let(l) => write!(
                f,
                "let {}: {} = {};",
                l.assignee.name,
                l.type_ann_opt
                    .clone()
                    .unwrap_or(Type::InferredType(InferredType {
                        name: Identifier::from("")
                    }))
                    .clone(),
                l.value_opt.unwrap_or(Expression::NoneExpr(NoneExpr {
                    kw_none: Keyword::None,
                    span: l.span
                }))
            ),
            Statement::Item(it) => match it {
                Item::ImportDecl(im) => {
                    write!(f, "{}import {};", im.visibility, im.import_tree)
                }
                Item::AliasDecl(ad) => write!(
                    f,
                    "{}alias {} = {};",
                    ad.visibility,
                    ad.alias_name,
                    ad.original_type_opt.unwrap_or(Type::UnitType(UnitType))
                ),
                Item::ConstantDecl(cd) => write!(
                    f,
                    "{}const {}: {} = {};",
                    cd.visibility,
                    cd.constant_name,
                    *cd.constant_type,
                    cd.value_opt.unwrap_or(ValueExpr::NoneExpr(NoneExpr {
                        kw_none: Keyword::None,
                        span: cd.span
                    }))
                ),
                Item::StaticVarDecl(svd) => write!(
                    f,
                    "{}static {}: {} = {};",
                    svd.visibility,
                    svd.var_name,
                    svd.var_type,
                    svd.assignee_opt
                        .unwrap_or(Box::new(AssigneeExpr::TupleExpr {
                            elements: Vec::new(),
                            span: svd.span
                        }))
                ),
                Item::ModuleItem(m) => {
                    write!(
                        f,
                        "{}module {} {{ #![{:?}] {:?} }}",
                        m.visibility,
                        m.module_name,
                        m.inner_attributes_opt.unwrap_or(Vec::new()),
                        m.items_opt.unwrap_or(Vec::new())
                    )
                }
                Item::TraitDef(td) => write!(
                    f,
                    "{}trait {} {{ #![{:?}] {:?} }}",
                    td.visibility,
                    td.trait_name,
                    td.inner_attributes_opt.unwrap_or(Vec::new()),
                    td.trait_items_opt.unwrap_or(Vec::new())
                ),
                Item::EnumDef(ed) => write!(
                    f,
                    "{}enum {} {{ {:?} }}",
                    ed.visibility, ed.enum_name, ed.variants
                ),
                Item::StructDef(sd) => write!(
                    f,
                    "{}struct {} {{ {:?} }}",
                    sd.visibility,
                    sd.struct_name,
                    sd.fields_opt.unwrap_or(Vec::new())
                ),
                Item::TupleStructDef(tsd) => write!(
                    f,
                    "{}struct {} {{ {:?} }}",
                    tsd.visibility,
                    tsd.struct_name,
                    tsd.elements_opt.unwrap_or(Vec::new())
                ),
                Item::InherentImplDef(iid) => write!(
                    f,
                    "impl {} {{ {:?} }}",
                    iid.nominal_type,
                    iid.associated_items_opt.unwrap_or(Vec::new())
                ),
                Item::TraitImplDef(tid) => write!(
                    f,
                    "impl {} for {} {{ {:?} }}",
                    tid.implemented_trait_path,
                    tid.implementing_type,
                    tid.associated_items_opt.unwrap_or(Vec::new())
                ),
                Item::FunctionItem(fi) => write!(
                    f,
                    "{}func {}({:?}) -> {} {{ {:?} }}",
                    fi.visibility,
                    fi.function_name,
                    fi.params_opt.unwrap_or(Vec::new()),
                    fi.return_type_opt
                        .unwrap_or(Box::new(Type::UnitType(UnitType))),
                    fi.block_opt
                ),
            },
            Statement::Expression(e) => write!(f, "{}", e),
        }
    }
}
