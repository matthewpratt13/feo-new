use super::{
    AssigneeExpr, BlockExpr, Identifier, IdentifierPatt, InnerAttr, Item, Keyword, OuterAttr,
    PathWildcard, ReferenceOp, SelfType, Type, TypePath, ValueExpr,
};

use crate::{
    parser::ty::build_item_path,
    span::{Span, Spanned},
};

use core::fmt;

///////////////////////////////////////////////////////////////////////////
// HELPER TYPES
///////////////////////////////////////////////////////////////////////////

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) enum EnumVariantType {
    Struct(EnumVariantStruct),
    TupleStruct(EnumVariantTupleStruct),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) enum FunctionOrMethodParam {
    FunctionParam(FunctionParam),
    MethodParam(SelfParam),
}

impl FunctionOrMethodParam {
    pub(crate) fn param_name(&self) -> Identifier {
        match self {
            FunctionOrMethodParam::FunctionParam(f) => f.param_name.name.clone(),
            FunctionOrMethodParam::MethodParam(_) => Identifier::from("self"),
        }
    }

    pub(crate) fn param_type(&self) -> Type {
        match self {
            FunctionOrMethodParam::FunctionParam(f) => *f.param_type.clone(),
            FunctionOrMethodParam::MethodParam(_) => Type::SelfType(SelfType),
        }
    }
}

impl fmt::Display for FunctionOrMethodParam {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            FunctionOrMethodParam::FunctionParam(t) => write!(f, "{t}"),
            FunctionOrMethodParam::MethodParam(t) => write!(f, "{t}"),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) enum InherentImplItem {
    ConstantDecl(ConstantDecl),
    FunctionItem(FunctionItem),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) enum TraitDefItem {
    AliasDecl(AliasDecl),
    ConstantDecl(ConstantDecl),
    FunctionItem(FunctionItem),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) enum TraitImplItem {
    AliasDecl(AliasDecl),
    ConstantDecl(ConstantDecl),
    FunctionItem(FunctionItem),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) enum Visibility {
    Private,           // default
    PubLib(PubLibVis), // `pub(lib)`
    Pub,               // accessible everywhere
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) struct EnumVariantStruct {
    pub(crate) struct_fields: Vec<StructDefField>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) struct EnumVariantTupleStruct {
    pub(crate) element_types: Vec<Type>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) struct EnumVariant {
    pub(crate) attributes_opt: Option<Vec<OuterAttr>>,
    pub(crate) visibility: Visibility,
    pub(crate) variant_name: Identifier,
    pub(crate) variant_type_opt: Option<EnumVariantType>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) struct FunctionParam {
    pub(crate) param_name: IdentifierPatt,
    pub(crate) param_type: Box<Type>,
}

impl fmt::Display for FunctionParam {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}: {}", self.param_name, self.param_type)
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) struct ImportTree {
    pub(crate) path_segments: Vec<PathSegment>,
    pub(crate) wildcard_opt: Option<PathWildcard>, // trailing `::*`
    pub(crate) as_clause_opt: Option<Identifier>,  // (`as`, `new_name`)
}

impl fmt::Display for ImportTree {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut paths: Vec<TypePath> = Vec::new();

        let mut segments = self.path_segments.clone();

        let mut import_root = if !segments.is_empty() {
            let mut paths = Vec::<TypePath>::from(segments.remove(0));

            if !paths.is_empty() {
                let mut path = paths.remove(0);

                for p in paths {
                    path = build_item_path(&path, p);
                }

                path
            } else {
                TypePath::from(Identifier::from(""))
            }
        } else {
            TypePath::from(Identifier::from(""))
        };

        let mut segment_counter: usize = 0;

        for seg in segments.clone() {
            segment_counter += 1;

            let path = build_item_path(&import_root, seg.root);

            if let Some(sub) = seg.subset_opt {
                import_root = path.clone();

                for t in sub.nested_trees {
                    for ps in t.path_segments {
                        for p in Vec::<TypePath>::from(ps) {
                            let path = build_item_path(&path.clone(), p);
                            paths.push(path);
                        }
                    }
                }
            } else {
                if segment_counter == segments.len() {
                    paths.push(path.clone());
                }

                if segment_counter < segments.len() {
                    import_root = path.clone();
                }
            }
        }

        let path_strings = paths
            .clone()
            .into_iter()
            .map(|p| p.to_string())
            .collect::<Vec<String>>();

        write!(f, "{:?}", path_strings)
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) struct PathSegment {
    pub(crate) root: TypePath, // e.g., `PathRoot::Identifier(_)`, `lib::module::Object`
    pub(crate) subset_opt: Option<PathSubset>, // e.g., `::{ Foo, Bar, .. }` (basic)
}

impl fmt::Display for PathSegment {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.root)
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) struct PathSubset {
    pub(crate) nested_trees: Vec<ImportTree>, // e.g., `::{ Foo, bar::baz{ FooBar, BAZ, .. }, .. }`
}

impl fmt::Display for PathSubset {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut trees_strings: Vec<String> = Vec::new();

        for t in self.clone().nested_trees {
            trees_strings.push(t.to_string())
        }

        let subset_path = if trees_strings.len() > 1 {
            trees_strings.join("::")
        } else {
            trees_strings
                .get(0)
                .expect("empty import path subset string vector")
                .clone()
        };

        write!(f, "::{{{}}}", subset_path)
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) struct PubLibVis {
    pub(crate) kw_pub: Keyword,
    pub(crate) kw_lib: Keyword,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) struct SelfParam {
    pub(crate) reference_op_opt: Option<ReferenceOp>,
    pub(crate) kw_self: Keyword,
}

impl fmt::Display for SelfParam {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:?}{}", self.reference_op_opt, self.kw_self)
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) struct StructDefField {
    pub(crate) attributes_opt: Option<Vec<OuterAttr>>,
    pub(crate) visibility: Visibility,
    pub(crate) field_name: Identifier,
    pub(crate) field_type: Box<Type>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) struct TupleStructDefField {
    pub(crate) attributes_opt: Option<Vec<OuterAttr>>,
    pub(crate) visibility: Visibility,
    pub(crate) field_type: Box<Type>,
}

///////////////////////////////////////////////////////////////////////////
// AST NODE STRUCTURES
///////////////////////////////////////////////////////////////////////////

#[derive(Clone, PartialEq, Eq)]
pub struct AliasDecl {
    pub(crate) attributes_opt: Option<Vec<OuterAttr>>,
    pub(crate) visibility: Visibility,
    pub(crate) kw_alias: Keyword,
    pub(crate) alias_name: Identifier,
    pub(crate) original_type_opt: Option<Type>,
    pub(crate) span: Span,
}

#[derive(Clone, PartialEq, Eq)]
pub struct ConstantDecl {
    pub(crate) attributes_opt: Option<Vec<OuterAttr>>,
    pub(crate) visibility: Visibility,
    pub(crate) kw_const: Keyword,
    pub(crate) constant_name: Identifier,
    pub(crate) constant_type: Box<Type>,
    pub(crate) value_opt: Option<ValueExpr>,
    pub(crate) span: Span,
}

#[derive(Clone, PartialEq, Eq)]
pub struct EnumDef {
    pub(crate) attributes_opt: Option<Vec<OuterAttr>>,
    pub(crate) visibility: Visibility,
    pub(crate) kw_enum: Keyword,
    pub(crate) enum_name: Identifier,
    pub(crate) variants: Vec<EnumVariant>,
    pub(crate) span: Span,
}

#[derive(Clone, PartialEq, Eq)]
pub struct FunctionItem {
    pub(crate) attributes_opt: Option<Vec<OuterAttr>>,
    pub(crate) visibility: Visibility,
    pub(crate) kw_func: Keyword,
    pub(crate) function_name: Identifier,
    pub(crate) params_opt: Option<Vec<FunctionOrMethodParam>>,
    pub(crate) return_type_opt: Option<Box<Type>>,
    pub(crate) block_opt: Option<BlockExpr>,
    pub(crate) span: Span,
}

#[derive(Clone, PartialEq, Eq)]
pub struct ImportDecl {
    pub(crate) attributes_opt: Option<Vec<OuterAttr>>,
    pub(crate) visibility: Visibility,
    pub(crate) kw_import: Keyword,
    pub(crate) import_tree: ImportTree,
    pub(crate) span: Span,
}

#[derive(Clone, PartialEq, Eq)]
pub struct InherentImplDef {
    pub(crate) attributes_opt: Option<Vec<OuterAttr>>,
    pub(crate) kw_impl: Keyword,
    pub(crate) nominal_type: TypePath,
    pub(crate) associated_items_opt: Option<Vec<InherentImplItem>>,
    pub(crate) span: Span,
}

#[derive(Clone, PartialEq, Eq)]
pub struct ModuleItem {
    pub(crate) outer_attributes_opt: Option<Vec<OuterAttr>>,
    pub(crate) visibility: Visibility,
    pub(crate) kw_module: Keyword,
    pub(crate) module_name: Identifier,
    pub(crate) inner_attributes_opt: Option<Vec<InnerAttr>>,
    pub(crate) items_opt: Option<Vec<Item>>,
    pub(crate) span: Span,
}

#[derive(Clone, PartialEq, Eq)]
pub struct StaticVarDecl {
    pub(crate) attributes_opt: Option<Vec<OuterAttr>>,
    pub(crate) visibility: Visibility,
    pub(crate) kw_static: Keyword,
    pub(crate) kw_mut_opt: Option<Keyword>,
    pub(crate) var_name: Identifier,
    pub(crate) var_type: Type,
    pub(crate) assignee_opt: Option<Box<AssigneeExpr>>,
    pub(crate) span: Span,
}

#[derive(Clone, PartialEq, Eq)]
pub struct StructDef {
    pub(crate) attributes_opt: Option<Vec<OuterAttr>>,
    pub(crate) visibility: Visibility,
    pub(crate) kw_struct: Keyword,
    pub(crate) struct_name: Identifier,
    pub(crate) fields_opt: Option<Vec<StructDefField>>,
    pub(crate) span: Span,
}

#[derive(Clone, PartialEq, Eq)]
pub struct TraitDef {
    pub(crate) outer_attributes_opt: Option<Vec<OuterAttr>>,
    pub(crate) visibility: Visibility,
    pub(crate) kw_trait: Keyword,
    pub(crate) trait_name: Identifier,
    pub(crate) inner_attributes_opt: Option<Vec<InnerAttr>>,
    pub(crate) trait_items_opt: Option<Vec<TraitDefItem>>,
    pub(crate) span: Span,
}

#[derive(Clone, PartialEq, Eq)]
pub struct TraitImplDef {
    pub(crate) attributes_opt: Option<Vec<OuterAttr>>,
    pub(crate) kw_impl: Keyword,
    pub(crate) implemented_trait_path: TypePath,
    pub(crate) kw_for: Keyword,
    pub(crate) implementing_type: Type,
    pub(crate) associated_items_opt: Option<Vec<TraitImplItem>>,
    pub(crate) span: Span,
}

#[derive(Clone, PartialEq, Eq)]
pub struct TupleStructDef {
    pub(crate) attributes_opt: Option<Vec<OuterAttr>>,
    pub(crate) visibility: Visibility,
    pub(crate) kw_struct: Keyword,
    pub(crate) struct_name: Identifier,
    pub(crate) fields_opt: Option<Vec<TupleStructDefField>>,
    pub(crate) span: Span,
}

impl Spanned for Item {
    fn span(&self) -> Span {
        match self.clone() {
            Item::ImportDecl(id) => id.span,
            Item::AliasDecl(ad) => ad.span,
            Item::ConstantDecl(cvd) => cvd.span,
            Item::StaticVarDecl(svd) => svd.span,
            Item::ModuleItem(mi) => mi.span,
            Item::TraitDef(td) => td.span,
            Item::EnumDef(ed) => ed.span,
            Item::StructDef(sd) => sd.span,
            Item::TupleStructDef(tsd) => tsd.span,
            Item::InherentImplDef(iid) => iid.span,
            Item::TraitImplDef(tid) => tid.span,
            Item::FunctionItem(fi) => fi.span,
        }
    }
}

impl fmt::Debug for Item {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::ImportDecl(arg0) => f
                .debug_struct("ImportDecl")
                .field("attributes_opt", &arg0.attributes_opt)
                .field("visibility", &arg0.visibility)
                .field("import_tree", &arg0.import_tree)
                .finish(),
            Self::AliasDecl(arg0) => f
                .debug_struct("AliasDecl")
                .field("attributes_opt", &arg0.attributes_opt)
                .field("visibility", &arg0.visibility)
                .field("alias_name", &arg0.alias_name)
                .field("original_type_opt", &arg0.original_type_opt)
                .finish(),
            Self::ConstantDecl(arg0) => f
                .debug_struct("ConstantDecl")
                .field("attributes_opt", &arg0.attributes_opt)
                .field("visibility", &arg0.visibility)
                .field("constant_name", &arg0.constant_name)
                .field("constant_type", &arg0.constant_type)
                .field("value_opt", &arg0.value_opt)
                .finish(),
            Self::StaticVarDecl(arg0) => f
                .debug_struct("StaticVarDecl")
                .field("attributes_opt", &arg0.attributes_opt)
                .field("visibility", &arg0.visibility)
                .field("kw_mut_opt", &arg0.kw_mut_opt)
                .field("var_name", &arg0.var_name)
                .field("var_type", &arg0.var_type)
                .field("assignee_opt", &arg0.assignee_opt)
                .finish(),
            Self::ModuleItem(arg0) => f
                .debug_struct("ModuleItem")
                .field("outer_attributes_opt", &arg0.outer_attributes_opt)
                .field("visibility", &arg0.visibility)
                .field("module_name", &arg0.module_name)
                .field("inner_attribute_opt", &arg0.inner_attributes_opt)
                .field("items_opt", &arg0.inner_attributes_opt)
                .finish(),
            Self::TraitDef(arg0) => f
                .debug_struct("TraitDef")
                .field("outer_attributes_opt", &arg0.outer_attributes_opt)
                .field("visibility", &arg0.visibility)
                .field("trait_name", &arg0.trait_name)
                .field("inner_attributes_opt", &arg0.inner_attributes_opt)
                .field("trait_items_opt", &arg0.trait_items_opt)
                .finish(),
            Self::EnumDef(arg0) => f
                .debug_struct("EnumDef")
                .field("attributes_opt", &arg0.attributes_opt)
                .field("visibility", &arg0.visibility)
                .field("enum_name", &arg0.enum_name)
                .field("variants", &arg0.variants)
                .finish(),
            Self::StructDef(arg0) => f
                .debug_struct("StructDef")
                .field("attributes_opt", &arg0.attributes_opt)
                .field("visibility", &arg0.visibility)
                .field("struct_name", &arg0.struct_name)
                .field("fields_opt", &arg0.fields_opt)
                .finish(),
            Self::TupleStructDef(arg0) => f
                .debug_struct("TupleStructDef")
                .field("attributes_opt", &arg0.attributes_opt)
                .field("visibility", &arg0.visibility)
                .field("struct_name", &arg0.struct_name)
                .field("elements_opt", &arg0.fields_opt)
                .finish(),
            Self::InherentImplDef(arg0) => f
                .debug_struct("InherentImplDef")
                .field("attributes_opt", &arg0.attributes_opt)
                .field("nominal_type", &arg0.nominal_type)
                .field("associated_items_opt", &arg0.associated_items_opt)
                .finish(),
            Self::TraitImplDef(arg0) => f
                .debug_struct("TraitImplDef")
                .field("attributes_opt", &arg0.attributes_opt)
                .field("implemented_trait_path", &arg0.implemented_trait_path)
                .field("implementing_type", &arg0.implementing_type)
                .field("associated_items_opt", &arg0.associated_items_opt)
                .finish(),
            Self::FunctionItem(arg0) => f
                .debug_struct("FunctionItem")
                .field("attributes_opt", &arg0.attributes_opt)
                .field("visibility", &arg0.visibility)
                .field("function_name", &arg0.function_name)
                .field("params_opt", &arg0.params_opt)
                .field("return_type_opt", &arg0.return_type_opt)
                .field("block_opt", &arg0.block_opt)
                .finish(),
        }
    }
}
