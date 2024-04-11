use crate::{
    ast::{
        AliasDecl, ConstantDecl, EnumDef, FunctionDef, ImportDecl, InherentImplDef, ModuleDef,
        StaticItemDecl, StructDef, TraitDef, TraitImplDef,
    },
    error::ErrorsEmitted,
};

use super::Parser;

pub(crate) trait ParseDeclaration
where
    Self: Sized,
{
    fn parse(parser: &mut Parser) -> Result<Self, ErrorsEmitted>;
}

pub(crate) trait ParseDefinition
where
    Self: Sized,
{
    fn parse(parser: &mut Parser) -> Result<Self, ErrorsEmitted>;
}

impl ParseDeclaration for ImportDecl {
    fn parse(parser: &mut Parser) -> Result<ImportDecl, ErrorsEmitted> {
        todo!()
    }
}

impl ParseDeclaration for AliasDecl {
    fn parse(parser: &mut Parser) -> Result<AliasDecl, ErrorsEmitted> {
        todo!()
    }
}

impl ParseDeclaration for ConstantDecl {
    fn parse(parser: &mut Parser) -> Result<ConstantDecl, ErrorsEmitted> {
        todo!()
    }
}

impl ParseDeclaration for StaticItemDecl {
    fn parse(parser: &mut Parser) -> Result<StaticItemDecl, ErrorsEmitted> {
        todo!()
    }
}

impl ParseDefinition for ModuleDef {
    fn parse(parser: &mut Parser) -> Result<ModuleDef, ErrorsEmitted> {
        todo!()
    }
}

impl ParseDefinition for TraitDef {
    fn parse(parser: &mut Parser) -> Result<TraitDef, ErrorsEmitted> {
        todo!()
    }
}

impl ParseDefinition for EnumDef {
    fn parse(parser: &mut Parser) -> Result<EnumDef, ErrorsEmitted> {
        todo!()
    }
}

impl ParseDefinition for StructDef {
    fn parse(parser: &mut Parser) -> Result<StructDef, ErrorsEmitted> {
        todo!()
    }
}

impl ParseDefinition for InherentImplDef {
    fn parse(parser: &mut Parser) -> Result<InherentImplDef, ErrorsEmitted> {
        todo!()
    }
}

impl ParseDefinition for TraitImplDef {
    fn parse(parser: &mut Parser) -> Result<TraitImplDef, ErrorsEmitted> {
        todo!()
    }
}

impl ParseDefinition for FunctionDef {
    fn parse(parser: &mut Parser) -> Result<FunctionDef, ErrorsEmitted> {
        todo!()
    }
}
