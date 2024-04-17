use crate::{
    ast::{EnumDef, FunctionDef, InherentImplDef, ModuleDef, StructDef, TraitDef, TraitImplDef},
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
