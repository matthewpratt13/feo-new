use crate::{
    ast::{
        EnumDef, FunctionDef, InherentImplDef, InnerAttr, ModuleDef, OuterAttr, StructDef,
        TraitDef, TraitImplDef, Visibility,
    },
    error::ErrorsEmitted,
};

use super::Parser;

pub(crate) trait ParseDeclaration
where
    Self: Sized,
{
    fn parse(
        parser: &mut Parser,
        attributes: Vec<OuterAttr>,
        visibility: Visibility,
    ) -> Result<Self, ErrorsEmitted>;
}

pub(crate) trait ParseDefinition
where
    Self: Sized,
{
    fn parse(
        parser: &mut Parser,
        attributes: Vec<OuterAttr>,
        visibility: Visibility,
    ) -> Result<Self, ErrorsEmitted>;
}

impl ModuleDef {
    pub(crate) fn parse(
        parser: &mut Parser,
        attributes: Vec<InnerAttr>,
        visibility: Visibility,
    ) -> Result<ModuleDef, ErrorsEmitted> {
        todo!()
    }
}

impl ParseDefinition for TraitDef {
    fn parse(
        parser: &mut Parser,
        attributes: Vec<OuterAttr>,
        visibility: Visibility,
    ) -> Result<TraitDef, ErrorsEmitted> {
        todo!()
    }
}

impl ParseDefinition for EnumDef {
    fn parse(
        parser: &mut Parser,
        attributes: Vec<OuterAttr>,
        visibility: Visibility,
    ) -> Result<EnumDef, ErrorsEmitted> {
        todo!()
    }
}

impl ParseDefinition for StructDef {
    fn parse(
        parser: &mut Parser,
        attributes: Vec<OuterAttr>,
        visibility: Visibility,
    ) -> Result<StructDef, ErrorsEmitted> {
        todo!()
    }
}

impl ParseDefinition for InherentImplDef {
    fn parse(
        parser: &mut Parser,
        attributes: Vec<OuterAttr>,
        visibility: Visibility,
    ) -> Result<InherentImplDef, ErrorsEmitted> {
        todo!()
    }
}

impl ParseDefinition for TraitImplDef {
    fn parse(
        parser: &mut Parser,
        attributes: Vec<OuterAttr>,
        visibility: Visibility,
    ) -> Result<TraitImplDef, ErrorsEmitted> {
        todo!()
    }
}

impl ParseDefinition for FunctionDef {
    fn parse(
        parser: &mut Parser,
        attributes: Vec<OuterAttr>,
        visibility: Visibility,
    ) -> Result<FunctionDef, ErrorsEmitted> {
        todo!()
    }
}
