use crate::{
    ast::{InherentImplDef, OuterAttr, TraitImplDef, Visibility},
    error::ErrorsEmitted,
};

use super::{ParseDefinition, Parser};

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
