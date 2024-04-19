use crate::{
    ast::{OuterAttr, TraitDef, Visibility},
    error::ErrorsEmitted,
};

use super::{item::ParseDefinition, Parser};

impl ParseDefinition for TraitDef {
    fn parse(
        parser: &mut Parser,
        attributes: Vec<OuterAttr>,
        visibility: Visibility,
    ) -> Result<TraitDef, ErrorsEmitted> {
        todo!()
    }
}
