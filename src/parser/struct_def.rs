use crate::{
    ast::{OuterAttr, StructDef, Visibility},
    error::ErrorsEmitted,
};

use super::{item::ParseDefinition, Parser};

impl ParseDefinition for StructDef {
    fn parse(
        parser: &mut Parser,
        attributes: Vec<OuterAttr>,
        visibility: Visibility,
    ) -> Result<StructDef, ErrorsEmitted> {
        todo!()
    }
}
