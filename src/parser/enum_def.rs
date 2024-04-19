use crate::{
    ast::{EnumDef, OuterAttr, Visibility},
    error::ErrorsEmitted,
};

use super::{item::ParseDefinition, Parser};

impl ParseDefinition for EnumDef {
    fn parse(
        parser: &mut Parser,
        attributes: Vec<OuterAttr>,
        visibility: Visibility,
    ) -> Result<EnumDef, ErrorsEmitted> {
        todo!()
    }
}
