use crate::{
    ast::{FunctionDef, OuterAttr, Visibility},
    error::ErrorsEmitted,
};

use super::{item::ParseDefinition, Parser};

impl ParseDefinition for FunctionDef {
    fn parse(
        parser: &mut Parser,
        attributes: Vec<OuterAttr>,
        visibility: Visibility,
    ) -> Result<FunctionDef, ErrorsEmitted> {
        todo!()
    }
}
