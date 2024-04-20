use crate::{
    ast::{InnerAttr, ModuleDef, Visibility},
    error::ErrorsEmitted,
};

use super::Parser;

impl ModuleDef {
    pub(crate) fn parse(
        parser: &mut Parser,
        attributes: Vec<InnerAttr>,
        visibility: Visibility,
    ) -> Result<ModuleDef, ErrorsEmitted> {
        todo!()
    }
}
