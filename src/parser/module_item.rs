use crate::{
    ast::{InnerAttr, ModuleItem, Visibility},
    error::ErrorsEmitted,
};

use super::Parser;

impl ModuleItem {
    pub(crate) fn parse(
        parser: &mut Parser,
        attributes: Vec<InnerAttr>,
        visibility: Visibility,
    ) -> Result<ModuleItem, ErrorsEmitted> {
        todo!()
    }
}
