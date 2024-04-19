use crate::{
    ast::{FunctionItem, InnerAttr, Visibility},
    error::ErrorsEmitted,
};

use super::Parser;

impl FunctionItem {
    pub(crate) fn parse(
        parser: &mut Parser,
        attributes: Vec<InnerAttr>,
        visibility: Visibility,
    ) -> Result<FunctionItem, ErrorsEmitted> {
        todo!()
    }
}
