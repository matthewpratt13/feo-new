use crate::{
    ast::{OuterAttr, Visibility},
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
