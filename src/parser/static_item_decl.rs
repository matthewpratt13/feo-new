use crate::{
    ast::{OuterAttr, StaticItemDecl},
    error::ErrorsEmitted,
};

use super::{item::ParseDeclaration, Parser};

impl ParseDeclaration for StaticItemDecl {
    fn parse(
        parser: &mut Parser,
        attributes: Vec<OuterAttr>,
    ) -> Result<StaticItemDecl, ErrorsEmitted> {
        todo!()
    }
}
