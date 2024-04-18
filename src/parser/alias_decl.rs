use crate::{ast::{AliasDecl, OuterAttr}, error::ErrorsEmitted};

use super::{item::ParseDeclaration, Parser};

impl ParseDeclaration for AliasDecl {
    fn parse(
        parser: &mut Parser,
        attributes: Vec<OuterAttr>,
    ) -> Result<AliasDecl, ErrorsEmitted> {
        todo!()
    }
}
