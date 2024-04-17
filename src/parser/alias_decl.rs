use crate::{ast::AliasDecl, error::ErrorsEmitted};

use super::{item::ParseDeclaration, Parser};

impl ParseDeclaration for AliasDecl {
    fn parse(parser: &mut Parser) -> Result<AliasDecl, ErrorsEmitted> {
        todo!()
    }
}
