use crate::{ast::ConstantDecl, error::ErrorsEmitted};

use super::{item::ParseDeclaration, Parser};

impl ParseDeclaration for ConstantDecl {
    fn parse(parser: &mut Parser) -> Result<ConstantDecl, ErrorsEmitted> {
        todo!()
    }
}
