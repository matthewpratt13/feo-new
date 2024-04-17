use crate::{ast::StaticItemDecl, error::ErrorsEmitted};

use super::{item::ParseDeclaration, Parser};

impl ParseDeclaration for StaticItemDecl {
    fn parse(parser: &mut Parser) -> Result<StaticItemDecl, ErrorsEmitted> {
        todo!()
    }
}
