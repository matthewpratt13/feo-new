use crate::{ast::{Declaration, Definition}, error::ErrorsEmitted};

use super::Parser;

pub(crate) trait ParseDeclaration {
    fn parse(parser: &mut Parser) -> Result<Declaration, ErrorsEmitted>;
}
pub(crate) trait ParseDefinition {
    fn parse(parser: &mut Parser) -> Result<Definition, ErrorsEmitted>;
}
