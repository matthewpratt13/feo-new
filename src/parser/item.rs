use crate::{ast::{Declaration, Definition}, error::ErrorsEmitted};

use super::Parser;

pub trait ParseDeclaration {
    fn parse(parser: &mut Parser) -> Result<Declaration, ErrorsEmitted>;
}
pub trait ParseDefinition {
    fn parse(parser: &mut Parser) -> Result<Definition, ErrorsEmitted>;
}
