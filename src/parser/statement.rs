use crate::{ast::Statement, error::ErrorsEmitted};

use super::Parser;

pub trait ParseStatement {
    fn parse(parser: &mut Parser) -> Result<Statement, ErrorsEmitted>;
}
