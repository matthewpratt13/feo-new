use crate::{ast::Expression, error::ErrorsEmitted};

use super::Parser;

pub trait ParseExpression {
    fn parse(parser: &mut Parser) -> Result<Expression, ErrorsEmitted>;
}
