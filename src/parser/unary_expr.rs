use crate::{
    ast::{UnaryExpr, UnaryOp},
    error::ErrorsEmitted,
};

use super::{Parser, Precedence};

pub(crate) fn parse_unary_expr(
    parser: &mut Parser,
    op: UnaryOp,
) -> Result<UnaryExpr, ErrorsEmitted> {
    let expression = parser.parse_expression(Precedence::Unary)?;
    parser.consume_token()?;

    match op {
        _ => Ok(UnaryExpr {
            expression: Box::new(expression),
            op,
        }),
    }
}
