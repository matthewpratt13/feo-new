use crate::{
    ast::{Expression, UnwrapExpr, UnwrapOp, ValueExpr},
    error::ErrorsEmitted,
};

use super::{parse::ParseOperation, Parser};

impl ParseOperation for UnwrapExpr {
    fn parse(parser: &mut Parser, left_expr: Expression) -> Result<Expression, ErrorsEmitted> {
        let operand = Box::new(ValueExpr::try_from(left_expr).map_err(|e| {
            parser.log_error(e);
            ErrorsEmitted
        })?);

        parser.next_token();

        let expr = UnwrapExpr {
            value_expr: operand,
            op: UnwrapOp,
        };

        Ok(Expression::Unwrap(expr))
    }
}

#[cfg(test)]
mod tests {
    use crate::{logger::LogLevel, parser::test_utils};

    #[test]
    fn parse_unwrap_expr() -> Result<(), ()> {
        let input = r#"foo?"#;

        let mut parser = test_utils::get_parser(input, LogLevel::Debug, false);

        let statements = parser.parse();

        match statements {
            Ok(t) => Ok(println!("{:#?}", t)),
            Err(_) => Err(println!("{:#?}", parser.logger.logs())),
        }
    }
}
