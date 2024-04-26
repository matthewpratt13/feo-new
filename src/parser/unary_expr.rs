use crate::{
    ast::{
        AssigneeExpr, BorrowExpr, DereferenceExpr, DereferenceOp, Expression, ReferenceOp,
        UnaryExpr, UnaryOp, ValueExpr,
    },
    error::ErrorsEmitted,
};

use super::{Parser, Precedence};

impl UnaryExpr {
    pub(crate) fn parse(parser: &mut Parser, op: UnaryOp) -> Result<Expression, ErrorsEmitted> {
        parser.consume_token();

        let expression = parser.parse_expression(Precedence::Unary)?;
        let value_expr = ValueExpr::try_from(expression).map_err(|e| {
            parser.log_error(e);
            ErrorsEmitted
        })?;

        let expr = UnaryExpr {
            op,
            expression: Box::new(value_expr),
        };

        Ok(Expression::Unary(expr))
    }
}

impl BorrowExpr {
    pub(crate) fn parse(parser: &mut Parser, op: ReferenceOp) -> Result<Expression, ErrorsEmitted> {
        parser.consume_token();

        let expression = parser.parse_expression(Precedence::Unary)?;

        let expr = BorrowExpr {
            op,
            expression: Box::new(expression),
        };

        Ok(Expression::Borrow(expr))
    }
}

impl DereferenceExpr {
    pub(crate) fn parse(parser: &mut Parser) -> Result<Expression, ErrorsEmitted> {
        parser.consume_token();

        let expression = AssigneeExpr::try_from(parser.parse_expression(Precedence::Unary)?)
            .map_err(|e| {
                parser.log_error(e);
                ErrorsEmitted
            })?;

        let expr = DereferenceExpr {
            op: DereferenceOp,
            expression,
        };

        Ok(Expression::Dereference(expr))
    }
}

#[cfg(test)]
mod tests {
    use crate::parser::test_utils;

    #[test]
    fn parse_unary_expr_negate() -> Result<(), ()> {
        let input = r#"-x"#;

        let mut parser = test_utils::get_parser(input, false);

        let expressions = parser.parse();

        match expressions {
            Ok(t) => Ok(println!("{:#?}", t)),
            Err(_) => Err(println!("{:#?}", parser.errors())),
        }
    }

    #[test]
    // #[ignore]
    fn parse_unary_expr_reference() -> Result<(), ()> {
        let input = r#"&x"#;

        let mut parser = test_utils::get_parser(input, false);

        let expressions = parser.parse();

        match expressions {
            Ok(t) => Ok(println!("{:#?}", t)),
            Err(_) => Err(println!("{:#?}", parser.errors())),
        }
    }
}
