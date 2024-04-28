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

        parser.consume_token();

        let expr = UnaryExpr {
            op,
            value_expr: Box::new(value_expr),
        };

        Ok(Expression::Unary(expr))
    }
}

impl BorrowExpr {
    pub(crate) fn parse(parser: &mut Parser, op: ReferenceOp) -> Result<Expression, ErrorsEmitted> {
        parser.consume_token();

        let expression = parser.parse_expression(Precedence::Unary)?;

        parser.consume_token();

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

        let expression = parser.parse_expression(Precedence::Unary)?;

        let assignee_expr = AssigneeExpr::try_from(expression).map_err(|e| {
            parser.log_error(e);
            ErrorsEmitted
        })?;

        parser.consume_token();

        let expr = DereferenceExpr {
            op: DereferenceOp,
            assignee_expr,
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

        let statements = parser.parse();

        match statements {
            Ok(t) => Ok(println!("{:#?}", t)),
            Err(_) => Err(println!("{:#?}", parser.errors())),
        }
    }

    #[test]
    // #[ignore]
    fn parse_borrow_expr() -> Result<(), ()> {
        let input = r#"&x"#;

        let mut parser = test_utils::get_parser(input, false);

        let statements = parser.parse();

        match statements {
            Ok(t) => Ok(println!("{:#?}", t)),
            Err(_) => Err(println!("{:#?}", parser.errors())),
        }
    }

    #[test]
    // #[ignore]
    fn parse_dereference_expr() -> Result<(), ()> {
        let input = r#"*x"#;

        let mut parser = test_utils::get_parser(input, false);

        let statements = parser.parse();

        match statements {
            Ok(t) => Ok(println!("{:#?}", t)),
            Err(_) => Err(println!("{:#?}", parser.errors())),
        }
    }
}
