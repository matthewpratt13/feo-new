use crate::{
    ast::{
        AssigneeExpr, BorrowExpr, DereferenceExpr, DereferenceOp, Expression, ReferenceOp,
        UnaryExpr, UnaryOp, ValueExpr,
    },
    error::ErrorsEmitted,
};

use super::{Parser, Precedence};

impl UnaryExpr {
    pub(crate) fn parse(
        parser: &mut Parser,
        unary_op: UnaryOp,
    ) -> Result<Expression, ErrorsEmitted> {
        parser.next_token();

        let operand = parser.parse_expression(Precedence::Unary)?;

        let value_expr = ValueExpr::try_from(operand).map_err(|e| {
            parser.log_error(e);
            ErrorsEmitted
        })?;

        let expr = UnaryExpr {
            unary_op,
            value_expr: Box::new(value_expr),
        };

        Ok(Expression::Unary(expr))
    }
}

impl BorrowExpr {
    pub(crate) fn parse(
        parser: &mut Parser,
        reference_op: ReferenceOp,
    ) -> Result<Expression, ErrorsEmitted> {
        parser.next_token();

        let operand = parser.parse_expression(Precedence::Unary)?;

        let expr = BorrowExpr {
            reference_op,
            expression: Box::new(operand),
        };

        Ok(Expression::Borrow(expr))
    }
}

impl DereferenceExpr {
    pub(crate) fn parse(
        parser: &mut Parser,
        dereference_op: DereferenceOp,
    ) -> Result<Expression, ErrorsEmitted> {
        parser.next_token();

        let operand = parser.parse_expression(Precedence::Unary)?;

        let assignee_expr = AssigneeExpr::try_from(operand).map_err(|e| {
            parser.log_error(e);
            ErrorsEmitted
        })?;

        let expr = DereferenceExpr {
            dereference_op,
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
