use crate::{
    ast::{
        AssigneeExpr, BinaryExpr, BinaryOp, ComparisonExpr, ComparisonOp, Expression, ValueExpr,
    },
    error::ErrorsEmitted,
    token::{Token, TokenType},
};

use super::Parser;

/// Parse a binary operation (e.g., arithmetic, logical and comparison expressions).
/// This method parses the operator and calls `parse_expression()` recursively to handle
/// the right-hand side of the expression.
impl BinaryExpr {
    pub(crate) fn parse(
        parser: &mut Parser,
        left_expr: Expression,
    ) -> Result<Expression, ErrorsEmitted> {
        let operator_token = parser.peek_current().unwrap_or(Token::EOF);

        let binary_op = match operator_token.token_type() {
            TokenType::Plus => Ok(BinaryOp::Add),
            TokenType::Minus => Ok(BinaryOp::Subtract),
            TokenType::Asterisk => Ok(BinaryOp::Multiply),
            TokenType::Slash => Ok(BinaryOp::Divide),
            TokenType::Percent => Ok(BinaryOp::Modulus),
            TokenType::DblAmpersand => Ok(BinaryOp::LogicalAnd),
            TokenType::DblPipe => Ok(BinaryOp::LogicalOr),
            TokenType::Ampersand => Ok(BinaryOp::BitwiseAnd),
            TokenType::Pipe => Ok(BinaryOp::BitwiseOr),
            TokenType::Caret => Ok(BinaryOp::BitwiseXor),
            TokenType::DblLessThan => Ok(BinaryOp::ShiftLeft),
            TokenType::DblGreaterThan => Ok(BinaryOp::ShiftRight),
            TokenType::DblAsterisk => Ok(BinaryOp::Exponentiation),
            _ => {
                parser.log_unexpected_str("binary operator");
                Err(ErrorsEmitted)
            }
        }?;

        parser.consume_token();

        let precedence = parser.get_precedence(&operator_token);

        let right_expr = parser.parse_expression(precedence)?;

        let lhs = ValueExpr::try_from(left_expr).map_err(|e| {
            parser.log_error(e);
            ErrorsEmitted
        })?;

        let rhs = ValueExpr::try_from(right_expr).map_err(|e| {
            parser.log_error(e);
            ErrorsEmitted
        })?;

        let expr = BinaryExpr {
            lhs: Box::new(lhs),
            binary_op,
            rhs: Box::new(rhs),
        };

        Ok(Expression::Binary(expr))
    }
}

impl ComparisonExpr {
    pub(crate) fn parse(
        parser: &mut Parser,
        left_expr: Expression,
    ) -> Result<Expression, ErrorsEmitted> {
        let operator_token = parser.peek_current().unwrap_or(Token::EOF);

        let comparison_op = match operator_token.token_type() {
            TokenType::DblEquals => Ok(ComparisonOp::Equal),
            TokenType::LessThan => Ok(ComparisonOp::LessThan),
            TokenType::GreaterThan => Ok(ComparisonOp::GreaterThan),
            TokenType::LessThanEquals => Ok(ComparisonOp::LessEqual),
            TokenType::GreaterThanEquals => Ok(ComparisonOp::GreaterEqual),

            _ => {
                parser.log_unexpected_str("comparison operator");
                Err(ErrorsEmitted)
            }
        }?;

        parser.consume_token();

        let precedence = parser.get_precedence(&operator_token);

        let right_expr = parser.parse_expression(precedence)?;

        let lhs = AssigneeExpr::try_from(left_expr).map_err(|e| {
            parser.log_error(e);
            ErrorsEmitted
        })?;

        let rhs = AssigneeExpr::try_from(right_expr).map_err(|e| {
            parser.log_error(e);
            ErrorsEmitted
        })?;

        let expr = ComparisonExpr {
            lhs,
            comparison_op,
            rhs,
        };

        Ok(Expression::Comparison(expr))
    }

    // pub(crate) fn parse(
    //     parser: &mut Parser,
    //     left_expr: Expression,
    //     op: ComparisonOp,
    // ) -> Result<ComparisonExpr, ErrorsEmitted> {
    //     println!("ENTER `parse_binary_expr()`");
    //     println!("CURRENT TOKEN: {:?}\n", parser.peek_current());

    //     let left_expr = AssigneeExpr::try_from(left_expr).map_err(|e| {
    //         parser.log_error(e);
    //         ErrorsEmitted
    //     })?;

    //     match op {
    //         ComparisonOp::Equal => {
    //             let right_expr = parser.parse_expression(Precedence::Equal)?;

    //             let value_expr = AssigneeExpr::try_from(right_expr).map_err(|e| {
    //                 parser.log_error(e);
    //                 ErrorsEmitted
    //             })?;
    //             Ok(ComparisonExpr {
    //                 lhs: left_expr,
    //                 op,
    //                 rhs: value_expr,
    //             })
    //         }
    //         ComparisonOp::NotEqual => {
    //             let right_expr = parser.parse_expression(Precedence::NotEqual)?;

    //             let value_expr = AssigneeExpr::try_from(right_expr).map_err(|e| {
    //                 parser.log_error(e);
    //                 ErrorsEmitted
    //             })?;
    //             Ok(ComparisonExpr {
    //                 lhs: left_expr,
    //                 op,
    //                 rhs: value_expr,
    //             })
    //         }
    //         ComparisonOp::LessThan => {
    //             let right_expr = parser.parse_expression(Precedence::LessThan)?;

    //             let value_expr = AssigneeExpr::try_from(right_expr).map_err(|e| {
    //                 parser.log_error(e);
    //                 ErrorsEmitted
    //             })?;
    //             Ok(ComparisonExpr {
    //                 lhs: left_expr,
    //                 op,
    //                 rhs: value_expr,
    //             })
    //         }
    //         ComparisonOp::LessEqual => {
    //             let right_expr = parser.parse_expression(Precedence::LessThanOrEqual)?;

    //             let value_expr = AssigneeExpr::try_from(right_expr).map_err(|e| {
    //                 parser.log_error(e);
    //                 ErrorsEmitted
    //             })?;
    //             Ok(ComparisonExpr {
    //                 lhs: left_expr,
    //                 op,
    //                 rhs: value_expr,
    //             })
    //         }
    //         ComparisonOp::GreaterThan => {
    //             let right_expr = parser.parse_expression(Precedence::GreaterThan)?;

    //             let value_expr = AssigneeExpr::try_from(right_expr).map_err(|e| {
    //                 parser.log_error(e);
    //                 ErrorsEmitted
    //             })?;
    //             Ok(ComparisonExpr {
    //                 lhs: left_expr,
    //                 op,
    //                 rhs: value_expr,
    //             })
    //         }
    //         ComparisonOp::GreaterEqual => {
    //             let right_expr = parser.parse_expression(Precedence::GreaterThanOrEqual)?;

    //             let value_expr = AssigneeExpr::try_from(right_expr).map_err(|e| {
    //                 parser.log_error(e);
    //                 ErrorsEmitted
    //             })?;
    //             Ok(ComparisonExpr {
    //                 lhs: left_expr,
    //                 op,
    //                 rhs: value_expr,
    //             })
    //         }
    //     }
    // }
}

#[cfg(test)]
mod tests {
    use crate::parser::test_utils;

    #[test]
    fn parse_binary_expr_add() -> Result<(), ()> {
        let input = r#"1 + 2"#;

        let mut parser = test_utils::get_parser(input, false);

        let expressions = parser.parse();

        match expressions {
            Ok(t) => Ok(println!("{:#?}", t)),
            Err(_) => Err(println!("{:#?}", parser.errors())),
        }
    }

    #[test]
    fn parse_binary_expr_multiply() -> Result<(), ()> {
        let input = r#"x * 2"#;

        let mut parser = test_utils::get_parser(input, false);

        let expressions = parser.parse();

        match expressions {
            Ok(t) => Ok(println!("{:#?}", t)),
            Err(_) => Err(println!("{:#?}", parser.errors())),
        }
    }

    #[test]
    fn parse_binary_expr_logical_and() -> Result<(), ()> {
        let input = r#"x && y"#;

        let mut parser = test_utils::get_parser(input, false);

        let expressions = parser.parse();

        match expressions {
            Ok(t) => Ok(println!("{:#?}", t)),
            Err(_) => Err(println!("{:#?}", parser.errors())),
        }
    }

    #[test]
    fn parse_binary_expr_bitwise_xor() -> Result<(), ()> {
        let input = r#"x ^ y"#;

        let mut parser = test_utils::get_parser(input, false);

        let expressions = parser.parse();

        match expressions {
            Ok(t) => Ok(println!("{:#?}", t)),
            Err(_) => Err(println!("{:#?}", parser.errors())),
        }
    }

    #[test]
    fn parse_binary_expr_shift_left() -> Result<(), ()> {
        let input = r#"2 << 4"#;

        let mut parser = test_utils::get_parser(input, false);

        let expressions = parser.parse();

        match expressions {
            Ok(t) => Ok(println!("{:#?}", t)),
            Err(_) => Err(println!("{:#?}", parser.errors())),
        }
    }

    #[test]
    fn parse_binary_expr_exponent() -> Result<(), ()> {
        let input = r#"x**2"#;

        let mut parser = test_utils::get_parser(input, false);

        let expressions = parser.parse();

        match expressions {
            Ok(t) => Ok(println!("{:#?}", t)),
            Err(_) => Err(println!("{:#?}", parser.errors())),
        }
    }

    #[test]
    fn parse_comparison_less_than() -> Result<(), ()> {
        let input = r#"4 < 5"#;

        let mut parser = test_utils::get_parser(input, false);

        let expressions = parser.parse();

        match expressions {
            Ok(t) => Ok(println!("{:#?}", t)),
            Err(_) => Err(println!("{:#?}", parser.errors())),
        }
    }
}
