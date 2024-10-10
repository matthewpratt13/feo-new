use core::fmt;

use crate::{
    ast::{
        AssigneeExpr, BinaryExpr, BinaryOp, ComparisonExpr, ComparisonOp, Expression, ValueExpr,
    },
    error::ErrorsEmitted,
    parser::{ParseOperatorExpr, Parser},
    span::Spanned,
    token::{Token, TokenType},
};

impl ParseOperatorExpr for BinaryExpr {
    /// Parse a binary operation (e.g., arithmetic, logical and comparison expressions).
    /// This method parses the operator and calls `parse_expression()` recursively to handle
    /// the right-hand side of the expression.
    fn parse(parser: &mut Parser, left_expr: Expression) -> Result<Expression, ErrorsEmitted> {
        let left_expr_span = &left_expr.span();

        let lhs: ValueExpr = left_expr.try_into().map_err(|e| {
            parser.emit_error(e);
            ErrorsEmitted
        })?;

        let operator_token = parser.current_token().cloned().unwrap_or(Token::EOF);

        let binary_op = match &operator_token.token_type() {
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
                parser
                    .emit_unexpected_token(&format!("binary arithmetic ({}, {}, {}, {}, {} or {}), logical ({}, {}) or bitwise ({}, {}, {}, {}, {}) operator", TokenType::Plus, TokenType::Minus, TokenType::Asterisk, TokenType::Slash, TokenType::Percent, TokenType::DblAsterisk, TokenType::DblAmpersand, TokenType::DblPipe, TokenType::Ampersand, TokenType::Pipe, TokenType::Caret, TokenType::DblLessThan, TokenType::DblGreaterThan));

                Err(ErrorsEmitted)
            }
        }?;

        let precedence = parser.get_precedence(&operator_token);

        parser.next_token();

        let rhs = parser.parse_value_expr(precedence)?;

        let span = parser.get_span(left_expr_span, &rhs.span());

        let expr = BinaryExpr {
            lhs: Box::new(lhs),
            binary_op,
            rhs: Box::new(rhs),
            span,
        };

        Ok(Expression::Binary(expr))
    }
}

impl fmt::Debug for BinaryExpr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("BinaryExpr")
            .field("lhs", &self.lhs)
            .field("binary_op", &self.binary_op)
            .field("rhs", &self.rhs)
            .finish()
    }
}

impl ParseOperatorExpr for ComparisonExpr {
    /// Parse a comparison operation (i.e., `==`, `!=`, `<`, `>`, `<=` and `>=`), based on
    /// the input operator.
    fn parse(parser: &mut Parser, left_expr: Expression) -> Result<Expression, ErrorsEmitted> {
        let left_expr_span = &left_expr.span();

        let lhs: AssigneeExpr = left_expr.try_into().map_err(|e| {
            parser.emit_error(e);
            ErrorsEmitted
        })?;

        let operator_token = parser.current_token().cloned().unwrap_or(Token::EOF);

        let comparison_op = match &operator_token.token_type() {
            TokenType::DblEquals => Ok(ComparisonOp::Equal),
            TokenType::BangEquals => Ok(ComparisonOp::NotEqual),
            TokenType::LessThan => Ok(ComparisonOp::LessThan),
            TokenType::GreaterThan => Ok(ComparisonOp::GreaterThan),
            TokenType::LessThanEquals => Ok(ComparisonOp::LessEqual),
            TokenType::GreaterThanEquals => Ok(ComparisonOp::GreaterEqual),
            _ => {
                parser.emit_unexpected_token(&format!(
                    "binary comparison operator ({}, {}, {}, {}, {} or {}",
                    TokenType::LessThan,
                    TokenType::GreaterThan,
                    TokenType::LessThanEquals,
                    TokenType::GreaterThanEquals,
                    TokenType::DblEquals,
                    TokenType::BangEquals
                ));
                Err(ErrorsEmitted)
            }
        }?;

        let precedence = parser.get_precedence(&operator_token);

        parser.next_token();

        let rhs = parser.parse_assignee_expr(precedence)?;

        let span = parser.get_span(left_expr_span, &rhs.span());

        let expr = ComparisonExpr {
            lhs,
            comparison_op,
            rhs,
            span,
        };

        Ok(Expression::Comparison(expr))
    }
}

impl fmt::Debug for ComparisonExpr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("ComparisonExpr")
            .field("lhs", &self.lhs)
            .field("comparison_op", &self.comparison_op)
            .field("rhs", &self.rhs)
            .finish()
    }
}

#[cfg(test)]
mod tests {
    use crate::{
        logger::LogLevel,
        parser::{test_utils, Precedence},
    };

    #[test]
    fn parse_binary_expr_add() -> Result<(), ()> {
        let input = r#"x + 5"#;

        let mut parser = test_utils::get_parser(input, LogLevel::Debug, false);

        let expression = parser.parse_expression(Precedence::Lowest);

        match expression {
            Ok(expr) => Ok(println!("{expr:#?}")),
            Err(_) => Err(println!("{:#?}", parser.errors)),
        }
    }

    // test for `Precedence::Difference` vs. `Precedence::Unary`
    #[test]
    fn parse_binary_expr_subtract() -> Result<(), ()> {
        let input = r#"x - 2"#;

        let mut parser = test_utils::get_parser(input, LogLevel::Debug, false);

        let expression = parser.parse_expression(Precedence::Lowest);

        match expression {
            Ok(expr) => Ok(println!("{expr:#?}")),
            Err(_) => Err(println!("{:#?}", parser.errors)),
        }
    }

    // test for `Precedence::Product` vs. `Precedence::Unary`
    #[test]
    fn parse_binary_expr_multiply() -> Result<(), ()> {
        let input = r#"x * 2"#;

        let mut parser = test_utils::get_parser(input, LogLevel::Debug, false);

        let expression = parser.parse_expression(Precedence::Lowest);

        match expression {
            Ok(expr) => Ok(println!("{expr:#?}")),
            Err(_) => Err(println!("{:#?}", parser.errors)),
        }
    }

    #[test]
    fn parse_binary_expr_modulus() -> Result<(), ()> {
        let input = r#"x % 2"#;

        let mut parser = test_utils::get_parser(input, LogLevel::Debug, false);

        let expression = parser.parse_expression(Precedence::Lowest);

        match expression {
            Ok(expr) => Ok(println!("{expr:#?}")),
            Err(_) => Err(println!("{:#?}", parser.errors)),
        }
    }

    #[test]
    fn parse_binary_expr_logical_and() -> Result<(), ()> {
        let input = r#"x && y"#;

        let mut parser = test_utils::get_parser(input, LogLevel::Debug, false);

        let expression = parser.parse_expression(Precedence::Lowest);

        match expression {
            Ok(expr) => Ok(println!("{expr:#?}")),
            Err(_) => Err(println!("{:#?}", parser.errors)),
        }
    }

    // test for `Precedence::LogicalOr` vs. `Precedence::Lowest`
    #[test]
    fn parse_binary_expr_logical_or() -> Result<(), ()> {
        let input = r#"x || y"#;

        let mut parser = test_utils::get_parser(input, LogLevel::Debug, false);

        let expression = parser.parse_expression(Precedence::Lowest);

        match expression {
            Ok(expr) => Ok(println!("{expr:#?}")),
            Err(_) => Err(println!("{:#?}", parser.errors)),
        }
    }

    // test for `Precedence::BitwiseAnd` vs. `Precedence::Unary`
    #[test]
    fn parse_binary_expr_bitwise_and() -> Result<(), ()> {
        let input = r#"x & y"#;

        let mut parser = test_utils::get_parser(input, LogLevel::Debug, false);

        let expression = parser.parse_expression(Precedence::Lowest);

        match expression {
            Ok(expr) => Ok(println!("{expr:#?}")),
            Err(_) => Err(println!("{:#?}", parser.errors)),
        }
    }

    // test for `Precedence::BitwiseOr` vs. `Precedence::Lowest`
    #[test]
    fn parse_binary_expr_bitwise_or() -> Result<(), ()> {
        let input = r#"x | y"#;

        let mut parser = test_utils::get_parser(input, LogLevel::Debug, false);

        let expression = parser.parse_expression(Precedence::Lowest);

        match expression {
            Ok(expr) => Ok(println!("{expr:#?}")),
            Err(_) => Err(println!("{:#?}", parser.errors)),
        }
    }

    #[test]
    fn parse_binary_expr_bitwise_xor() -> Result<(), ()> {
        let input = r#"x ^ y"#;

        let mut parser = test_utils::get_parser(input, LogLevel::Debug, false);

        let expression = parser.parse_expression(Precedence::Lowest);

        match expression {
            Ok(expr) => Ok(println!("{expr:#?}")),
            Err(_) => Err(println!("{:#?}", parser.errors)),
        }
    }

    #[test]
    fn parse_binary_expr_shift_left() -> Result<(), ()> {
        let input = r#"2 << 4"#;

        let mut parser = test_utils::get_parser(input, LogLevel::Debug, false);

        let expression = parser.parse_expression(Precedence::Lowest);

        match expression {
            Ok(expr) => Ok(println!("{expr:#?}")),
            Err(_) => Err(println!("{:#?}", parser.errors)),
        }
    }

    #[test]
    fn parse_binary_expr_exponent() -> Result<(), ()> {
        let input = r#"x**2"#;

        let mut parser = test_utils::get_parser(input, LogLevel::Debug, false);

        let expression = parser.parse_expression(Precedence::Lowest);

        match expression {
            Ok(expr) => Ok(println!("{expr:#?}")),
            Err(_) => Err(println!("{:#?}", parser.errors)),
        }
    }

    #[test]
    fn parse_comparison_expr_less_than() -> Result<(), ()> {
        let input = r#"x < 5"#;

        let mut parser = test_utils::get_parser(input, LogLevel::Debug, false);

        let expression = parser.parse_expression(Precedence::Lowest);

        match expression {
            Ok(expr) => Ok(println!("{expr:#?}")),
            Err(_) => Err(println!("{:#?}", parser.errors)),
        }
    }

    #[test]
    fn parse_comparison_expr_greater_equal() -> Result<(), ()> {
        let input = r#"x >= 2"#;

        let mut parser = test_utils::get_parser(input, LogLevel::Debug, false);

        let expression = parser.parse_expression(Precedence::Lowest);

        match expression {
            Ok(expr) => Ok(println!("{expr:#?}")),
            Err(_) => Err(println!("{:#?}", parser.errors)),
        }
    }

    // test for `Precedence::NotEqual` vs. `Precedence::Unary`
    #[test]
    fn parse_comparison_expr_not_equal() -> Result<(), ()> {
        let input = r#"x != 2"#;

        let mut parser = test_utils::get_parser(input, LogLevel::Debug, false);

        let expression = parser.parse_expression(Precedence::Lowest);

        match expression {
            Ok(expr) => Ok(println!("{expr:#?}")),
            Err(_) => Err(println!("{:#?}", parser.errors)),
        }
    }
}
