use crate::{
    ast::{
        AssigneeExpr, BinaryExpr, BinaryOp, ComparisonExpr, ComparisonOp, Expression, ValueExpr,
    },
    error::ErrorsEmitted,
    logger::{LogLevel, LogMsg},
    parser::{ParseOperation, Parser},
    token::{Token, TokenType},
};

impl ParseOperation for BinaryExpr {
    /// Parse a binary operation (e.g., arithmetic, logical and comparison expressions).
    /// This method parses the operator and calls `parse_expression()` recursively to handle
    /// the right-hand side of the expression.
    fn parse(parser: &mut Parser, left_expr: Expression) -> Result<Expression, ErrorsEmitted> {
        // **log event and current token** [REMOVE IN PROD]
        parser.logger.log(
            LogLevel::Debug,
            LogMsg::from("entering `BinaryExpr::parse()`"),
        );
        parser.log_current_token(true);

        let lhs = ValueExpr::try_from(left_expr).map_err(|e| {
            parser.log_error(e);
            ErrorsEmitted
        })?;

        let operator_token = parser.current_token().unwrap_or(Token::EOF);

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
                parser.log_unexpected_token("binary arithmetic (`+`, `-`, `*`, `/`, `%` or `**`), logical (`&&` or `||`) or bitwise (`&`, `|`, `^`, `<<` or `>>`) operator") ;
                Err(ErrorsEmitted)
            }
        }?;

        let precedence = parser.get_precedence(&operator_token);

        parser.next_token();

        let right_expr = parser.parse_expression(precedence)?;

        let rhs = ValueExpr::try_from(right_expr).map_err(|e| {
            parser.log_error(e);
            ErrorsEmitted
        })?;

        let expr = BinaryExpr {
            lhs: Box::new(lhs),
            binary_op,
            rhs: Box::new(rhs),
        };

        // **log event and current token** [REMOVE IN PROD]
        parser.logger.log(
            LogLevel::Debug,
            LogMsg::from("exiting `BinaryExpr::parse()`"),
        );
        parser.log_current_token(false);

        Ok(Expression::Binary(expr))
    }
}

impl ParseOperation for ComparisonExpr {
    /// Parse a comparison operation (i.e., `==`, `!=`, `<`, `>`, `<=` and `>=`), based on
    /// the input operator.
    fn parse(parser: &mut Parser, left_expr: Expression) -> Result<Expression, ErrorsEmitted> {
        // **log event and current token** [REMOVE IN PROD]
        parser.logger.log(
            LogLevel::Debug,
            LogMsg::from("entering `ComparisonExpr::parse()`"),
        );
        parser.log_current_token(true);

        let lhs = AssigneeExpr::try_from(left_expr).map_err(|e| {
            parser.log_error(e);
            ErrorsEmitted
        })?;

        let operator_token = parser.current_token().unwrap_or(Token::EOF);

        let comparison_op = match operator_token.token_type() {
            TokenType::DblEquals => Ok(ComparisonOp::Equal),
            TokenType::BangEquals => Ok(ComparisonOp::NotEqual),
            TokenType::LessThan => Ok(ComparisonOp::LessThan),
            TokenType::GreaterThan => Ok(ComparisonOp::GreaterThan),
            TokenType::LessThanEquals => Ok(ComparisonOp::LessEqual),
            TokenType::GreaterThanEquals => Ok(ComparisonOp::GreaterEqual),
            _ => {
                parser.log_unexpected_token(
                    "binary comparison operator (`<`, `>`, `<=`, `>=`, `==` or `!=`",
                );
                Err(ErrorsEmitted)
            }
        }?;

        let precedence = parser.get_precedence(&operator_token);

        parser.next_token();

        let right_expr = parser.parse_expression(precedence)?;

        let rhs = AssigneeExpr::try_from(right_expr).map_err(|e| {
            parser.log_error(e);
            ErrorsEmitted
        })?;

        let expr = ComparisonExpr {
            lhs,
            comparison_op,
            rhs,
        };

        // **log event and current token** [REMOVE IN PROD]
        parser.logger.log(
            LogLevel::Debug,
            LogMsg::from("exiting `ComparisonExpr::parse()`"),
        );
        parser.log_current_token(false);

        Ok(Expression::Comparison(expr))
    }
}

#[cfg(test)]
mod tests {
    use crate::{logger::LogLevel, parser::test_utils};

    #[test]
    fn parse_binary_expr_add() -> Result<(), ()> {
        let input = r#"x + 5"#;

        let mut parser = test_utils::get_parser(input, LogLevel::Debug, false);

        let statements = parser.parse();

        match statements {
            Ok(t) => Ok(println!("{:#?}", t)),
            Err(_) => Err(println!("{:#?}", parser.logger.logs())),
        }
    }

    // test for `Precedence::Difference` vs. `Precedence::Unary`
    #[test]
    fn parse_binary_expr_subtract() -> Result<(), ()> {
        let input = r#"x - 2"#;

        let mut parser = test_utils::get_parser(input, LogLevel::Debug, false);

        let statements = parser.parse();

        match statements {
            Ok(t) => Ok(println!("{:#?}", t)),
            Err(_) => Err(println!("{:#?}", parser.logger.logs())),
        }
    }

    // test for `Precedence::Product` vs. `Precedence::Unary`
    #[test]
    fn parse_binary_expr_multiply() -> Result<(), ()> {
        let input = r#"x * 2"#;

        let mut parser = test_utils::get_parser(input, LogLevel::Debug, false);

        let statements = parser.parse();

        match statements {
            Ok(t) => Ok(println!("{:#?}", t)),
            Err(_) => Err(println!("{:#?}", parser.logger.logs())),
        }
    }

    #[test]
    fn parse_binary_expr_modulus() -> Result<(), ()> {
        let input = r#"x % 2"#;

        let mut parser = test_utils::get_parser(input, LogLevel::Debug, false);

        let statements = parser.parse();

        match statements {
            Ok(t) => Ok(println!("{:#?}", t)),
            Err(_) => Err(println!("{:#?}", parser.logger.logs())),
        }
    }

    #[test]
    fn parse_binary_expr_logical_and() -> Result<(), ()> {
        let input = r#"x && y"#;

        let mut parser = test_utils::get_parser(input, LogLevel::Debug, false);

        let statements = parser.parse();

        match statements {
            Ok(t) => Ok(println!("{:#?}", t)),
            Err(_) => Err(println!("{:#?}", parser.logger.logs())),
        }
    }

    // test for `Precedence::LogicalOr` vs. `Precedence::Lowest`
    #[test]
    fn parse_binary_expr_logical_or() -> Result<(), ()> {
        let input = r#"x || y"#;

        let mut parser = test_utils::get_parser(input, LogLevel::Debug, false);

        let statements = parser.parse();

        match statements {
            Ok(t) => Ok(println!("{:#?}", t)),
            Err(_) => Err(println!("{:#?}", parser.logger.logs())),
        }
    }

    // test for `Precedence::BitwiseAnd` vs. `Precedence::Unary`
    #[test]
    fn parse_binary_expr_bitwise_and() -> Result<(), ()> {
        let input = r#"x & y"#;

        let mut parser = test_utils::get_parser(input, LogLevel::Debug, false);

        let statements = parser.parse();

        match statements {
            Ok(t) => Ok(println!("{:#?}", t)),
            Err(_) => Err(println!("{:#?}", parser.logger.logs())),
        }
    }

    // test for `Precedence::BitwiseOr` vs. `Precedence::Lowest`
    #[test]
    fn parse_binary_expr_bitwise_or() -> Result<(), ()> {
        let input = r#"x | y"#;

        let mut parser = test_utils::get_parser(input, LogLevel::Debug, false);

        let statements = parser.parse();

        match statements {
            Ok(t) => Ok(println!("{:#?}", t)),
            Err(_) => Err(println!("{:#?}", parser.logger.logs())),
        }
    }

    #[test]
    fn parse_binary_expr_bitwise_xor() -> Result<(), ()> {
        let input = r#"x ^ y"#;

        let mut parser = test_utils::get_parser(input, LogLevel::Debug, false);

        let statements = parser.parse();

        match statements {
            Ok(t) => Ok(println!("{:#?}", t)),
            Err(_) => Err(println!("{:#?}", parser.logger.logs())),
        }
    }

    #[test]
    fn parse_binary_expr_shift_left() -> Result<(), ()> {
        let input = r#"2 << 4"#;

        let mut parser = test_utils::get_parser(input, LogLevel::Debug, false);

        let statements = parser.parse();

        match statements {
            Ok(t) => Ok(println!("{:#?}", t)),
            Err(_) => Err(println!("{:#?}", parser.logger.logs())),
        }
    }

    #[test]
    fn parse_binary_expr_exponent() -> Result<(), ()> {
        let input = r#"x**2"#;

        let mut parser = test_utils::get_parser(input, LogLevel::Debug, false);

        let statements = parser.parse();

        match statements {
            Ok(t) => Ok(println!("{:#?}", t)),
            Err(_) => Err(println!("{:#?}", parser.logger.logs())),
        }
    }

    #[test]
    fn parse_comparison_expr_less_than() -> Result<(), ()> {
        let input = r#"x < 5"#;

        let mut parser = test_utils::get_parser(input, LogLevel::Debug, false);

        let statements = parser.parse();

        match statements {
            Ok(t) => Ok(println!("{:#?}", t)),
            Err(_) => Err(println!("{:#?}", parser.logger.logs())),
        }
    }

    #[test]
    fn parse_comparison_expr_greater_equal() -> Result<(), ()> {
        let input = r#"x >= 2"#;

        let mut parser = test_utils::get_parser(input, LogLevel::Debug, false);

        let statements = parser.parse();

        match statements {
            Ok(t) => Ok(println!("{:#?}", t)),
            Err(_) => Err(println!("{:#?}", parser.logger.logs())),
        }
    }

    // test for `Precedence::NotEqual` vs. `Precedence::Unary`
    #[test]
    fn parse_comparison_expr_not_equal() -> Result<(), ()> {
        let input = r#"x != 2"#;

        let mut parser = test_utils::get_parser(input, LogLevel::Debug, false);

        let statements = parser.parse();

        match statements {
            Ok(t) => Ok(println!("{:#?}", t)),
            Err(_) => Err(println!("{:#?}", parser.logger.logs())),
        }
    }
}
