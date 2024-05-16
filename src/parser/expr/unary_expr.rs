use crate::{
    ast::{
        AssigneeExpr, DereferenceExpr, DereferenceOp, Expression, ReferenceExpr, ReferenceOp,
        UnaryExpr, UnaryOp, ValueExpr,
    },
    error::ErrorsEmitted,
    parser::{ParseSimpleExpr, Parser, Precedence},
    token::Token,
};

/// Parse a unary operation, specifically NOT (`!`) and negate (`-`), based on the input operator.
impl ParseSimpleExpr for UnaryExpr {
    fn parse(parser: &mut Parser) -> Result<Expression, ErrorsEmitted> {
        let unary_op = match parser.current_token() {
            Some(Token::Minus { .. }) => Ok(UnaryOp::Negate),
            Some(Token::Bang { .. }) => Ok(UnaryOp::Not),
            _ => {
                parser.log_unexpected_token("unary operator (`-` or `!`)");
                Err(ErrorsEmitted)
            }
        }?;

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

impl ParseSimpleExpr for ReferenceExpr {
    /// Parse a unary reference operation – i.e., borrow (`&`) or mutable reference (`&mut`) –
    /// based on the input operator.
    fn parse(parser: &mut Parser) -> Result<Expression, ErrorsEmitted> {
        let reference_op = match parser.current_token() {
            Some(Token::Ampersand { .. }) => Ok(ReferenceOp::Borrow),
            Some(Token::AmpersandMut { .. }) => Ok(ReferenceOp::MutableBorrow),
            _ => {
                parser.log_unexpected_token("reference operator (`&` or `&mut`)");
                Err(ErrorsEmitted)
            }
        }?;

        parser.next_token();

        let operand = parser.parse_expression(Precedence::Unary)?;

        let expr = ReferenceExpr {
            reference_op,
            expression: Box::new(operand),
        };

        Ok(Expression::Reference(expr))
    }
}

impl ParseSimpleExpr for DereferenceExpr {
    /// Parse a unary dereference operation with the operator `*`.
    fn parse(parser: &mut Parser) -> Result<Expression, ErrorsEmitted> {
        let dereference_op = match parser.current_token() {
            Some(Token::Asterisk { .. }) => Ok(DereferenceOp),
            _ => {
                parser.log_unexpected_token("dereference operator (`*`)");
                Err(ErrorsEmitted)
            }
        }?;

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
    use crate::{logger::LogLevel, parser::test_utils};

    #[test]
    fn parse_unary_expr_negate() -> Result<(), ()> {
        let input = r#"-x"#;

        let mut parser = test_utils::get_parser(input, LogLevel::Debug, false);

        let statements = parser.parse();

        match statements {
            Ok(t) => Ok(println!("{:#?}", t)),
            Err(_) => Err(println!("{:#?}", parser.logger.messages())),
        }
    }

    #[test]
    fn parse_unary_expr_not() -> Result<(), ()> {
        let input = r#"!x.is_odd()"#;

        let mut parser = test_utils::get_parser(input, LogLevel::Debug, false);

        let statements = parser.parse();

        match statements {
            Ok(t) => Ok(println!("{:#?}", t)),
            Err(_) => Err(println!("{:#?}", parser.logger.messages())),
        }
    }

    #[test]
    // #[ignore]
    fn parse_reference_expr_borrow() -> Result<(), ()> {
        let input = r#"&x"#;

        let mut parser = test_utils::get_parser(input, LogLevel::Debug, false);

        let statements = parser.parse();

        match statements {
            Ok(t) => Ok(println!("{:#?}", t)),
            Err(_) => Err(println!("{:#?}", parser.logger.messages())),
        }
    }

    #[test]
    // #[ignore]
    fn parse_reference_expr_mut() -> Result<(), ()> {
        let input = r#"&mut x"#;

        let mut parser = test_utils::get_parser(input, LogLevel::Debug, false);

        let statements = parser.parse();

        match statements {
            Ok(t) => Ok(println!("{:#?}", t)),
            Err(_) => Err(println!("{:#?}", parser.logger.messages())),
        }
    }

    #[test]
    // #[ignore]
    fn parse_dereference_expr() -> Result<(), ()> {
        let input = r#"*x"#;

        let mut parser = test_utils::get_parser(input, LogLevel::Debug, false);

        let statements = parser.parse();

        match statements {
            Ok(t) => Ok(println!("{:#?}", t)),
            Err(_) => Err(println!("{:#?}", parser.logger.messages())),
        }
    }
}
