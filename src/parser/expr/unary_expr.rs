use crate::{
    ast::{DereferenceExpr, DereferenceOp, ReferenceExpr, ReferenceOp, UnaryExpr, UnaryOp},
    error::ErrorsEmitted,
    parser::{ParseSimpleExpr, Parser, Precedence},
    span::Spanned,
    token::Token,
};

/// Parse a unary operation, specifically NOT (`!`) and negate (`-`), based on the input operator.
impl ParseSimpleExpr for UnaryExpr {
    fn parse(parser: &mut Parser) -> Result<UnaryExpr, ErrorsEmitted> {
        let first_token = parser.current_token().cloned();

        let unary_op = match &first_token {
            Some(Token::Minus { .. }) => Ok(UnaryOp::Negate),
            Some(Token::Bang { .. }) => Ok(UnaryOp::Not),
            _ => {
                parser.log_unexpected_token("unary operator (`-` or `!`)");
                Err(ErrorsEmitted)
            }
        }?;

        parser.next_token();

        let value_expr = parser.parse_value_expr(Precedence::Unary)?;

        let span = parser.get_span(&first_token.unwrap().span(), &value_expr.span());

        let expr = UnaryExpr {
            unary_op,
            value_expr: Box::new(value_expr),
            span,
        };

        Ok(expr)
    }
}

impl ParseSimpleExpr for ReferenceExpr {
    /// Parse a unary reference operation – i.e., borrow (`&`) or mutable reference (`&mut`) –
    /// based on the input operator.
    fn parse(parser: &mut Parser) -> Result<ReferenceExpr, ErrorsEmitted> {
        let first_token = parser.current_token().cloned();

        let reference_op = match &first_token {
            Some(Token::Ampersand { .. }) => Ok(ReferenceOp::Borrow),
            Some(Token::AmpersandMut { .. }) => Ok(ReferenceOp::MutableBorrow),
            _ => {
                parser.log_unexpected_token("reference operator (`&` or `&mut`)");
                Err(ErrorsEmitted)
            }
        }?;

        parser.next_token();

        let operand = parser.parse_expression(Precedence::Unary)?;

        let span = parser.get_span(&first_token.unwrap().span(), &operand.span());

        let expr = ReferenceExpr {
            reference_op,
            expression: Box::new(operand),
            span,
        };

        Ok(expr)
    }
}

impl ParseSimpleExpr for DereferenceExpr {
    /// Parse a unary dereference operation with the operator `*`.
    fn parse(parser: &mut Parser) -> Result<DereferenceExpr, ErrorsEmitted> {
        let first_token = parser.current_token().cloned();

        let dereference_op = if let Some(Token::Asterisk { .. }) = &first_token {
            parser.next_token();
            Ok(DereferenceOp)
        } else {
            parser.log_unexpected_token("dereference operator (`*`)");
            Err(ErrorsEmitted)
        }?;

        let assignee_expr = parser.parse_assignee_expr(Precedence::Unary)?;

        let span = parser.get_span(&first_token.unwrap().span(), &assignee_expr.span());

        let expr = DereferenceExpr {
            dereference_op,
            assignee_expr,
            span,
        };

        Ok(expr)
    }
}

#[cfg(test)]
mod tests {
    use crate::{
        logger::LogLevel,
        parser::{test_utils, Precedence},
    };

    #[test]
    fn parse_unary_expr_negate() -> Result<(), ()> {
        let input = r#"-x"#;

        let mut parser = test_utils::get_parser(input, LogLevel::Debug, false);

        let expression = parser.parse_expression(Precedence::Lowest);

        match expression {
            Ok(e) => Ok(println!("{:#?}", e)),
            Err(_) => Err(println!("{:#?}", parser.logger.messages())),
        }
    }

    #[test]
    fn parse_unary_expr_not() -> Result<(), ()> {
        let input = r#"!x.is_odd()"#;

        let mut parser = test_utils::get_parser(input, LogLevel::Debug, false);

        let expression = parser.parse_expression(Precedence::Lowest);

        match expression {
            Ok(e) => Ok(println!("{:#?}", e)),
            Err(_) => Err(println!("{:#?}", parser.logger.messages())),
        }
    }

    #[test]
    // #[ignore]
    fn parse_reference_expr_borrow() -> Result<(), ()> {
        let input = r#"&x"#;

        let mut parser = test_utils::get_parser(input, LogLevel::Debug, false);

        let expression = parser.parse_expression(Precedence::Lowest);

        match expression {
            Ok(e) => Ok(println!("{:#?}", e)),
            Err(_) => Err(println!("{:#?}", parser.logger.messages())),
        }
    }

    #[test]
    // #[ignore]
    fn parse_reference_expr_mut() -> Result<(), ()> {
        let input = r#"&mut x"#;

        let mut parser = test_utils::get_parser(input, LogLevel::Debug, false);

        let expression = parser.parse_expression(Precedence::Lowest);

        match expression {
            Ok(e) => Ok(println!("{:#?}", e)),
            Err(_) => Err(println!("{:#?}", parser.logger.messages())),
        }
    }

    #[test]
    // #[ignore]
    fn parse_dereference_expr() -> Result<(), ()> {
        let input = r#"*x"#;

        let mut parser = test_utils::get_parser(input, LogLevel::Debug, false);

        let expression = parser.parse_expression(Precedence::Lowest);

        match expression {
            Ok(e) => Ok(println!("{:#?}", e)),
            Err(_) => Err(println!("{:#?}", parser.logger.messages())),
        }
    }
}
