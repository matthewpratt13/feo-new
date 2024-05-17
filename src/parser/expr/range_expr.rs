use crate::{
    ast::{AssigneeExpr, Expression, RangeExpr, RangeOp},
    error::{ErrorsEmitted, ParserErrorKind},
    parser::{ParseOperatorExpr, Parser},
    token::{Token, TokenType},
};

impl ParseOperatorExpr for RangeExpr {
    fn parse(parser: &mut Parser, left_expr: Expression) -> Result<Expression, ErrorsEmitted> {
        let from_assignee_expr = AssigneeExpr::try_from(left_expr).map_err(|e| {
            parser.log_error(e);
            ErrorsEmitted
        })?;

        let operator_token = parser.current_token().unwrap_or(Token::EOF);

        let range_op = match operator_token.token_type() {
            TokenType::DblDot => Ok(RangeOp::RangeExclusive),
            TokenType::DotDotEquals => Ok(RangeOp::RangeInclusive),
            TokenType::EOF => {
                parser.log_unexpected_eoi();
                Err(ErrorsEmitted)
            }
            _ => {
                parser.log_unexpected_token(&format!(
                    "range operator ({} or {})",
                    RangeOp::RangeExclusive,
                    RangeOp::RangeInclusive
                ));
                Err(ErrorsEmitted)
            }
        }?;

        parser.next_token();

        if let Some(Token::EOF) = parser.current_token() {
            let expr = RangeExpr {
                from_expr_opt: Some(Box::new(from_assignee_expr)),
                range_op: {
                    if range_op == RangeOp::RangeInclusive {
                        parser.log_error(ParserErrorKind::UnexpectedRangeOp {
                            expected: RangeOp::RangeExclusive.to_string(),
                            found: range_op.to_string(),
                        });
                        return Err(ErrorsEmitted);
                    } else {
                        range_op
                    }
                },
                to_expr_opt: None,
            };

            return Ok(Expression::Range(expr));
        }

        let precedence = parser.get_precedence(&operator_token);

        let expression = parser.parse_expression(precedence)?;

        let to_assignee_expr = AssigneeExpr::try_from(expression).map_err(|e| {
            parser.log_error(e);
            ErrorsEmitted
        })?;

        let expr = Ok(RangeExpr {
            from_expr_opt: Some(Box::new(from_assignee_expr)),
            range_op,
            to_expr_opt: Some(Box::new(to_assignee_expr)),
        })?;

        Ok(Expression::Range(expr))
    }
}

impl RangeExpr {
    pub(crate) fn parse_prefix(parser: &mut Parser) -> Result<Expression, ErrorsEmitted> {
        let operator_token = parser.current_token().unwrap_or(Token::EOF);

        let range_op = match operator_token {
            Token::DblDot { .. } => Ok(RangeOp::RangeExclusive),
            Token::DotDotEquals { .. } => Ok(RangeOp::RangeInclusive),
            _ => {
                parser.log_unexpected_token(&format!(
                    "range operator ({} or {})",
                    RangeOp::RangeExclusive,
                    RangeOp::RangeInclusive
                ));
                Err(ErrorsEmitted)
            }
        }?;

        parser.next_token();

        if parser.current_token().is_none() {
            let expr = RangeExpr {
                from_expr_opt: None,
                range_op: range_op.clone(),
                to_expr_opt: {
                    if range_op == RangeOp::RangeInclusive {
                        parser.log_error(ParserErrorKind::UnexpectedRangeOp {
                            expected: RangeOp::RangeExclusive.to_string(),
                            found: range_op.to_string(),
                        });
                        return Err(ErrorsEmitted);
                    } else {
                        None
                    }
                },
            };

            return Ok(Expression::Range(expr));
        }

        let precedence = parser.get_precedence(&operator_token);

        let expression = parser.parse_expression(precedence)?;

        let to_assignee_expr = AssigneeExpr::try_from(expression).map_err(|e| {
            parser.log_error(e);
            ErrorsEmitted
        })?;

        parser.next_token();

        let expr = RangeExpr {
            from_expr_opt: None,
            range_op,
            to_expr_opt: Some(Box::new(to_assignee_expr)),
        };

        Ok(Expression::Range(expr))
    }
}

#[cfg(test)]
mod tests {
    use crate::{logger::LogLevel, parser::test_utils};

    #[test]
    fn parse_range_expr_excl() -> Result<(), ()> {
        let input = r#"0..foo"#;

        let mut parser = test_utils::get_parser(input, LogLevel::Debug, false);

        let statements = parser.parse();

        match statements {
            Ok(t) => Ok(println!("{:#?}", t)),
            Err(_) => Err(println!("{:#?}", parser.logger.messages())),
        }
    }

    #[test]
    fn parse_range_expr_incl() -> Result<(), ()> {
        let input = r#"..=20"#;

        let mut parser = test_utils::get_parser(input, LogLevel::Debug, false);

        let statements = parser.parse();

        match statements {
            Ok(t) => Ok(println!("{:#?}", t)),
            Err(_) => Err(println!("{:#?}", parser.logger.messages())),
        }
    }

    #[test]
    fn parse_range_expr_full() -> Result<(), ()> {
        let input = r#".."#;

        let mut parser = test_utils::get_parser(input, LogLevel::Debug, false);

        let statements = parser.parse();

        match statements {
            Ok(t) => Ok(println!("{:#?}", t)),
            Err(_) => Err(println!("{:#?}", parser.logger.messages())),
        }
    }
}
