use crate::{
    ast::{Expression, Literal, RangeExpr, RangeOp},
    error::ErrorsEmitted,
    parser::{ParseOperation, Parser},
    token::{Token, TokenType},
};

impl ParseOperation for RangeExpr {
    fn parse(parser: &mut Parser, left_expr: Expression) -> Result<Expression, ErrorsEmitted> {
        // TODO: should be `AssigneeExpr` (can be more than just numbers and identifiers)
        let from = match left_expr.clone() {
            Expression::Literal(l) => match l {
                Literal::Int(_) | Literal::UInt(_) | Literal::BigUInt(_) => Ok(left_expr),
                _ => {
                    parser.log_unexpected_token("numeric value");
                    Err(ErrorsEmitted)
                }
            },
            Expression::Path(_) => {
                // TODO: should be `UnexpectedExpression` (but will be replaced)
                parser.log_unexpected_token("identifier");
                Err(ErrorsEmitted)
            }
            _ => {
                // TODO: should be `UnexpectedExpression` (but will be replaced)
                parser.log_unexpected_token("numeric value or identifier");
                Err(ErrorsEmitted)
            }
        }?;

        let operator_token = parser.current_token().unwrap_or(Token::EOF);

        let range_op = match operator_token.token_type() {
            TokenType::DblDot => Ok(RangeOp::RangeExclusive),
            TokenType::DotDotEquals => Ok(RangeOp::RangeInclusive),
            // TODO: handle `None` case (`UnexpectedEndOfInput`)
            _ => {
                parser.log_unexpected_token("range operator (`..` or `..=`)");
                Err(ErrorsEmitted)
            }
        }?;

        parser.next_token();

        let precedence = parser.get_precedence(&operator_token);

        let expression = parser.parse_expression(precedence)?;

        // TODO: should be `AssigneeExpr` (can be more than just numbers and identifiers)
        let to = match expression.clone() {
            Expression::Literal(l) => match l {
                Literal::Int(_) | Literal::UInt(_) | Literal::BigUInt(_) => Ok(expression),
                _ => {
                    parser.log_unexpected_token("numeric value");
                    Err(ErrorsEmitted)
                }
            },

            Expression::Path(_) => Ok(expression),

            _ => {
                // TODO: should be `UnexpectedExpression` (but will be replaced)
                parser.log_unexpected_token("numeric value or identifier");
                Err(ErrorsEmitted)
            }
        };

        let expr = match to.is_ok() {
            true => Ok(RangeExpr {
                from_opt: Some(Box::new(from)),
                range_op,
                to_opt: Some(Box::new(to?)),
            }),
            false => Ok(RangeExpr {
                from_opt: Some(Box::new(from)),
                range_op: range_op.clone(),
                to_opt: {
                    if range_op == RangeOp::RangeInclusive {
                        parser.log_unexpected_token("exclusive range operator (`..`)");
                        return Err(ErrorsEmitted);
                    } else {
                        None
                    }
                },
            }),
        }?;

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
                parser.log_unexpected_token("range operator (`..` or `..=`)");
                Err(ErrorsEmitted)
            }
        }?;

        parser.next_token();

        if parser.current_token().is_none() {
            let expr = RangeExpr {
                from_opt: None,
                range_op: range_op.clone(),
                to_opt: {
                    if range_op == RangeOp::RangeInclusive {
                        parser.log_unexpected_token("exclusive range operator (`..`)");
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

        // TODO: convert to `AssigneeExpr`

        // TODO: can be more than just numbers or identifiers (i.e., all `AssigneeExpr`)
        let to = match expression.clone() {
            Expression::Literal(l) => match l {
                Literal::Int(_) | Literal::UInt(_) | Literal::BigUInt(_) => Ok(expression),
                _ => {
                    parser.log_unexpected_token("numeric value");
                    Err(ErrorsEmitted)
                }
            },

            Expression::Path(_) => Ok(expression),

            _ => {
                // TODO: should be `UnexpectedExpr` (but will be replaced)
                parser.log_unexpected_token("numeric value or identifier");
                Err(ErrorsEmitted)
            }
        }?;

        parser.next_token();

        let expr = RangeExpr {
            from_opt: None,
            range_op,
            to_opt: Some(Box::new(to)),
        };

        Ok(Expression::Range(expr))
    }
}

#[cfg(test)]
mod tests {
    use crate::{logger::LogLevel, parser::test_utils};

    #[test]
    fn parse_range_expr_excl() -> Result<(), ()> {
        let input = r#"1..10"#;

        let mut parser = test_utils::get_parser(input, LogLevel::Debug, false);

        let statements = parser.parse();

        match statements {
            Ok(t) => Ok(println!("{:#?}", t)),
            Err(_) => Err(println!("{:#?}", parser.logger.logs())),
        }
    }

    #[test]
    fn parse_range_expr_incl() -> Result<(), ()> {
        let input = r#"..=20"#;

        let mut parser = test_utils::get_parser(input, LogLevel::Debug, false);

        let statements = parser.parse();

        match statements {
            Ok(t) => Ok(println!("{:#?}", t)),
            Err(_) => Err(println!("{:#?}", parser.logger.logs())),
        }
    }

    #[test]
    fn parse_range_expr_full() -> Result<(), ()> {
        let input = r#".."#;

        let mut parser = test_utils::get_parser(input, LogLevel::Debug, false);

        let statements = parser.parse();

        match statements {
            Ok(t) => Ok(println!("{:#?}", t)),
            Err(_) => Err(println!("{:#?}", parser.logger.logs())),
        }
    }
}
