use crate::{
    ast::{Expression, Literal, RangeExpr, RangeOp},
    error::ErrorsEmitted,
    token::{Token, TokenType},
};

use super::Parser;

impl RangeExpr {
    pub(crate) fn parse(
        parser: &mut Parser,
        left_expr: Expression,
    ) -> Result<Expression, ErrorsEmitted> {
        let from = match left_expr.clone() {
            Expression::Literal(l) => match l {
                Literal::Int(_) | Literal::UInt(_) | Literal::BigUInt(_) => Ok(left_expr),
                _ => {
                    parser.log_unexpected_str("numeric literal");
                    Err(ErrorsEmitted)
                }
            },
            Expression::Path(_) => {
                parser.log_unexpected_str("path expression");
                Err(ErrorsEmitted)
            }
            _ => {
                parser.log_unexpected_str("numeric literal or path expression");
                Err(ErrorsEmitted)
            }
        }?;

        let operator_token = parser.peek_current().unwrap_or(Token::EOF);

        let range_op = match operator_token.token_type() {
            TokenType::DblDot => Ok(RangeOp::RangeExclusive),
            TokenType::DotDotEquals => Ok(RangeOp::RangeInclusive),
            _ => {
                parser.log_unexpected_str("range operator");
                Err(ErrorsEmitted)
            }
        }?;

        parser.consume_token();

        let precedence = parser.get_precedence(&operator_token);

        let expression = parser.parse_expression(precedence)?;

        let to = match expression.clone() {
            Expression::Literal(l) => match l {
                Literal::Int(_) | Literal::UInt(_) | Literal::BigUInt(_) => Ok(expression),
                _ => {
                    parser.log_unexpected_str("numeric literal");
                    Err(ErrorsEmitted)
                }
            },

            Expression::Path(_) => Ok(expression),

            _ => {
                parser.log_unexpected_str("numeric literal or path expression");
                Err(ErrorsEmitted)
            }
        };

        let expr = if to.is_ok() {
            RangeExpr {
                from_opt: Some(Box::new(from)),
                range_op,
                to_opt: Some(Box::new(to?)),
            }
        } else {
            RangeExpr {
                from_opt: Some(Box::new(from)),
                range_op: range_op.clone(),
                to_opt: {
                    if range_op == RangeOp::RangeInclusive {
                        parser.log_unexpected_token(TokenType::DblDot);
                        return Err(ErrorsEmitted);
                    } else {
                        None
                    }
                },
            }
        };

        Ok(Expression::Range(expr))
    }

    pub(crate) fn parse_prefix(parser: &mut Parser) -> Result<Expression, ErrorsEmitted> {
        let operator_token = parser.peek_current().unwrap_or(Token::EOF);

        let range_op = match operator_token {
            Token::DblDot { .. } => Ok(RangeOp::RangeExclusive),
            Token::DotDotEquals { .. } => Ok(RangeOp::RangeInclusive),
            _ => {
                parser.log_unexpected_str("range operator");
                Err(ErrorsEmitted)
            }
        }?;

        parser.consume_token();

        if parser.peek_current().is_none() {
            let expr = RangeExpr {
                from_opt: None,
                range_op: range_op.clone(),
                to_opt: {
                    if range_op == RangeOp::RangeInclusive {
                        parser.log_unexpected_token(TokenType::DblDot);
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

        let to = match expression.clone() {
            Expression::Literal(l) => match l {
                Literal::Int(_) | Literal::UInt(_) | Literal::BigUInt(_) => Ok(expression),
                _ => {
                    parser.log_unexpected_str("numeric literal");
                    Err(ErrorsEmitted)
                }
            },

            Expression::Path(_) => Ok(expression),

            _ => {
                parser.log_unexpected_str("numeric literal or path expression");
                Err(ErrorsEmitted)
            }
        }?;

        parser.consume_token();

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
    use crate::parser::test_utils;

    #[test]
    fn parse_range_expr_excl() -> Result<(), ()> {
        let input = r#"1..10"#;

        let mut parser = test_utils::get_parser(input, false);

        let expressions = parser.parse();

        match expressions {
            Ok(t) => Ok(println!("{:#?}", t)),
            Err(_) => Err(println!("{:#?}", parser.errors())),
        }
    }

    #[test]
    fn parse_range_expr_incl() -> Result<(), ()> {
        let input = r#"..=20"#;

        let mut parser = test_utils::get_parser(input, false);

        let expressions = parser.parse();

        match expressions {
            Ok(t) => Ok(println!("{:#?}", t)),
            Err(_) => Err(println!("{:#?}", parser.errors())),
        }
    }

    #[test]
    fn parse_range_expr_full() -> Result<(), ()> {
        let input = r#".."#;

        let mut parser = test_utils::get_parser(input, false);

        let expressions = parser.parse();

        match expressions {
            Ok(t) => Ok(println!("{:#?}", t)),
            Err(_) => Err(println!("{:#?}", parser.errors())),
        }
    }
}
