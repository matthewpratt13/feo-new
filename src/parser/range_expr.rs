use crate::{
    ast::{Expression, Literal, RangeExpr, RangeOp},
    error::ErrorsEmitted,
    token::TokenType,
};

use super::{Parser, Precedence};

impl RangeExpr {
    pub(crate) fn parse(
        parser: &mut Parser,
        from: Expression,
        op: RangeOp,
    ) -> Result<Expression, ErrorsEmitted> {
        let from = match from.clone() {
            Expression::Literal(l) => match l {
                Literal::Int(_) | Literal::UInt(_) | Literal::BigUInt(_) => Ok(from),
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

        let expression = parser.parse_expression(Precedence::Range)?;

        let to = match expression.clone() {
            Expression::Literal(l) => match l {
                Literal::Int(_) | Literal::UInt(_) | Literal::BigUInt(_) => Ok(expression),
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
        };

        let expr = if to.is_ok() {
            RangeExpr {
                from_opt: Some(Box::new(from)),
                op,
                to_opt: Some(Box::new(to?)),
            }
        } else {
            RangeExpr {
                from_opt: Some(Box::new(from)),
                op: op.clone(),
                to_opt: {
                    if op == RangeOp::RangeInclusive {
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
