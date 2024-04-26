use crate::{
    ast::{Expression, Literal, RangeExpr, RangeOp},
    error::ErrorsEmitted,
};

use super::{Parser, Precedence};

impl RangeExpr {
    pub(crate) fn parse(
        parser: &mut Parser,
        from: Expression,
        op: RangeOp,
    ) -> Result<RangeExpr, ErrorsEmitted> {
        let from = match from.clone() {
            Expression::Literal(l) => match l {
                Literal::Int(_) | Literal::UInt(_) | Literal::BigUInt(_) => Ok(from),
                _ => {
                    parser.log_unexpected_token("numeric literal".to_string());
                    Err(ErrorsEmitted)
                }
            },
            Expression::Path(_) => {
                parser.log_unexpected_token("path expression".to_string());
                Err(ErrorsEmitted)
            }
            _ => {
                parser.log_unexpected_token("numeric literal or path expression".to_string());
                Err(ErrorsEmitted)
            }
        }?;

        let expression = parser.parse_expression(Precedence::Range)?;

        let to = match expression.clone() {
            Expression::Literal(l) => match l {
                Literal::Int(_) | Literal::UInt(_) | Literal::BigUInt(_) => Ok(expression),
                _ => {
                    parser.log_unexpected_token("numeric literal".to_string());
                    Err(ErrorsEmitted)
                }
            },
            Expression::Path(_) => {
                parser.log_unexpected_token("path expression".to_string());
                Err(ErrorsEmitted)
            }
            _ => {
                parser.log_unexpected_token("numeric literal or path expression".to_string());
                Err(ErrorsEmitted)
            }
        };

        if to.is_ok() {
            Ok(RangeExpr {
                from_opt: Some(Box::new(from)),
                op,
                to_opt: Some(Box::new(to?)),
            })
        } else {
            Ok(RangeExpr {
                from_opt: Some(Box::new(from)),
                op: op.clone(),
                to_opt: {
                    if op == RangeOp::RangeInclusive {
                        parser.log_unexpected_token("`..`".to_string());
                        return Err(ErrorsEmitted);
                    } else {
                        None
                    }
                },
            })
        }
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
