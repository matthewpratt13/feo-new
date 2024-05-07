use crate::{
    ast::{Literal, Pattern, RangeOp, RangePatt},
    error::ErrorsEmitted,
    parser::Parser,
    token::{Token, TokenType},
};

impl RangePatt {
    pub(crate) fn parse(parser: &mut Parser, left_patt: Pattern) -> Result<Pattern, ErrorsEmitted> {
        let from = match left_patt.clone() {
            Pattern::Literal(l) => match l {
                Literal::Int(_) | Literal::UInt(_) | Literal::BigUInt(_) => Ok(left_patt),
                _ => {
                    parser.log_unexpected_token("numeric literal");
                    Err(ErrorsEmitted)
                }
            },
            Pattern::PathPatt(_) => {
                parser.log_unexpected_token("path pattern");
                Err(ErrorsEmitted)
            }
            _ => {
                parser.log_unexpected_token("numeric literal or path pattern");
                Err(ErrorsEmitted)
            }
        }?;

        let operator_token = parser.current_token().unwrap_or(Token::EOF);

        let range_op = match operator_token.token_type() {
            TokenType::DblDot => Ok(RangeOp::RangeExclusive),
            TokenType::DotDotEquals => Ok(RangeOp::RangeInclusive),
            _ => {
                parser.log_unexpected_token("range operator");
                Err(ErrorsEmitted)
            }
        }?;

        parser.next_token();

        let pattern = parser.parse_pattern()?;

        let to = match pattern.clone() {
            Pattern::Literal(l) => match l {
                Literal::Int(_) | Literal::UInt(_) | Literal::BigUInt(_) => Ok(pattern),
                _ => {
                    parser.log_unexpected_token("numeric literal");
                    Err(ErrorsEmitted)
                }
            },

            Pattern::PathPatt(_) => Ok(pattern),

            _ => {
                parser.log_unexpected_token("numeric literal or path pattern");
                Err(ErrorsEmitted)
            }
        };

        let expr = match to.is_ok() {
            true => RangePatt {
                from_opt: Some(Box::new(from)),
                range_op,
                to_opt: Some(Box::new(to?)),
            },
            false => RangePatt {
                from_opt: Some(Box::new(from)),
                range_op: range_op.clone(),
                to_opt: {
                    if range_op == RangeOp::RangeInclusive {
                        parser.log_unexpected_token("`..`");
                        return Err(ErrorsEmitted);
                    } else {
                        None
                    }
                },
            },
        };

        Ok(Pattern::RangePatt(expr))
    }

    pub(crate) fn parse_prefix(parser: &mut Parser) -> Result<Pattern, ErrorsEmitted> {
        let operator_token = parser.current_token().unwrap_or(Token::EOF);

        let range_op = match operator_token {
            Token::DblDot { .. } => Ok(RangeOp::RangeExclusive),
            Token::DotDotEquals { .. } => Ok(RangeOp::RangeInclusive),
            _ => {
                parser.log_unexpected_token("range operator");
                Err(ErrorsEmitted)
            }
        }?;

        parser.next_token();

        if parser.current_token().is_none() {
            let patt = RangePatt {
                from_opt: None,
                range_op: range_op.clone(),
                to_opt: {
                    if range_op == RangeOp::RangeInclusive {
                        parser.log_unexpected_token("`..`");
                        return Err(ErrorsEmitted);
                    } else {
                        None
                    }
                },
            };

            return Ok(Pattern::RangePatt(patt));
        }

        let pattern = parser.parse_pattern()?;

        let to = match pattern.clone() {
            Pattern::Literal(l) => match l {
                Literal::Int(_) | Literal::UInt(_) | Literal::BigUInt(_) => Ok(pattern),
                _ => {
                    parser.log_unexpected_token("numeric literal");
                    Err(ErrorsEmitted)
                }
            },

            Pattern::PathPatt(_) => Ok(pattern),

            _ => {
                parser.log_unexpected_token("numeric literal or path pattern");
                Err(ErrorsEmitted)
            }
        }?;

        parser.next_token();

        let patt = RangePatt {
            from_opt: None,
            range_op,
            to_opt: Some(Box::new(to)),
        };

        Ok(Pattern::RangePatt(patt))
    }
}
