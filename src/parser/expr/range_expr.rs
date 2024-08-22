use crate::{
    ast::{AssigneeExpr, Expression, RangeExpr, RangeOp},
    error::{ErrorsEmitted, ParserErrorKind},
    parser::{ParseOperatorExpr, Parser},
    span::Spanned,
    token::{Token, TokenType},
};

use core::fmt;

impl RangeExpr {
    pub(crate) fn parse_prefix(parser: &mut Parser) -> Result<RangeExpr, ErrorsEmitted> {
        let operator_token = parser.current_token().cloned().unwrap_or(Token::EOF);

        let range_op = match &operator_token {
            Token::DblDot { .. } => Ok(RangeOp::RangeExclusive),
            Token::DotDotEquals { .. } => Ok(RangeOp::RangeInclusive),
            _ => {
                parser.log_unexpected_token(&format!(
                    "range operator (`{}` or `{}`)",
                    RangeOp::RangeExclusive,
                    RangeOp::RangeInclusive
                ));
                Err(ErrorsEmitted)
            }
        }?;

        parser.next_token();

        if parser.current_token().is_none() {
            let last_token = parser.peek_behind_by(1);
            let span = parser.get_span(&operator_token.span(), &last_token.unwrap().span());

            let expr = RangeExpr {
                from_expr_opt: None,
                range_op: range_op.clone(),
                to_expr_opt: {
                    if range_op == RangeOp::RangeInclusive {
                        parser.log_error(ParserErrorKind::UnexpectedRangeOp {
                            expected: format!("`{}`", RangeOp::RangeExclusive),
                            found: format!("`{}`", range_op),
                        });
                        return Err(ErrorsEmitted);
                    } else {
                        None
                    }
                },
                span,
            };

            return Ok(expr);
        }

        let precedence = parser.get_precedence(&operator_token);

        let to_assignee_expr = parser.parse_assignee_expr(precedence)?;

        let span = parser.get_span(&operator_token.span(), &to_assignee_expr.span());

        parser.next_token();

        Ok(RangeExpr {
            from_expr_opt: None,
            range_op,
            to_expr_opt: Some(Box::new(to_assignee_expr)),
            span,
        })
    }
}

impl ParseOperatorExpr for RangeExpr {
    fn parse(parser: &mut Parser, left_expr: Expression) -> Result<Expression, ErrorsEmitted> {
        let left_expr_span = &left_expr.span();

        let from_assignee_expr: AssigneeExpr = left_expr.try_into().map_err(|e| {
            parser.log_error(e);
            ErrorsEmitted
        })?;

        let operator_token = parser.current_token().cloned().unwrap_or(Token::EOF);

        let range_op = match &operator_token.token_type() {
            TokenType::DblDot => Ok(RangeOp::RangeExclusive),
            TokenType::DotDotEquals => Ok(RangeOp::RangeInclusive),
            TokenType::EOF => {
                parser.log_unexpected_eoi();
                Err(ErrorsEmitted)
            }
            _ => {
                parser.log_unexpected_token(&format!(
                    "range operator (`{}` or `{}`)",
                    RangeOp::RangeExclusive,
                    RangeOp::RangeInclusive
                ));
                Err(ErrorsEmitted)
            }
        }?;

        parser.next_token();

        if let Some(Token::EOF) = parser.current_token() {
            let last_token = parser.peek_behind_by(1);

            let span = parser.get_span(left_expr_span, &last_token.unwrap().span());

            let expr = RangeExpr {
                from_expr_opt: Some(Box::new(from_assignee_expr)),
                range_op: {
                    if range_op == RangeOp::RangeInclusive {
                        parser.log_error(ParserErrorKind::UnexpectedRangeOp {
                            expected: format!("`{}`", RangeOp::RangeExclusive),
                            found: format!("`{}`", range_op),
                        });
                        return Err(ErrorsEmitted);
                    } else {
                        range_op
                    }
                },
                to_expr_opt: None,
                span,
            };

            return Ok(Expression::Range(expr));
        }

        let precedence = parser.get_precedence(&operator_token);

        let to_assignee_expr = parser.parse_assignee_expr(precedence)?;

        let span = parser.get_span(left_expr_span, &to_assignee_expr.span());

        let expr = Ok(RangeExpr {
            from_expr_opt: Some(Box::new(from_assignee_expr)),
            range_op,
            to_expr_opt: Some(Box::new(to_assignee_expr)),
            span,
        })?;

        Ok(Expression::Range(expr))
    }
}

impl fmt::Debug for RangeExpr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("RangeExpr")
            .field("from_expr_opt", &self.from_expr_opt)
            .field("range_op", &self.range_op)
            .field("to_expr_opt", &self.to_expr_opt)
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
    fn parse_range_expr_excl() -> Result<(), ()> {
        let input = r#"0..foo"#;

        let mut parser = test_utils::get_parser(input, LogLevel::Debug, false);

        let expression = parser.parse_expression(Precedence::Lowest);

        match expression {
            Ok(e) => Ok(println!("{:#?}", e)),
            Err(_) => Err(println!("{:#?}", parser.logger.messages())),
        }
    }

    #[test]
    fn parse_range_expr_incl() -> Result<(), ()> {
        let input = r#"..=20"#;

        let mut parser = test_utils::get_parser(input, LogLevel::Debug, false);

        let expression = parser.parse_expression(Precedence::Lowest);

        match expression {
            Ok(e) => Ok(println!("{:#?}", e)),
            Err(_) => Err(println!("{:#?}", parser.logger.messages())),
        }
    }

    #[test]
    fn parse_range_expr_full() -> Result<(), ()> {
        let input = r#".."#;

        let mut parser = test_utils::get_parser(input, LogLevel::Debug, false);

        let expression = parser.parse_expression(Precedence::Lowest);

        match expression {
            Ok(e) => Ok(println!("{:#?}", e)),
            Err(_) => Err(println!("{:#?}", parser.logger.messages())),
        }
    }
}
