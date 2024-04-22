use crate::{
    ast::{Delimiter, Expression, GroupedExpr, MatchArm, MatchExpr, PlaceExpr, UnderscoreExpr},
    error::{ErrorsEmitted, ParserErrorKind},
    token::Token,
};

use super::{Parser, Precedence};

impl MatchExpr {
    pub(crate) fn parse(parser: &mut Parser) -> Result<MatchExpr, ErrorsEmitted> {
        let kw_match = parser.expect_keyword(Token::Match {
            name: "match".to_string(),
            span: parser.stream.span(),
        })?;

        let mut match_arms: Vec<MatchArm> = Vec::new();

        let scrutinee = PlaceExpr::try_from(parser.parse_expression(Precedence::Lowest)?)?;

        let open_brace = if let Some(Token::LBrace { .. }) = parser.consume_token() {
            Ok(Delimiter::LBrace)
        } else {
            parser.log_unexpected_token("`{`".to_string());
            Err(ErrorsEmitted(()))
        }?;

        loop {
            if let Some(Token::RBrace { .. }) = parser.peek_current() {
                break;
            }

            let case = parser.parse_expression(Precedence::Lowest)?;

            let guard_opt = if let Expression::Underscore(UnderscoreExpr { .. }) = case {
                if let Some(Token::If { .. }) = parser.peek_current() {
                    let kw_if = parser.expect_keyword(Token::If {
                        name: "if".to_string(),
                        span: parser.stream.span(),
                    })?;

                    if let Some(Token::LParen { .. }) = parser.peek_current() {
                        parser.consume_token();
                    } else {
                        parser.log_unexpected_token("`(`".to_string());
                    };

                    let expr = GroupedExpr::parse(parser)?;

                    Some((kw_if, Box::new(expr)))
                } else {
                    None
                }
            } else {
                None
            };

            let fat_arrow = parser.expect_separator(Token::FatArrow {
                punc: "=>".to_string(),
                span: parser.stream.span(),
            })?;

            let logic = parser.parse_expression(Precedence::Lowest)?;

            let arm = MatchArm {
                case: Box::new(case),
                guard_opt,
                fat_arrow,
                logic: Box::new(logic),
            };

            match_arms.push(arm);

            match parser.peek_current() {
                Some(Token::Comma { .. }) => {
                    parser.consume_token();
                    continue;
                }
                Some(Token::RBrace { .. }) => break,
                Some(_) => {
                    parser.log_unexpected_token("`,` or `}`".to_string());
                }
                None => break,
            }
        }

        let final_arm = if let Some(a) = match_arms.pop() {
            a
        } else {
            parser.log_error(ParserErrorKind::TokenNotFound {
                expected: "match arm".to_string(),
            });
            return Err(ErrorsEmitted(()));
        };

        let close_brace = if let Some(Token::RBrace { .. }) = parser.peek_current() {
            parser.consume_token();
            Ok(Delimiter::RBrace)
        } else {
            parser.log_missing_delimiter('}');
            Err(ErrorsEmitted(()))
        }?;

        Ok(MatchExpr {
            kw_match,
            scrutinee,
            open_brace,
            arms_opt: {
                if match_arms.is_empty() {
                    None
                } else {
                    Some(match_arms)
                }
            },
            final_arm,
            close_brace,
        })
    }
}

#[cfg(test)]
mod tests {
    use crate::parser::test_utils;

    #[test]
    fn parse_match_expr() -> Result<(), ()> {
        let input = r#"
        match x {
            0 => false,
            _ if (x > 5) => true,
            _ => false
        }"#;

        let mut parser = test_utils::get_parser(input, false);

        let expressions = parser.parse();

        match expressions {
            Ok(t) => Ok(println!("{:#?}", t)),
            Err(_) => Err(println!("{:#?}", parser.errors())),
        }
    }
}
