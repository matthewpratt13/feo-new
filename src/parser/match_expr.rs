use crate::{
    ast::{GroupedExpr, MatchArm, MatchExpr},
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

        let scrutinee = parser.parse_expression(Precedence::Lowest)?;

        let open_brace = parser.expect_delimiter(Token::LBrace {
            delim: '{',
            span: parser.stream.span(),
        })?;

        loop {
            if let Some(Token::RBrace { .. }) = parser.peek_current() {
                parser.consume_token();
                break;
            }

            let case = parser.parse_expression(Precedence::Lowest)?;

            let token = parser.peek_current().ok_or({
                parser.log_error(ParserErrorKind::UnexpectedEndOfInput);
                ErrorsEmitted(())
            })?;

            let guard_opt = if let Some(Token::If { .. }) = parser.peek_current() {
                let kw_if = parser.expect_keyword(token)?;

                if let Some(Token::LParen { .. }) = parser.peek_current() {
                    Some((kw_if, GroupedExpr::parse(parser)?))
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

            if let Some(Token::Comma { .. }) = parser.peek_current() {
                continue;
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

        let close_brace = parser.expect_delimiter(Token::RBrace {
            delim: '}',
            span: parser.stream.span(),
        })?;

        if match_arms.is_empty() {
            Ok(MatchExpr {
                kw_match,
                scrutinee: Box::new(scrutinee),
                open_brace,
                arms_opt: None,
                final_arm,
                close_brace,
            })
        } else {
            Ok(MatchExpr {
                kw_match,
                scrutinee: Box::new(scrutinee),
                open_brace,
                arms_opt: Some(match_arms),
                final_arm,
                close_brace,
            })
        }
    }
}
