use crate::{
    ast::{
        AssigneeExpr, Delimiter, Expression, GroupedExpr, MatchArm, MatchExpr, Pattern,
        UnderscoreExpr,
    },
    error::{ErrorsEmitted, ParserErrorKind},
    token::{Token, TokenType},
};

use super::{Parser, Precedence};

impl MatchExpr {
    pub(crate) fn parse(parser: &mut Parser) -> Result<Expression, ErrorsEmitted> {
        let kw_match = parser.expect_keyword(TokenType::Match)?;

        let mut match_arms: Vec<MatchArm> = Vec::new();

        let scrutinee = AssigneeExpr::try_from(parser.parse_expression(Precedence::Lowest)?)
            .map_err(|e| {
                parser.log_error(e);
                ErrorsEmitted
            })?;

        let open_brace = if let Some(Token::LBrace { .. }) = parser.consume_token() {
            Ok(Delimiter::LBrace)
        } else {
            parser.log_unexpected_token(TokenType::LBrace);
            Err(ErrorsEmitted)
        }?;

        loop {
            if let Some(Token::RBrace { .. }) = parser.peek_current() {
                break;
            }

            let expression = parser.parse_expression(Precedence::Lowest)?;
            let case = Pattern::try_from(expression).map_err(|e| {
                parser.log_error(e);
                ErrorsEmitted
            })?;

            let guard_opt = if let Pattern::WildcardPatt(UnderscoreExpr { .. }) = case {
                if let Some(Token::If { .. }) = parser.peek_current() {
                    let kw_if = parser.expect_keyword(TokenType::If)?;

                    // if let Some(Token::LParen { .. }) = parser.peek_current() {
                    //     parser.consume_token();
                    // } else {
                    //     parser.log_unexpected_token(TokenType::LParen);
                    // };

                    let expr = GroupedExpr::parse(parser)?;

                    Some((kw_if, Box::new(expr)))
                } else {
                    None
                }
            } else {
                None
            };

            parser.expect_separator(TokenType::FatArrow)?;

            let logic = parser.parse_expression(Precedence::Lowest)?;

            let arm = MatchArm {
                case,
                guard_opt,
                logic: Box::new(logic),
            };

            match_arms.push(arm);

            parser.consume_token();

            match parser.peek_current() {
                Some(Token::Comma { .. }) => {
                    parser.consume_token();
                    continue;
                }
                Some(Token::RBrace { .. }) => break,
                Some(_) => {
                    parser.log_unexpected_str("`,` or `}`");
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
            return Err(ErrorsEmitted);
        };

        let close_brace = parser.expect_delimiter(TokenType::RBrace)?;

        let expr = MatchExpr {
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
        };

        Ok(Expression::Match(expr))
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
