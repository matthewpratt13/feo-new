use crate::{
    ast::{AssigneeExpr, BlockExpr, Delimiter, Expression, Keyword, MatchArm, MatchExpr, Pattern},
    error::{ErrorsEmitted, ParserErrorKind},
    token::{Token, TokenType},
};

use super::{test_utils::log_token, Parser, Precedence};

impl MatchExpr {
    pub(crate) fn parse(parser: &mut Parser) -> Result<Expression, ErrorsEmitted> {
        log_token(parser, "enter `MatchExpr::parse()`", true);

        let kw_match = parser.expect_keyword(TokenType::Match)?;

        let matched_expression = parser.parse_expression(Precedence::Assignment)?;

        log_token(parser, "matched expression (scrutinee)`", true);

        let scrutinee = AssigneeExpr::try_from(matched_expression).map_err(|e| {
            parser.log_error(e);
            ErrorsEmitted
        })?;

        let open_brace = if let Some(Token::LBrace { .. }) = parser.current_token() {
            parser.next_token();
            log_token(parser, "consume token", false);
            Ok(Delimiter::LBrace)
        } else {
            parser.log_unexpected_token(TokenType::LBrace);
            Err(ErrorsEmitted)
        }?;

        let mut match_arms = parse_match_arms(parser)?;

        // let match_arms = collection::get_collection_braces_comma(parser, MatchArm::parse)?;

        let final_arm = if let Some(a) = match_arms.pop() {
            Box::new(a)
        } else {
            parser.log_error(ParserErrorKind::TokenNotFound {
                expected: "match arm".to_string(),
            });
            return Err(ErrorsEmitted);
        };

        let close_brace = if let Some(Token::RBrace { .. }) = parser.current_token() {
            parser.next_token();
            Ok(Delimiter::RBrace)
        } else {
            parser.log_error(ParserErrorKind::MissingDelimiter {
                delim: TokenType::RBrace,
            });
            Err(ErrorsEmitted)
        }?;

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

        log_token(parser, "exit `MatchExpr::parse()`", true);

        Ok(Expression::Match(expr))
    }
}

impl MatchArm {
    pub(crate) fn parse(
        parser: &mut Parser,
        expression: Expression,
    ) -> Result<Expression, ErrorsEmitted> {
        log_token(parser, "enter `MatchArm::parse()`", true);

        let pattern = Pattern::try_from(expression).map_err(|e| {
            parser.log_error(e);
            ErrorsEmitted
        })?;

        let guard_opt = if let Some(Token::If { .. }) = parser.current_token() {
            parser.next_token();
            let expr = Box::new(parser.parse_expression(Precedence::Assignment)?);
            Some((Keyword::If, expr))
        } else {
            None
        };

        if let Some(Token::FatArrow { .. }) = parser.current_token() {
            log_token(parser, "encounter `=>`", false);

            parser.next_token();
            log_token(parser, "consume token", true);
        } else {
            parser.log_unexpected_token(TokenType::FatArrow);
            return Err(ErrorsEmitted);
        }

        let body = if let Some(Token::LBrace { .. }) = parser.current_token() {
            let expr = Box::new(BlockExpr::parse(parser)?);
            expr
        } else {
            let expr = Box::new(parser.parse_expression(Precedence::Lowest)?);
            expr
        };

        if let Some(Token::Comma { .. }) = parser.current_token() {
            log_token(parser, "encounter `,`", false);

            parser.next_token();
            log_token(parser, "consume token", true);
        }

        log_token(parser, "exit `MatchArm::parse()`", true);

        let expr = MatchArm {
            pattern,
            guard_opt,
            body,
        };

        Ok(Expression::MatchArm(expr))
    }
}

fn parse_match_arms(parser: &mut Parser) -> Result<Vec<Expression>, ErrorsEmitted> {
    let mut match_arms: Vec<Expression> = Vec::new();

    while !matches!(
        parser.current_token(),
        Some(Token::RBrace { .. } | Token::EOF)
    ) {
        let expression = parser.parse_expression(Precedence::Assignment)?;

        let arm = MatchArm::parse(parser, expression)?;
        match_arms.push(arm);

        log_token(parser, "foo", true);

        if let Some(Token::Comma { .. }) = parser.current_token() {
            log_token(parser, "encounter `,`", false);

            parser.next_token();
            log_token(parser, "consume token", true);
        }
    }

    Ok(match_arms)
}

#[cfg(test)]
mod tests {
    use crate::parser::test_utils;

    #[test]
    fn parse_match_expr() -> Result<(), ()> {
        let input = r#"
        match x {
            0 => false,
            _ if x > 5 => true,
            _ => false
        }"#;

        let mut parser = test_utils::get_parser(input, false);

        let statements = parser.parse();

        match statements {
            Ok(t) => Ok(println!("{:#?}", t)),
            Err(_) => Err(println!("{:#?}", parser.errors())),
        }
    }
}
