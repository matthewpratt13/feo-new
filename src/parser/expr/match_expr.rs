use crate::{
    ast::{BlockExpr, Expression, Keyword, MatchArm, MatchExpr},
    error::{ErrorsEmitted, ParserErrorKind},
    parser::{ParseConstructExpr, ParseControlExpr, Parser, Precedence},
    token::{Token, TokenType},
};

use core::fmt;

impl ParseControlExpr for MatchExpr {
    fn parse(parser: &mut Parser) -> Result<MatchExpr, ErrorsEmitted> {
        let first_token = parser.current_token().cloned();

        let kw_match = if let Some(Token::Match { .. }) = &first_token {
            parser.next_token();
            Ok(Keyword::Match)
        } else {
            parser.log_unexpected_token(&TokenType::Match.to_string());
            Err(ErrorsEmitted)
        }?;

        // notify the parser of the error, but let it try to parse anyway and return its own error
        // i.e., do not `return Err(ErrorsEmitted);`
        if let Some(Token::LBrace { .. } | Token::Semicolon { .. }) = parser.current_token() {
            parser.log_error(ParserErrorKind::MissingExpression {
                expected: "scrutinee expression".to_string(),
            });
        }

        let scrutinee = parser.parse_assignee_expr(Precedence::Lowest)?;

        parser.expect_open_brace()?;

        let mut match_arms: Vec<MatchArm> = Vec::new();

        while !matches!(
            parser.current_token(),
            Some(Token::RBrace { .. } | Token::EOF)
        ) {
            let arm = parse_match_arm(parser)?;
            match_arms.push(arm);

            if let Some(Token::Comma { .. }) = parser.current_token() {
                parser.next_token();
            }
        }

        let final_arm = if let Some(a) = match_arms.pop() {
            Ok(Box::new(a))
        } else {
            parser.log_missing("patt", "match arm");
            parser.next_token();
            Err(ErrorsEmitted)
        }?;

        let span = parser.get_braced_item_span(first_token.as_ref())?;

        Ok(MatchExpr {
            kw_match,
            scrutinee,
            match_arms_opt: {
                match match_arms.is_empty() {
                    true => None,
                    false => Some(match_arms),
                }
            },
            final_arm,
            span,
        })
    }
}

impl fmt::Debug for MatchExpr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("MatchExpr")
            .field("scrutinee", &self.scrutinee)
            .field("match_arms_opt", &self.match_arms_opt)
            .field("final_arm", &self.final_arm)
            .finish()
    }
}

fn parse_match_arm(parser: &mut Parser) -> Result<MatchArm, ErrorsEmitted> {
    let matched_pattern = parser.parse_pattern()?;

    let guard_opt = if let Some(Token::If { .. }) = parser.current_token() {
        parser.next_token();
        let expr = parser.parse_expression(Precedence::Lowest)?;
        Some(Box::new(expr))
    } else {
        None
    };

    parser.expect_token(TokenType::FatArrow)?;

    let arm_expression = if let Some(Token::LBrace { .. }) = parser.current_token() {
        Ok(Box::new(Expression::Block(BlockExpr::parse(parser)?)))
    } else {
        let expr = Box::new(parser.parse_expression(Precedence::Lowest)?);

        match parser.current_token() {
            Some(Token::Comma { .. }) => {
                parser.next_token();
                Ok(expr)
            }
            Some(Token::RBrace { .. }) => Ok(expr),
            Some(Token::EOF) | None => {
                parser.log_unexpected_eoi();
                parser.log_missing_token(&TokenType::Comma.to_string());
                Err(ErrorsEmitted)
            }
            _ => {
                parser.log_unexpected_token(&TokenType::Comma.to_string());
                Err(ErrorsEmitted)
            }
        }
    }?;

    Ok(MatchArm {
        matched_pattern,
        guard_opt,
        arm_expression,
    })
}

#[cfg(test)]
mod tests {
    use crate::{
        logger::LogLevel,
        parser::{test_utils, Precedence},
    };

    #[test]
    fn parse_match_expr() -> Result<(), ()> {
        let input = r#"
        match x {
            0 => false,
            1..=5 => false,
            _ if x > 5 => true,
            _ => false
        }"#;

        let mut parser = test_utils::get_parser(input, LogLevel::Debug, false);

        let expression = parser.parse_expression(Precedence::Lowest);

        match expression {
            Ok(e) => Ok(println!("{:#?}", e)),
            Err(_) => Err(println!("{:#?}", parser.logger.messages())),
        }
    }
}
