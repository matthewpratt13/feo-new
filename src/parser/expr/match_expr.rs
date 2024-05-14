use crate::{
    ast::{AssigneeExpr, BlockExpr, Delimiter, Expression, Keyword, MatchArm, MatchExpr},
    error::ErrorsEmitted,
    parser::{ParseConstruct, ParseControl, Parser, Precedence},
    span::Position,
    token::Token,
};

impl ParseControl for MatchExpr {
    fn parse(parser: &mut Parser) -> Result<Expression, ErrorsEmitted> {
        let kw_match = if let Some(Token::Match { .. }) = parser.current_token() {
            parser.next_token();
            Ok(Keyword::Match)
        } else {
            parser.log_unexpected_token("`match`");
            Err(ErrorsEmitted)
        }?;

        if let Some(Token::LBrace { .. } | Token::Semicolon { .. }) = parser.current_token() {
            parser.log_missing("expr", "scrutinee expression");
            return Err(ErrorsEmitted);
        }

        let matched_expression = parser.parse_expression(Precedence::Lowest)?;

        let scrutinee = AssigneeExpr::try_from(matched_expression).map_err(|e| {
            parser.log_error(e);
            ErrorsEmitted
        })?;

        let open_brace = match parser.current_token() {
            Some(Token::LBrace { .. }) => {
                let position = Position::new(parser.current, &parser.stream.span().input());
                parser.next_token();
                Ok(Delimiter::LBrace { position })
            }
            Some(Token::EOF) | None => {
                parser.log_unexpected_eoi();
                Err(ErrorsEmitted)
            }
            _ => {
                parser.log_unexpected_token("`{`");
                Err(ErrorsEmitted)
            }
        }?;

        let mut arms = Vec::new();

        while !matches!(
            parser.current_token(),
            Some(Token::RBrace { .. } | Token::EOF)
        ) {
            let arm = parse_match_arm(parser)?;
            arms.push(arm);

            if let Some(Token::Comma { .. }) = parser.current_token() {
                parser.next_token();
            }
        }

        let final_arm = if let Some(a) = arms.pop() {
            Ok(Box::new(a))
        } else {
            parser.log_missing("patt", "match arm");
            Err(ErrorsEmitted)
        }?;

        match parser.current_token() {
            Some(Token::RBrace { .. }) => {
                parser.next_token();

                let expr = MatchExpr {
                    kw_match,
                    scrutinee,
                    arms_opt: {
                        match arms.is_empty() {
                            true => None,
                            false => Some(arms),
                        }
                    },
                    final_arm,
                };

                Ok(Expression::Match(expr))
            }
            Some(Token::EOF) | None => {
                parser.log_unmatched_delimiter(&open_brace);
                parser.log_missing_token("`}`");
                Err(ErrorsEmitted)
            }
            _ => {
                parser.log_unexpected_token("`}`");
                Err(ErrorsEmitted)
            }
        }
    }
}

fn parse_match_arm(parser: &mut Parser) -> Result<MatchArm, ErrorsEmitted> {
    let pattern = parser.parse_pattern()?;

    let guard_opt = if let Some(Token::If { .. }) = parser.current_token() {
        parser.next_token();
        let expr = parser.parse_expression(Precedence::Lowest)?;
        Some((Keyword::If, Box::new(expr)))
    } else {
        None
    };

    match parser.current_token() {
        Some(Token::FatArrow { .. }) => {
            parser.next_token();
        }
        Some(Token::EOF) | None => {
            parser.log_unexpected_eoi();
            return Err(ErrorsEmitted);
        }
        _ => {
            parser.log_unexpected_token("`=>`");
            return Err(ErrorsEmitted);
        }
    }

    let body = if let Some(Token::LBrace { .. }) = parser.current_token() {
        Ok(Box::new(BlockExpr::parse(parser)?))
    } else {
        let expr = Box::new(parser.parse_expression(Precedence::Lowest)?);
        match parser.current_token() {
            Some(Token::Comma { .. }) => {
                parser.next_token();
                Ok(expr)
            }
            Some(Token::RBrace { .. }) => Ok(expr),
            Some(Token::EOF) | None => {
                parser.log_missing_token("`,`");
                Err(ErrorsEmitted)
            }
            _ => {
                parser.log_unexpected_token("`,`");
                Err(ErrorsEmitted)
            }
        }
    }?;

    let arm = MatchArm {
        pattern,
        guard_opt,
        body,
    };

    Ok(arm)
}

#[cfg(test)]
mod tests {
    use crate::{logger::LogLevel, parser::test_utils};

    #[test]
    fn parse_match_expr() -> Result<(), ()> {
        let input = r#"
        match x {
            0 => false,
            _ if x > 5 => true,
            _ => false
        }"#;

        let mut parser = test_utils::get_parser(input, LogLevel::Debug, false);

        let statements = parser.parse();

        match statements {
            Ok(t) => Ok(println!("{:#?}", t)),
            Err(_) => Err(println!("{:#?}", parser.logger.logs())),
        }
    }
}
