use crate::{
    ast::{
        AssigneeExpr, BlockExpr, Delimiter, Expression, Identifier, Keyword, Literal, MatchArm,
        MatchExpr, Pattern,
    },
    error::{ErrorsEmitted, ParserErrorKind},
    token::Token,
};

use super::{
    parse::{ParseConstruct, ParseControl},
    Parser, Precedence,
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

        let matched_expression = parser.parse_expression(Precedence::Lowest)?;

        let scrutinee = AssigneeExpr::try_from(matched_expression).map_err(|e| {
            parser.log_error(e);
            ErrorsEmitted
        })?;

        let open_brace = if let Some(Token::LBrace { .. }) = parser.next_token() {
            Ok(Delimiter::LBrace)
        } else {
            parser.log_unexpected_token("`{`");
            Err(ErrorsEmitted)
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
            parser.log_missing_token("match arm");
            Err(ErrorsEmitted)
        }?;

        let close_brace = if let Some(Token::RBrace { .. }) = parser.next_token() {
            Ok(Delimiter::RBrace)
        } else {
            parser.log_missing_token("`}`");
            parser.log_unmatched_delimiter(open_brace.clone());
            Err(ErrorsEmitted)
        }?;

        let expr = MatchExpr {
            kw_match,
            scrutinee,
            open_brace,
            arms_opt: {
                match arms.is_empty() {
                    true => None,
                    false => Some(arms),
                }
            },
            final_arm,
            close_brace,
        };

        Ok(Expression::Match(expr))
    }
}

fn parse_match_arm(parser: &mut Parser) -> Result<MatchArm, ErrorsEmitted> {
    let pattern = parse_pattern(parser)?;

    let guard_opt = if let Some(Token::If { .. }) = parser.current_token() {
        parser.next_token();
        let expr = parser.parse_expression(Precedence::Lowest)?;
        Some((Keyword::If, Box::new(expr)))
    } else {
        None
    };

    if let Some(Token::FatArrow { .. }) = parser.current_token() {
        parser.next_token();
    } else {
        parser.log_unexpected_token("`=>`");
        return Err(ErrorsEmitted);
    }

    let body = if let Some(Token::LBrace { .. }) = parser.current_token() {
        Box::new(BlockExpr::parse(parser)?)
    } else {
        Box::new(parser.parse_expression(Precedence::Lowest)?)
    };

    let arm = MatchArm {
        pattern,
        guard_opt,
        body,
    };

    Ok(arm)
}

// TODO: create a higher-level `parse_pattern()` function to handle all cases needing a `Pattern`
// TODO: (i.e., to replace this function)
fn parse_pattern(parser: &mut Parser) -> Result<Pattern, ErrorsEmitted> {
    match parser.current_token() {
        Some(Token::UIntLiteral { value, .. }) => {
            parser.next_token();
            Ok(Pattern::Literal(Literal::UInt(value)))
        }
        Some(Token::Identifier { name, .. }) => {
            parser.next_token();
            if &name == "_" {
                Ok(Pattern::WildcardPatt(Identifier("_".to_string())))
            } else {
                todo!()
            }
        }
        _ => {
            parser.log_error(ParserErrorKind::InvalidTokenContext {
                token: parser.current_token(),
            });

            Err(ErrorsEmitted)
        }
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
