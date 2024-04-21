use crate::{
    ast::{Expression, GroupedExpr, MatchArm, MatchExpr, UnderscoreExpr},
    error::{ErrorsEmitted, ParserErrorKind},
    token::Token,
};

use super::{Parser, Precedence};

impl MatchExpr {
    pub(crate) fn parse(parser: &mut Parser) -> Result<MatchExpr, ErrorsEmitted> {
        println!("ENTER `MatchExpr::parse()`");
        println!("CURRENT TOKEN: {:?}\n", parser.peek_current());

        let kw_match = parser.expect_keyword(Token::Match {
            name: "match".to_string(),
            span: parser.stream.span(),
        })?;

        println!("CURRENT TOKEN AFTER `match`: {:?}\n", parser.peek_current());

        let mut match_arms: Vec<MatchArm> = Vec::new();

        let scrutinee = parser.parse_expression(Precedence::Lowest)?;

        println!("SCRUTINEE: {:?}", scrutinee.clone());
        println!("CURRENT TOKEN: {:?}\n", parser.peek_current());

        let open_brace = parser.expect_delimiter(Token::LBrace {
            delim: '{',
            span: parser.stream.span(),
        })?;

        println!(
            "CURRENT TOKEN GOING INTO MATCH ARMS: {:?}\n",
            parser.peek_current()
        );

        loop {
            if let Some(Token::RBrace { .. }) = parser.peek_current() {
                // parser.consume_token();
                break;
            }

            let case = parser.parse_expression(Precedence::Lowest)?;

            println!("CASE: {:?}", case.clone());
            println!("CURRENT TOKEN: {:?}\n", parser.peek_current());

            let guard_opt = if let Expression::Underscore(UnderscoreExpr { .. }) = case {
                // parser.consume_token();

                if let Some(Token::If { .. }) = parser.peek_current() {
                    let kw_if = parser.expect_keyword(Token::If {
                        name: "if".to_string(),
                        span: parser.stream.span(),
                    })?;

                    println!("TOKEN AFTER `if`: {:?}", parser.peek_current());

                    let _ = parser.expect_delimiter(Token::LParen {
                        delim: '(',
                        span: parser.stream.span(),
                    });

                    let expr = GroupedExpr::parse(parser)?;

                    println!(
                        "CURRENT TOKEN AFTER GROUPED EXPR: {:?}\n",
                        parser.peek_current()
                    );

                    // parser.consume_token();

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
            println!("LOGIC: {:?}", logic);
            println!("CURRENT TOKEN: {:?}\n", parser.peek_current());

            let arm = MatchArm {
                case: Box::new(case),
                guard_opt,
                fat_arrow,
                logic: Box::new(logic),
            };

            match_arms.push(arm);

            if let Some(Token::Comma { .. }) = parser.peek_current() {
                parser.consume_token();
                continue;
            }
        }

        println!("MATCH ARMS: {:#?}", match_arms.clone());
        println!("CURRENT TOKEN: {:?}\n", parser.peek_current());

        let final_arm = if let Some(a) = match_arms.pop() {
            a
        } else {
            parser.log_error(ParserErrorKind::TokenNotFound {
                expected: "match arm".to_string(),
            });
            return Err(ErrorsEmitted(()));
        };

        println!("FINAL MATCH ARM: {:#?}", final_arm);
        println!("CURRENT TOKEN: {:?}\n", parser.peek_current());

        let close_brace = parser.expect_delimiter(Token::RBrace {
            delim: '}',
            span: parser.stream.span(),
        })?;

        if match_arms.is_empty() {
            println!("EXIT `MatchExpr::parse()` WITHOUT OPTIONAL MATCH ARMS");
            println!("CURRENT TOKEN: {:?}\n", parser.peek_current());
            Ok(MatchExpr {
                kw_match,
                scrutinee: Box::new(scrutinee),
                open_brace,
                arms_opt: None,
                final_arm,
                close_brace,
            })
        } else {
            println!("EXIT `MatchExpr::parse()` WITH OPTIONAL MATCH ARMS");
            println!("CURRENT TOKEN: {:?}\n", parser.peek_current());
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
