use crate::{
    ast::{BlockExpr, Expression, ForInExpr, Pattern},
    error::ErrorsEmitted,
    token::{Token, TokenType},
};

use super::{test_utils::log_token, Parser, Precedence};

impl ForInExpr {
    pub(crate) fn parse(parser: &mut Parser) -> Result<Expression, ErrorsEmitted> {
        log_token(parser, "enter `ForInExpr::parse()`", true);

        let kw_for = parser.expect_keyword(TokenType::For)?;

        let assignee =
            if let Some(Token::Identifier { .. } | Token::Ref { .. } | Token::Mut { .. }) =
                parser.current_token()
            {
                parser.get_identifier_patt()
            } else {
                let expression = parser.parse_expression(Precedence::Lowest)?;
                Pattern::try_from(expression).map_err(|e| {
                    parser.log_error(e);
                    ErrorsEmitted
                })
            }?;

        let kw_in = parser.expect_keyword(TokenType::In)?;

        let iterable = parser.parse_expression(Precedence::Lowest)?;

        let block = if let Some(Token::LBrace { .. }) = parser.current_token() {
            Ok(Box::new(BlockExpr::parse(parser)?))
        } else {
            parser.log_unexpected_token(TokenType::LBrace);
            Err(ErrorsEmitted)
        }?;

        let expr = ForInExpr {
            kw_for,
            assignee,
            kw_in,
            iterable: Box::new(iterable),
            block,
        };

        log_token(parser, "exit `ForInExpr::parse()`", true);

        Ok(Expression::ForIn(expr))
    }
}

#[cfg(test)]
mod tests {
    use crate::parser::test_utils;

    #[test]
    fn parse_for_in_expr() -> Result<(), ()> {
        let input = r#"
        for x in 0..=5 {
            x += 1;

            let y = 15;

            for z in y {
                print("foo");
            }
        }"#;

        let mut parser = test_utils::get_parser(input, false);

        let statements = parser.parse();

        match statements {
            Ok(t) => Ok(println!("{:#?}", t)),
            Err(_) => Err(println!("{:#?}", parser.errors())),
        }
    }
}
