use crate::{
    ast::{BlockExpr, Expression, ForInExpr, Pattern},
    error::ErrorsEmitted,
    token::{Token, TokenType},
};

use super::{Parser, Precedence};

impl ForInExpr {
    pub(crate) fn parse(parser: &mut Parser) -> Result<Expression, ErrorsEmitted> {
        let kw_for = parser.expect_keyword(TokenType::For)?;

        let assignee =
            if let Some(Token::Identifier { .. } | Token::Ref { .. } | Token::Mut { .. }) =
                parser.peek_current()
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

        let block = Box::new(BlockExpr::parse(parser)?);

        let expr = ForInExpr {
            kw_for,
            assignee,
            kw_in,
            iterable: Box::new(iterable),
            block,
        };

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

        let expressions = parser.parse();

        match expressions {
            Ok(t) => Ok(println!("{:#?}", t)),
            Err(_) => Err(println!("{:#?}", parser.errors())),
        }
    }
}
