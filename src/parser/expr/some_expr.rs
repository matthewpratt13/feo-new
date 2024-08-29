use crate::{
    ast::{Keyword, SomeExpr},
    error::ErrorsEmitted,
    parser::{ParseConstructExpr, Parser},
    token::{Token, TokenType},
};

use core::fmt;

impl ParseConstructExpr for SomeExpr {
    fn parse(parser: &mut Parser) -> Result<SomeExpr, ErrorsEmitted> {
        let first_token = parser.current_token().cloned();

        let kw_some = if let Some(Token::Some { .. }) = &first_token {
            parser.next_token();
            Ok(Keyword::Some)
        } else {
            parser.log_unexpected_token(&TokenType::Some.to_string());
            Err(ErrorsEmitted)
        }?;

        let expression = parser.expect_grouped_expr()?;

        let span = parser.get_span(&first_token.unwrap().span(), &expression.span);

        Ok(SomeExpr {
            kw_some,
            expression: Box::new(expression),
            span,
        })
    }
}

impl fmt::Debug for SomeExpr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("SomeExpr")
            .field("expression", &self.expression)
            .finish()
    }
}

#[cfg(test)]
mod tests {
    use crate::{
        logger::LogLevel,
        parser::{test_utils, Precedence},
    };

    #[test]
    fn parse_some_expr() -> Result<(), ()> {
        let input = r#"Some(foo)"#;

        let mut parser = test_utils::get_parser(input, LogLevel::Debug, false);

        let expression = parser.parse_expression(Precedence::Lowest);

        match expression {
            Ok(expr) => Ok(println!("{expr:#?}")),
            Err(_) => Err(println!("{:#?}", parser.errors)),
        }
    }

    #[test]
    fn parse_none_expr() -> Result<(), ()> {
        let input = r#"None"#;

        let mut parser = test_utils::get_parser(input, LogLevel::Debug, false);

        let expression = parser.parse_expression(Precedence::Lowest);

        match expression {
            Ok(expr) => Ok(println!("{expr:#?}")),
            Err(_) => Err(println!("{:#?}", parser.errors)),
        }
    }
}
