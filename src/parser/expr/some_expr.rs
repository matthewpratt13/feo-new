use crate::{
    ast::{GroupedExpr, Keyword, SomeExpr},
    error::ErrorsEmitted,
    parser::{ParseConstructExpr, Parser},
    token::Token,
};

impl ParseConstructExpr for SomeExpr {
    fn parse(parser: &mut Parser) -> Result<SomeExpr, ErrorsEmitted> {
        let first_token = parser.current_token().cloned();

        let kw_some = if let Some(Token::Some { .. }) = &first_token {
            parser.next_token();
            Ok(Keyword::Some)
        } else {
            parser.log_unexpected_token("`Some`");
            Err(ErrorsEmitted)
        }?;

        let expression = match parser.current_token() {
            Some(Token::LParen { .. }) => Ok(GroupedExpr::parse(parser)?),
            Some(Token::EOF) | None => {
                parser.log_unexpected_eoi();
                Err(ErrorsEmitted)
            }
            _ => {
                parser.log_unexpected_token("`(`");
                Err(ErrorsEmitted)
            }
        }?;

        let span = parser.get_span(&first_token.unwrap().span(), &expression.span);

        let expr = SomeExpr {
            kw_some,
            expression: Box::new(expression),
            span,
        };

        Ok(expr)
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
            Ok(e) => Ok(println!("{:#?}", e)),
            Err(_) => Err(println!("{:#?}", parser.logger.messages())),
        }
    }

    #[test]
    fn parse_none_expr() -> Result<(), ()> {
        let input = r#"None"#;

        let mut parser = test_utils::get_parser(input, LogLevel::Debug, false);

        let expression = parser.parse_expression(Precedence::Lowest);

        match expression {
            Ok(e) => Ok(println!("{:#?}", e)),
            Err(_) => Err(println!("{:#?}", parser.logger.messages())),
        }
    }
}
