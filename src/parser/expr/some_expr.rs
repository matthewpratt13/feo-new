use crate::{
    ast::{Expression, GroupedExpr, Keyword, SomeExpr},
    error::ErrorsEmitted,
    parser::{ParseConstruct, Parser},
    token::Token,
};

impl ParseConstruct for SomeExpr {
    fn parse(parser: &mut Parser) -> Result<Expression, ErrorsEmitted> {
        let kw_some = if let Some(Token::Some { .. }) = parser.current_token() {
            parser.next_token();
            Ok(Keyword::Some)
        } else {
            parser.log_unexpected_token("`Some`");
            Err(ErrorsEmitted)
        }?;

        let expression = if let Some(Token::LParen { .. }) = parser.current_token() {
            Ok(Box::new(GroupedExpr::parse(parser)?))
            // TODO: handle `None` case
        } else {
            parser.log_unexpected_token("`(`");
            Err(ErrorsEmitted)
        }?;

        let expr = SomeExpr {
            kw_some,
            expression,
        };

        Ok(Expression::SomeExpr(expr))
    }
}

#[cfg(test)]
mod tests {
    use crate::{logger::LogLevel, parser::test_utils};

    #[test]
    fn parse_some_expr() -> Result<(), ()> {
        let input = r#"Some(foo)"#;

        let mut parser = test_utils::get_parser(input, LogLevel::Debug, false);

        let statements = parser.parse();

        match statements {
            Ok(t) => Ok(println!("{:#?}", t)),
            Err(_) => Err(println!("{:#?}", parser.logger.logs())),
        }
    }

    #[test]
    fn parse_none_expr() -> Result<(), ()> {
        let input = r#"None"#;

        let mut parser = test_utils::get_parser(input, LogLevel::Debug, false);

        let statements = parser.parse();

        match statements {
            Ok(t) => Ok(println!("{:#?}", t)),
            Err(_) => Err(println!("{:#?}", parser.logger.logs())),
        }
    }
}
