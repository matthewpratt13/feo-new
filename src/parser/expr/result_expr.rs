use crate::{
    ast::{Expression, GroupedExpr, Keyword, ResultExpr},
    error::ErrorsEmitted,
    parser::{ParseConstruct, Parser},
    token::Token,
};

impl ParseConstruct for ResultExpr {
    fn parse(parser: &mut Parser) -> Result<Expression, ErrorsEmitted> {
        let kw_ok_or_err = match parser.current_token() {
            Some(Token::Ok { .. }) => Ok(Keyword::Ok),
            Some(Token::Err { .. }) => Ok(Keyword::Err),
            _ => {
                parser.log_unexpected_token("`Ok` or `Err`");
                Err(ErrorsEmitted)
            }
        }?;

        parser.next_token();

        let expression = if let Some(Token::LParen { .. }) = parser.current_token() {
            Ok(Box::new(GroupedExpr::parse(parser)?))
            // TODO: handle `None` case
        } else {
            parser.log_unexpected_token("`(`");
            Err(ErrorsEmitted)
        }?;

        let expr = ResultExpr {
            kw_ok_or_err,
            expression,
        };

        Ok(Expression::ResultExpr(expr))
    }
}

#[cfg(test)]
mod tests {
    use crate::{logger::LogLevel, parser::test_utils};

    #[test]
    fn parse_result_expr() -> Result<(), ()> {
        let input = r#"Ok(foo)"#;

        let mut parser = test_utils::get_parser(input, LogLevel::Debug, false);

        let statements = parser.parse();

        match statements {
            Ok(t) => Ok(println!("{:#?}", t)),
            Err(_) => Err(println!("{:#?}", parser.logger.logs())),
        }
    }
}
