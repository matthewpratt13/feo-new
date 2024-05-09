use crate::{
    ast::{Expression, Keyword, ReturnExpr},
    error::ErrorsEmitted,
    parser::{ParseConstruct, Parser, Precedence},
    token::Token,
};

impl ParseConstruct for ReturnExpr {
    fn parse(parser: &mut Parser) -> Result<Expression, ErrorsEmitted> {
        let kw_return = if let Some(Token::Return { .. }) = parser.current_token() {
            parser.next_token();
            Ok(Keyword::Return)
        } else {
            parser.log_unexpected_token("`return`");
            Err(ErrorsEmitted)
        }?;

        let expression_opt = match parser.current_token().is_some() {
            true => Some(Box::new(parser.parse_expression(Precedence::Lowest)?)),
            false => None,
        };

        let expr = ReturnExpr {
            kw_return,
            expression_opt,
        };

        Ok(Expression::Return(expr))
    }
}

#[cfg(test)]
mod tests {
    use crate::{logger::LogLevel, parser::test_utils};

    #[test]
    fn parse_return_expr() -> Result<(), ()> {
        let input = r#"return foo"#;

        let mut parser = test_utils::get_parser(input, LogLevel::Debug, false);

        let statements = parser.parse();

        match statements {
            Ok(t) => Ok(println!("{:#?}", t)),
            Err(_) => Err(println!("{:#?}", parser.logger.logs())),
        }
    }
}