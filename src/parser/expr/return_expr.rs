use crate::{
    ast::{Keyword, ReturnExpr},
    error::ErrorsEmitted,
    parser::{ParseConstructExpr, Parser, Precedence},
    token::Token,
};

impl ParseConstructExpr for ReturnExpr {
    fn parse(parser: &mut Parser) -> Result<ReturnExpr, ErrorsEmitted> {
        let kw_return = if let Some(Token::Return { .. }) = parser.current_token() {
            parser.next_token();
            Ok(Keyword::Return)
        } else {
            parser.log_unexpected_token("`return`");
            Err(ErrorsEmitted)
        }?;

        let expression_opt = match parser.current_token() {
            Some(Token::RBrace { .. } | Token::Semicolon { .. } | Token::EOF) | None => None,
            _ => Some(Box::new(parser.parse_expression(Precedence::Lowest)?)),
        };

        if let Some(Token::Semicolon { .. }) = parser.current_token() {
            parser.next_token();
        }

        let expr = ReturnExpr {
            kw_return,
            expression_opt,
        };

        Ok(expr)
    }
}

#[cfg(test)]
mod tests {
    use crate::{logger::LogLevel, parser::test_utils};

    #[test]
    fn parse_return_expr() -> Result<(), ()> {
        let input = r#"return (x + 2)"#;

        let mut parser = test_utils::get_parser(input, LogLevel::Debug, false);

        let statements = parser.parse();

        match statements {
            Ok(t) => Ok(println!("{:#?}", t)),
            Err(_) => Err(println!("{:#?}", parser.logger.messages())),
        }
    }
}
