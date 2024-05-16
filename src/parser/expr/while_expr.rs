use crate::{
    ast::{BlockExpr, Expression, GroupedExpr, Keyword, WhileExpr},
    error::ErrorsEmitted,
    parser::{ParseConstruct, ParseControl, Parser},
    token::Token,
};

impl ParseControl for WhileExpr {
    fn parse(parser: &mut Parser) -> Result<Expression, ErrorsEmitted> {
        let kw_while = if let Some(Token::While { .. }) = parser.current_token() {
            parser.next_token();
            Ok(Keyword::While)
        } else {
            parser.log_unexpected_token("`while`");
            Err(ErrorsEmitted)
        }?;

        let condition = match parser.current_token() {
            Some(Token::LParen { .. }) => Ok(Box::new(GroupedExpr::parse(parser)?)),
            Some(Token::EOF) | None => {
                parser.log_missing_token("`(`");
                Err(ErrorsEmitted)
            }
            _ => {
                parser.log_unexpected_token("`(`");
                Err(ErrorsEmitted)
            }
        }?;

        let block = match parser.current_token() {
            Some(Token::LBrace { .. }) => Ok(Box::new(BlockExpr::parse(parser)?)),
            Some(Token::EOF) | None => {
                parser.log_missing_token("`{`");
                Err(ErrorsEmitted)
            }
            _ => {
                parser.log_unexpected_token("`{`");
                Err(ErrorsEmitted)
            }
        }?;

        let expr = WhileExpr {
            kw_while,
            condition,
            block,
        };

        Ok(Expression::While(expr))
    }
}

#[cfg(test)]
mod tests {
    use crate::{logger::LogLevel, parser::test_utils};

    #[test]
    fn parse_while_expr() -> Result<(), ()> {
        let input = r#"
        while (x < 5) {
            x += 1;
        }"#;

        let mut parser = test_utils::get_parser(input, LogLevel::Debug, false);

        let statements = parser.parse();

        match statements {
            Ok(t) => Ok(println!("{:#?}", t)),
            Err(_) => Err(println!("{:#?}", parser.logger.messages())),
        }
    }
}
