use crate::{
    ast::{Delimiter, Expression, GroupedExpr},
    error::ErrorsEmitted,
    logger::{LogLevel, LogMsg},
    parser::{ParseConstruct, Parser, Precedence},
    token::Token,
};

impl ParseConstruct for GroupedExpr {
    fn parse(parser: &mut Parser) -> Result<Expression, ErrorsEmitted> {
        parser.logger.log(
            LogLevel::Debug,
            LogMsg::from("entering `GroupedExpr::parse()`"),
        );
        parser.log_current_token(false);

        let open_paren = if let Some(Token::LParen { .. }) = parser.current_token() {
            parser.next_token();
            Ok(Delimiter::LParen)
        } else {
            parser.log_unexpected_token("`(`");
            Err(ErrorsEmitted)
        }?;

        let expression = parser.parse_expression(Precedence::Lowest)?;

        let close_paren = if let Some(Token::RParen { .. }) = parser.next_token() {
            Ok(Delimiter::RParen)
        } else {
            parser.log_missing_token("`)`");
            parser.log_unmatched_delimiter(&open_paren);
            Err(ErrorsEmitted)
        }?;

        let expr = GroupedExpr {
            open_paren,
            expression: Box::new(expression),
            close_paren,
        };

        parser.logger.log(
            LogLevel::Debug,
            LogMsg::from("exiting `GroupedExpr::parse()`"),
        );
        parser.log_current_token(false);

        Ok(Expression::Grouped(expr))
    }
}

#[cfg(test)]
mod tests {
    use crate::{logger::LogLevel, parser::test_utils};

    #[test]
    fn parse_grouped_expr() -> Result<(), ()> {
        let input = r#"(x + 2)"#;

        let mut parser = test_utils::get_parser(input, LogLevel::Debug, false);

        let statements = parser.parse();

        match statements {
            Ok(t) => Ok(println!("{:#?}", t)),
            Err(_) => Err(println!("{:#?}", parser.logger.logs())),
        }
    }
}
