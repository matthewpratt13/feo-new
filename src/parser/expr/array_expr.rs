use crate::{
    ast::{ArrayExpr, Delimiter, Expression},
    error::ErrorsEmitted,
    logger::{LogLevel, LogMsg},
    parser::{collection, ParseConstruct, Parser, Precedence},
    token::Token,
};

impl ParseConstruct for ArrayExpr {
    fn parse(parser: &mut Parser) -> Result<Expression, ErrorsEmitted> {
        parser.logger.log(
            LogLevel::Debug,
            LogMsg::from("entering `ArrayExpr::parse()`"),
        );
        parser.log_current_token(true);

        let open_bracket = if let Some(Token::LBracket { .. }) = parser.current_token() {
            parser.next_token();
            Ok(Delimiter::LBracket)
        } else {
            parser.log_unexpected_token("`[`");
            Err(ErrorsEmitted)
        }?;

        let elements_opt =
            collection::get_expressions(parser, Precedence::Lowest, Delimiter::RBracket)?;

        let close_bracket = if let Some(Token::RBracket { .. }) = parser.next_token() {
            Ok(Delimiter::RBracket)
        } else {
            parser.log_missing_token("`]`");
            parser.log_unmatched_delimiter(&open_bracket);
            Err(ErrorsEmitted)
        }?;

        let expr = ArrayExpr {
            open_bracket,
            elements_opt,
            close_bracket,
        };

        parser.logger.log(
            LogLevel::Debug,
            LogMsg::from("exiting `ArrayExpr::parse()`"),
        );
        parser.log_current_token(true);

        Ok(Expression::Array(expr))
    }
}

#[cfg(test)]
mod tests {
    use crate::{logger::LogLevel, parser::test_utils};

    #[test]
    fn parse_array_expr_empty() -> Result<(), ()> {
        let input = r#"[]"#;

        let mut parser = test_utils::get_parser(input, LogLevel::Debug, false);

        let statements = parser.parse();

        match statements {
            Ok(t) => Ok(println!("{:#?}", t)),
            Err(_) => Err(println!("{:#?}", parser.logger.logs())),
        }
    }

    #[test]

    fn parse_array_expr_with_elements() -> Result<(), ()> {
        let input = r#"[1, 2, 3]"#;

        let mut parser = test_utils::get_parser(input, LogLevel::Debug, false);

        let statements = parser.parse();

        match statements {
            Ok(t) => Ok(println!("{:#?}", t)),
            Err(_) => Err(println!("{:#?}", parser.logger.logs())),
        }
    }
}
