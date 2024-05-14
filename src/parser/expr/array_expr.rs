use crate::{
    ast::{ArrayExpr, Delimiter, Expression},
    error::ErrorsEmitted,
    parser::{collection, ParseConstruct, Parser, Precedence},
    token::Token,
};

impl ParseConstruct for ArrayExpr {
    fn parse(parser: &mut Parser) -> Result<Expression, ErrorsEmitted> {
        let open_bracket = match parser.current_token() {
            Some(Token::LBracket { .. }) => {
                parser.next_token();
                Ok(Delimiter::LBracket)
            }
            _ => {
                parser.log_unexpected_token("`[`");
                Err(ErrorsEmitted)
            }
        }?;

        let elements_opt =
            collection::get_expressions(parser, Precedence::Lowest, Delimiter::RBracket)?;

        match parser.current_token() {
            Some(Token::RBracket { .. }) => {
                parser.next_token();
                Ok(Expression::Array(ArrayExpr { elements_opt }))
            }
            Some(Token::EOF) | None => {
                parser.log_unmatched_delimiter(&open_bracket);
                parser.log_unexpected_eoi();
                Err(ErrorsEmitted)
            }
            _ => {
                parser.log_unexpected_token("`]`");
                Err(ErrorsEmitted)
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::{logger::LogLevel, parser::test_utils};

    #[test]
    fn parse_array_expr_empty() -> Result<(), ()> {
        let input = r#"["#;

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
