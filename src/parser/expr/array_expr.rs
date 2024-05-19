use crate::{
    ast::{ArrayExpr, Delimiter},
    error::ErrorsEmitted,
    parser::{collection, ParseConstructExpr, Parser, Precedence},
    token::Token,
};

impl ParseConstructExpr for ArrayExpr {
    fn parse(parser: &mut Parser) -> Result<ArrayExpr, ErrorsEmitted> {
        let open_bracket = match parser.current_token() {
            Some(Token::LBracket { .. }) => {
                let position = parser.current_position();
                parser.next_token();
                Ok(Delimiter::LBracket { position })
            }
            _ => {
                parser.log_unexpected_token("`[`");
                Err(ErrorsEmitted)
            }
        }?;

        let elements_opt = collection::get_expressions(parser, Precedence::Lowest, &open_bracket)?;

        match parser.current_token() {
            Some(Token::RBracket { .. }) => {
                parser.next_token();
                Ok(ArrayExpr { elements_opt })
            }
            _ => {
                parser.log_unmatched_delimiter(&open_bracket);
                Err(ErrorsEmitted)
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::{
        logger::LogLevel,
        parser::{test_utils, Precedence},
    };

    #[test]
    fn parse_array_expr_empty() -> Result<(), ()> {
        let input = r#"[]"#;

        let mut parser = test_utils::get_parser(input, LogLevel::Debug, false);

        let expression = parser.parse_expression(Precedence::Lowest);

        match expression {
            Ok(e) => Ok(println!("{:#?}", e)),
            Err(_) => Err(println!("{:#?}", parser.logger.messages())),
        }
    }

    #[test]
    fn parse_array_expr_with_elements() -> Result<(), ()> {
        let input = r#"[1, 2, 3]"#;

        let mut parser = test_utils::get_parser(input, LogLevel::Debug, false);

        let expression = parser.parse_expression(Precedence::Lowest);

        match expression {
            Ok(e) => Ok(println!("{:#?}", e)),
            Err(_) => Err(println!("{:#?}", parser.logger.messages())),
        }
    }
}
