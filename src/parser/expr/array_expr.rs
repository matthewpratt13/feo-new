use crate::{
    ast::{ArrayExpr, Delimiter},
    error::ErrorsEmitted,
    parser::{collection, ParseConstructExpr, Parser, Precedence},
    token::Token,
};

use core::fmt;

impl ParseConstructExpr for ArrayExpr {
    fn parse(parser: &mut Parser) -> Result<ArrayExpr, ErrorsEmitted> {
        let first_token = parser.current_token().cloned();

        let open_bracket = match &first_token {
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
                let span = parser.get_span_by_token(&first_token.unwrap());
                parser.next_token();
                Ok(ArrayExpr { elements_opt, span })
            }
            _ => {
                parser.log_unmatched_delimiter(&open_bracket);
                Err(ErrorsEmitted)
            }
        }
    }
}

impl fmt::Debug for ArrayExpr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("ArrayExpr")
            .field("elements_opt", &self.elements_opt)
            .finish()
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
