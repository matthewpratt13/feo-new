use crate::{
    ast::{ArrayExpr, AssigneeExpr, Delimiter, Expression},
    error::ErrorsEmitted,
    token::Token,
};

use super::{Parser, Precedence};

impl ArrayExpr {
    pub(crate) fn parse(parser: &mut Parser) -> Result<ArrayExpr, ErrorsEmitted> {
        let open_bracket = if let Some(Token::LBracket { .. }) = parser.consume_token() {
            Ok(Delimiter::LBracket)
        } else {
            parser.log_unexpected_token("`[`".to_string());
            Err(ErrorsEmitted(()))
        }?;

        let mut elements: Vec<AssigneeExpr> = Vec::new();

        loop {
            if let Some(Token::RBracket { .. }) = parser.peek_current() {
                break;
            }

            let element = match parser.parse_expression(Precedence::Lowest) {
                Ok(e) => Ok(e),
                Err(_) => {
                    parser.log_unexpected_token("array element".to_string());
                    Err(ErrorsEmitted(()))
                }
            }?;

            elements.push(element);

            match parser.peek_current() {
                Some(Token::Comma { .. }) => {
                    parser.consume_token();
                    continue;
                }
                Some(Token::RBracket { .. }) => break,

                Some(_) => {
                    parser.log_unexpected_token("`,` or `]`".to_string());
                }

                None => break,
            }
        }

        let close_bracket = if let Some(Token::RBracket { .. }) = parser.consume_token() {
            Ok(Delimiter::RBracket)
        } else {
            parser.log_missing_delimiter(']');
            Err(ErrorsEmitted(()))
        }?;

        Ok(ArrayExpr {
            open_bracket,
            elements_opt: {
                if elements.is_empty() {
                    None
                } else {
                    Some(elements)
                }
            },
            close_bracket,
        })
    }
}

#[cfg(test)]
mod tests {
    use crate::parser::test_utils;

    #[test]
    fn parse_array_expr_empty() -> Result<(), ()> {
        let input = r#"[]"#;

        let mut parser = test_utils::get_parser(input, false);

        let expressions = parser.parse();

        match expressions {
            Ok(t) => Ok(println!("{:#?}", t)),
            Err(_) => Err(println!("{:#?}", parser.errors())),
        }
    }

    #[test]

    fn parse_array_expr_with_elements() -> Result<(), ()> {
        let input = r#"[1, 2, 3]"#;

        let mut parser = test_utils::get_parser(input, false);

        let expressions = parser.parse();

        match expressions {
            Ok(t) => Ok(println!("{:#?}", t)),
            Err(_) => Err(println!("{:#?}", parser.errors())),
        }
    }
}
