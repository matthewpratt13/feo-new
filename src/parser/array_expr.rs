use crate::{
    ast::{ArrayExpr, Delimiter, Expression},
    error::ErrorsEmitted,
    token::{Token, TokenType},
};

use super::{Parser, Precedence};

impl ArrayExpr {
    pub(crate) fn parse(parser: &mut Parser) -> Result<Expression, ErrorsEmitted> {
        let open_bracket = if let Some(Token::LBracket { .. }) = parser.consume_token() {
            Ok(Delimiter::LBracket)
        } else {
            parser.log_unexpected_token(TokenType::LBracket);
            Err(ErrorsEmitted)
        }?;

        let mut elements: Vec<Expression> = Vec::new();

        loop {
            if let Some(Token::RBracket { .. }) = parser.peek_current() {
                break;
            }

            let element = match parser.parse_expression(Precedence::Lowest) {
                Ok(e) => Ok(e),
                Err(_) => {
                    parser.log_unexpected_str("array element");
                    Err(ErrorsEmitted)
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
                    parser.log_unexpected_token(TokenType::Comma);
                }

                None => break,
            }
        }

        let close_bracket = parser.expect_delimiter(TokenType::RBracket)?;

        // let close_bracket = if let Some(Token::RBracket { .. }) = parser.consume_token() {
        //     Ok(Delimiter::RBracket)
        // } else {
        //     parser.log_missing_delimiter_foo(TokenType::RBracket);
        //     Err(ErrorsEmitted)
        // }?;

        let expr = ArrayExpr {
            open_bracket,
            elements_opt: {
                if elements.is_empty() {
                    None
                } else {
                    Some(elements)
                }
            },
            close_bracket,
        };

        Ok(Expression::Array(expr))
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
        let input = r#"[1, 2]"#;

        let mut parser = test_utils::get_parser(input, false);

        let expressions = parser.parse();

        match expressions {
            Ok(t) => Ok(println!("{:#?}", t)),
            Err(_) => Err(println!("{:#?}", parser.errors())),
        }
    }
}
