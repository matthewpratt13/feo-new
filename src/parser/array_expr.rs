use crate::{
    ast::{ArrayExpr, Expression},
    error::{ErrorsEmitted, ParserErrorKind},
    token::Token,
};

use super::{Parser, Precedence};

impl ArrayExpr {
    pub(crate) fn parse(parser: &mut Parser) -> Result<ArrayExpr, ErrorsEmitted> {
        let open_bracket = parser.expect_delimiter(Token::LBracket {
            delim: '[',
            span: parser.stream.span(),
        })?;

        let mut elements: Vec<Expression> = Vec::new();

        loop {
            if let Some(Token::RBracket { .. }) = parser.peek_current() {
                break;
            }

            let element = parser.parse_expression(Precedence::Lowest)?;
            elements.push(element);

            match parser.peek_current() {
                Some(Token::Comma { .. }) => {
                    parser.consume_token();
                    continue;
                }
                Some(Token::RBracket { .. }) => break,

                _ => {
                    parser.log_error(ParserErrorKind::MissingDelimiter { delim: ']' });
                }
            }
        }

        let close_bracket = parser.expect_delimiter(Token::RBracket {
            delim: ']',
            span: parser.stream.span(),
        })?;

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
        let input = r#"[1, 2, 3, 4]"#;

        let mut parser = test_utils::get_parser(input, false);

        let expressions = parser.parse();

        match expressions {
            Ok(t) => Ok(println!("{:#?}", t)),
            Err(_) => Err(println!("{:#?}", parser.errors())),
        }
    }
}
