use crate::{
    ast::{ArrayExpr, Delimiter, Expression},
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
                parser.consume_token();
                break;
            }

            let element = parser.parse_expression(Precedence::Lowest)?;
            elements.push(element);

            let token = parser.consume_token();

            match token {
                Some(Token::Comma { .. }) => continue,
                Some(Token::RBracket { .. }) => break,
                Some(t) => {
                    parser.log_error(ParserErrorKind::UnexpectedToken {
                        expected: "`,` or `]`".to_string(),
                        found: t,
                    });
                }
                None => parser.log_error(ParserErrorKind::TokenNotFound {
                    expected: "`]`".to_string(),
                }),
            }
        }

        // while !parser.is_expected_token(&Token::RBracket {
        //     delim: ']',
        //     span: parser.stream.span(),
        // }) {
        //     let element = parser.parse_expression(Precedence::Lowest)?;
        //     elements.push(element);

        //     if !parser.tokens_match(Token::Comma {
        //         punc: ',',
        //         span: parser.stream.span(),
        //     }) {
        //         println!("CURRENT TOKEN: {:#?}", parser.peek_current());
        //         break;
        //     }
        // }

        // let close_bracket = parser.expect_delimiter(Token::RBracket {
        //     delim: ']',
        //     span: parser.stream.span(),
        // });

        if !parser.errors().is_empty() {
            return Err(ErrorsEmitted(()));
        }

        Ok(ArrayExpr {
            open_bracket,
            elements,
            close_bracket: Delimiter::RBracket,
        })
    }
}

#[cfg(test)]
mod tests {
    use crate::parser::test_utils;

    #[test]
    fn parse_array_expr() -> Result<(), ()> {
        let input = r#"[1, 2, 3, 4]~"#;

        let mut parser = test_utils::get_parser(input, true);

        let expressions = parser.parse();

        match expressions {
            Ok(t) => Ok(println!("{:#?}", t)),
            Err(_) => Err(println!("{:#?}", parser.errors())),
        }
    }
}
