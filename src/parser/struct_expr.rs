use crate::{
    ast::{Delimiter, Expression, Identifier, StructExpr, StructField, TupleStructExpr},
    error::{ErrorsEmitted, ParserErrorKind},
    token::Token,
};

use super::{Parser, Precedence};

impl StructExpr {
    pub(crate) fn parse(
        parser: &mut Parser,
        path: Expression,
    ) -> Result<StructExpr, ErrorsEmitted> {
        let mut fields: Vec<StructField> = Vec::new();

        let open_brace = parser.expect_delimiter(Token::LBrace {
            delim: '{',
            span: parser.stream.span(),
        })?;

        loop {
            let token = parser.consume_token();

            let name = match token {
                Some(Token::Identifier { name, .. }) => Ok(name),
                Some(Token::RBrace { .. }) => break,
                Some(t) => {
                    parser.log_error(ParserErrorKind::UnexpectedToken {
                        expected: "identifier or `}`".to_string(),
                        found: t,
                    });
                    Err(ErrorsEmitted(()))
                }
                None => {
                    parser.log_error(ParserErrorKind::UnexpectedEndOfInput);
                    Err(ErrorsEmitted(()))
                }
            };

            let _ = parser.expect_separator(Token::Colon {
                punc: ':',
                span: parser.stream.span(),
            });

            let value = parser.parse_expression(Precedence::Lowest);

            let field = StructField {
                name: Identifier(name?),
                value: value?,
            };

            fields.push(field);

            let token = parser.consume_token();

            match token {
                Some(Token::Comma { .. }) => continue,
                Some(Token::RBrace { .. }) => break,
                Some(t) => {
                    parser.log_error(ParserErrorKind::UnexpectedToken {
                        expected: "`,` or `}`".to_string(),
                        found: t,
                    });
                }
                None => {
                    parser.log_error(ParserErrorKind::UnexpectedEndOfInput);
                }
            }
        }

        let close_brace = parser.expect_delimiter(Token::RBrace {
            delim: '}',
            span: parser.stream.span(),
        });

        if fields.is_empty() {
            parser.log_error(ParserErrorKind::TokenNotFound {
                expected: "struct field".to_string(),
            });
        }

        if !parser.errors().is_empty() {
            return Err(ErrorsEmitted(()));
        }

        Ok(StructExpr {
            path: Box::new(path),
            open_brace,
            fields,
            close_brace: close_brace?,
        })
    }
}

impl TupleStructExpr {
    fn parse(parser: &mut Parser, path: Expression) -> Result<Self, ErrorsEmitted> {
        let mut elements: Vec<Expression> = Vec::new();

        let open_paren = parser.expect_delimiter(Token::LParen {
            delim: '(',
            span: parser.stream.span(),
        })?;

        loop {
            if let Some(Token::RParen { .. }) = parser.peek_current() {
                parser.consume_token();
                break;
            }

            let element = parser.parse_expression(Precedence::Lowest)?;
            elements.push(element);

            let token = parser.consume_token();

            match token {
                Some(Token::Comma { .. }) => continue,
                Some(Token::RParen { .. }) => break,
                Some(t) => {
                    parser.log_error(ParserErrorKind::UnexpectedToken {
                        expected: "`,` or `)`".to_string(),
                        found: t,
                    });
                }
                None => {
                    parser.log_error(ParserErrorKind::UnexpectedEndOfInput);
                }
            }
        }

        if elements.is_empty() {
            parser.log_error(ParserErrorKind::TokenNotFound {
                expected: "tuple struct element".to_string(),
            });
        }

        if !parser.errors().is_empty() {
            return Err(ErrorsEmitted(()));
        }

        Ok(TupleStructExpr {
            path: Box::new(path),
            open_paren,
            elements,
            close_paren: Delimiter::RParen,
        })
    }
}

#[cfg(test)]
mod tests {
    use crate::parser::test_utils;

    #[test]
    fn parse_struct_expr() -> Result<(), ()> {
        let input = r#"
        SomeStruct {
            foo: "bar",
            baz: 10
        }"#;

        let mut parser = test_utils::get_parser(input, true);

        let expressions = parser.parse();

        match expressions {
            Ok(t) => Ok(println!("{:#?}", t)),
            Err(_) => Err(println!("{:#?}", parser.errors())),
        }
    }
}
