use crate::{
    ast::{
        Delimiter, Expression, Identifier, PathExpr, PathPrefix, StructExpr, StructField,
        TupleStructExpr,
    },
    error::{ErrorsEmitted, ParserErrorKind},
    token::Token,
};

use super::{Parser, Precedence};

impl StructExpr {
    pub(crate) fn parse(
        parser: &mut Parser,
        root: PathPrefix,
    ) -> Result<StructExpr, ErrorsEmitted> {
        let path = PathExpr {
            root,
            tree_opt: None,
            wildcard_opt: None,
        };

        let open_brace = parser.expect_delimiter(Token::LBrace {
            delim: '{',
            span: parser.stream.span(),
        })?;

        let mut fields: Vec<StructField> = Vec::new();

        loop {
            if let Some(Token::RBrace { .. }) = parser.peek_current() {
                break;
            }

            let token = parser.consume_token();

            let name = match token {
                Some(Token::Identifier { name, .. }) => Ok(name),
                Some(Token::RBrace { .. }) => break,
                _ => {
                    parser.log_error(ParserErrorKind::MissingDelimiter { delim: '}' });
                    Err(ErrorsEmitted(()))
                }
            }?;

            let _ = parser.expect_separator(Token::Colon {
                punc: ':',
                span: parser.stream.span(),
            });

            let value = parser.parse_expression(Precedence::Lowest)?;

            let field = StructField {
                name: Identifier(name),
                value,
            };

            fields.push(field);

            match parser.peek_current() {
                Some(Token::Comma { .. }) => {
                    parser.consume_token();
                    continue;
                }
                Some(Token::RBrace { .. }) => break,
                _ => {
                    parser.log_error(ParserErrorKind::MissingDelimiter { delim: '}' });
                }
            }
        }

        let close_brace = parser.expect_delimiter(Token::RBrace {
            delim: '}',
            span: parser.stream.span(),
        })?;

        // if !parser.errors().is_empty() {
        //     return Err(ErrorsEmitted(()));
        // }

        Ok(StructExpr {
            path,
            open_brace: Delimiter::LBrace,
            fields_opt: {
                if fields.is_empty() {
                    None
                } else {
                    Some(fields)
                }
            },
            close_brace,
        })
    }
}

impl TupleStructExpr {
    fn parse(parser: &mut Parser, path: PathExpr) -> Result<Self, ErrorsEmitted> {
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
                Some(t) => parser.log_error(ParserErrorKind::UnexpectedToken {
                    expected: "`,` or `)`".to_string(),
                    found: Some(t),
                }),
                None => {
                    parser.log_error(ParserErrorKind::MissingDelimiter { delim: ')' });
                }
            }
        }

        if !parser.errors().is_empty() {
            return Err(ErrorsEmitted(()));
        }

        if elements.is_empty() {
            Ok(TupleStructExpr {
                path,
                open_paren,
                elements_opt: None,
                close_paren: Delimiter::RParen,
            })
        } else {
            Ok(TupleStructExpr {
                path,
                open_paren,
                elements_opt: Some(elements),
                close_paren: Delimiter::RParen,
            })
        }
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
            baz: -10,
        }"#;

        let mut parser = test_utils::get_parser(input, false);

        let expressions = parser.parse();

        match expressions {
            Ok(t) => Ok(println!("{:#?}", t)),
            Err(_) => Err(println!("{:#?}", parser.errors())),
        }
    }
}
