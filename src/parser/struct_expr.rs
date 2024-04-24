use crate::{
    ast::{
        Delimiter, Expression, Identifier, OuterAttr, PathExpr, StructExpr, StructField,
        TupleStructExpr,
    },
    error::ErrorsEmitted,
    token::Token,
};

use super::{Parser, Precedence};

impl StructExpr {
    pub(crate) fn parse(parser: &mut Parser, path: PathExpr) -> Result<StructExpr, ErrorsEmitted> {
        let open_brace = if let Some(Token::LBrace { .. }) = parser.consume_token() {
            Ok(Delimiter::LBrace)
        } else {
            parser.log_unexpected_token("`{`".to_string());
            Err(ErrorsEmitted(()))
        }?;

        let mut fields: Vec<StructField> = Vec::new();

        loop {
            if let Some(Token::RBrace { .. }) = parser.peek_current() {
                break;
            }

            let mut attributes: Vec<OuterAttr> = Vec::new();

            while let Some(oa) = parser.get_outer_attr() {
                attributes.push(oa);
                parser.consume_token();
            }

            let name = match parser.consume_token() {
                Some(Token::Identifier { name, .. }) => Ok(name),
                Some(Token::RBrace { .. }) => break,
                _ => {
                    parser.log_missing_delimiter('}');
                    Err(ErrorsEmitted(()))
                }
            }?;

            let _ = parser.expect_separator(Token::Colon {
                punc: ':',
                span: parser.stream.span(),
            });

            let value = parser.parse_expression(Precedence::Lowest)?;

            let field = StructField {
                attributes_opt: {
                    if attributes.is_empty() {
                        None
                    } else {
                        Some(attributes)
                    }
                },
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
                _ => break,
            }
        }

        let close_brace = if let Some(Token::RBrace { .. }) = parser.peek_current() {
            parser.consume_token();
            Ok(Delimiter::RBrace)
        } else {
            parser.log_missing_delimiter('}');
            Err(ErrorsEmitted(()))
        }?;

        Ok(StructExpr {
            path,
            open_brace,
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

// TODO: test when issue regarding similarity between `TupleStructExpr` and `CallExpr` is resolved
impl TupleStructExpr {
    pub(crate) fn parse(
        parser: &mut Parser,
        path: PathExpr,
    ) -> Result<TupleStructExpr, ErrorsEmitted> {
        let open_paren = if let Some(Token::LParen { .. }) = parser.consume_token() {
            Ok(Delimiter::LParen)
        } else {
            parser.log_unexpected_token("`(`".to_string());
            Err(ErrorsEmitted(()))
        }?;

        let mut elements: Vec<Expression> = Vec::new();

        loop {
            if let Some(Token::RParen { .. }) = parser.peek_current() {
                break;
            }

            let element = parser.parse_expression(Precedence::Lowest)?;
            elements.push(element);

            match parser.peek_current() {
                Some(Token::Comma { .. }) => {
                    parser.consume_token();
                    continue;
                }
                Some(Token::RParen { .. }) => break,
                _ => break,
            }
        }

        let close_paren = if let Some(Token::RParen { .. }) = parser.peek_current() {
            parser.consume_token();
            Ok(Delimiter::RParen)
        } else {
            parser.log_missing_delimiter(')');
            Err(ErrorsEmitted(()))
        }?;

        Ok(TupleStructExpr {
            path,
            open_paren,
            elements_opt: {
                if elements.is_empty() {
                    None
                } else {
                    Some(elements)
                }
            },
            close_paren,
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
