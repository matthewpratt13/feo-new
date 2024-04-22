use crate::{
    ast::{Delimiter, Identifier, OuterAttr, StructDef, StructDefField, Visibility},
    error::{ErrorsEmitted, ParserErrorKind},
    token::Token,
};

use super::{item::ParseDefinition, Parser};

impl ParseDefinition for StructDef {
    fn parse(
        parser: &mut Parser,
        attributes: Vec<OuterAttr>,
        visibility: Visibility,
    ) -> Result<StructDef, ErrorsEmitted> {
        let kw_struct = parser.expect_keyword(Token::Struct {
            name: "struct".to_string(),
            span: parser.stream.span(),
        })?;

        let mut fields: Vec<StructDefField> = Vec::new();

        let token = parser.consume_token();

        let struct_name = if let Some(Token::Identifier { name, .. }) = token {
            Ok(Identifier(name))
        } else {
            parser.log_error(ParserErrorKind::UnexpectedToken {
                expected: "identifier".to_string(),
                found: token,
            });
            Err(ErrorsEmitted(()))
        }?;

        let open_brace = if let Some(Token::LBrace { .. }) = parser.consume_token() {
            Ok(Delimiter::LBrace)
        } else {
            parser.log_unexpected_token("`{`".to_string());
            Err(ErrorsEmitted(()))
        }?;

        loop {
            if let Some(Token::RBrace { .. }) = parser.peek_current() {
                break;
            }

            let mut field_attributes: Vec<OuterAttr> = Vec::new();

            while let Some(oa) = parser.get_outer_attr() {
                field_attributes.push(oa);
                parser.consume_token();
            }

            let field_visibility = parser.get_visibility()?;

            let token = parser.consume_token();

            let field_name = if let Some(Token::Identifier { name, .. }) = token {
                Ok(Identifier(name))
            } else {
                parser.log_error(ParserErrorKind::UnexpectedToken {
                    expected: "identifier".to_string(),
                    found: token,
                });
                Err(ErrorsEmitted(()))
            }?;

            let _ = parser.expect_separator(Token::Colon {
                punc: ':',
                span: parser.stream.span(),
            })?;

            let field_type = Box::new(parser.get_type()?);

            let field = if field_attributes.is_empty() {
                Ok(StructDefField {
                    attributes_opt: None,
                    visibility: field_visibility,
                    field_name,
                    field_type,
                })
            } else {
                Ok(StructDefField {
                    attributes_opt: Some(field_attributes),
                    visibility: field_visibility,
                    field_name,
                    field_type,
                })
            }?;

            fields.push(field);

            let token = parser.consume_token();

            match token {
                Some(Token::Comma { .. }) => {
                    continue;
                }
                Some(Token::RBrace { .. }) => break,
                Some(t) => parser.log_error(ParserErrorKind::UnexpectedToken {
                    expected: "`,` or `}`".to_string(),
                    found: Some(t),
                }),
                None => {
                    parser.log_error(ParserErrorKind::MissingDelimiter { delim: '}' });
                }
            }
        }

        let close_brace = if let Some(Token::RBrace { .. }) = parser.peek_current() {
            parser.consume_token();
            Ok(Delimiter::RBrace)
        } else {
            parser.log_missing_delimiter('}');
            Err(ErrorsEmitted(()))
        }?;

        if attributes.is_empty() {
            Ok(StructDef {
                attributes_opt: None,
                visibility,
                kw_struct,
                struct_name,
                open_brace,
                fields,
                close_brace,
            })
        } else {
            Ok(StructDef {
                attributes_opt: Some(attributes),
                visibility,
                kw_struct,
                struct_name,
                open_brace,
                fields,
                close_brace,
            })
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::parser::test_utils;

    #[test]
    fn parse_struct_def() -> Result<(), ()> {
        let input = r#"
        #[event]
        pub struct Foo {
            from: h160,
            #[topic]
            to: h160,
            amount: u256,
        }"#;

        let mut parser = test_utils::get_parser(input, false);

        let expressions = parser.parse();

        match expressions {
            Ok(t) => Ok(println!("{:#?}", t)),
            Err(_) => Err(println!("{:#?}", parser.errors())),
        }
    }
}
