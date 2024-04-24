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

            if let Some(Token::Identifier { name, .. }) = token {
                let field_name = Identifier(name);

                let _ = parser.expect_separator(Token::Colon {
                    punc: ':',
                    span: parser.stream.span(),
                })?;

                let field_type = Box::new(parser.get_type()?);

                let field = StructDefField {
                    attributes_opt: {
                        if field_attributes.is_empty() {
                            None
                        } else {
                            Some(field_attributes)
                        }
                    },
                    visibility: field_visibility,
                    field_name,
                    field_type,
                };

                fields.push(field);
            }

            match parser.peek_current() {
                Some(Token::Comma { .. }) => {
                    parser.consume_token();
                    continue;
                }
                Some(Token::RBrace { .. }) => break,
                Some(_) => parser.log_unexpected_token("`,` or `}`".to_string()),
                None => break,
            }
        }

        let close_brace = if let Some(Token::RBrace { .. }) = parser.peek_current() {
            parser.consume_token();
            Ok(Delimiter::RBrace)
        } else {
            parser.log_missing_delimiter('}');
            Err(ErrorsEmitted(()))
        }?;

        Ok(StructDef {
            attributes_opt: {
                if attributes.is_empty() {
                    None
                } else {
                    Some(attributes)
                }
            },
            visibility,
            kw_struct,
            struct_name,
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
