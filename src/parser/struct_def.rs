use crate::{
    ast::{
        Delimiter, Identifier, OuterAttr, StructDef, StructDefField, TupleStructDef, TupleStructDefField, Type, Visibility
    },
    error::ErrorsEmitted,
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
            parser.log_unexpected_token("identifier".to_string());
            Err(ErrorsEmitted)
        }?;

        let open_brace = if let Some(Token::LBrace { .. }) = parser.consume_token() {
            Ok(Delimiter::LBrace)
        } else {
            parser.log_unexpected_token("`{`".to_string());
            Err(ErrorsEmitted)
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

                let field_type = Box::new(Type::parse(parser)?);

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
            Err(ErrorsEmitted)
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

impl ParseDefinition for TupleStructDef {
    fn parse(
        parser: &mut Parser,
        attributes: Vec<OuterAttr>,
        visibility: Visibility,
    ) -> Result<TupleStructDef, ErrorsEmitted> {
        let kw_struct = parser.expect_keyword(Token::Struct {
            name: "struct".to_string(),
            span: parser.stream.span(),
        })?;

        let mut fields: Vec<TupleStructDefField> = Vec::new();

        let token = parser.consume_token();

        let struct_name = if let Some(Token::Identifier { name, .. }) = token {
            Ok(Identifier(name))
        } else {
            parser.log_unexpected_token("identifier".to_string());
            Err(ErrorsEmitted)
        }?;

        let open_paren = if let Some(Token::LParen { .. }) = parser.consume_token() {
            Ok(Delimiter::LParen)
        } else {
            parser.log_unexpected_token("`(`".to_string());
            Err(ErrorsEmitted)
        }?;

        loop {
            if let Some(Token::RParen { .. }) = parser.peek_current() {
                break;
            }

            let mut field_attributes: Vec<OuterAttr> = Vec::new();

            while let Some(oa) = parser.get_outer_attr() {
                field_attributes.push(oa);
                parser.consume_token();
            }

            let field_visibility = parser.get_visibility()?;


            let field_type = Box::new(Type::parse(parser)?);

            let field = TupleStructDefField {
                visibility: field_visibility,
                field_type,
            };

            fields.push(field);

            match parser.peek_current() {
                Some(Token::Comma { .. }) => {
                    parser.consume_token();
                    continue;
                }
                Some(Token::RParen { .. }) => break,
                Some(_) => parser.log_unexpected_token("`,` or `)`".to_string()),
                None => break,
            }
        }

        let close_paren = if let Some(Token::RParen { .. }) = parser.peek_current() {
            parser.consume_token();
            Ok(Delimiter::RParen)
        } else {
            parser.log_missing_delimiter(')');
            Err(ErrorsEmitted)
        }?;

        let _ = parser.expect_separator(Token::Semicolon {
            punc: ';',
            span: parser.stream.span(),
        })?;

        Ok(TupleStructDef {
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
            open_paren,
            fields_opt: {
                if fields.is_empty() {
                    None
                } else {
                    Some(fields)
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

    #[test]
    fn parse_tuple_struct_def() -> Result<(), ()> {
        let input = r#"
        #[event]
        pub struct Foo(
            h160,
            h160,
            u256,
        );"#;

        let mut parser = test_utils::get_parser(input, false);

        let expressions = parser.parse();

        match expressions {
            Ok(t) => Ok(println!("{:#?}", t)),
            Err(_) => Err(println!("{:#?}", parser.errors())),
        }
    }
}
