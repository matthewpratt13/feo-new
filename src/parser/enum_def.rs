use crate::{
    ast::{
        Delimiter, EnumDef, EnumVariant, EnumVariantStruct, EnumVariantTuple, EnumVariantType,
        Identifier, OuterAttr, Type, Visibility,
    },
    error::{ErrorsEmitted, ParserErrorKind},
    token::Token,
};

use super::{item::ParseDefinition, Parser};

impl ParseDefinition for EnumDef {
    fn parse(
        parser: &mut Parser,
        attributes: Vec<OuterAttr>,
        visibility: Visibility,
    ) -> Result<EnumDef, ErrorsEmitted> {
        let kw_enum = parser.expect_keyword(Token::Enum {
            name: "enum".to_string(),
            span: parser.stream.span(),
        })?;

        let mut variants: Vec<EnumVariant> = Vec::new();

        let enum_name = if let Some(Token::Identifier { name, .. }) = parser.consume_token() {
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

            let mut attributes: Vec<OuterAttr> = Vec::new();

            while let Some(oa) = parser.get_outer_attr() {
                attributes.push(oa);
                parser.consume_token();
            }

            let variant = EnumVariant::parse(parser, attributes)?;
            variants.push(variant);

            match parser.consume_token() {
                Some(Token::Comma { .. }) => continue,
                Some(Token::RBrace { .. }) => break,
                Some(t) => parser.log_error(ParserErrorKind::UnexpectedToken {
                    expected: "`,` or `}`".to_string(),
                    found: Some(t),
                }),
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

        Ok(EnumDef {
            attributes_opt: if attributes.is_empty() {
                None
            } else {
                Some(attributes)
            },
            visibility,
            kw_enum,
            enum_name,
            open_brace,
            variants,
            close_brace,
        })
    }
}

impl EnumVariant {
    fn parse(
        parser: &mut Parser,
        attributes: Vec<OuterAttr>,
    ) -> Result<EnumVariant, ErrorsEmitted> {
        let token = parser.consume_token();

        let visibility = parser.get_visibility()?;

        let variant_name = if let Some(Token::Identifier { name, .. }) = token {
            Ok(Identifier(name))
        } else {
            parser.log_error(ParserErrorKind::UnexpectedToken {
                expected: "identifier".to_string(),
                found: token,
            });
            Err(ErrorsEmitted)
        }?;

        let token = parser.peek_current();

        let variant_type_opt = if let Some(Token::LBrace { .. }) = token {
            let variant_struct = EnumVariantStruct::parse(parser)?;
            Some(EnumVariantType::Struct(variant_struct))
        } else if let Some(Token::LParen { .. }) = token {
            let variant_tuple = EnumVariantTuple::parse(parser)?;
            Some(EnumVariantType::Tuple(variant_tuple))
        } else {
            None
        };

        if !parser.errors().is_empty() {
            return Err(ErrorsEmitted);
        }

        Ok(EnumVariant {
            attributes_opt: {
                if attributes.is_empty() {
                    None
                } else {
                    Some(attributes)
                }
            },
            visibility,
            variant_name,
            variant_type_opt,
        })
    }
}

impl EnumVariantStruct {
    pub(crate) fn parse(parser: &mut Parser) -> Result<EnumVariantStruct, ErrorsEmitted> {
        let open_brace = if let Some(Token::LBrace { .. }) = parser.consume_token() {
            Ok(Delimiter::LBrace)
        } else {
            parser.log_unexpected_token("`{`".to_string());
            Err(ErrorsEmitted)
        }?;

        let mut fields: Vec<(Identifier, Type)> = Vec::new();

        loop {
            if let Some(Token::RBrace { .. }) = parser.peek_current() {
                break;
            }

            let token = parser.consume_token();

            let field_name = if let Some(Token::Identifier { name, .. }) = token {
                Ok(Identifier(name))
            } else {
                parser.log_error(ParserErrorKind::UnexpectedToken {
                    expected: "identifier".to_string(),
                    found: token,
                });
                Err(ErrorsEmitted)
            }?;

            let _ = parser.expect_separator(Token::Colon {
                punc: ':',
                span: parser.stream.span(),
            })?;

            let field_type = Type::parse(parser)?;
            fields.push((field_name, field_type));

            let token = parser.peek_current();

            match token {
                Some(Token::Comma { .. }) => {
                    parser.consume_token();
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
            Err(ErrorsEmitted)
        }?;

        if fields.is_empty() {
            Ok(EnumVariantStruct {
                open_brace,
                fields_opt: None,
                close_brace,
            })
        } else {
            Ok(EnumVariantStruct {
                open_brace,
                fields_opt: Some(fields),
                close_brace,
            })
        }
    }
}

impl EnumVariantTuple {
    pub(crate) fn parse(parser: &mut Parser) -> Result<EnumVariantTuple, ErrorsEmitted> {
        let open_paren = if let Some(Token::LParen { .. }) = parser.consume_token() {
            Ok(Delimiter::LParen)
        } else {
            parser.log_unexpected_token("`(`".to_string());
            Err(ErrorsEmitted)
        }?;

        let mut element_types: Vec<Type> = Vec::new();

        loop {
            if let Some(Token::RParen { .. }) = parser.peek_current() {
                break;
            }

            let element_type = Type::parse(parser)?;
            element_types.push(element_type);

            let token = parser.peek_current();

            match token {
                Some(Token::Comma { .. }) => {
                    parser.consume_token();
                    continue;
                }
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

        let close_paren = if let Some(Token::RParen { .. }) = parser.peek_current() {
            parser.consume_token();
            Ok(Delimiter::RParen)
        } else {
            parser.log_missing_delimiter(')');
            Err(ErrorsEmitted)
        }?;

        Ok(EnumVariantTuple {
            open_paren,
            element_types,
            close_paren,
        })
    }
}

#[cfg(test)]
mod tests {
    use crate::parser::test_utils;

    #[test]
    fn parse_enum_def() -> Result<(), ()> {
        let input = r#"
        #[error]
        pub enum Foo 
            OrdinaryVariant,
            StructVariant { param1: SomeType, param2: AnotherType },
            TupleVariant(SomeType, AnotherType),
        }"#;

        let mut parser = test_utils::get_parser(input, false);

        let expressions = parser.parse();

        match expressions {
            Ok(t) => Ok(println!("{:#?}", t)),
            Err(_) => Err(println!("{:#?}", parser.errors())),
        }
    }
}
