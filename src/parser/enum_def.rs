use crate::{
    ast::{
        EnumDef, EnumVariant, EnumVariantStruct, EnumVariantTuple, EnumVariantType, Identifier,
        OuterAttr, Type, Visibility,
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

        let token = parser.consume_token();

        let enum_name = if let Some(Token::Identifier { name, .. }) = token {
            Ok(Identifier(name))
        } else {
            parser.log_error(ParserErrorKind::UnexpectedToken {
                expected: "identifier".to_string(),
                found: token,
            });
            Err(ErrorsEmitted(()))
        }?;

        let open_brace = parser.expect_delimiter(Token::LBrace {
            delim: '{',
            span: parser.stream.span(),
        })?;

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

            let token = parser.consume_token();

            match token {
                Some(Token::Comma { .. }) => continue,
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

        let close_brace = parser.expect_delimiter(Token::RBrace {
            delim: '}',
            span: parser.stream.span(),
        })?;

        if attributes.is_empty() {
            Ok(EnumDef {
                attributes_opt: None,
                visibility,
                kw_enum,
                enum_name,
                open_brace,
                variants,
                close_brace,
            })
        } else {
            Ok(EnumDef {
                attributes_opt: Some(attributes),
                visibility,
                kw_enum,
                enum_name,
                open_brace,
                variants,
                close_brace,
            })
        }
    }
}

impl EnumVariant {
    fn parse(
        parser: &mut Parser,
        attributes: Vec<OuterAttr>,
    ) -> Result<EnumVariant, ErrorsEmitted> {
        let token = parser.consume_token();

        let variant_name = if let Some(Token::Identifier { name, .. }) = token {
            Ok(Identifier(name))
        } else {
            parser.log_error(ParserErrorKind::UnexpectedToken {
                expected: "identifier".to_string(),
                found: token,
            });
            Err(ErrorsEmitted(()))
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
            return Err(ErrorsEmitted(()));
        }

        if attributes.is_empty() {
            Ok(EnumVariant {
                attributes_opt: None,
                variant_name,
                variant_type_opt,
            })
        } else {
            Ok(EnumVariant {
                attributes_opt: Some(attributes),
                variant_name,
                variant_type_opt,
            })
        }
    }
}

impl EnumVariantStruct {
    pub(crate) fn parse(parser: &mut Parser) -> Result<EnumVariantStruct, ErrorsEmitted> {
        let open_brace = parser.expect_delimiter(Token::LBrace {
            delim: '{',
            span: parser.stream.span(),
        })?;

        let mut fields: Vec<(Identifier, Type)> = Vec::new();

        loop {
            if let Some(Token::RBrace { .. }) = parser.peek_current() {
                break;
            }

            let token = parser.peek_current();

            let field_name = if let Some(Token::Identifier { name, .. }) = token {
                parser.consume_token();
                Ok(Identifier(name))
            } else {
                parser.log_error(ParserErrorKind::UnexpectedToken {
                    expected: "identifier".to_string(),
                    found: token,
                });
                Err(ErrorsEmitted(()))
            }?;

            let token = parser.peek_current();

            if let Some(Token::Colon { .. }) = token {
                parser.consume_token();
            } else {
                parser.log_error(ParserErrorKind::UnexpectedToken {
                    expected: "`:`".to_string(),
                    found: token,
                });
                return Err(ErrorsEmitted(()));
            }

            let field_type = parser.get_type()?;

            fields.push((field_name, field_type));

            match token {
                Some(Token::Comma { .. }) => continue,
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

        let close_brace = parser.expect_delimiter(Token::RBrace {
            delim: '}',
            span: parser.stream.span(),
        })?;

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
        let open_paren = parser.expect_delimiter(Token::LParen {
            delim: '(',
            span: parser.stream.span(),
        })?;

        let mut element_types: Vec<Type> = Vec::new();

        loop {
            if let Some(Token::RParen { .. }) = parser.peek_current() {
                break;
            }

            let element_type = parser.get_type()?;
            element_types.push(element_type);

            let token = parser.peek_current();

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

        let close_paren = parser.expect_delimiter(Token::RParen {
            delim: ')',
            span: parser.stream.span(),
        })?;

        Ok(EnumVariantTuple {
            open_paren,
            element_types,
            close_paren,
        })
    }
}
