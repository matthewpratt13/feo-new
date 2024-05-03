use crate::{
    ast::{
        Delimiter, EnumDef, EnumVariant, EnumVariantStruct, EnumVariantTuple, EnumVariantType,
        Identifier, OuterAttr, Type, Visibility,
    },
    error::{ErrorsEmitted, ParserErrorKind},
    token::{Token, TokenType},
};

use super::{collection, item::ParseDefinition, Parser};

impl ParseDefinition for EnumDef {
    fn parse(
        parser: &mut Parser,
        attributes_opt: Option<Vec<OuterAttr>>,
        visibility: Visibility,
    ) -> Result<EnumDef, ErrorsEmitted> {
        let kw_enum = parser.expect_keyword(TokenType::Enum)?;

        let mut variants: Vec<EnumVariant> = Vec::new();

        let enum_name = if let Some(Token::Identifier { name, .. }) = parser.next_token() {
            Ok(Identifier(name))
        } else {
            parser.log_unexpected_str("identifier");
            Err(ErrorsEmitted)
        }?;

        let open_brace = if let Some(Token::LBrace { .. }) = parser.next_token() {
            Ok(Delimiter::LBrace)
        } else {
            parser.log_unexpected_token(TokenType::LBrace);
            Err(ErrorsEmitted)
        }?;

        // TODO: replace with `while` loop
        loop {
            if let Some(Token::RBrace { .. }) = parser.current_token() {
                break;
            }

            let attributes_opt =
                collection::get_attributes::<OuterAttr>(parser, OuterAttr::outer_attr);

            let variant = EnumVariant::parse(parser, attributes_opt)?;
            variants.push(variant);

            match parser.current_token() {
                Some(Token::Comma { .. }) => {
                    parser.next_token();
                    continue;
                }
                Some(Token::RBrace { .. }) => break,
                Some(t) => parser.log_error(ParserErrorKind::UnexpectedToken {
                    expected: "`,` or `}`".to_string(),
                    found: Some(t),
                }),
                None => break,
            }
        }

        let close_brace = if let Some(Token::RBrace { .. }) = parser.next_token() {
            Ok(Delimiter::RBrace)
        } else {
            parser.log_error(ParserErrorKind::MissingDelimiter {
                delim: TokenType::RBrace,
            });
            Err(ErrorsEmitted)
        }?;

        Ok(EnumDef {
            attributes_opt,
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
        attributes_opt: Option<Vec<OuterAttr>>,
    ) -> Result<EnumVariant, ErrorsEmitted> {
        let token = parser.next_token();

        let visibility = Visibility::visibility(parser)?;

        let variant_name = if let Some(Token::Identifier { name, .. }) = token {
            Ok(Identifier(name))
        } else {
            parser.log_error(ParserErrorKind::UnexpectedToken {
                expected: "identifier".to_string(),
                found: token,
            });
            Err(ErrorsEmitted)
        }?;

        let token = parser.current_token();

        let variant_type_opt = match token {
            Some(Token::LBrace { .. }) => {
                let variant_struct = EnumVariantStruct::parse(parser)?;
                Some(EnumVariantType::Struct(variant_struct))
            }
            Some(Token::LParen { .. }) => {
                let variant_tuple = EnumVariantTuple::parse(parser)?;
                Some(EnumVariantType::Tuple(variant_tuple))
            }
            _ => None,
        };

        Ok(EnumVariant {
            attributes_opt,
            visibility,
            variant_name,
            variant_type_opt,
        })
    }
}

impl EnumVariantStruct {
    pub(crate) fn parse(parser: &mut Parser) -> Result<EnumVariantStruct, ErrorsEmitted> {
        let open_brace = if let Some(Token::LBrace { .. }) = parser.next_token() {
            Ok(Delimiter::LBrace)
        } else {
            parser.log_unexpected_token(TokenType::LBrace);
            Err(ErrorsEmitted)
        }?;

        let mut fields: Vec<(Identifier, Type)> = Vec::new();

        // TODO: replace with `while loop`
        loop {
            if let Some(Token::RBrace { .. }) = parser.current_token() {
                break;
            }

            let token = parser.next_token();

            let field_name = if let Some(Token::Identifier { name, .. }) = token {
                Ok(Identifier(name))
            } else {
                parser.log_error(ParserErrorKind::UnexpectedToken {
                    expected: "identifier".to_string(),
                    found: token,
                });
                Err(ErrorsEmitted)
            }?;

            parser.expect_separator(TokenType::Colon)?;

            let field_type = Type::parse(parser)?;
            fields.push((field_name, field_type));

            let token = parser.current_token();

            match token {
                Some(Token::Comma { .. }) => {
                    parser.next_token();
                    continue;
                }
                Some(Token::RBrace { .. }) => break,
                Some(t) => parser.log_error(ParserErrorKind::UnexpectedToken {
                    expected: "`,` or `}`".to_string(),
                    found: Some(t),
                }),
                None => {
                    parser.expect_delimiter(TokenType::RBrace)?;
                }
            }
        }

        let close_brace = parser.expect_delimiter(TokenType::RBrace)?;

        match fields.is_empty() {
            true => Ok(EnumVariantStruct {
                open_brace,
                fields_opt: None,
                close_brace,
            }),
            false => Ok(EnumVariantStruct {
                open_brace,
                fields_opt: Some(fields),
                close_brace,
            }),
        }
    }
}

impl EnumVariantTuple {
    pub(crate) fn parse(parser: &mut Parser) -> Result<EnumVariantTuple, ErrorsEmitted> {
        let open_paren = if let Some(Token::LParen { .. }) = parser.next_token() {
            Ok(Delimiter::LParen)
        } else {
            parser.log_unexpected_token(TokenType::LParen);
            Err(ErrorsEmitted)
        }?;

        let element_types =
            if let Some(t) = collection::get_collection(parser, Type::parse, Delimiter::RParen)? {
                Ok(t)
            } else {
                parser.log_unexpected_str("type");
                Err(ErrorsEmitted)
            }?;

        let close_paren = parser.expect_delimiter(TokenType::RParen)?;

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
        pub enum Foo {
            OrdinaryVariant,
            StructVariant { param1: SomeType, param2: AnotherType },
            TupleVariant(SomeType, AnotherType),
        }"#;

        let mut parser = test_utils::get_parser(input, false);

        let statements = parser.parse();

        match statements {
            Ok(t) => Ok(println!("{:#?}", t)),
            Err(_) => Err(println!("{:#?}", parser.errors())),
        }
    }
}
