use crate::{
    ast::{
        Delimiter, EnumDef, EnumVariant, EnumVariantStruct, EnumVariantTuple, EnumVariantType,
        Identifier, OuterAttr, Type, Visibility,
    },
    error::{ErrorsEmitted, ParserErrorKind},
    token::{Token, TokenType},
};

use super::{collection, parse::ParseDefinition, Parser};

impl ParseDefinition for EnumDef {
    fn parse(
        parser: &mut Parser,
        attributes_opt: Option<Vec<OuterAttr>>,
        visibility: Visibility,
    ) -> Result<EnumDef, ErrorsEmitted> {
        let kw_enum = parser.expect_keyword(TokenType::Enum)?;

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

        let mut variants: Vec<EnumVariant> = Vec::new();

        while !matches!(
            parser.current_token(),
            Some(Token::RBrace { .. } | Token::EOF)
        ) {
            let attributes_opt =
                collection::get_attributes::<OuterAttr>(parser, OuterAttr::outer_attr);

            let variant = parse_enum_variant(parser, attributes_opt)?;
            variants.push(variant);

            if let Some(Token::Comma { .. }) = parser.current_token() {
                parser.next_token();
            } else if !matches!(
                parser.current_token(),
                Some(Token::RBrace { .. } | Token::EOF)
            ) {
                parser.log_unexpected_str("`,` or `}`");
                return Err(ErrorsEmitted);
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

fn parse_enum_variant(
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
            let variant_struct = parse_enum_variant_struct(parser)?;
            Some(EnumVariantType::Struct(variant_struct))
        }
        Some(Token::LParen { .. }) => {
            let variant_tuple = parse_enum_variant_tuple(parser)?;
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

fn parse_enum_variant_struct(parser: &mut Parser) -> Result<EnumVariantStruct, ErrorsEmitted> {
    let open_brace = if let Some(Token::LBrace { .. }) = parser.next_token() {
        Ok(Delimiter::LBrace)
    } else {
        parser.log_unexpected_token(TokenType::LBrace);
        Err(ErrorsEmitted)
    }?;

    let fields = parse_enum_variant_struct_fields(parser)?;

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

fn parse_enum_variant_struct_fields(
    parser: &mut Parser,
) -> Result<Vec<(Identifier, Type)>, ErrorsEmitted> {
    let mut fields: Vec<(Identifier, Type)> = Vec::new();

    while !matches!(
        parser.current_token(),
        Some(Token::RBrace { .. } | Token::EOF { .. })
    ) {
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

        if let Some(Token::Comma { .. }) = parser.current_token() {
            parser.next_token();
        } else if !matches!(
            parser.current_token(),
            Some(Token::RBrace { .. } | Token::EOF)
        ) {
            parser.log_unexpected_str("`,` or `}`");
            return Err(ErrorsEmitted);
        }
    }
    Ok(fields)
}

fn parse_enum_variant_tuple(parser: &mut Parser) -> Result<EnumVariantTuple, ErrorsEmitted> {
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
