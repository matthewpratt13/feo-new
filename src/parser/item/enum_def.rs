use crate::{
    ast::{
        Delimiter, EnumDef, EnumVariant, EnumVariantStruct, EnumVariantTuple, EnumVariantType,
        Identifier, Keyword, OuterAttr, StructDefField, Type, Visibility,
    },
    error::ErrorsEmitted,
    parser::{collection, Parser},
    span::Position,
    token::Token,
};

use super::ParseDefItem;

impl ParseDefItem for EnumDef {
    fn parse(
        parser: &mut Parser,
        attributes_opt: Option<Vec<OuterAttr>>,
        visibility: Visibility,
    ) -> Result<EnumDef, ErrorsEmitted> {
        let kw_enum = if let Some(Token::Enum { .. }) = parser.current_token() {
            parser.next_token();
            Ok(Keyword::Enum)
        } else {
            parser.log_unexpected_token("`enum`");
            Err(ErrorsEmitted)
        }?;

        let enum_name = match parser.next_token() {
            Some(Token::Identifier { name, .. }) => Ok(Identifier(name)),
            Some(Token::EOF) | None => {
                parser.log_unexpected_eoi();
                Err(ErrorsEmitted)
            }
            _ => {
                parser.log_unexpected_token("enum identifier");
                Err(ErrorsEmitted)
            }
        }?;

        let open_brace = match parser.current_token() {
            Some(Token::LBrace { .. }) => {
                let position = Position::new(parser.current, &parser.stream.span().input());
                parser.next_token();
                Ok(Delimiter::LBrace { position })
            }
            Some(Token::EOF) | None => {
                parser.log_missing_token("`{`");
                Err(ErrorsEmitted)
            }
            _ => {
                parser.log_unexpected_token("`{`");
                Err(ErrorsEmitted)
            }
        }?;

        let variants = parse_enum_variants(parser)?;

        match parser.current_token() {
            Some(Token::RBrace { .. }) => {
                parser.next_token();

                Ok(EnumDef {
                    attributes_opt,
                    visibility,
                    kw_enum,
                    enum_name,
                    variants,
                })
            }
            Some(Token::EOF) | None => {
                parser.log_unmatched_delimiter(&open_brace);
                parser.log_unexpected_eoi();
                Err(ErrorsEmitted)
            }
            _ => {
                parser.log_unexpected_token("`}`");
                Err(ErrorsEmitted)
            }
        }
    }
}

fn parse_enum_variants(parser: &mut Parser) -> Result<Vec<EnumVariant>, ErrorsEmitted> {
    let mut variants: Vec<EnumVariant> = Vec::new();

    while !matches!(
        parser.current_token(),
        Some(Token::RBrace { .. } | Token::EOF)
    ) {
        let attributes_opt = collection::get_attributes::<OuterAttr>(parser, OuterAttr::outer_attr);

        let variant = parse_enum_variant(parser, attributes_opt)?;
        variants.push(variant);

        if let Some(Token::Comma { .. }) = parser.current_token() {
            parser.next_token();
        } else if !matches!(
            parser.current_token(),
            Some(Token::RBrace { .. } | Token::EOF)
        ) {
            parser.log_unexpected_token("`,` or `}`");
            return Err(ErrorsEmitted);
        }
    }

    Ok(variants)
}

fn parse_enum_variant(
    parser: &mut Parser,
    attributes_opt: Option<Vec<OuterAttr>>,
) -> Result<EnumVariant, ErrorsEmitted> {
    let token = parser.next_token();

    let visibility = Visibility::visibility(parser)?;

    let variant_name = match token {
        Some(Token::Identifier { name, .. }) => Ok(Identifier(name)),
        Some(Token::EOF) | None => {
            parser.log_unexpected_eoi();
            Err(ErrorsEmitted)
        }
        _ => {
            parser.log_unexpected_token("enum variant identifier");
            Err(ErrorsEmitted)
        }
    }?;

    let variant_type_opt = match parser.current_token() {
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
    let open_brace = if let Some(Token::LBrace { .. }) = parser.current_token() {
        let position = Position::new(parser.current, &parser.stream.span().input());
        parser.next_token();
        Ok(Delimiter::LBrace { position })
    } else {
        parser.log_unexpected_token("`{`");
        Err(ErrorsEmitted)
    }?;

    let struct_fields = if let Some(sdf) =
        collection::get_collection(parser, StructDefField::parse, &open_brace)?
    {
        Ok(sdf)
    } else {
        parser.log_missing("item field", "struct field");
        Err(ErrorsEmitted)
    }?;

    match parser.current_token() {
        Some(Token::RBrace { .. }) => {
            parser.next_token();

            Ok(EnumVariantStruct { struct_fields })
        }
        Some(Token::EOF) | None => {
            parser.log_unmatched_delimiter(&open_brace);
            parser.log_missing_token("`}`");
            Err(ErrorsEmitted)
        }
        _ => {
            parser.log_unexpected_token("`}`");
            Err(ErrorsEmitted)
        }
    }
}

fn parse_enum_variant_tuple(parser: &mut Parser) -> Result<EnumVariantTuple, ErrorsEmitted> {
    let open_paren = if let Some(Token::LParen { .. }) = parser.current_token() {
        let position = Position::new(parser.current, &parser.stream.span().input());
        parser.next_token();
        Ok(Delimiter::LParen { position })
    } else {
        parser.log_unexpected_token("`(`");
        Err(ErrorsEmitted)
    }?;

    let element_types =
        if let Some(t) = collection::get_collection(parser, Type::parse, &open_paren)? {
            Ok(t)
        } else {
            parser.log_missing("type", "tuple element type annotation");
            Err(ErrorsEmitted)
        }?;

    match parser.current_token() {
        Some(Token::RParen { .. }) => {
            parser.next_token();

            Ok(EnumVariantTuple { element_types })
        }
        Some(Token::EOF) | None => {
            parser.log_unmatched_delimiter(&open_paren);
            parser.log_missing_token("`)`");
            Err(ErrorsEmitted)
        }
        _ => {
            parser.log_unexpected_token("`)`");
            Err(ErrorsEmitted)
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::{logger::LogLevel, parser::test_utils};

    #[test]
    fn parse_enum_def() -> Result<(), ()> {
        let input = r#"
        #[error]
        pub enum Foo {
            OrdinaryVariant,
            StructVariant { param1: SomeType, param2: AnotherType },
            TupleVariant(SomeType, AnotherType),
        }"#;

        let mut parser = test_utils::get_parser(input, LogLevel::Debug, false);

        let statements = parser.parse();

        match statements {
            Ok(t) => Ok(println!("{:#?}", t)),
            Err(_) => Err(println!("{:#?}", parser.logger.messages())),
        }
    }
}
