use crate::{
    ast::{
        Delimiter, EnumDef, EnumVariant, EnumVariantStruct, EnumVariantTuple, EnumVariantType,
        Identifier, Keyword, OuterAttr, StructDefField, Type, Visibility,
    },
    error::ErrorsEmitted,
    token::Token,
};

use super::{collection, parse::ParseDefinition, Parser};

impl ParseDefinition for EnumDef {
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

        let enum_name = if let Some(Token::Identifier { name, .. }) = parser.next_token() {
            Ok(Identifier(name))
        } else {
            parser.log_unexpected_token("identifier");
            Err(ErrorsEmitted)
        }?;

        let open_brace = if let Some(Token::LBrace { .. }) = parser.next_token() {
            Ok(Delimiter::LBrace)
        } else {
            parser.log_unexpected_token("`{`");
            Err(ErrorsEmitted)
        }?;

        let variants = parse_enum_variants(parser)?;

        let close_brace = if let Some(Token::RBrace { .. }) = parser.next_token() {
            Ok(Delimiter::RBrace)
        } else {
            parser.log_missing_token("`}`");
            parser.log_unmatched_delimiter(open_brace.clone());
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

    let variant_name = if let Some(Token::Identifier { name, .. }) = token {
        Ok(Identifier(name))
    } else {
        parser.log_unexpected_token("identifier");
        Err(ErrorsEmitted)
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
    let open_brace = if let Some(Token::LBrace { .. }) = parser.next_token() {
        Ok(Delimiter::LBrace)
    } else {
        parser.log_unexpected_token("`{`");
        Err(ErrorsEmitted)
    }?;

    let fields = if let Some(sdf) =
        collection::get_collection(parser, StructDefField::parse, Delimiter::RBrace)?
    {
        Ok(sdf)
    } else {
        parser.log_missing_token("struct definition field");
        Err(ErrorsEmitted)
    }?;

    let close_brace = if let Some(Token::RBrace { .. }) = parser.next_token() {
        Ok(Delimiter::RBrace)
    } else {
        parser.log_missing_token("`}`");
        parser.log_unmatched_delimiter(open_brace.clone());
        Err(ErrorsEmitted)
    }?;

    Ok(EnumVariantStruct {
        open_brace,
        fields,
        close_brace,
    })
}

fn parse_enum_variant_tuple(parser: &mut Parser) -> Result<EnumVariantTuple, ErrorsEmitted> {
    let open_paren = if let Some(Token::LParen { .. }) = parser.next_token() {
        Ok(Delimiter::LParen)
    } else {
        parser.log_unexpected_token("`(`");
        Err(ErrorsEmitted)
    }?;

    let element_types =
        if let Some(t) = collection::get_collection(parser, Type::parse, Delimiter::RParen)? {
            Ok(t)
        } else {
            parser.log_unexpected_token("type");
            Err(ErrorsEmitted)
        }?;

    let close_paren = if let Some(Token::RParen { .. }) = parser.next_token() {
        Ok(Delimiter::RParen)
    } else {
        parser.log_missing_token("`)`");
        parser.log_unmatched_delimiter(open_paren.clone());
        Err(ErrorsEmitted)
    }?;

    Ok(EnumVariantTuple {
        open_paren,
        element_types,
        close_paren,
    })
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
            Err(_) => Err(println!("{:#?}", parser.logger.logs())),
        }
    }
}
