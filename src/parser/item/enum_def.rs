use core::fmt;

use crate::{
    ast::{
        EnumDef, EnumVariant, EnumVariantKind, EnumVariantStruct, EnumVariantTupleStruct,
        Identifier, Keyword, OuterAttr, StructDefField, Type, TypePath, Visibility,
    },
    error::ErrorsEmitted,
    parser::{get_attributes, get_collection, ParseDefItem, Parser},
    token::{Token, TokenType},
};

use super::parse_generic_params;

impl ParseDefItem for EnumDef {
    fn parse(
        parser: &mut Parser,
        attributes_opt: Option<Vec<OuterAttr>>,
        visibility: Visibility,
    ) -> Result<EnumDef, ErrorsEmitted> {
        let first_token = parser.current_token().cloned();

        let kw_enum = if let Some(Token::Enum { .. }) = &first_token {
            parser.next_token();
            Ok(Keyword::Enum)
        } else {
            parser.emit_unexpected_token(&TokenType::Enum.to_string());
            Err(ErrorsEmitted)
        }?;

        let enum_name = parser.expect_identifier("enum name")?;

        let generic_params_opt = parse_generic_params(parser)?;

        parser.expect_open_brace()?;

        let variants = parse_enum_variants(parser, enum_name.clone())?;

        let span = parser.get_braced_item_span(first_token.as_ref())?;

        Ok(EnumDef {
            attributes_opt,
            visibility,
            kw_enum,
            enum_name,
            generic_params_opt,
            variants,
            span,
        })
    }
}

impl fmt::Debug for EnumDef {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("EnumDef")
            .field("attributes_opt", &self.attributes_opt)
            .field("visibility", &self.visibility)
            .field("enum_name", &self.enum_name)
            .field("variants", &self.variants)
            .finish()
    }
}

fn parse_enum_variants(
    parser: &mut Parser,
    enum_name: Identifier,
) -> Result<Vec<EnumVariant>, ErrorsEmitted> {
    let mut variants: Vec<EnumVariant> = Vec::new();

    while !matches!(
        parser.current_token(),
        Some(Token::RBrace { .. } | Token::EOF)
    ) {
        let attributes_opt = get_attributes::<OuterAttr>(parser, OuterAttr::outer_attr);

        let variant = parse_enum_variant(parser, attributes_opt, enum_name.clone())?;
        variants.push(variant);

        if let Some(Token::Comma { .. }) = parser.current_token() {
            parser.next_token();
        } else if !matches!(
            parser.current_token(),
            Some(Token::RBrace { .. } | Token::EOF)
        ) {
            parser.emit_unexpected_token(&format!("{} or {}", TokenType::Comma, TokenType::RBrace));
            return Err(ErrorsEmitted);
        }
    }

    Ok(variants)
}

fn parse_enum_variant(
    parser: &mut Parser,
    attributes_opt: Option<Vec<OuterAttr>>,
    enum_name: Identifier,
) -> Result<EnumVariant, ErrorsEmitted> {
    let visibility = Visibility::visibility(parser)?;

    let variant_name = parser.expect_identifier("enum variant name")?;

    let variant_path = TypePath::from(vec![enum_name, variant_name.clone()]);

    let variant_kind = match parser.current_token() {
        Some(Token::LBrace { .. }) => {
            let variant_struct = parse_enum_variant_struct(parser)?;
            EnumVariantKind::Struct(variant_struct)
        }
        Some(Token::LParen { .. }) => {
            let variant_tuple = parse_enum_variant_tuple(parser)?;
            EnumVariantKind::TupleStruct(variant_tuple)
        }
        _ => EnumVariantKind::Standard,
    };

    Ok(EnumVariant {
        variant_path,
        attributes_opt,
        visibility,
        variant_kind,
    })
}

fn parse_enum_variant_struct(parser: &mut Parser) -> Result<EnumVariantStruct, ErrorsEmitted> {
    let open_brace = parser.expect_open_brace()?;

    let struct_fields =
        if let Some(sdf) = get_collection(parser, StructDefField::parse, &open_brace)? {
            Ok(sdf)
        } else {
            parser.emit_missing_node("identifier", "struct field");
            parser.next_token();
            Err(ErrorsEmitted)
        }?;

    let _ = parser.get_braced_item_span(None);

    Ok(EnumVariantStruct { struct_fields })
}

fn parse_enum_variant_tuple(parser: &mut Parser) -> Result<EnumVariantTupleStruct, ErrorsEmitted> {
    let open_paren = parser.expect_open_paren()?;

    let element_types = if let Some(t) = get_collection(parser, Type::parse, &open_paren)? {
        Ok(t)
    } else {
        parser.emit_missing_node("type", "tuple element type");
        parser.next_token();
        Err(ErrorsEmitted)
    }?;

    let _ = parser.get_parenthesized_item_span(None);

    Ok(EnumVariantTupleStruct { element_types })
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

        let statement = parser.parse_statement();

        match statement {
            Ok(stmt) => Ok(println!("{stmt:#?}")),
            Err(_) => Err(println!("{:#?}", parser.errors)),
        }
    }

    // TODO: add test for enum def with generics (e.g., `enum Foo<T> { Variant<T>, … }`)
}
