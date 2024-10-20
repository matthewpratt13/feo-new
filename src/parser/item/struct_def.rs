use core::fmt;

use crate::{
    ast::{
        Keyword, OuterAttr, StructDef, StructDefField, TupleStructDef, TupleStructDefField, Type,
        Visibility,
    },
    error::ErrorsEmitted,
    parser::{get_attributes, get_collection, ParseDefItem, Parser},
    token::{Token, TokenType},
};

use super::parse_generic_params;

impl ParseDefItem for StructDef {
    fn parse(
        parser: &mut Parser,
        attributes_opt: Option<Vec<OuterAttr>>,
        visibility: Visibility,
    ) -> Result<StructDef, ErrorsEmitted> {
        let first_token = parser.current_token().cloned();

        let kw_struct = if let Some(Token::Struct { .. }) = &first_token {
            parser.next_token();
            Ok(Keyword::Struct)
        } else {
            parser.emit_unexpected_token(&TokenType::Struct.to_string());
            Err(ErrorsEmitted)
        }?;

        let struct_name = parser.expect_identifier("struct name")?;

        let generic_params_opt = parse_generic_params(parser)?;

        let open_brace = parser.expect_open_brace()?;

        let fields_opt = get_collection(parser, StructDefField::parse, &open_brace)?;

        let span = parser.get_braced_item_span(first_token.as_ref())?;

        Ok(StructDef {
            attributes_opt,
            visibility,
            kw_struct,
            struct_name,
            generic_params_opt,
            fields_opt,
            span,
        })
    }
}

impl fmt::Debug for StructDef {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("StructDef")
            .field("attributes_opt", &self.attributes_opt)
            .field("visibility", &self.visibility)
            .field("struct_name", &self.struct_name)
            .field("fields_opt", &self.fields_opt)
            .finish()
    }
}

impl ParseDefItem for TupleStructDef {
    fn parse(
        parser: &mut Parser,
        attributes_opt: Option<Vec<OuterAttr>>,
        visibility: Visibility,
    ) -> Result<TupleStructDef, ErrorsEmitted> {
        let first_token = parser.current_token().cloned();

        let kw_struct = if let Some(Token::Struct { .. }) = &first_token {
            parser.next_token();
            Ok(Keyword::Struct)
        } else {
            parser.emit_unexpected_token(&TokenType::Struct.to_string());
            Err(ErrorsEmitted)
        }?;

        let struct_name = parser.expect_identifier("struct name")?;

        let generic_params_opt = parse_generic_params(parser)?;

        let open_paren = parser.expect_open_paren()?;

        let tuple_struct_fields_opt =
            get_collection(parser, parse_tuple_struct_def_field, &open_paren)?;

        parser.expect_closing_paren()?;

        let span = parser.get_decl_item_span(first_token.as_ref())?;

        Ok(TupleStructDef {
            attributes_opt,
            visibility,
            kw_struct,
            struct_name,
            generic_params_opt,
            fields_opt: tuple_struct_fields_opt,
            span,
        })
    }
}

impl fmt::Debug for TupleStructDef {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("TupleStructDef")
            .field("attributes_opt", &self.attributes_opt)
            .field("visibility", &self.visibility)
            .field("struct_name", &self.struct_name)
            .field("elements_opt", &self.fields_opt)
            .finish()
    }
}

impl StructDefField {
    pub(crate) fn parse(parser: &mut Parser) -> Result<StructDefField, ErrorsEmitted> {
        let attributes_opt = get_attributes(parser, OuterAttr::outer_attr);

        let visibility = Visibility::visibility(parser)?;

        let field_name = parser.expect_identifier("struct field name")?;

        parser.expect_token(TokenType::Colon)?;

        let field_type = match parser.current_token() {
            Some(Token::Comma { .. }) => {
                parser.emit_missing_node("type", "struct field type");
                parser.next_token();
                Err(ErrorsEmitted)
            }
            Some(Token::RBrace { .. }) => {
                parser.emit_missing_node("type", "struct field type");
                Err(ErrorsEmitted)
            }
            Some(Token::EOF) | None => {
                parser.emit_unexpected_eoi();
                parser.emit_missing_node("type", "struct field type");
                Err(ErrorsEmitted)
            }
            _ => Type::parse(parser),
        }?;

        Ok(StructDefField {
            attributes_opt,
            visibility,
            field_name,
            field_type: Box::new(field_type),
        })
    }
}

fn parse_tuple_struct_def_field(parser: &mut Parser) -> Result<TupleStructDefField, ErrorsEmitted> {
    let attributes_opt = get_attributes(parser, OuterAttr::outer_attr);

    let visibility = Visibility::visibility(parser)?;

    let field_type = match parser.current_token() {
        Some(Token::Comma { .. } | Token::RParen { .. }) => {
            parser.emit_missing_node("type", "struct field type");
            parser.next_token();
            Err(ErrorsEmitted)
        }
        Some(Token::Semicolon { .. }) => {
            parser.emit_missing_node("type", "struct field type");
            Err(ErrorsEmitted)
        }
        Some(Token::EOF) | None => {
            parser.emit_unexpected_eoi();
            parser.emit_missing_node("type", "struct field type");
            Err(ErrorsEmitted)
        }
        _ => Type::parse(parser),
    }?;

    Ok(TupleStructDefField {
        attributes_opt,
        visibility,
        field_type: Box::new(field_type),
    })
}

#[cfg(test)]
mod tests {
    use crate::{logger::LogLevel, parser::test_utils};

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

        let mut parser = test_utils::get_parser(input, LogLevel::Debug, false);

        let statement = parser.parse_statement();

        match statement {
            Ok(stmt) => Ok(println!("{stmt:#?}")),
            Err(_) => Err(println!("{:#?}", parser.errors)),
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

        let mut parser = test_utils::get_parser(input, LogLevel::Debug, false);

        let statement = parser.parse_statement();

        match statement {
            Ok(stmt) => Ok(println!("{stmt:#?}")),
            Err(_) => Err(println!("{:#?}", parser.errors)),
        }
    }

    // TODO: add test for struct def with generics (e.g., `struct Foo<T: Bar, U> { a: T, b: U, … }`)
}
