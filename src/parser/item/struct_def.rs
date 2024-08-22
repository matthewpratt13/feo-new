use super::{collection, parse_generic_params, ParseDefItem, Parser};

use crate::{
    ast::{
        Keyword, OuterAttr, StructDef, StructDefField, TupleStructDef, TupleStructDefField, Type,
        Visibility,
    },
    error::ErrorsEmitted,
    token::{Token, TokenType},
};

use core::fmt;

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
            parser.log_unexpected_token("`struct`");
            Err(ErrorsEmitted)
        }?;

        let struct_name = parser.expect_identifier()?;

        let generic_params_opt = parse_generic_params(parser)?;

        let open_brace = parser.expect_delimiter(TokenType::LBrace).and_then(|d| {
            d.ok_or_else(|| {
                parser.logger.warn(&format!(
                    "bad input to `Parser::expect_delimiter()` function. Expected delimiter token, found {:?}",
                    parser.current_token()
                ));
                ErrorsEmitted
            })
        })?;

        let fields_opt = collection::get_collection(parser, StructDefField::parse, &open_brace)?;

        match parser.current_token() {
            Some(Token::RBrace { .. }) => {
                let span = parser.get_span_by_token(&first_token.unwrap());
                parser.next_token();

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
            parser.log_unexpected_token("`struct`");
            Err(ErrorsEmitted)
        }?;

        let struct_name = parser.expect_identifier()?;

        let generic_params_opt = parse_generic_params(parser)?;

        let open_paren = parser.expect_delimiter(TokenType::LParen).and_then(|d| {
            d.ok_or_else(|| {
                parser.logger.warn(&format!(
                    "bad input to `Parser::expect_delimiter()` function. Expected delimiter token, found {:?}",
                    parser.current_token()
                ));
                ErrorsEmitted
            })
        })?;
        let tuple_struct_fields_opt =
            collection::get_collection(parser, parse_tuple_struct_def_field, &open_paren)?;

        // TODO: replace with `expect_closing_paren()`
        match parser.current_token() {
            Some(Token::RParen { .. }) => {
                parser.next_token();
            }
            Some(Token::EOF) | None => {
                parser.log_unmatched_delimiter(&open_paren);
                parser.log_unexpected_eoi();
                return Err(ErrorsEmitted);
            }
            _ => {
                parser.log_unexpected_token("`)`");
                return Err(ErrorsEmitted);
            }
        }

        match parser.current_token() {
            Some(Token::Semicolon { .. }) => {
                let span = parser.get_span_by_token(&first_token.unwrap());
                parser.next_token();

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
            Some(Token::EOF) | None => {
                parser.log_missing_token("`;`");
                Err(ErrorsEmitted)
            }
            _ => {
                parser.log_unexpected_token("`;`");
                Err(ErrorsEmitted)
            }
        }
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
        let attributes_opt = collection::get_attributes(parser, OuterAttr::outer_attr);

        let visibility = Visibility::visibility(parser)?;

        let field_name = parser.expect_identifier()?;

        parser.expect_token(TokenType::Colon)?;

        let field_type = Box::new(Type::parse(parser)?);

        let field = StructDefField {
            attributes_opt,
            visibility,
            field_name,
            field_type,
        };

        Ok(field)
    }
}

fn parse_tuple_struct_def_field(parser: &mut Parser) -> Result<TupleStructDefField, ErrorsEmitted> {
    let attributes_opt = collection::get_attributes(parser, OuterAttr::outer_attr);

    let visibility = Visibility::visibility(parser)?;

    let field_type = Box::new(Type::parse(parser)?);

    let field = TupleStructDefField {
        attributes_opt,
        visibility,
        field_type,
    };

    Ok(field)
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
            Ok(s) => Ok(println!("{:#?}", s)),
            Err(_) => Err(println!("{:#?}", parser.logger.messages())),
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
            Ok(s) => Ok(println!("{:#?}", s)),
            Err(_) => Err(println!("{:#?}", parser.logger.messages())),
        }
    }

    // TODO: add test for struct def with generics (e.g., `struct Foo<T: Bar, U> { a: T, b: U, … }`)
}
