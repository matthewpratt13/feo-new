use crate::{
    ast::{
        Delimiter, Identifier, Keyword, OuterAttr, StructDef, StructDefField, TupleStructDef,
        TupleStructDefField, Type, Visibility,
    },
    error::ErrorsEmitted,
    span::Position,
    token::Token,
};

use super::{collection, ParseDefinition, Parser};

impl ParseDefinition for StructDef {
    fn parse(
        parser: &mut Parser,
        attributes_opt: Option<Vec<OuterAttr>>,
        visibility: Visibility,
    ) -> Result<StructDef, ErrorsEmitted> {
        let kw_struct = if let Some(Token::Struct { .. }) = parser.current_token() {
            parser.next_token();
            Ok(Keyword::Struct)
        } else {
            parser.log_unexpected_token("`struct`");
            Err(ErrorsEmitted)
        }?;

        let struct_name = match parser.next_token() {
            Some(Token::Identifier { name, .. }) => Ok(Identifier(name)),
            Some(Token::EOF) | None => {
                parser.log_unexpected_eoi();
                Err(ErrorsEmitted)
            }

            _ => {
                parser.log_unexpected_token("struct name");
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

        let fields_opt = collection::get_collection(parser, StructDefField::parse, &open_brace)?;

        match parser.current_token() {
            Some(Token::RBrace { .. }) => {
                parser.next_token();

                Ok(StructDef {
                    attributes_opt,
                    visibility,
                    kw_struct,
                    struct_name,
                    fields_opt,
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

impl ParseDefinition for TupleStructDef {
    fn parse(
        parser: &mut Parser,
        attributes_opt: Option<Vec<OuterAttr>>,
        visibility: Visibility,
    ) -> Result<TupleStructDef, ErrorsEmitted> {
        let kw_struct = if let Some(Token::Struct { .. }) = parser.current_token() {
            parser.next_token();
            Ok(Keyword::Struct)
        } else {
            parser.log_unexpected_token("`struct`");
            Err(ErrorsEmitted)
        }?;

        let struct_name = if let Some(Token::Identifier { name, .. }) = parser.next_token() {
            Ok(Identifier(name))
        } else {
            parser.log_unexpected_token("struct name");
            Err(ErrorsEmitted)
        }?;

        let open_paren = match parser.current_token() {
            Some(Token::LParen { .. }) => {
                let position = Position::new(parser.current, &parser.stream.span().input());
                parser.next_token();
                Ok(Delimiter::LParen { position })
            }
            Some(Token::EOF) | None => {
                parser.log_missing_token("`(`");
                Err(ErrorsEmitted)
            }
            _ => {
                parser.log_unexpected_token("`(`");
                Err(ErrorsEmitted)
            }
        }?;

        let tuple_struct_fields_opt =
            collection::get_collection(parser, parse_tuple_struct_def_field, &open_paren)?;

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
                parser.next_token();
                Ok(TupleStructDef {
                    attributes_opt,
                    visibility,
                    kw_struct,
                    struct_name,
                    tuple_struct_fields_opt,
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

impl StructDefField {
    pub(crate) fn parse(parser: &mut Parser) -> Result<StructDefField, ErrorsEmitted> {
        let attributes_opt = collection::get_attributes(parser, OuterAttr::outer_attr);

        let visibility = Visibility::visibility(parser)?;

        let field_name = if let Some(Token::Identifier { name, .. }) = parser.current_token() {
            parser.next_token();
            Ok(Identifier(name))
        } else {
            parser.log_missing_token("struct field identifier");
            Err(ErrorsEmitted)
        }?;

        match parser.current_token() {
            Some(Token::Colon { .. }) => {
                parser.next_token();
            }
            Some(Token::EOF) | None => {
                parser.log_missing_token("`:`");
                return Err(ErrorsEmitted);
            }

            _ => {
                parser.log_unexpected_token("`:`");
                return Err(ErrorsEmitted);
            }
        }

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

        let statements = parser.parse();

        match statements {
            Ok(t) => Ok(println!("{:#?}", t)),
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

        let statements = parser.parse();

        match statements {
            Ok(t) => Ok(println!("{:#?}", t)),
            Err(_) => Err(println!("{:#?}", parser.logger.messages())),
        }
    }
}
