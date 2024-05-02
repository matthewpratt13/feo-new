use crate::{
    ast::{
        Delimiter, Identifier, OuterAttr, StructDef, StructDefField, TupleStructDef,
        TupleStructDefField, Type, Visibility,
    },
    error::ErrorsEmitted,
    token::{Token, TokenType},
};

use super::{item::ParseDefinition, Parser};

impl ParseDefinition for StructDef {
    fn parse(
        parser: &mut Parser,
        attributes: Vec<OuterAttr>,
        visibility: Visibility,
    ) -> Result<StructDef, ErrorsEmitted> {
        let kw_struct = parser.expect_keyword(TokenType::Struct)?;

        let mut fields: Vec<StructDefField> = Vec::new();

        let token = parser.next_token();

        let struct_name = if let Some(Token::Identifier { name, .. }) = token {
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

        loop {
            if let Some(Token::RBrace { .. }) = parser.current_token() {
                break;
            }

            let mut field_attributes: Vec<OuterAttr> = Vec::new();

            while let Some(oa) = OuterAttr::outer_attr(parser) {
                field_attributes.push(oa);
                parser.next_token();
            }

            let field_visibility = Visibility::visibility(parser)?;

            let token = parser.next_token();

            if let Some(Token::Identifier { name, .. }) = token {
                let field_name = Identifier(name);

                let _ = parser.expect_separator(TokenType::Colon)?;

                let field_type = Box::new(Type::parse(parser)?);

                let field = StructDefField {
                    attributes_opt: {
                        if field_attributes.is_empty() {
                            None
                        } else {
                            Some(field_attributes)
                        }
                    },
                    visibility: field_visibility,
                    field_name,
                    field_type,
                };

                fields.push(field);
            }

            match parser.current_token() {
                Some(Token::Comma { .. }) => {
                    parser.next_token();
                    continue;
                }
                Some(Token::RBrace { .. }) => break,
                Some(_) => parser.log_unexpected_str("`,` or `}`"),
                None => break,
            }
        }

        let close_brace = parser.expect_delimiter(TokenType::RBrace)?;

        Ok(StructDef {
            attributes_opt: {
                if attributes.is_empty() {
                    None
                } else {
                    Some(attributes)
                }
            },
            visibility,
            kw_struct,
            struct_name,
            open_brace,
            fields_opt: {
                if fields.is_empty() {
                    None
                } else {
                    Some(fields)
                }
            },
            close_brace,
        })
    }
}

impl ParseDefinition for TupleStructDef {
    fn parse(
        parser: &mut Parser,
        attributes: Vec<OuterAttr>,
        visibility: Visibility,
    ) -> Result<TupleStructDef, ErrorsEmitted> {
        let kw_struct = parser.expect_keyword(TokenType::Struct)?;

        let mut fields: Vec<TupleStructDefField> = Vec::new();

        let token = parser.next_token();

        let struct_name = if let Some(Token::Identifier { name, .. }) = token {
            Ok(Identifier(name))
        } else {
            parser.log_unexpected_str("identifier");
            Err(ErrorsEmitted)
        }?;

        let open_paren = if let Some(Token::LParen { .. }) = parser.next_token() {
            Ok(Delimiter::LParen)
        } else {
            parser.log_unexpected_token(TokenType::LParen);

            Err(ErrorsEmitted)
        }?;

        loop {
            if let Some(Token::RParen { .. }) = parser.current_token() {
                break;
            }

            let mut field_attributes: Vec<OuterAttr> = Vec::new();

            while let Some(oa) = OuterAttr::outer_attr(parser) {
                field_attributes.push(oa);
                parser.next_token();
            }

            let field_visibility = Visibility::visibility(parser)?;

            let field_type = Box::new(Type::parse(parser)?);

            let field = TupleStructDefField {
                visibility: field_visibility,
                field_type,
            };

            fields.push(field);

            match parser.current_token() {
                Some(Token::Comma { .. }) => {
                    parser.next_token();
                    continue;
                }
                Some(Token::RParen { .. }) => break,
                Some(_) => parser.log_unexpected_str("`,` or `)`"),
                None => break,
            }
        }

        let close_paren = parser.expect_delimiter(TokenType::RParen)?;

        parser.expect_separator(TokenType::Semicolon)?;

        Ok(TupleStructDef {
            attributes_opt: {
                if attributes.is_empty() {
                    None
                } else {
                    Some(attributes)
                }
            },
            visibility,
            kw_struct,
            struct_name,
            open_paren,
            fields_opt: {
                if fields.is_empty() {
                    None
                } else {
                    Some(fields)
                }
            },
            close_paren,
        })
    }
}

#[cfg(test)]
mod tests {
    use crate::parser::test_utils;

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

        let mut parser = test_utils::get_parser(input, false);

        let statements = parser.parse();

        match statements {
            Ok(t) => Ok(println!("{:#?}", t)),
            Err(_) => Err(println!("{:#?}", parser.errors())),
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

        let mut parser = test_utils::get_parser(input, false);

        let statements = parser.parse();

        match statements {
            Ok(t) => Ok(println!("{:#?}", t)),
            Err(_) => Err(println!("{:#?}", parser.errors())),
        }
    }
}
