use crate::{
    ast::{
        Delimiter, Identifier, OuterAttr, StructDef, StructDefField, TupleStructDef,
        TupleStructDefField, Type, Visibility,
    },
    error::{ErrorsEmitted, ParserErrorKind},
    token::{Token, TokenType},
};

use super::{collection, parse::ParseDefinition, Parser};

impl ParseDefinition for StructDef {
    fn parse(
        parser: &mut Parser,
        attributes_opt: Option<Vec<OuterAttr>>,
        visibility: Visibility,
    ) -> Result<StructDef, ErrorsEmitted> {
        let kw_struct = parser.expect_keyword(TokenType::Struct)?;

        let struct_name = if let Some(Token::Identifier { name, .. }) = parser.next_token() {
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

        let fields_opt =
            collection::get_collection(parser, parse_struct_def_field, Delimiter::RBrace)?;

        let close_brace = if let Some(Token::RBrace { .. }) = parser.next_token() {
            Ok(Delimiter::RBrace)
        } else {
            parser.log_error(ParserErrorKind::MissingDelimiter {
                delim: TokenType::RBrace,
            });
            Err(ErrorsEmitted)
        }?;

        Ok(StructDef {
            attributes_opt,
            visibility,
            kw_struct,
            struct_name,
            open_brace,
            fields_opt,
            close_brace,
        })
    }
}

impl ParseDefinition for TupleStructDef {
    fn parse(
        parser: &mut Parser,
        attributes_opt: Option<Vec<OuterAttr>>,
        visibility: Visibility,
    ) -> Result<TupleStructDef, ErrorsEmitted> {
        let kw_struct = parser.expect_keyword(TokenType::Struct)?;

        let struct_name = if let Some(Token::Identifier { name, .. }) = parser.current_token() {
            parser.next_token();
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

        let fields_opt =
            collection::get_collection(parser, parse_tuple_struct_def_field, Delimiter::RParen)?;

        let close_paren = if let Some(Token::RParen { .. }) = parser.next_token() {
            Ok(Delimiter::RParen)
        } else {
            parser.log_error(ParserErrorKind::MissingDelimiter {
                delim: TokenType::RParen,
            });
            Err(ErrorsEmitted)
        }?;

        parser.expect_separator(TokenType::Semicolon)?;

        Ok(TupleStructDef {
            attributes_opt,
            visibility,
            kw_struct,
            struct_name,
            open_paren,
            fields_opt,
            close_paren,
        })
    }
}

fn parse_struct_def_field(parser: &mut Parser) -> Result<StructDefField, ErrorsEmitted> {
    let attributes_opt = collection::get_attributes(parser, OuterAttr::outer_attr);

    let visibility = Visibility::visibility(parser)?;

    let token = parser.next_token();

    let field = if let Some(Token::Identifier { name, .. }) = token {
        let field_name = Identifier(name);

        parser.expect_separator(TokenType::Colon)?;

        let field_type = Box::new(Type::parse(parser)?);

        StructDefField {
            attributes_opt,
            visibility,
            field_name,
            field_type,
        }
    } else {
        parser.log_unexpected_str("identifier`");
        return Err(ErrorsEmitted);
    };

    Ok(field)
}

fn parse_tuple_struct_def_field(parser: &mut Parser) -> Result<TupleStructDefField, ErrorsEmitted> {
    let visibility = Visibility::visibility(parser)?;

    let field_type = Box::new(Type::parse(parser)?);

    let field = TupleStructDefField {
        visibility,
        field_type,
    };

    Ok(field)
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
