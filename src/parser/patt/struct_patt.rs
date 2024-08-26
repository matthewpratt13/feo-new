use crate::{
    ast::{
        Identifier, PathPatt, PathRoot, Pattern, SelfType, StructPatt, StructPattField,
        TupleStructPatt,
    },
    error::ErrorsEmitted,
    parser::{collection, ParsePattern, Parser},
    token::{Token, TokenType},
};

impl ParsePattern for StructPatt {
    fn parse_patt(parser: &mut Parser) -> Result<StructPatt, ErrorsEmitted> {
        let path_root = match parser.current_token() {
            Some(Token::Identifier { name, .. }) => {
                Ok(PathRoot::Identifier(Identifier::from(name)))
            }
            Some(Token::SelfType { .. }) => Ok(PathRoot::SelfType(SelfType)),
            _ => {
                parser.log_unexpected_token("identifier or `Self`");
                Err(ErrorsEmitted)
            }
        }?;

        let struct_path = PathPatt {
            path_root,
            tree_opt: None,
        };

        parser.next_token();

        let open_brace = parser.expect_open_brace()?;

        let struct_fields_opt =
            collection::get_collection(parser, parse_struct_patt_field, &open_brace)?;

        let _ = parser.get_braced_item_span(None)?;

        Ok(StructPatt {
            struct_path,
            struct_fields_opt,
        })
    }
}

fn parse_struct_patt_field(parser: &mut Parser) -> Result<StructPattField, ErrorsEmitted> {
    let field_name = parser.expect_identifier()?;

    parser.expect_token(TokenType::Colon)?;

    let field_value = parser.parse_pattern()?;

    Ok(StructPattField {
        field_name,
        field_value,
    })
}

impl ParsePattern for TupleStructPatt {
    fn parse_patt(parser: &mut Parser) -> Result<TupleStructPatt, ErrorsEmitted> {
        let path_root = match parser.current_token() {
            Some(Token::Identifier { name, .. }) => {
                Ok(PathRoot::Identifier(Identifier::from(name)))
            }
            Some(Token::SelfType { .. }) => Ok(PathRoot::SelfType(SelfType)),
            _ => {
                parser.log_unexpected_token("identifier or `Self`");
                Err(ErrorsEmitted)
            }
        }?;

        let struct_path = PathPatt {
            path_root,
            tree_opt: None,
        };

        parser.next_token();

        parser.expect_open_paren()?;

        let struct_elements_opt = parse_tuple_struct_patterns(parser)?;

        let _ = parser.get_parenthesized_item_span(None)?;

        Ok(TupleStructPatt {
            struct_path,
            struct_elements_opt,
        })
    }
}

fn parse_tuple_struct_patterns(parser: &mut Parser) -> Result<Option<Vec<Pattern>>, ErrorsEmitted> {
    let mut patterns: Vec<Pattern> = Vec::new();

    while !matches!(
        parser.current_token(),
        Some(Token::RParen { .. } | Token::EOF),
    ) {
        let pattern = parser.parse_pattern()?;
        patterns.push(pattern);

        if let Some(Token::Comma { .. }) = parser.current_token() {
            parser.next_token();
        } else if !matches!(
            parser.current_token(),
            Some(Token::RParen { .. } | Token::EOF)
        ) {
            parser.log_unexpected_token("`,` or `)`");
            return Err(ErrorsEmitted);
        }
    }

    match patterns.is_empty() {
        true => Ok(None),
        false => Ok(Some(patterns)),
    }
}
