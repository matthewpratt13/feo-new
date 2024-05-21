use crate::{
    ast::{
        Delimiter, Identifier, PathPatt, PathRoot, Pattern, SelfType, StructPatt, StructPattField,
        TupleStructPatt,
    },
    error::ErrorsEmitted,
    parser::{collection, ParsePattern, Parser},
    span::Position,
    token::Token,
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

        let open_brace = if let Some(Token::LBrace { .. }) = parser.current_token() {
            let position = Position::new(parser.current, &parser.stream.span().input());
            parser.next_token();
            Ok(Delimiter::LBrace { position })
        } else {
            parser.log_unexpected_token("`{`");
            Err(ErrorsEmitted)
        }?;

        let fields_opt = collection::get_collection(parser, parse_struct_patt_field, &open_brace)?;

        match parser.current_token() {
            Some(Token::RBrace { .. }) => {
                parser.next_token();
                Ok(StructPatt {
                    struct_path,
                    fields_opt,
                })
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
}

fn parse_struct_patt_field(parser: &mut Parser) -> Result<StructPattField, ErrorsEmitted> {
    let field_name = if let Some(Token::Identifier { name, .. }) = parser.current_token().cloned() {
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

    let field_value = parser.parse_pattern()?;

    let field = StructPattField {
        field_name,
        field_value,
    };

    Ok(field)
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

        let tuple_struct_path = PathPatt {
            path_root,
            tree_opt: None,
        };

        parser.next_token();

        let open_paren = if let Some(Token::LParen { .. }) = parser.current_token() {
            let position = Position::new(parser.current, &parser.stream.span().input());
            parser.next_token();
            Ok(Delimiter::LParen { position })
        } else {
            parser.log_unexpected_token("`(`");
            Err(ErrorsEmitted)
        }?;

        let elements_opt = parse_tuple_struct_patterns(parser)?;

        match parser.current_token() {
            Some(Token::RParen { .. }) => {
                parser.next_token();
                Ok(TupleStructPatt {
                    tuple_struct_path,
                    elements_opt,
                })
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
