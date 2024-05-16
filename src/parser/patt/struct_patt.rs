use crate::{
    ast::{
        Delimiter, Identifier, PathPatt, PathRoot, Pattern, SelfType, StructPatt, StructPattField,
    },
    error::ErrorsEmitted,
    parser::{collection, Parser},
    span::Position,
    token::Token,
};

impl StructPatt {
    pub(crate) fn parse(parser: &mut Parser) -> Result<Pattern, ErrorsEmitted> {
        let path_root = match parser.current_token() {
            Some(Token::Identifier { name, .. }) => {
                Ok(PathRoot::Identifier(Identifier::from(&name)))
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
                Ok(Pattern::StructPatt(StructPatt {
                    struct_path,
                    fields_opt,
                }))
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

    let field_value = parser.parse_pattern()?;

    let field = StructPattField {
        field_name,
        field_value,
    };

    Ok(field)
}
