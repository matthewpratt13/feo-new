use crate::{
    ast::{Delimiter, Identifier, PathPatt, Pattern, StructPatt, StructPattField},
    error::ErrorsEmitted,
    parser::{collection, Parser},
    token::Token,
};

impl StructPatt {
    pub(crate) fn parse(parser: &mut Parser, path: PathPatt) -> Result<Pattern, ErrorsEmitted> {
        let open_brace = if let Some(Token::LBrace { .. }) = parser.current_token() {
            parser.next_token();
            Ok(Delimiter::LBrace)
        } else {
            parser.log_unexpected_token("`{`");
            Err(ErrorsEmitted)
        }?;

        let fields_opt =
            collection::get_collection(parser, parse_struct_patt_field, Delimiter::RBrace)?;

        let close_brace = if let Some(Token::RBrace { .. }) = parser.next_token() {
            Ok(Delimiter::RBrace)
        } else {
            parser.log_missing_token("`}`");
            parser.log_unmatched_delimiter(open_brace.clone());
            Err(ErrorsEmitted)
        }?;

        let patt = StructPatt {
            path,
            open_brace,
            fields_opt,
            close_brace,
        };

        Ok(Pattern::StructPatt(patt))
    }
}

fn parse_struct_patt_field(parser: &mut Parser) -> Result<StructPattField, ErrorsEmitted> {
    let field_name = if let Some(Token::Identifier { name, .. }) = parser.current_token() {
        parser.next_token();
        Ok(Identifier(name))
    } else {
        parser.log_missing_token("identifier");
        Err(ErrorsEmitted)
    }?;

    match parser.next_token() {
        Some(Token::Colon { .. }) => (),
        Some(_) => parser.log_unexpected_token("`:`"),
        None => parser.log_missing_token("`:`"),
    }

    let field_value = parser.parse_pattern()?;

    let field = StructPattField {
        field_name,
        field_value,
    };

    Ok(field)
}
