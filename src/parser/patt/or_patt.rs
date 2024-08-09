use crate::{
    ast::{Delimiter, OrPatt, Pattern},
    error::ErrorsEmitted,
    parser::{collection, ParsePattern, Parser},
    token::Token,
};

impl ParsePattern for OrPatt {
    fn parse_patt(parser: &mut Parser) -> Result<OrPatt, ErrorsEmitted> {
        let first_pattern = parser.parse_pattern()?;

        let first_pipe = match parser.current_token() {
            Some(Token::Pipe { .. }) => {
                let position = parser.current_position();
                parser.next_token();
                Delimiter::Pipe { position }
            }
            _ => {
                return Err(parser.log_unexpected_token("pipe operator"));
            }
        };

        let subsequent_patterns_opt =
            collection::get_collection(parser, parse_pattern, &first_pipe)?;

        Ok(OrPatt {
            first_pattern: Box::new(first_pattern),
            subsequent_patterns: {
                if let Some(patts) = subsequent_patterns_opt {
                    patts
                } else {
                    return Err(parser.log_missing("patt", "additional patterns"));
                }
            },
        })
    }
}

fn parse_pattern(parser: &mut Parser) -> Result<Pattern, ErrorsEmitted> {
    parser.parse_pattern()
}

// TODO: add tests