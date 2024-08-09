use crate::{
    ast::{Delimiter, OrPatt, Pattern},
    error::ErrorsEmitted,
    parser::{collection, Parser},
    token::Token,
};

impl OrPatt {
    pub(crate) fn parse_patt(
        parser: &mut Parser,
        first_pattern: Box<Pattern>,
    ) -> Result<OrPatt, ErrorsEmitted> {
        ////////////////////////////////////////////////////////////////////////////////
        parser.logger.debug("entering `OrPatt::parse_patt()`…");
        parser.log_current_token(true);
        ////////////////////////////////////////////////////////////////////////////////

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

        ////////////////////////////////////////////////////////////////////////////////
        parser.logger.debug("exiting `OrPatt::parse_patt()`…");
        parser.log_current_token(true);
        ////////////////////////////////////////////////////////////////////////////////

        Ok(OrPatt {
            first_pattern,
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

#[cfg(test)]
mod tests {
    use crate::{logger::LogLevel, parser::test_utils};

    #[test]
    fn parse_or_patt() -> Result<(), ()> {
        let input = r#"a | b | c"#;

        let mut parser = test_utils::get_parser(input, LogLevel::Debug, false);

        let pattern = parser.parse_pattern();

        match pattern {
            Ok(p) => Ok(println!("{:#?}", p)),
            Err(_) => Err(println!("{:#?}", parser.logger.messages())),
        }
    }
}
