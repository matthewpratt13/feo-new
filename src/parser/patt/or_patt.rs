use crate::{
    ast::{Delimiter, OrPatt, Pattern},
    error::ErrorsEmitted,
    parser::{get_collection, Parser},
    token::{Token, TokenType},
};

impl OrPatt {
    pub(crate) fn parse_patt(
        parser: &mut Parser,
        first_pattern: Box<Pattern>,
    ) -> Result<OrPatt, ErrorsEmitted> {
        let first_pipe = match parser.current_token() {
            Some(Token::Pipe { .. }) => {
                let position = parser.current_position();
                parser.next_token();
                Delimiter::Pipe { position }
            }
            _ => {
                parser.emit_unexpected_token(&format!("pipe operator ({})", TokenType::Pipe));
                return Err(ErrorsEmitted);
            }
        };

        let subsequent_patterns_opt = get_collection(parser, parse_pattern, &first_pipe)?;

        Ok(OrPatt {
            first_pattern,
            subsequent_patterns: {
                if let Some(patts) = subsequent_patterns_opt {
                    patts
                } else {
                    parser.emit_missing_node("patt", "additional patterns");
                    parser.next_token();
                    return Err(ErrorsEmitted);
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

        match parser.parse_pattern() {
            Ok(p) => Ok(println!("{:#?}", p)),
            Err(_) => Err(println!("{:#?}", parser.errors)),
        }
    }
}
