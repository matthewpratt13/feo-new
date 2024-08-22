use super::Parser;

use crate::{
    ast::{Delimiter, Keyword, PubLibVis, Visibility},
    error::ErrorsEmitted,
    span::Position,
    token::{Token, TokenType},
};

use core::fmt;

impl Visibility {
    pub(crate) fn visibility(parser: &mut Parser) -> Result<Visibility, ErrorsEmitted> {
        match parser.current_token() {
            Some(Token::Pub { .. }) => {
                parser.next_token();

                match parser.current_token() {
                    Some(Token::LParen { .. }) => {
                        let open_paren = Delimiter::LParen {
                            position: Position::new(parser.current, &parser.stream.span().input()),
                        };
                        parser.next_token();

                        let kw_lib = parser
                            .expect_token(TokenType::Lib)
                            .and_then(|_| Ok(Keyword::Lib))?;

                        parser.expect_closing_paren(&open_paren)?;

                        let pub_lib = PubLibVis {
                            kw_pub: Keyword::Pub,
                            kw_lib,
                        };

                        Ok(Visibility::PubLib(pub_lib))
                    }
                    _ => Ok(Visibility::Pub),
                }
            }
            _ => Ok(Visibility::Private),
        }
    }
}

impl fmt::Display for Visibility {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Visibility::Private => write!(f, ""),
            Visibility::PubLib(_) => write!(f, "pub(lib) "),
            Visibility::Pub => write!(f, "pub "),
        }
    }
}
