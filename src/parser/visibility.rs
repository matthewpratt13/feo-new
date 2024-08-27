use super::Parser;

use crate::{
    ast::{Keyword, PubLibVis, Visibility},
    error::ErrorsEmitted,
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
                        parser.expect_open_paren()?;

                        let kw_lib = parser
                            .expect_token(TokenType::Lib)
                            .and_then(|_| Ok(Keyword::Lib))?;

                        parser.expect_closing_paren()?;

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
