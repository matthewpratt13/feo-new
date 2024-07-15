use core::fmt;

use crate::{
    ast::{Delimiter, Keyword, PubLibVis, Visibility},
    error::ErrorsEmitted,
    span::Position,
    token::Token,
};

use super::Parser;

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

                        let kw_lib = match parser.current_token() {
                            Some(Token::Lib { .. }) => {
                                parser.next_token();
                                Ok(Keyword::Lib)
                            }
                            Some(Token::EOF) | None => {
                                parser.log_unexpected_eoi();
                                Err(ErrorsEmitted)
                            }
                            _ => {
                                parser.log_unexpected_token("`lib`");
                                Err(ErrorsEmitted)
                            }
                        }?;

                        match parser.current_token() {
                            Some(Token::RParen { .. }) => {
                                parser.next_token();

                                let pub_lib = PubLibVis {
                                    kw_pub: Keyword::Pub,
                                    kw_lib,
                                };

                                Ok(Visibility::PubLib(pub_lib))
                            }
                            Some(Token::EOF) | None => {
                                parser.log_unmatched_delimiter(&open_paren);
                                parser.log_unexpected_eoi();
                                Err(ErrorsEmitted)
                            }
                            _ => {
                                parser.log_unexpected_token("`)`");
                                Err(ErrorsEmitted)
                            }
                        }
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
