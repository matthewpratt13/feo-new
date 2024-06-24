use core::fmt;

use crate::{
    ast::{Delimiter, Keyword, PubPackageVis, Visibility},
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

                        let kw_package = match parser.current_token() {
                            Some(Token::Package { .. }) => {
                                parser.next_token();
                                Ok(Keyword::Package)
                            }
                            Some(Token::EOF) | None => {
                                parser.log_unexpected_eoi();
                                Err(ErrorsEmitted)
                            }
                            _ => {
                                parser.log_unexpected_token("`package`");
                                Err(ErrorsEmitted)
                            }
                        }?;

                        match parser.current_token() {
                            Some(Token::RParen { .. }) => {
                                parser.next_token();

                                let pub_package = PubPackageVis {
                                    kw_pub: Keyword::Pub,
                                    kw_package,
                                };

                                Ok(Visibility::PubPackage(pub_package))
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
            Visibility::PubPackage(_) => write!(f, "pub(package)"),
            Visibility::Pub => write!(f, "pub"),
        }
    }
}
