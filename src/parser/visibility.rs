use crate::{
    ast::{Delimiter, Keyword, PubPackageVis, Visibility},
    error::ErrorsEmitted,
    token::Token,
};

use super::Parser;

impl Visibility {
    pub(crate) fn visibility(parser: &mut Parser) -> Result<Visibility, ErrorsEmitted> {
        match parser.current_token().as_ref() {
            Some(Token::Pub { .. }) => {
                parser.next_token();

                match parser.current_token().as_ref() {
                    Some(Token::LParen { .. }) => {
                        parser.next_token();

                        let kw_package = match parser.current_token().as_ref() {
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

                        let close_paren = if let Some(Token::RParen { .. }) = parser.next_token() {
                            Ok(Delimiter::RParen)
                        } else {
                            parser.log_unmatched_delimiter(&Delimiter::LParen);
                            parser.log_missing_token("`)`");
                            Err(ErrorsEmitted)
                        }?;

                        let pub_package = PubPackageVis {
                            kw_pub: Keyword::Pub,
                            open_paren: Delimiter::LParen,
                            kw_package,
                            close_paren,
                        };

                        Ok(Visibility::PubPackage(pub_package))
                    }
                    _ => Ok(Visibility::Pub),
                }
            }
            _ => Ok(Visibility::Private),
        }
    }
}
