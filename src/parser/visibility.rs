use crate::{
    ast::{Delimiter, Keyword, PubPackageVis, Visibility},
    error::ErrorsEmitted,
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
                        parser.next_token();

                        let kw_package = if let Some(Token::Package { .. }) = parser.current_token()
                        {
                            parser.next_token();
                            Ok(Keyword::Package)
                            // TODO: handle `None` case (`UnexpectedEndOfInput`)
                        } else {
                            parser.log_unexpected_token("`package`");
                            Err(ErrorsEmitted)
                        }?;

                        let close_paren = if let Some(Token::RParen { .. }) = parser.next_token() {
                            Ok(Delimiter::RParen)
                        } else {
                            parser.log_missing_token("`)`");
                            parser.log_unmatched_delimiter(&Delimiter::LParen);
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
