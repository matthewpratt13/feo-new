use crate::{
    ast::{ReferenceOp, ReferencePatt},
    error::ErrorsEmitted,
    parser::{ParsePattern, Parser},
    token::Token,
};

impl ParsePattern for ReferencePatt {
    fn parse_patt(parser: &mut Parser) -> Result<ReferencePatt, ErrorsEmitted> {
        let reference_op = match parser.current_token() {
            Some(Token::Ampersand { .. }) => Ok(ReferenceOp::Borrow),
            Some(Token::AmpersandMut { .. }) => Ok(ReferenceOp::MutableBorrow),
            _ => {
                parser.log_unexpected_token("reference operator (`&` or `&mut`");
                Err(ErrorsEmitted)
            }
        }?;

        parser.next_token();

        let pattern = Box::new(parser.parse_pattern()?);

        Ok(ReferencePatt {
            reference_op,
            pattern,
        })
    }
}
