use crate::{
    ast::{IdentifierPatt, Pattern},
    error::ErrorsEmitted,
};

use super::Parser;

impl IdentifierPatt {
    pub(crate) fn parse(parser: &mut Parser) -> Result<Pattern, ErrorsEmitted> {
        todo!()
    }
}
