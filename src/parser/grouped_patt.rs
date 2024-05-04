use crate::{
    ast::{GroupedPatt, Pattern},
    error::ErrorsEmitted,
};

use super::Parser;

impl GroupedPatt {
    pub(crate) fn parse(parser: &mut Parser) -> Result<Pattern, ErrorsEmitted> {
        todo!()
    }
}
