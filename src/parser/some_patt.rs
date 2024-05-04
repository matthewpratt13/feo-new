
use crate::{
    ast::{SomePatt, Pattern},
    error::ErrorsEmitted,
};

use super::Parser;

impl SomePatt {
    pub(crate) fn parse(parser: &mut Parser) -> Result<Pattern, ErrorsEmitted> {
        todo!()
    }
}
