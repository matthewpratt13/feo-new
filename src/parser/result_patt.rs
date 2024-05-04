use crate::{
    ast::{Pattern, ResultPatt},
    error::ErrorsEmitted,
};

use super::Parser;

impl ResultPatt {
    pub(crate) fn parse(parser: &mut Parser) -> Result<Pattern, ErrorsEmitted> {
        todo!()
    }
}
