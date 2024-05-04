use crate::{
    ast::{Pattern, TuplePatt},
    error::ErrorsEmitted,
};

use super::Parser;

impl TuplePatt {
    pub(crate) fn parse(parser: &mut Parser) -> Result<Pattern, ErrorsEmitted> {
        todo!()
    }
}
