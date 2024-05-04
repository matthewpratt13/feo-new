use crate::{
    ast::{Pattern, RangePatt},
    error::ErrorsEmitted,
};

use super::Parser;

impl RangePatt {
    pub(crate) fn parse(parser: &mut Parser) -> Result<Pattern, ErrorsEmitted> {
        todo!()
    }

    pub(crate) fn parse_prefix(parser: &mut Parser) -> Result<Pattern, ErrorsEmitted> {
        todo!()
    }
}
