use crate::{
    ast::{PathPatt, Pattern, StructPatt},
    error::ErrorsEmitted,
};

use super::Parser;

impl StructPatt {
    pub(crate) fn parse(parser: &mut Parser, path: PathPatt) -> Result<Pattern, ErrorsEmitted> {
        todo!()
    }
}
