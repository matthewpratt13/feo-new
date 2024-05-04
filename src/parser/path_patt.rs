use crate::{
    ast::{PathPatt, PathPrefix, Pattern},
    error::ErrorsEmitted,
};

use super::Parser;

impl PathPatt {
    pub(crate) fn parse(parser: &mut Parser, root: PathPrefix) -> Result<Pattern, ErrorsEmitted> {
        todo!()
    }
}
