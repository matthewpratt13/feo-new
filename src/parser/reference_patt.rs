use crate::{
    ast::{Pattern, ReferenceOp, ReferencePatt},
    error::ErrorsEmitted,
};

use super::Parser;

impl ReferencePatt {
    pub(crate) fn parse(
        parser: &mut Parser,
        reference_op: ReferenceOp,
    ) -> Result<Pattern, ErrorsEmitted> {
        parser.next_token();

        let pattern = parser.parse_pattern()?;

        let patt = ReferencePatt {
            reference_op,
            pattern: Box::new(pattern),
        };

        Ok(Pattern::ReferencePatt(patt))
    }
}
