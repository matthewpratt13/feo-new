use crate::{
    ast::{Pattern, ReferenceOp, ReferencePatt},
    error::ErrorsEmitted,
    parser::Parser,
};

impl ReferencePatt {
    pub(crate) fn parse(
        parser: &mut Parser,
        reference_op: ReferenceOp,
    ) -> Result<Pattern, ErrorsEmitted> {
        parser.next_token();

        let pattern = Box::new(parser.parse_pattern()?);

        let patt = ReferencePatt {
            reference_op,
            pattern,
        };

        Ok(Pattern::ReferencePatt(patt))
    }
}
