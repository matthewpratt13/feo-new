use crate::{
    ast::{RangeOp, RangePatt},
    error::{ErrorsEmitted, ParserErrorKind},
    parser::{ParsePattern, Parser},
    token::Token,
};

impl ParsePattern for RangePatt {
    fn parse_patt(parser: &mut Parser) -> Result<RangePatt, ErrorsEmitted> {
        match parser.current_token() {
            Some(Token::DotDotEquals { .. }) => parse_range_to_incl(parser),
            _ => parse_range_from(parser),
        }
    }
}

/// Parse from- (exclusive) and from-to (inclusive) patterns.
/// I.e., `<patt>..` and `<patt>..=<patt>`.
fn parse_range_from(parser: &mut Parser) -> Result<RangePatt, ErrorsEmitted> {
    let from_pattern = match parser.current_token() {
        Some(
            Token::IntLiteral { .. }
            | Token::UIntLiteral { .. }
            | Token::BigUIntLiteral { .. }
            | Token::ByteLiteral { .. }
            | Token::CharLiteral { .. },
        ) => parser.parse_pattern(),
        _ => {
            parser.log_unexpected_token("numeric or text value, or identifier");
            Err(ErrorsEmitted)
        }
    }?;

    let range_op = match parser.current_token() {
        Some(Token::DblDot { .. }) => Ok(RangeOp::RangeExclusive),
        Some(Token::DotDotEquals { .. }) => Ok(RangeOp::RangeInclusive),
        Some(Token::EOF) | None => {
            parser.log_unexpected_eoi();
            Err(ErrorsEmitted)
        }
        _ => {
            parser.log_unexpected_token("numeric or text value, or identifier");
            Err(ErrorsEmitted)
        }
    }?;

    parser.next_token();

    let to_pattern_opt = match parser.current_token() {
        Some(
            Token::IntLiteral { .. }
            | Token::UIntLiteral { .. }
            | Token::BigUIntLiteral { .. }
            | Token::ByteLiteral { .. }
            | Token::CharLiteral { .. }
            | Token::Identifier { .. },
        ) => {
            let pattern = parser.parse_pattern()?;
            Ok(Some(Box::new(pattern)))
        }

        Some(Token::EOF) | None => {
            if range_op != RangeOp::RangeExclusive {
                parser.log_error(ParserErrorKind::UnexpectedRangeOp {
                    expected: format!("`{}`", RangeOp::RangeExclusive),
                    found: format!("`{}`", range_op),
                });
                Err(ErrorsEmitted)
            } else {
                Ok(None)
            }
        }

        _ => {
            parser.log_unexpected_token("numeric value or identifier");
            Err(ErrorsEmitted)
        }
    }?;

    Ok(RangePatt {
        from_pattern_opt: Some(Box::new(from_pattern)),
        range_op,
        to_pattern_opt,
    })
}

/// Parse a to (inclusive) pattern. I.e., `..=<patt>`.
fn parse_range_to_incl(parser: &mut Parser) -> Result<RangePatt, ErrorsEmitted> {
    let range_op = match parser.current_token() {
        Some(Token::DotDotEquals { .. }) => Ok(RangeOp::RangeInclusive),
        _ => {
            parser.log_unexpected_token("inclusive range operator (`..=`)");
            Err(ErrorsEmitted)
        }
    }?;

    parser.next_token();

    let to_pattern_opt = match parser.current_token() {
        Some(
            Token::IntLiteral { .. }
            | Token::UIntLiteral { .. }
            | Token::BigUIntLiteral { .. }
            | Token::ByteLiteral { .. }
            | Token::CharLiteral { .. }
            | Token::Identifier { .. },
        ) => {
            let pattern = parser.parse_pattern()?;
            Ok(Some(Box::new(pattern)))
        }

        Some(Token::EOF) | None => {
            parser.log_unexpected_eoi();
            Err(ErrorsEmitted)
        }

        _ => {
            parser.log_unexpected_token("numeric value or identifier");
            Err(ErrorsEmitted)
        }
    }?;

    Ok(RangePatt {
        from_pattern_opt: None,
        range_op,
        to_pattern_opt,
    })
}
