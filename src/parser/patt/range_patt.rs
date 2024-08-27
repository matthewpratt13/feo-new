use crate::{
    ast::{Pattern, RangeOp, RangePatt},
    error::{ErrorsEmitted, ParserErrorKind},
    parser::Parser,
    token::{Token, TokenType},
};

impl RangePatt {
    /// Parse from- (exclusive) and from-to (inclusive) patterns.
    /// I.e., `<patt>..` and `<patt>..=<patt>`.
    pub(crate) fn parse_from(
        parser: &mut Parser,
        from_pattern: Pattern,
    ) -> Result<RangePatt, ErrorsEmitted> {
        let range_op = match parser.current_token() {
            Some(Token::DblDot { .. }) => Ok(RangeOp::RangeExclusive),
            Some(Token::DotDotEquals { .. }) => Ok(RangeOp::RangeInclusive),
            Some(Token::EOF) | None => {
                parser.log_unexpected_eoi();
                Err(ErrorsEmitted)
            }
            _ => {
                parser.log_unexpected_token(&format!(
                    "range operator ({} or {})",
                    TokenType::DblDot,
                    TokenType::DotDotEquals
                ));
                Err(ErrorsEmitted)
            }
        }?;

        let to_pattern_opt = match parser.peek_ahead_by(1) {
            Some(
                Token::IntLiteral { .. }
                | Token::UIntLiteral { .. }
                | Token::BigUIntLiteral { .. }
                | Token::ByteLiteral { .. }
                | Token::CharLiteral { .. }
                | Token::Identifier { .. },
            ) => {
                parser.next_token();
                let pattern = parser.parse_pattern()?;
                Ok(Some(Box::new(pattern)))
            }

            Some(Token::FatArrow { .. } | Token::If { .. }) => {
                parser.next_token();
                Ok(None)
            }

            Some(Token::EOF) | None => {
                parser.next_token();

                if range_op != RangeOp::RangeExclusive {
                    parser.log_error(ParserErrorKind::UnexpectedRangeOp {
                        expected: TokenType::DblDot.to_string(),
                        found: format!("`{}`", range_op),
                    });
                    Err(ErrorsEmitted)
                } else {
                    Ok(None)
                }
            }

            _ => {
                parser.log_unexpected_token(&format!(
                    "numeric, {} or {} literal, identifier, {} or guard statement",
                    TokenType::ByteType,
                    TokenType::CharType,
                    TokenType::FatArrow
                ));
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
    pub(crate) fn parse_to_incl(parser: &mut Parser) -> Result<RangePatt, ErrorsEmitted> {
        let range_op = match parser.current_token() {
            Some(Token::DotDotEquals { .. }) => Ok(RangeOp::RangeInclusive),
            _ => {
                parser.log_unexpected_token(&format!(
                    "inclusive range operator ({})",
                    TokenType::DotDotEquals
                ));
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
                parser.log_unexpected_token(&format!(
                    "numeric, {} or {} literal, or identifier",
                    TokenType::ByteType,
                    TokenType::CharType
                ));
                Err(ErrorsEmitted)
            }
        }?;

        Ok(RangePatt {
            from_pattern_opt: None,
            range_op,
            to_pattern_opt,
        })
    }
}
