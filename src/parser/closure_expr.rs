use crate::{
    ast::{BinaryOp, ClosureExpr, ClosureParam, ClosureParams, Identifier, Separator},
    error::{ErrorsEmitted, ParserErrorKind},
    token::Token,
};

use super::{Parser, Precedence};

impl ClosureExpr {
    pub(crate) fn parse(parser: &mut Parser) -> Result<ClosureExpr, ErrorsEmitted> {
        let token = parser.consume_token();

        let params = match token {
            Some(Token::Pipe { .. }) => {
                let mut vec: Vec<ClosureParam> = Vec::new();

                let curr_token = parser.peek_current().ok_or({
                    parser.log_error(ParserErrorKind::UnexpectedEndOfInput);
                    ErrorsEmitted(())
                });

                while !parser.tokens_match(Token::Pipe {
                    punc: '|',
                    span: parser.stream.span(),
                }) {
                    let id = if let Some(Token::Identifier { name, .. }) = parser.consume_token() {
                        Ok(Identifier(name))
                    } else {
                        parser.log_error(ParserErrorKind::UnexpectedToken {
                            expected: "identifier".to_string(),
                            found: curr_token.clone()?,
                        });
                        Err(ErrorsEmitted(()))
                    };

                    let _ = parser.expect_separator(Token::Colon {
                        punc: ':',
                        span: parser.stream.span(),
                    });

                    let ty = if let Ok(t) = parser.get_type() {
                        Some(t)
                    } else {
                        None
                    };

                    let param = ClosureParam { id: id?, ty };
                    vec.push(param);

                    if let Some(Token::Comma { .. }) = parser.peek_current() {
                        parser.consume_token();
                    }
                }

                Ok(ClosureParams::Some(
                    BinaryOp::BitwiseOr,
                    vec,
                    BinaryOp::BitwiseOr,
                ))
            }
            Some(Token::DblPipe { .. }) => Ok(ClosureParams::None(BinaryOp::LogicalOr)),
            Some(t) => {
                parser.log_error(ParserErrorKind::UnexpectedToken {
                    expected: "`|` or `||`".to_string(),
                    found: t,
                });
                Err(ErrorsEmitted(()))
            }

            None => {
                parser.log_error(ParserErrorKind::UnexpectedEndOfInput);
                Err(ErrorsEmitted(()))
            }
        };

        let return_type_opt = if parser.is_expected_token(&Token::ThinArrow {
            punc: "->".to_string(),
            span: parser.stream.span(),
        }) {
            parser.consume_token();

            let ty = parser.get_type();
            Some((Separator::ThinArrow, ty?))
        } else {
            None
        };

        let curr_token = parser.peek_current().ok_or({
            parser.log_error(ParserErrorKind::UnexpectedEndOfInput);
            ErrorsEmitted(())
        });

        if return_type_opt.is_some()
            && !parser.is_expected_token(&Token::LBrace {
                delim: '{',
                span: parser.stream.span(),
            })
        {
            parser.log_error(ParserErrorKind::UnexpectedToken {
                expected: "`{`".to_string(),
                found: curr_token?,
            })
        }

        let expression = parser.parse_expression(Precedence::Lowest)?;

        if !parser.errors().is_empty() {
            return Err(ErrorsEmitted(()));
        }

        Ok(ClosureExpr {
            params: params?,
            return_type_opt,
            expression: Box::new(expression),
        })
    }
}
