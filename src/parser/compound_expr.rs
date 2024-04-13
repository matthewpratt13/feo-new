use crate::{
    ast::{
        ArrayExpr, BinaryOp, BlockExpr, ClosureExpr, ClosureParam, ClosureParams, Delimiter,
        Expression, GroupedExpr, Identifier, Separator, Statement, TupleExpr,
    },
    error::{ErrorsEmitted, ParserErrorKind},
    token::Token,
};

use super::{Parser, Precedence};

pub(crate) trait ParseCompoundExpr
where
    Self: Sized,
{
    fn parse(parser: &mut Parser) -> Result<Self, ErrorsEmitted>;
}

impl ParseCompoundExpr for ClosureExpr {
    fn parse(parser: &mut Parser) -> Result<ClosureExpr, ErrorsEmitted> {
        let token = parser.consume_token();

        let params = match token {
            Ok(Token::Pipe { .. }) => {
                let mut vec: Vec<ClosureParam> = Vec::new();

                let curr_token = parser.peek_current().ok_or({
                    parser.log_error(ParserErrorKind::UnexpectedEndOfInput);
                    ErrorsEmitted(())
                });

                while !parser.tokens_match(Token::Pipe {
                    punc: '|',
                    span: parser.stream.span(),
                }) {
                    let id = if let Ok(Token::Identifier { name, .. }) = parser.consume_token() {
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
                        parser.consume_token()?;
                    }
                }

                Ok(ClosureParams::Some(
                    BinaryOp::BitwiseOr,
                    vec,
                    BinaryOp::BitwiseOr,
                ))
            }
            Ok(Token::DblPipe { .. }) => Ok(ClosureParams::None(BinaryOp::LogicalOr)),
            _ => {
                parser.log_error(ParserErrorKind::UnexpectedToken {
                    expected: "`|` or `||`".to_string(),
                    found: token?,
                });
                Err(ErrorsEmitted(()))
            }
        };

        let return_type_opt = if parser.is_expected_token(&Token::ThinArrow {
            punc: "->".to_string(),
            span: parser.stream.span(),
        }) {
            parser.consume_token()?;

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

impl ParseCompoundExpr for ArrayExpr {
    fn parse(parser: &mut Parser) -> Result<ArrayExpr, ErrorsEmitted> {
        let mut elements: Vec<Expression> = Vec::new();

        while !parser.is_expected_token(&Token::RBracket {
            delim: ']',
            span: parser.stream.span(),
        }) {
            let element = parser.parse_expression(Precedence::Lowest)?;
            elements.push(element);

            if !parser.tokens_match(Token::Comma {
                punc: ',',
                span: parser.stream.span(),
            }) {
                break;
            }
        }

        let close_bracket = parser.expect_delimiter(Token::RBracket {
            delim: ']',
            span: parser.stream.span(),
        });

        if !parser.errors().is_empty() {
            return Err(ErrorsEmitted(()));
        }

        Ok(ArrayExpr {
            open_bracket: Delimiter::LBracket,
            elements,
            close_bracket: close_bracket?,
        })
    }
}

impl ParseCompoundExpr for TupleExpr {
    fn parse(parser: &mut Parser) -> Result<TupleExpr, ErrorsEmitted> {
        let mut elements: Vec<Expression> = Vec::new();

        while !parser.is_expected_token(&Token::RParen {
            delim: ')',
            span: parser.stream.span(),
        }) {
            let element = parser.parse_expression(Precedence::Lowest)?;
            elements.push(element);

            if !parser.tokens_match(Token::Comma {
                punc: ',',
                span: parser.stream.span(),
            }) {
                break;
            }
        }

        let close_paren = parser.expect_delimiter(Token::RParen {
            delim: ')',
            span: parser.stream.span(),
        });

        if elements.is_empty() {
            parser.log_error(ParserErrorKind::TokenNotFound {
                expected: "tuple element or `,`".to_string(),
            });
        }

        if !parser.errors().is_empty() {
            return Err(ErrorsEmitted(()));
        }

        Ok(TupleExpr {
            open_paren: Delimiter::LParen,
            elements,
            close_paren: close_paren?,
        })
    }
}

impl ParseCompoundExpr for BlockExpr {
    fn parse(parser: &mut Parser) -> Result<BlockExpr, ErrorsEmitted> {
        let mut statements: Vec<Statement> = Vec::new();

        while !parser.is_expected_token(&Token::RBrace {
            delim: '}',
            span: parser.stream.span(),
        }) {
            let statement = parser.parse_statement()?;
            statements.push(statement);
        }

        let terminal_expression_opt = if let Ok(e) = parser.parse_expression(Precedence::Lowest) {
            Some(Box::new(e))
        } else {
            None
        };

        let close_brace = parser.expect_delimiter(Token::RBrace {
            delim: '}',
            span: parser.stream.span(),
        });

        if statements.is_empty() {
            parser.log_error(ParserErrorKind::TokenNotFound {
                expected: "statement".to_string(),
            });
        }

        if !parser.errors().is_empty() {
            return Err(ErrorsEmitted(()));
        }

        Ok(BlockExpr {
            open_brace: Delimiter::LBrace,
            statements,
            terminal_expression_opt,
            close_brace: close_brace?,
        })
    }
}

impl ParseCompoundExpr for GroupedExpr {
    fn parse(parser: &mut Parser) -> Result<GroupedExpr, ErrorsEmitted> {
        let expression = parser.parse_expression(Precedence::Lowest)?;

        let close_paren = parser.expect_delimiter(Token::RParen {
            delim: ')',
            span: parser.stream.span(),
        });

        if !parser.errors().is_empty() {
            return Err(ErrorsEmitted(()));
        }

        Ok(GroupedExpr {
            open_paren: Delimiter::LParen,
            expression: Box::new(expression),
            close_paren: close_paren?,
        })
    }
}
