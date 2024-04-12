use crate::{
    ast::{
        ArrayExpr, BinaryOp, BlockExpr, ClosureExpr, ClosureParams, Delimiter, Expression,
        GroupedExpr, Identifier, Param, Separator, Statement, TupleExpr,
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
                let mut vec: Vec<Param> = Vec::new();

                let curr_token = parser.peek_current().ok_or({
                    parser.log_error(ParserErrorKind::UnexpectedEndOfInput);
                    ErrorsEmitted(())
                })?;

                while !parser.is_expected_token(&Token::Pipe {
                    punc: '|',
                    span: parser.stream.span(),
                }) {
                    let id = if let Ok(Token::Identifier { name, .. }) = parser.consume_token() {
                        Ok(Identifier(name))
                    } else {
                        parser.log_error(ParserErrorKind::UnexpectedToken {
                            expected: "identifier".to_string(),
                            found: curr_token.clone(),
                        });
                        Err(ErrorsEmitted(()))
                    };

                    let ty = if let Ok(t) = parser.get_type() {
                        Some(t)
                    } else {
                        None
                    };

                    let param = Param { id: id?, ty };

                    vec.push(param);
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

        let expression = parser.parse_expression(Precedence::Lowest);

        Ok(ClosureExpr {
            params: params?,
            return_type_opt,
            expression: Box::new(expression?),
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
            elements.push(parser.parse_expression(Precedence::Lowest)?);

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

        Ok(ArrayExpr {
            open_bracket: Delimiter::LBrace,
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
            elements.push(parser.parse_expression(Precedence::Lowest)?);

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

        // parse expressions until a closing brace
        while !parser.is_expected_token(&Token::RBrace {
            delim: '}',
            span: parser.stream.span(),
        }) {
            statements.push(parser.parse_statement()?);
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

        Ok(GroupedExpr {
            open_paren: Delimiter::LParen,
            expression: Box::new(expression),
            close_paren: close_paren?,
        })
    }
}
