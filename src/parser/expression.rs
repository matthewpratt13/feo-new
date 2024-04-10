use crate::{
    ast::{
        expression::{
            BlockExpr, CallExpr, ClosureExpr, FieldAccessExpr,
            GroupedExpr, IndexExpr, MethodCallExpr, PathExpr, RangeExpr, ReturnExpr, TupleExpr,
            TupleIndexExpr, TypeCastExpr, UnwrapExpr,
        },
        Delimiter, Expression, Identifier, Statement,
    },
    error::{ErrorsEmitted, ParserErrorKind},
    token::Token,
};

use super::{Parser, Precedence};

///////////////////////////////////////////////////////////////////////////

/// Trait that provides a common interface for parsing different expressions.
pub(crate) trait ParseExpression {
    fn parse(parser: &mut Parser, expr: Expression) -> Result<Expression, ErrorsEmitted>;
}

///////////////////////////////////////////////////////////////////////////

impl ParseExpression for PathExpr {
    fn parse(parser: &mut Parser, prefix: Expression) -> Result<Expression, ErrorsEmitted> {
        todo!()
    }
}

impl ParseExpression for MethodCallExpr {
    fn parse(parser: &mut Parser, expr: Expression) -> Result<Expression, ErrorsEmitted> {
        todo!()
    }
}

impl ParseExpression for TypeCastExpr {
    fn parse(parser: &mut Parser, operand: Expression) -> Result<Expression, ErrorsEmitted> {
        let kw_as = parser.expect_keyword(Token::As {
            name: "as".to_string(),
            span: parser.stream.span(),
        })?;

        let new_type = parser.get_type()?;

        Ok(Expression::TypeCast(TypeCastExpr {
            operand: Box::new(operand),
            kw_as,
            new_type,
        }))
    }
}

impl ParseExpression for GroupedExpr {
    fn parse(parser: &mut Parser, expr: Expression) -> Result<Expression, ErrorsEmitted> {
        let token = parser.expect_token(Token::RParen {
            delim: ')',
            span: parser.stream.span(),
        });

        if let Ok(Token::RParen { .. }) = token {
            Ok(Expression::Grouped(GroupedExpr {
                open_paren: Delimiter::LParen,
                expression: Box::new(expr),
                close_paren: Delimiter::RParen,
            }))
        } else if let Ok(Token::Comma { .. }) = token {
            parser.unconsume(); // go back to the input expression and try to parse a tuple
            TupleExpr::parse(parser, expr)
        } else {
            Err(token.unwrap_err())
        }
    }
}

impl ParseExpression for BlockExpr {
    fn parse(parser: &mut Parser, first_expr: Expression) -> Result<Expression, ErrorsEmitted> {
        let open_brace = parser.expect_delimiter(Token::LBrace {
            delim: '{',
            span: parser.stream.span(),
        })?;

        let mut statements: Vec<Statement> = Vec::new();

        // parse expressions until a closing brace
        while !parser.is_expected_token(Token::RBrace {
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
        })?;

        Ok(Expression::Block(BlockExpr {
            open_brace,
            statements,
            terminal_expression_opt,
            close_brace,
        }))
    }
}

impl ParseExpression for FieldAccessExpr {
    fn parse(parser: &mut Parser, object: Expression) -> Result<Expression, ErrorsEmitted> {
        let dot = parser.expect_separator(Token::Dot {
            punc: '.',
            span: parser.stream.span(),
        })?;

        let token = parser.consume_token();

        if let Ok(Token::Identifier { name, .. }) = token {
            if let Some(Token::LParen { .. }) = parser.peek_ahead_by(1) {
                parser.unconsume();
                MethodCallExpr::parse(parser, object)
            } else {
                Ok(Expression::FieldAccess(FieldAccessExpr {
                    object: Box::new(object),
                    dot,
                    field: Identifier(name),
                }))
            }
        } else if let Ok(Token::UIntLiteral { .. }) = token {
            parser.unconsume();
            TupleIndexExpr::parse(parser, object)
        } else {
            parser.log_error(ParserErrorKind::UnexpectedToken {
                expected: "identifier after `.`".to_string(),
                found: token?,
            });
            Err(ErrorsEmitted(()))
        }
    }
}

impl ParseExpression for CallExpr {
    fn parse(parser: &mut Parser, callee: Expression) -> Result<Expression, ErrorsEmitted> {
        let mut args: Vec<Expression> = Vec::new(); // store function arguments

        let open_paren = parser.expect_delimiter(Token::LParen {
            delim: '(',
            span: parser.stream.span(),
        })?;

        // parse arguments – separated by commas – until a closing parenthesis
        loop {
            if let Some(Token::RParen { delim: ')', .. }) = parser.peek_current() {
                // end of arguments
                parser.consume_token()?;
                break;
            }

            let arg_expr = parser.parse_expression(Precedence::Lowest);
            args.push(arg_expr?);

            // error handling
            let token = parser.consume_token();

            match token {
                Ok(Token::Comma { .. }) => continue, // more arguments
                Ok(Token::RParen { .. }) => break,   // end of arguments
                _ => {
                    parser.log_error(ParserErrorKind::UnexpectedToken {
                        expected: "`,` or `)`".to_string(),
                        found: token?,
                    });

                    return Err(ErrorsEmitted(()));
                }
            }
        }

        if args.is_empty() {
            Ok(Expression::Call(CallExpr {
                open_paren,
                callee: Box::new(callee),
                args_opt: None,
                close_paren: Delimiter::RParen,
            }))
        } else {
            Ok(Expression::Call(CallExpr {
                open_paren,
                callee: Box::new(callee),
                args_opt: Some(args),
                close_paren: Delimiter::RParen,
            }))
        }
    }
}

impl ParseExpression for IndexExpr {
    fn parse(parser: &mut Parser, array: Expression) -> Result<Expression, ErrorsEmitted> {
        let open_bracket = parser.expect_delimiter(Token::LBracket {
            delim: '[',
            span: parser.stream.span(),
        })?;

        let token = parser.consume_token();

        let index = if let Ok(Token::UIntLiteral { value, .. }) = token {
            value
        } else if let Ok(_) = token {
            parser.log_error(ParserErrorKind::UnexpectedToken {
                expected: "array index".to_string(),
                found: token?,
            });

            return Err(ErrorsEmitted(()));
        } else {
            parser.log_error(ParserErrorKind::UnexpectedEndOfInput);
            return Err(ErrorsEmitted(()));
        };

        let close_bracket = parser.expect_delimiter(Token::RBracket {
            delim: ']',
            span: parser.stream.span(),
        })?;

        Ok(Expression::Index(IndexExpr {
            array: Box::new(array),
            open_bracket,
            index,
            close_bracket,
        }))
    }
}

impl ParseExpression for TupleIndexExpr {
    fn parse(parser: &mut Parser, expr: Expression) -> Result<Expression, ErrorsEmitted> {
        todo!()
    }
}

impl ParseExpression for UnwrapExpr {
    fn parse(parser: &mut Parser, expr: Expression) -> Result<Expression, ErrorsEmitted> {
        todo!()
    }
}

impl ParseExpression for RangeExpr {
    fn parse(parser: &mut Parser, expr: Expression) -> Result<Expression, ErrorsEmitted> {
        todo!()
    }
}

impl ParseExpression for ReturnExpr {
    fn parse(parser: &mut Parser, expr: Expression) -> Result<Expression, ErrorsEmitted> {
        todo!()
    }
}

impl ParseExpression for ClosureExpr {
    fn parse(parser: &mut Parser, expr: Expression) -> Result<Expression, ErrorsEmitted> {
        todo!()
    }
}

impl ParseExpression for TupleExpr {
    fn parse(parser: &mut Parser, first_expr: Expression) -> Result<Expression, ErrorsEmitted> {
        todo!()
    }
}
