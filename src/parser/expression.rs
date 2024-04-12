use crate::{
    ast::{
        CallExpr, Delimiter, Expression, FieldAccessExpr, Identifier, IndexExpr, MethodCallExpr,
        RangeExpr, StructExpr, StructField, TupleIndexExpr, TupleStructExpr, TypeCastExpr,
        UnwrapExpr,
    },
    error::{ErrorsEmitted, ParserErrorKind},
    token::Token,
};

use super::{Parser, Precedence};

///////////////////////////////////////////////////////////////////////////

pub(crate) trait ParseExpression
where
    Self: Sized,
{
    fn parse(parser: &mut Parser, expr: Expression) -> Result<Self, ErrorsEmitted>;
}

///////////////////////////////////////////////////////////////////////////

impl ParseExpression for MethodCallExpr {
    fn parse(parser: &mut Parser, expr: Expression) -> Result<MethodCallExpr, ErrorsEmitted> {
        todo!()
    }
}

impl ParseExpression for FieldAccessExpr {
    fn parse(parser: &mut Parser, object: Expression) -> Result<FieldAccessExpr, ErrorsEmitted> {
        let dot = parser.expect_separator(Token::Dot {
            punc: '.',
            span: parser.stream.span(),
        })?;

        let token = parser.consume_token()?;

        if let Token::Identifier { name, .. } = token {
            Ok(FieldAccessExpr {
                object: Box::new(object),
                dot,
                field: Identifier(name),
            })
        } else {
            parser.log_error(ParserErrorKind::UnexpectedToken {
                expected: "identifier after `.`".to_string(),
                found: token,
            });
            Err(ErrorsEmitted(()))
        }
    }
}

impl ParseExpression for CallExpr {
    fn parse(parser: &mut Parser, callee: Expression) -> Result<CallExpr, ErrorsEmitted> {
        let mut args: Vec<Expression> = Vec::new(); // store function arguments

        let open_paren = parser.expect_delimiter(Token::LParen {
            delim: '(',
            span: parser.stream.span(),
        })?;

        // parse arguments – separated by commas – until a closing parenthesis
        loop {
            if let Some(Token::RParen { .. }) = parser.peek_current() {
                // end of arguments
                parser.consume_token()?;
                break;
            }

            let arg_expr = parser.parse_expression(Precedence::Call);
            args.push(arg_expr?);

            // error handling
            let token = parser.consume_token();

            match token {
                Ok(Token::Comma { .. }) => continue, // more arguments
                Ok(Token::RParen { .. }) => break,   // end of call expression
                _ => {
                    parser.log_error(ParserErrorKind::UnexpectedToken {
                        expected: "`,` or `)`".to_string(),
                        found: token?,
                    });
                }
            }
        }

        if !parser.errors().is_empty() {
            return Err(ErrorsEmitted(()));
        }

        if args.is_empty() {
            Ok(CallExpr {
                callee: Box::new(callee),
                open_paren,
                args_opt: None,
                close_paren: Delimiter::RParen,
            })
        } else {
            Ok(CallExpr {
                callee: Box::new(callee),
                open_paren,
                args_opt: Some(args),
                close_paren: Delimiter::RParen,
            })
        }
    }
}

impl ParseExpression for IndexExpr {
    fn parse(parser: &mut Parser, array: Expression) -> Result<IndexExpr, ErrorsEmitted> {
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
        });

        Ok(IndexExpr {
            array: Box::new(array),
            open_bracket,
            index,
            close_bracket: close_bracket?,
        })
    }
}

impl ParseExpression for TupleIndexExpr {
    fn parse(parser: &mut Parser, expr: Expression) -> Result<TupleIndexExpr, ErrorsEmitted> {
        todo!()
    }
}

impl ParseExpression for UnwrapExpr {
    fn parse(parser: &mut Parser, expr: Expression) -> Result<UnwrapExpr, ErrorsEmitted> {
        todo!()
    }
}

impl ParseExpression for TypeCastExpr {
    fn parse(parser: &mut Parser, operand: Expression) -> Result<TypeCastExpr, ErrorsEmitted> {
        let kw_as = parser.expect_keyword(Token::As {
            name: "as".to_string(),
            span: parser.stream.span(),
        })?;

        let new_type = parser.get_type();

        Ok(TypeCastExpr {
            operand: Box::new(operand),
            kw_as,
            new_type: new_type?,
        })
    }
}

impl ParseExpression for RangeExpr {
    fn parse(parser: &mut Parser, expr: Expression) -> Result<RangeExpr, ErrorsEmitted> {
        todo!()
    }
}

impl ParseExpression for StructExpr {
    fn parse(parser: &mut Parser, path: Expression) -> Result<StructExpr, ErrorsEmitted> {
        let mut fields: Vec<StructField> = Vec::new(); // store struct fields

        let open_brace = parser.expect_delimiter(Token::LBrace {
            delim: '{',
            span: parser.stream.span(),
        })?;

        // parse struct fields – separated by commas – until a closing brace
        loop {
            // get the field name
            let token = parser.consume_token();

            let field_name = match token {
                Ok(Token::Identifier { name, .. }) => name,
                Ok(Token::RBrace { .. }) => break, // end of struct
                _ => {
                    parser.log_error(ParserErrorKind::UnexpectedToken {
                        expected: "identifier or `}`".to_string(),
                        found: token?,
                    });
                    return Err(ErrorsEmitted(()));
                }
            };

            let colon = parser.expect_separator(Token::Colon {
                punc: ':',
                span: parser.stream.span(),
            });

            // parse field value
            let field_value = parser.parse_expression(Precedence::Lowest);

            // push field to list of fields
            fields.push(StructField {
                name: Identifier(field_name),
                value: field_value?,
            });

            // error handling
            let token = parser.consume_token();

            match token {
                Ok(Token::Comma { .. }) => continue, // more arguments
                Ok(Token::RBrace { .. }) => break,   // end of struct
                _ => {
                    parser.log_error(ParserErrorKind::UnexpectedToken {
                        expected: "`,` or `}`".to_string(),
                        found: token?,
                    });
                }
            }
        }

        let close_brace = parser.expect_delimiter(Token::RBrace {
            delim: '}',
            span: parser.stream.span(),
        });

        if !parser.errors().is_empty() {
            return Err(ErrorsEmitted(()));
        }

        Ok(StructExpr {
            path: Box::new(path),
            open_brace,
            fields,
            close_brace: close_brace?,
        })
    }
}

impl ParseExpression for TupleStructExpr {
    fn parse(parser: &mut Parser, path: Expression) -> Result<Self, ErrorsEmitted> {
        let mut fields: Vec<Expression> = Vec::new(); // store struct fields

        let open_paren = parser.expect_delimiter(Token::LParen {
            delim: '(',
            span: parser.stream.span(),
        })?;

        // parse struct fields – separated by commas – until a closing brace
        loop {
            if let Some(Token::RParen { .. }) = parser.peek_current() {
                // end of fields
                parser.consume_token()?;
                break;
            }

            let field = parser.parse_expression(Precedence::Lowest);
            fields.push(field?);

            // error handling
            let token = parser.consume_token();

            match token {
                Ok(Token::Comma { .. }) => continue, // more arguments
                Ok(Token::RParen { .. }) => break,   // end of struct
                _ => {
                    parser.log_error(ParserErrorKind::UnexpectedToken {
                        expected: "`,` or `)`".to_string(),
                        found: token?,
                    });
                }
            }

            if !parser.errors().is_empty() {
                return Err(ErrorsEmitted(()));
            }
        }

        Ok(TupleStructExpr {
            path: Box::new(path),
            open_paren,
            elements: fields,
            close_paren: Delimiter::RParen,
        })
    }
}
