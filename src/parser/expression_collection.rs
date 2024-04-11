use crate::{
    ast::{
        expression::{
            ArrayExpr, BlockExpr, GroupedExpr, PathExpr, StructExpr, StructField, TupleExpr,
            TupleStructExpr,
        },
        Delimiter, Expression, Identifier, Statement,
    },
    error::{ErrorsEmitted, ParserErrorKind},
    token::Token,
};

use super::{expression::ParseExpression, Parser, Precedence};

pub(crate) trait ParseExpressionCollection
where
    Self: Sized,
{
    fn parse(parser: &mut Parser) -> Result<Self, ErrorsEmitted>;
}

impl ParseExpressionCollection for ArrayExpr {
    fn parse(parser: &mut Parser) -> Result<ArrayExpr, ErrorsEmitted> {
        let open_bracket = parser.expect_delimiter(Token::LBracket {
            delim: '[',
            span: parser.stream.span(),
        })?;

        let mut elements: Vec<Expression> = Vec::new();

        while !parser.is_expected_token(Token::RBracket {
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
        })?;

        Ok(ArrayExpr {
            open_bracket,
            elements,
            close_bracket,
        })
    }
}

impl ParseExpressionCollection for TupleExpr {
    fn parse(parser: &mut Parser) -> Result<TupleExpr, ErrorsEmitted> {
        let open_paren = parser.expect_delimiter(Token::LParen {
            delim: '(',
            span: parser.stream.span(),
        })?;

        let mut elements: Vec<Expression> = Vec::new();

        while !parser.is_expected_token(Token::RParen {
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
        })?;

        Ok(TupleExpr {
            open_paren,
            elements,
            close_paren,
        })
    }
}

impl ParseExpressionCollection for StructExpr {
    fn parse(parser: &mut Parser) -> Result<StructExpr, ErrorsEmitted> {
        let mut fields: Vec<StructField> = Vec::new(); // stores struct fields

        let token = parser.consume_token()?;

        let path = if let Token::Identifier { name, .. } | Token::SelfType { name, .. } = token {
            let expr = parser.parse_primary()?;
            PathExpr::parse(parser, expr)?
        } else {
            parser.log_error(ParserErrorKind::UnexpectedToken {
                expected: "path".to_string(),
                found: token,
            });
            return Err(ErrorsEmitted(()));
        };

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
            })?;

            // parse field value
            let field_value = parser.parse_expression(Precedence::Lowest)?;

            // push field to list of fields
            fields.push(StructField {
                name: Identifier(field_name),
                value: field_value,
            });

            // error handling
            let token = parser.consume_token();

            match token {
                Ok(Token::Comma { .. }) => continue, // more fields
                Ok(Token::RBrace { .. }) => break,   // end of struct
                _ => {
                    parser.log_error(ParserErrorKind::UnexpectedToken {
                        expected: "`,` or `}`".to_string(),
                        found: token?,
                    });
                    return Err(ErrorsEmitted(()));
                }
            }
        }

        let close_brace = parser.expect_delimiter(Token::RBrace {
            delim: '}',
            span: parser.stream.span(),
        })?;

        Ok(StructExpr {
            path,
            open_brace,
            fields,
            close_brace,
        })
    }
}

impl ParseExpressionCollection for TupleStructExpr {
    fn parse(parser: &mut Parser) -> Result<Self, ErrorsEmitted> {
        let token = parser.consume_token()?;

        let path = if let Token::Identifier { name, .. } | Token::SelfType { name, .. } = token {
            let expr = parser.parse_primary()?;
            PathExpr::parse(parser, expr)?
        } else {
            parser.log_error(ParserErrorKind::UnexpectedToken {
                expected: "path".to_string(),
                found: token,
            });
            return Err(ErrorsEmitted(()));
        };

        let mut fields: Vec<Expression> = Vec::new();

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
            fields.push(arg_expr?);

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

        if fields.is_empty() {
            Ok(TupleStructExpr {
                open_paren,
                path,
                fields_opt: None,
                close_paren: Delimiter::RParen,
            })
        } else {
            Ok(TupleStructExpr {
                open_paren,
                path,
                fields_opt: Some(fields),
                close_paren: Delimiter::RParen,
            })
        }
    }
}

impl ParseExpressionCollection for BlockExpr {
    fn parse(parser: &mut Parser) -> Result<BlockExpr, ErrorsEmitted> {
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

        Ok(BlockExpr {
            open_brace,
            statements,
            terminal_expression_opt,
            close_brace,
        })
    }
}

impl ParseExpressionCollection for GroupedExpr {
    fn parse(parser: &mut Parser) -> Result<GroupedExpr, ErrorsEmitted> {
        let open_paren = parser.expect_delimiter(Token::RParen {
            delim: '(',
            span: parser.stream.span(),
        })?;

        let expression = parser.parse_expression(Precedence::Lowest)?;

        let close_paren = parser.expect_delimiter(Token::RParen {
            delim: ')',
            span: parser.stream.span(),
        })?;

        Ok(GroupedExpr {
            open_paren,
            expression: Box::new(expression),
            close_paren,
        })
    }
}
