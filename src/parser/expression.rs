use crate::{
    ast::{
        CallExpr, Delimiter, Expression, FieldAccessExpr, Identifier, IndexExpr, MethodCallExpr,
        PathExpr, RangeExpr, RangeOp, Separator, StructExpr, StructField, TupleIndexExpr,
        TupleStructExpr, TypeCastExpr, UnwrapExpr, UnwrapOp,
    },
    error::{ErrorsEmitted, ParserErrorKind},
    token::Token,
};

use super::{Parser, Precedence};

pub(crate) trait ParseExpression
where
    Self: Sized,
{
    fn parse(parser: &mut Parser, expr: Expression) -> Result<Self, ErrorsEmitted>;
}

impl ParseExpression for PathExpr {
    fn parse(parser: &mut Parser, prefix: Expression) -> Result<PathExpr, ErrorsEmitted> {
        println!("ENTER `PathExpr::parse()`");

        let mut tree: Vec<Identifier> = Vec::new();

        let token = parser.consume_token();

        let root = match prefix {
            Expression::Path(p) => Ok(p.root),
            _ => {
                let token = token.ok_or({
                    parser.log_error(ParserErrorKind::UnexpectedEndOfInput);
                    ErrorsEmitted(())
                })?;

                parser.log_error(ParserErrorKind::UnexpectedToken {
                    expected: "path expression prefix".to_string(),
                    found: token,
                });
                Err(ErrorsEmitted(()))
            }
        }?;

        while let Some(Token::DblColon { .. }) = parser.peek_current() {
            parser.consume_token();

            if let Some(Token::Identifier { name, .. }) = parser.peek_current() {
                tree.push(Identifier(name));
            } else {
                break;
            }
        }

        if !parser.errors().is_empty() {
            return Err(ErrorsEmitted(()));
        }

        if tree.is_empty() {
            Ok(PathExpr {
                root,
                tree_opt: None,
            })
        } else {
            Ok(PathExpr {
                root,
                tree_opt: Some(tree),
            })
        }
    }
}

impl ParseExpression for MethodCallExpr {
    fn parse(parser: &mut Parser, receiver: Expression) -> Result<MethodCallExpr, ErrorsEmitted> {
        let mut args: Vec<Expression> = Vec::new();

        let token = parser.consume_token();

        let method_name = if let Some(Token::Identifier { name, .. }) = token {
            Ok(Identifier(name))
        } else if let Some(t) = token {
            parser.log_error(ParserErrorKind::UnexpectedToken {
                expected: "identifier after `.`".to_string(),
                found: t,
            });
            Err(ErrorsEmitted(()))
        } else {
            parser.log_error(ParserErrorKind::UnexpectedEndOfInput);
            Err(ErrorsEmitted(()))
        };

        let open_paren = parser.expect_delimiter(Token::LParen {
            delim: '(',
            span: parser.stream.span(),
        });

        loop {
            if let Some(Token::RParen { .. }) = parser.peek_current() {
                parser.consume_token();
                break;
            }

            let arg_expr = parser.parse_expression(Precedence::Lowest)?;
            args.push(arg_expr);

            let curr_token = parser.consume_token();

            match curr_token {
                Some(Token::Comma { .. }) => continue,
                Some(Token::RParen { .. }) => break,
                _ => {
                    parser.log_error(ParserErrorKind::TokenNotFound {
                        expected: "`)`".to_string(),
                    });
                    break;
                }
            }
        }

        if !parser.errors().is_empty() {
            return Err(ErrorsEmitted(()));
        }

        if args.is_empty() {
            Ok(MethodCallExpr {
                receiver: Box::new(receiver),
                dot: Separator::Dot,
                method_name: method_name?,
                open_paren: open_paren?,
                args_opt: None,
                close_paren: Delimiter::RParen,
            })
        } else {
            Ok(MethodCallExpr {
                receiver: Box::new(receiver),
                dot: Separator::Dot,
                method_name: method_name?,
                open_paren: open_paren?,
                args_opt: Some(args),
                close_paren: Delimiter::RParen,
            })
        }
    }
}

impl ParseExpression for FieldAccessExpr {
    fn parse(parser: &mut Parser, object: Expression) -> Result<FieldAccessExpr, ErrorsEmitted> {
        let token = parser.consume_token();

        if let Some(Token::Identifier { name, .. }) = token {
            Ok(FieldAccessExpr {
                object: Box::new(object),
                dot: Separator::Dot,
                field: Identifier(name),
            })
        } else if let Some(t) = token {
            parser.log_error(ParserErrorKind::UnexpectedToken {
                expected: "identifier after `.`".to_string(),
                found: t,
            });
            Err(ErrorsEmitted(()))
        } else {
            parser.log_error(ParserErrorKind::UnexpectedEndOfInput);
            Err(ErrorsEmitted(()))
        }
    }
}

impl ParseExpression for CallExpr {
    fn parse(parser: &mut Parser, callee: Expression) -> Result<CallExpr, ErrorsEmitted> {
        let mut args: Vec<Expression> = Vec::new();

        loop {
            if let Some(Token::RParen { .. }) = parser.peek_current() {
                parser.consume_token();
                break;
            }

            let arg_expr = parser.parse_expression(Precedence::Lowest)?;
            args.push(arg_expr);

            let token = parser.consume_token();

            match token {
                Some(Token::Comma { .. }) => continue,
                Some(Token::RParen { .. }) => break,
                Some(t) => {
                    parser.log_error(ParserErrorKind::UnexpectedToken {
                        expected: "`,` or `)`".to_string(),
                        found: t,
                    });
                }
                None => parser.log_error(ParserErrorKind::UnexpectedEndOfInput),
            }
        }

        if !parser.errors().is_empty() {
            return Err(ErrorsEmitted(()));
        }

        if args.is_empty() {
            Ok(CallExpr {
                callee: Box::new(callee),
                open_paren: Delimiter::LParen,
                args_opt: None,
                close_paren: Delimiter::RParen,
            })
        } else {
            Ok(CallExpr {
                callee: Box::new(callee),
                open_paren: Delimiter::LParen,
                args_opt: Some(args),
                close_paren: Delimiter::RParen,
            })
        }
    }
}

impl ParseExpression for IndexExpr {
    fn parse(parser: &mut Parser, array: Expression) -> Result<IndexExpr, ErrorsEmitted> {
        let token = parser.consume_token();

        let index = if let Some(Token::UIntLiteral { value, .. }) = token {
            Ok(value)
        } else if let Some(t) = token {
            parser.log_error(ParserErrorKind::UnexpectedToken {
                expected: "unsigned integer".to_string(),
                found: t,
            });
            Err(ErrorsEmitted(()))
        } else {
            parser.log_error(ParserErrorKind::UnexpectedEndOfInput);
            Err(ErrorsEmitted(()))
        };

        let close_bracket = parser.expect_delimiter(Token::RBracket {
            delim: ']',
            span: parser.stream.span(),
        });

        if !parser.errors().is_empty() {
            return Err(ErrorsEmitted(()));
        }

        Ok(IndexExpr {
            array: Box::new(array),
            open_bracket: Delimiter::LBracket,
            index: index?,
            close_bracket: close_bracket?,
        })
    }
}

impl ParseExpression for TupleIndexExpr {
    fn parse(parser: &mut Parser, operand: Expression) -> Result<TupleIndexExpr, ErrorsEmitted> {
        let token = parser.consume_token();

        let index = if let Some(Token::UIntLiteral { value, .. }) = token {
            Ok(value)
        } else if let Some(t) = token {
            parser.log_error(ParserErrorKind::UnexpectedToken {
                expected: "unsigned integer".to_string(),
                found: t,
            });
            Err(ErrorsEmitted(()))
        } else {
            parser.log_error(ParserErrorKind::UnexpectedEndOfInput);
            Err(ErrorsEmitted(()))
        };

        if !parser.errors().is_empty() {
            return Err(ErrorsEmitted(()));
        }

        Ok(TupleIndexExpr {
            operand: Box::new(operand),
            dot: Separator::Dot,
            index: index?,
        })
    }
}

// impl ParseExpression for StructExpr {
//     fn parse(parser: &mut Parser, path: Expression) -> Result<StructExpr, ErrorsEmitted> {
//         let mut fields: Vec<StructField> = Vec::new();

//         let open_brace = parser.expect_delimiter(Token::LBrace {
//             delim: '{',
//             span: parser.stream.span(),
//         })?;

//         loop {
//             let token = parser.consume_token();

//             let name = match token {
//                 Ok(Token::Identifier { name, .. }) => Ok(name),
//                 Ok(Token::RBrace { .. }) => break,
//                 _ => {
//                     parser.log_error(ParserErrorKind::UnexpectedToken {
//                         expected: "identifier or `}`".to_string(),
//                         found: token?,
//                     });
//                     Err(ErrorsEmitted(()))
//                 }
//             };

//             let _ = parser.expect_separator(Token::Colon {
//                 punc: ':',
//                 span: parser.stream.span(),
//             });

//             let value = parser.parse_expression(Precedence::Lowest);

//             let field = StructField {
//                 name: Identifier(name?),
//                 value: value?,
//             };

//             fields.push(field);

//             let token = parser.consume_token();

//             match token {
//                 Ok(Token::Comma { .. }) => continue,
//                 Ok(Token::RBrace { .. }) => break,
//                 _ => {
//                     parser.log_error(ParserErrorKind::UnexpectedToken {
//                         expected: "`,` or `}`".to_string(),
//                         found: token?,
//                     });
//                 }
//             }
//         }

//         let close_brace = parser.expect_delimiter(Token::RBrace {
//             delim: '}',
//             span: parser.stream.span(),
//         });

//         if fields.is_empty() {
//             parser.log_error(ParserErrorKind::TokenNotFound {
//                 expected: "struct field".to_string(),
//             });
//         }

//         if !parser.errors().is_empty() {
//             return Err(ErrorsEmitted(()));
//         }

//         Ok(StructExpr {
//             path: Box::new(path),
//             open_brace,
//             fields,
//             close_brace: close_brace?,
//         })
//     }
// }

// impl ParseExpression for TupleStructExpr {
//     fn parse(parser: &mut Parser, path: Expression) -> Result<Self, ErrorsEmitted> {
//         let mut elements: Vec<Expression> = Vec::new();

//         let open_paren = parser.expect_delimiter(Token::LParen {
//             delim: '(',
//             span: parser.stream.span(),
//         })?;

//         loop {
//             if let Some(Token::RParen { .. }) = parser.peek_current() {
//                 parser.consume_token()?;
//                 break;
//             }

//             let element = parser.parse_expression(Precedence::Lowest)?;
//             elements.push(element);

//             let token = parser.consume_token();

//             match token {
//                 Ok(Token::Comma { .. }) => continue,
//                 Ok(Token::RParen { .. }) => break,
//                 _ => {
//                     parser.log_error(ParserErrorKind::UnexpectedToken {
//                         expected: "`,` or `)`".to_string(),
//                         found: token?,
//                     });
//                 }
//             }
//         }

//         if elements.is_empty() {
//             parser.log_error(ParserErrorKind::TokenNotFound {
//                 expected: "tuple struct element".to_string(),
//             });
//         }

//         if !parser.errors().is_empty() {
//             return Err(ErrorsEmitted(()));
//         }

//         Ok(TupleStructExpr {
//             path: Box::new(path),
//             open_paren,
//             elements,
//             close_paren: Delimiter::RParen,
//         })
//     }
// }

#[cfg(test)]
mod tests {

    use crate::test_utils;

    #[test]
    #[ignore]
    fn test_path_expr() -> Result<(), ()> {
        let input = r#"package::module::Object"#;

        let mut parser = test_utils::get_parser(input);

        let expressions = parser.parse();

        match expressions {
            Ok(t) => Ok(println!("{:#?}", t)),
            Err(_) => Err(println!("{:#?}", parser.errors())),
        }
    }

    #[test]
    fn test_method_call_expr() -> Result<(), ()> {
        let input = r#"receiver.method(foo, bar)"#;

        let mut parser = test_utils::get_parser(input);

        let expressions = parser.parse();

        match expressions {
            Ok(t) => Ok(println!("{:#?}", t)),
            Err(_) => Err(println!("{:#?}", parser.errors())),
        }
    }

    #[test]
    fn test_field_access_expr() -> Result<(), ()> {
        let input = r#"object.field"#;

        let mut parser = test_utils::get_parser(input);

        let expressions = parser.parse();

        match expressions {
            Ok(t) => Ok(println!("{:#?}", t)),
            Err(_) => Err(println!("{:#?}", parser.errors())),
        }
    }

    #[test]
    fn test_call_expr() -> Result<(), ()> {
        let input = r#"foo(bar)"#;

        let mut parser = test_utils::get_parser(input);

        let expressions = parser.parse();

        match expressions {
            Ok(t) => Ok(println!("{:#?}", t)),
            Err(_) => Err(println!("{:#?}", parser.errors())),
        }
    }

    #[test]
    fn test_index_expr() -> Result<(), ()> {
        let input = r#"array[0]"#;

        let mut parser = test_utils::get_parser(input);

        let expressions = parser.parse();

        match expressions {
            Ok(t) => Ok(println!("{:#?}", t)),
            Err(_) => Err(println!("{:#?}", parser.errors())),
        }
    }

    #[test]
    fn test_tuple_index_expr() -> Result<(), ()> {
        let input = r#"tuple.0"#;

        let mut parser = test_utils::get_parser(input);

        let expressions = parser.parse();

        match expressions {
            Ok(t) => Ok(println!("{:#?}", t)),
            Err(_) => Err(println!("{:#?}", parser.errors())),
        }
    }

    #[test]
    fn test_unwrap_expr() -> Result<(), ()> {
        let input = r#"(x + 2)?"#;

        let mut parser = test_utils::get_parser(input);

        let expressions = parser.parse();

        match expressions {
            Ok(t) => Ok(println!("{:#?}", t)),
            Err(_) => Err(println!("{:#?}", parser.errors())),
        }
    }

    #[test]
    fn test_type_cast_expr() -> Result<(), ()> {
        let input = r#"x as u32"#;

        let mut parser = test_utils::get_parser(input);

        let expressions = parser.parse();

        match expressions {
            Ok(t) => Ok(println!("{:#?}", t)),
            Err(_) => Err(println!("{:#?}", parser.errors())),
        }
    }

}
