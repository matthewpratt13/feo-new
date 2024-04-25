use crate::{
    ast::{FunctionOrMethodParam, Identifier, PathExpr, PathPrefix, PrimitiveType, Type},
    error::{ErrorsEmitted, ParserErrorKind},
    token::Token,
};

use super::Parser;

impl Type {
    /// Match a `Token` to a `Type` and return the `Type` or emit an error.
    pub(crate) fn parse(parser: &mut Parser) -> Result<Type, ErrorsEmitted> {
        println!("ENTER `get_type()`");
        println!("CURRENT TOKEN: {:?}", parser.peek_current());
        let token = parser.consume_token();

        match token {
            Some(Token::I32Type { .. }) => Ok(Type::I32(PrimitiveType::I32)),
            Some(Token::I64Type { .. }) => Ok(Type::I64(PrimitiveType::I64)),
            Some(Token::I128Type { .. }) => Ok(Type::I128(PrimitiveType::I128)),
            Some(Token::U8Type { .. }) => Ok(Type::U8(PrimitiveType::U8)),
            Some(Token::U16Type { .. }) => Ok(Type::U16(PrimitiveType::U16)),
            Some(Token::U32Type { .. }) => Ok(Type::U32(PrimitiveType::U32)),
            Some(Token::U64Type { .. }) => Ok(Type::U64(PrimitiveType::U64)),
            Some(Token::U128Type { .. }) => Ok(Type::U128(PrimitiveType::U128)),
            Some(Token::U256Type { .. }) => Ok(Type::U256(PrimitiveType::U256)),
            Some(Token::U512Type { .. }) => Ok(Type::U512(PrimitiveType::U512)),
            Some(Token::ByteType { .. }) => Ok(Type::Byte(PrimitiveType::Byte)),
            Some(Token::B2Type { .. }) => Ok(Type::B2(PrimitiveType::B2)),
            Some(Token::B4Type { .. }) => Ok(Type::B4(PrimitiveType::B4)),
            Some(Token::B8Type { .. }) => Ok(Type::B8(PrimitiveType::B8)),
            Some(Token::B16Type { .. }) => Ok(Type::B16(PrimitiveType::B16)),
            Some(Token::B32Type { .. }) => Ok(Type::B32(PrimitiveType::B32)),
            Some(Token::H160Type { .. }) => Ok(Type::H160(PrimitiveType::H160)),
            Some(Token::H256Type { .. }) => Ok(Type::H256(PrimitiveType::H256)),
            Some(Token::H512Type { .. }) => Ok(Type::H512(PrimitiveType::H512)),
            Some(Token::StrType { .. }) => Ok(Type::Str(PrimitiveType::Str)),
            Some(Token::CharType { .. }) => Ok(Type::Char(PrimitiveType::Char)),
            Some(Token::BoolType { .. }) => Ok(Type::Bool(PrimitiveType::Bool)),
            Some(Token::LParen { .. }) => {
                if let Some(Token::RParen { .. }) = parser.peek_current() {
                    parser.consume_token();
                    Ok(Type::UnitType)
                } else {
                    let mut types: Vec<Type> = Vec::new();

                    loop {
                        if let Some(Token::Comma { .. }) = parser.peek_current() {
                            parser.consume_token();
                        }

                        if let Some(Token::RParen { .. }) = parser.peek_current() {
                            break;
                        }

                        let ty = Type::parse(parser)?;
                        types.push(ty);

                        let token = parser.peek_current();

                        match token {
                            Some(Token::Comma { .. }) => {
                                parser.consume_token();
                                continue;
                            }
                            Some(Token::RParen { .. }) => break,
                            Some(t) => parser.log_error(ParserErrorKind::UnexpectedToken {
                                expected: "`,` or `)`".to_string(),
                                found: Some(t),
                            }),
                            None => {
                                parser.log_missing_delimiter(')');
                            }
                        }
                    }

                    if let Some(Token::RParen { .. }) = parser.peek_current() {
                        parser.consume_token();
                    } else {
                        parser.log_missing_delimiter(')');
                    }

                    Ok(Type::Tuple(types))
                }
            }
            Some(Token::LBracket { .. }) => {
                let ty = Type::parse(parser)?;

                if let Some(Token::Semicolon { .. }) = parser.peek_current() {
                    parser.consume_token();
                } else {
                    parser.log_unexpected_token("`;`".to_string());
                }

                let num_elements =
                    if let Some(Token::UIntLiteral { value, .. }) = parser.consume_token() {
                        Ok(value)
                    } else {
                        parser.log_unexpected_token("unsigned integer".to_string());
                        Err(ErrorsEmitted)
                    }?;

                if let Some(Token::RBracket { .. }) = parser.peek_current() {
                    parser.consume_token();
                } else {
                    parser.log_missing_delimiter(']');
                }

                Ok(Type::Array {
                    element_type: Box::new(ty),
                    num_elements,
                })
            }
            Some(Token::Func { .. }) => {
                let mut params: Vec<FunctionOrMethodParam> = Vec::new();

                let function_name = if let Some(Token::Identifier { name, .. }) = token {
                    Ok(Identifier(name))
                } else {
                    parser.log_unexpected_token("identifier".to_string());
                    Err(ErrorsEmitted)
                }?;

                if let Some(Token::LBrace { .. }) = parser.peek_current() {
                    parser.consume_token();
                } else {
                    parser.log_unexpected_token("`{`".to_string());
                };

                if let Some(Token::LParen { .. }) = parser.peek_current() {
                    parser.consume_token();
                } else {
                    parser.log_unexpected_token("`(`".to_string());
                }

                // `&parser` and `&mut parser` can only occur as the first parameter in a method
                if let Some(Token::Ampersand { .. } | Token::AmpersandMut { .. }) =
                    parser.peek_current()
                {
                    let param = FunctionOrMethodParam::parse(parser)?;
                    params.push(param);
                }

                loop {
                    if let Some(Token::Comma { .. }) = parser.peek_current() {
                        parser.consume_token();
                    }

                    if let Some(Token::RParen { .. }) = parser.peek_current() {
                        break;
                    }

                    let param = FunctionOrMethodParam::parse(parser)?;
                    params.push(param);

                    let token = parser.peek_current();

                    match token {
                        Some(Token::Comma { .. }) => {
                            parser.consume_token();
                            continue;
                        }
                        Some(Token::RParen { .. }) => break,
                        Some(t) => parser.log_unexpected_token("`,` or `)`".to_string()),
                        None => {
                            parser.log_missing_delimiter(')');
                        }
                    }
                }

                if let Some(Token::RParen { .. }) = parser.peek_current() {
                    parser.consume_token();
                } else {
                    parser.log_missing_delimiter(')');
                }

                let return_type_opt = if let Some(Token::ThinArrow { .. }) = parser.peek_current() {
                    parser.consume_token();
                    Some(Box::new(Type::parse(parser)?))
                } else {
                    None
                };

                Ok(Type::Function {
                    function_name,
                    params_opt: {
                        if params.is_empty() {
                            None
                        } else {
                            Some(params)
                        }
                    },
                    return_type_opt,
                })
            }
            Some(Token::Ampersand { .. } | Token::AmpersandMut { .. }) => {
                let inner_type = Box::new(Type::parse(parser)?);
                Ok(Type::Reference(inner_type))
            }

            Some(Token::VecType { .. }) => {
                let _ = parser.expect_separator(Token::LessThan {
                    punc: '<',
                    span: parser.stream.span(),
                })?;

                let ty = Type::parse(parser)?;

                let _ = parser.expect_separator(Token::GreaterThan {
                    punc: '>',
                    span: parser.stream.span(),
                })?;

                Ok(Type::Vec(Box::new(ty)))
            }

            Some(Token::MappingType { .. }) => {
                let _ = parser.expect_separator(Token::LessThan {
                    punc: '<',
                    span: parser.stream.span(),
                })?;

                let key_type = Box::new(Type::parse(parser)?);

                let _ = parser.expect_separator(Token::Comma {
                    punc: '.',
                    span: parser.stream.span(),
                })?;

                let value_type = Box::new(Type::parse(parser)?);

                let _ = parser.expect_separator(Token::GreaterThan {
                    punc: '>',
                    span: parser.stream.span(),
                })?;

                Ok(Type::Mapping {
                    key_type,
                    value_type,
                })
            }

            Some(Token::OptionType { .. }) => {
                let _ = parser.expect_separator(Token::LessThan {
                    punc: '<',
                    span: parser.stream.span(),
                })?;

                let ty = Type::parse(parser)?;

                let _ = parser.expect_separator(Token::GreaterThan {
                    punc: '>',
                    span: parser.stream.span(),
                })?;

                Ok(Type::Option(Box::new(ty)))
            }

            Some(Token::ResultType { .. }) => {
                let _ = parser.expect_separator(Token::LessThan {
                    punc: '<',
                    span: parser.stream.span(),
                })?;

                let ok = Box::new(Type::parse(parser)?);

                let _ = parser.expect_separator(Token::Comma {
                    punc: '.',
                    span: parser.stream.span(),
                })?;

                let err = Box::new(Type::parse(parser)?);

                let _ = parser.expect_separator(Token::GreaterThan {
                    punc: '>',
                    span: parser.stream.span(),
                })?;

                Ok(Type::Result { ok, err })
            }

            Some(Token::Identifier { name, .. }) => {
                let path = PathExpr::parse(parser, PathPrefix::Identifier(Identifier(name)))?;
                Ok(Type::UserDefined(path))
            }

            Some(Token::Package { .. }) => {
                let path = PathExpr::parse(parser, PathPrefix::Package)?;
                Ok(Type::UserDefined(path))
            }

            Some(Token::Super { .. }) => {
                let path = PathExpr::parse(parser, PathPrefix::Super)?;
                Ok(Type::UserDefined(path))
            }

            Some(Token::SelfKeyword { .. }) => {
                let path = PathExpr::parse(parser, PathPrefix::SelfKeyword)?;
                Ok(Type::UserDefined(path))
            }

            Some(Token::SelfType { .. }) => {
                let path = PathExpr::parse(parser, PathPrefix::SelfType)?;
                Ok(Type::UserDefined(path))
            }

            _ => {
                parser.log_error(ParserErrorKind::UnexpectedToken {
                    expected: "type annotation".to_string(),
                    found: token,
                });
                Err(ErrorsEmitted)
            }
        }
    }
}