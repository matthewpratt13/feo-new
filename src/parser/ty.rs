use crate::{
    ast::{FunctionOrMethodParam, Identifier, Type},
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
            Some(Token::I32Type { name, .. }) => Ok(Type::I32(name)),
            Some(Token::I64Type { name, .. }) => Ok(Type::I64(name)),
            Some(Token::I128Type { name, .. }) => Ok(Type::I128(name)),
            Some(Token::U8Type { name, .. }) => Ok(Type::U8(name)),
            Some(Token::U16Type { name, .. }) => Ok(Type::U16(name)),
            Some(Token::U32Type { name, .. }) => Ok(Type::U32(name)),
            Some(Token::U64Type { name, .. }) => Ok(Type::U64(name)),
            Some(Token::U128Type { name, .. }) => Ok(Type::U128(name)),
            Some(Token::U256Type { name, .. }) => Ok(Type::U256(name)),
            Some(Token::U512Type { name, .. }) => Ok(Type::U512(name)),
            Some(Token::ByteType { name, .. }) => Ok(Type::Byte(name)),
            Some(Token::B2Type { name, .. }) => Ok(Type::B2(name)),
            Some(Token::B4Type { name, .. }) => Ok(Type::B4(name)),
            Some(Token::B8Type { name, .. }) => Ok(Type::B8(name)),
            Some(Token::B16Type { name, .. }) => Ok(Type::B16(name)),
            Some(Token::B32Type { name, .. }) => Ok(Type::B32(name)),
            Some(Token::H160Type { name, .. }) => Ok(Type::H160(name)),
            Some(Token::H256Type { name, .. }) => Ok(Type::H256(name)),
            Some(Token::H512Type { name, .. }) => Ok(Type::H512(name)),
            Some(Token::StrType { name, .. }) => Ok(Type::Str(name)),
            Some(Token::CharType { name, .. }) => Ok(Type::Char(name)),
            Some(Token::BoolType { name, .. }) => Ok(Type::Bool(name)),
            Some(Token::SelfType { name, .. }) => Ok(Type::SelfType(name)),
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
                        Err(ErrorsEmitted(()))
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
                    Err(ErrorsEmitted(()))
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

            Some(Token::Identifier { name, .. }) => Ok(Type::UserDefined(name)),

            _ => {
                parser.log_error(ParserErrorKind::UnexpectedToken {
                    expected: "type annotation".to_string(),
                    found: token,
                });
                Err(ErrorsEmitted(()))
            }
        }
    }
}
