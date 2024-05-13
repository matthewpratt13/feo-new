use crate::{
    ast::{
        BigUInt, Bool, Byte, Bytes, Char, Delimiter, FunctionOrMethodParam, FunctionPtr, Hash,
        Identifier, InferredType, Int, PathExpr, PathPrefix, ReferenceOp, SelfType, Str, Type,
        UInt, Unit,
    },
    error::ErrorsEmitted,
    logger::{LogLevel, LogMsg},
    token::Token,
    B16, B2, B32, B4, B8, H160, H256, H512, U256, U512,
};

use super::{collection, Parser};

impl Type {
    /// Match a `Token` to a `Type` and return the `Type` or emit an error.
    pub(crate) fn parse(parser: &mut Parser) -> Result<Type, ErrorsEmitted> {
        parser
            .logger
            .log(LogLevel::Debug, LogMsg::from("entering `Type::parse()`"));
        parser.log_current_token(false);

        let token = parser.next_token();

        match token.as_ref() {
            Some(Token::I32Type { .. }) => Ok(Type::I32(Int::I32(i32::default()))),
            Some(Token::I64Type { .. }) => Ok(Type::I64(Int::I64(i64::default()))),
            Some(Token::I128Type { .. }) => Ok(Type::I128(Int::I128(i128::default()))),
            Some(Token::U8Type { .. }) => Ok(Type::U8(UInt::U8(u8::default()))),
            Some(Token::U16Type { .. }) => Ok(Type::U16(UInt::U16(u16::default()))),
            Some(Token::U32Type { .. }) => Ok(Type::U32(UInt::U32(u32::default()))),
            Some(Token::U64Type { .. }) => Ok(Type::U64(UInt::U64(u64::default()))),
            Some(Token::U128Type { .. }) => Ok(Type::U128(UInt::U128(u128::default()))),
            Some(Token::U256Type { .. }) => Ok(Type::U256(BigUInt::U256(U256::default()))),
            Some(Token::U512Type { .. }) => Ok(Type::U512(BigUInt::U512(U512::default()))),
            Some(Token::ByteType { .. }) => Ok(Type::Byte(Byte::from(u8::default()))),
            Some(Token::B2Type { .. }) => Ok(Type::B2(Bytes::B2(B2::default()))),
            Some(Token::B4Type { .. }) => Ok(Type::B4(Bytes::B4(B4::default()))),
            Some(Token::B8Type { .. }) => Ok(Type::B8(Bytes::B8(B8::default()))),
            Some(Token::B16Type { .. }) => Ok(Type::B16(Bytes::B16(B16::default()))),
            Some(Token::B32Type { .. }) => Ok(Type::B32(Bytes::B32(B32::default()))),
            Some(Token::H160Type { .. }) => Ok(Type::H160(Hash::H160(H160::default()))),
            Some(Token::H256Type { .. }) => Ok(Type::H256(Hash::H256(H256::default()))),
            Some(Token::H512Type { .. }) => Ok(Type::H512(Hash::H512(H512::default()))),
            Some(Token::StrType { .. }) => Ok(Type::Str(Str::from(String::default().as_str()))),
            Some(Token::CharType { .. }) => Ok(Type::Char(Char::from(char::default()))),
            Some(Token::BoolType { .. }) => Ok(Type::Bool(Bool::from(bool::default()))),
            Some(Token::LParen { .. }) => parse_tuple_type(parser),
            Some(Token::LBracket { .. }) => parse_array_type(parser),
            Some(Token::Func { .. }) => parse_function_type(token, parser),
            Some(Token::Ampersand { .. }) => {
                let inner_type = Box::new(Type::parse(parser)?);
                Ok(Type::Reference {
                    reference_op: ReferenceOp::Borrow,
                    inner_type,
                })
            }
            Some(Token::AmpersandMut { .. }) => {
                let inner_type = Box::new(Type::parse(parser)?);
                Ok(Type::Reference {
                    reference_op: ReferenceOp::MutableBorrow,
                    inner_type,
                })
            }
            Some(Token::VecType { .. }) => {
                match parser.current_token().as_ref() {
                    Some(Token::LessThan { .. }) => {
                        parser.next_token();
                    }
                    Some(Token::EOF) | None => {
                        parser.log_missing_token("`<`");
                        return Err(ErrorsEmitted);
                    }
                    _ => {
                        parser.log_unexpected_token("`<`");
                        return Err(ErrorsEmitted);
                    }
                }

                let ty = Type::parse(parser)?;

                match parser.current_token().as_ref() {
                    Some(Token::GreaterThan { .. }) => {
                        parser.next_token();
                        Ok(Type::Vec(Box::new(ty)))
                    }
                    Some(Token::EOF) | None => {
                        parser.log_missing_token("`>`");
                        Err(ErrorsEmitted)
                    }
                    _ => {
                        parser.log_unexpected_token("`>`");
                        Err(ErrorsEmitted)
                    }
                }
            }

            Some(Token::MappingType { .. }) => {
                match parser.current_token().as_ref() {
                    Some(Token::LessThan { .. }) => {
                        parser.next_token();
                    }
                    Some(Token::EOF) | None => {
                        parser.log_missing_token("`<`");
                        return Err(ErrorsEmitted);
                    }
                    _ => {
                        parser.log_unexpected_token("`<`");
                        return Err(ErrorsEmitted);
                    }
                }

                let key_type = Box::new(Type::parse(parser)?);

                match parser.current_token().as_ref() {
                    Some(Token::Comma { .. }) => {
                        parser.next_token();
                    }
                    Some(Token::EOF) | None => {
                        parser.log_missing_token("`,`");
                        return Err(ErrorsEmitted);
                    }
                    _ => {
                        parser.log_unexpected_token("`,`");
                        return Err(ErrorsEmitted);
                    }
                }

                let value_type = Box::new(Type::parse(parser)?);

                match parser.current_token().as_ref() {
                    Some(Token::GreaterThan { .. }) => {
                        parser.next_token();

                        Ok(Type::Mapping {
                            key_type,
                            value_type,
                        })
                    }
                    Some(Token::EOF) | None => {
                        parser.log_missing_token("`>`");
                        Err(ErrorsEmitted)
                    }
                    _ => {
                        parser.log_unexpected_token("`>`");
                        Err(ErrorsEmitted)
                    }
                }
            }

            Some(Token::OptionType { .. }) => {
                match parser.current_token().as_ref() {
                    Some(Token::LessThan { .. }) => {
                        parser.next_token();
                    }
                    Some(Token::EOF) | None => {
                        parser.log_missing_token("`<`");
                        return Err(ErrorsEmitted);
                    }
                    _ => {
                        parser.log_unexpected_token("`<`");
                        return Err(ErrorsEmitted);
                    }
                }

                let ty = Type::parse(parser)?;

                match parser.current_token().as_ref() {
                    Some(Token::GreaterThan { .. }) => {
                        parser.next_token();

                        Ok(Type::Option {
                            inner_type: Box::new(ty),
                        })
                    }

                    Some(Token::EOF) | None => {
                        parser.log_missing_token("`>`");
                        Err(ErrorsEmitted)
                    }
                    _ => {
                        parser.log_unexpected_token("`>`");
                        Err(ErrorsEmitted)
                    }
                }
            }

            Some(Token::ResultType { .. }) => {
                match parser.current_token().as_ref() {
                    Some(Token::LessThan { .. }) => {
                        parser.next_token();
                    }
                    Some(Token::EOF) | None => {
                        parser.log_missing_token("`<`");
                        return Err(ErrorsEmitted);
                    }
                    _ => {
                        parser.log_unexpected_token("`<`");
                        return Err(ErrorsEmitted);
                    }
                }
                let ok = Box::new(Type::parse(parser)?);

                match parser.current_token().as_ref() {
                    Some(Token::Comma { .. }) => {
                        parser.next_token();
                    }
                    Some(Token::EOF) | None => {
                        parser.log_missing_token("`,`");
                        return Err(ErrorsEmitted);
                    }
                    _ => {
                        parser.log_unexpected_token("`,`");
                        return Err(ErrorsEmitted);
                    }
                }
                let err = Box::new(Type::parse(parser)?);

                match parser.current_token().as_ref() {
                    Some(Token::GreaterThan { .. }) => {
                        parser.next_token();
                        Ok(Type::Result { ok, err })
                    }
                    Some(Token::EOF) | None => {
                        parser.log_missing_token("`>`");
                        Err(ErrorsEmitted)
                    }
                    _ => {
                        parser.log_unexpected_token("`>`");
                        Err(ErrorsEmitted)
                    }
                }
            }

            Some(Token::Identifier { name, .. }) => {
                if name == "_" {
                    let ty = InferredType {
                        underscore: Identifier::from(name),
                    };

                    Ok(Type::InferredType(ty))
                } else {
                    let path =
                        PathExpr::parse(parser, PathPrefix::Identifier(Identifier::from(name)))?;
                    Ok(Type::UserDefined(path))
                }
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

            Some(Token::SelfType { .. }) => match parser.peek_ahead_by(1) {
                Some(Token::DblColon { .. }) => {
                    let path = PathExpr::parse(parser, PathPrefix::SelfType(SelfType))?;
                    Ok(Type::UserDefined(path))
                }
                _ => Ok(Type::SelfType(SelfType)),
            },

            Some(Token::EOF) | None => {
                parser.log_unexpected_eoi();
                Err(ErrorsEmitted)
            }

            _ => {
                parser.log_unexpected_token("type annotation");
                Err(ErrorsEmitted)
            }
        }
    }
}

fn parse_function_type(token: Option<Token>, parser: &mut Parser) -> Result<Type, ErrorsEmitted> {
    let mut params: Vec<FunctionOrMethodParam> = Vec::new();

    let function_name = if let Some(Token::Identifier { name, .. }) = token {
        Ok(Identifier(name))
    } else {
        parser.log_unexpected_token("function name");
        Err(ErrorsEmitted)
    }?;

    let open_paren = match parser.current_token().as_ref() {
        Some(Token::LParen { .. }) => {
            parser.next_token();
            Ok(Delimiter::LParen)
        }
        Some(Token::EOF) | None => {
            parser.log_missing_token("`(`");
            Err(ErrorsEmitted)
        }
        _ => {
            parser.log_unexpected_token("`(`");
            Err(ErrorsEmitted)
        }
    }?;

    // `&self` and `&mut self` can only occur as the first parameter in a method
    if let Some(Token::Ampersand { .. } | Token::AmpersandMut { .. }) = parser.current_token() {
        let param = FunctionOrMethodParam::parse(parser)?;
        params.push(param);
    }

    let subsequent_params =
        collection::get_collection(parser, FunctionOrMethodParam::parse, Delimiter::RParen)?;

    if subsequent_params.is_some() {
        params.append(&mut subsequent_params.unwrap())
    };

    let close_paren = if let Some(Token::RParen { .. }) = parser.next_token() {
        Ok(Delimiter::RParen)
    } else {
        parser.log_missing_token("`)`");
        parser.log_unmatched_delimiter(&open_paren);
        Err(ErrorsEmitted)
    }?;

    let return_type_opt = if let Some(Token::ThinArrow { .. }) = parser.current_token() {
        parser.next_token();

        if parser.current_token().is_some() {
            Ok(Some(Box::new(Type::parse(parser)?)))
        } else {
            parser.log_missing("type", "function return type");
            Err(ErrorsEmitted)
        }
    } else {
        Ok(None)
    }?;

    let ty = FunctionPtr {
        function_name,
        open_paren,
        params_opt: {
            if params.is_empty() {
                None
            } else {
                Some(params)
            }
        },
        close_paren,
        return_type_opt,
    };

    Ok(Type::FunctionPtr(ty))
}

fn parse_array_type(parser: &mut Parser) -> Result<Type, ErrorsEmitted> {
    let ty = Type::parse(parser)?;

    if let Some(Token::Semicolon { .. }) = parser.current_token() {
        parser.next_token();
    } else {
        parser.log_unexpected_token("`;`");
    }

    let num_elements = match parser.next_token() {
        Some(Token::UIntLiteral { value, .. }) => Ok(value),
        Some(Token::EOF) | None => {
            parser.log_unexpected_eoi();
            Err(ErrorsEmitted)
        }
        _ => {
            parser.log_unexpected_token("array length (unsigned integer)");
            Err(ErrorsEmitted)
        }
    }?;

    match parser.current_token().as_ref() {
        Some(Token::RBracket { .. }) => {
            parser.next_token();

            Ok(Type::Array {
                element_type: Box::new(ty),
                num_elements,
            })
        }
        Some(Token::EOF) | None => {
            parser.log_missing_token("`]`");
            Err(ErrorsEmitted)
        }
        _ => {
            parser.log_unexpected_token("`]`");
            Err(ErrorsEmitted)
        }
    }
}

fn parse_tuple_type(parser: &mut Parser) -> Result<Type, ErrorsEmitted> {
    if let Some(Token::RParen { .. }) = parser.current_token() {
        parser.next_token();
        Ok(Type::UnitType(Unit))
    } else if let Some(Token::Comma { .. }) = parser.peek_ahead_by(1) {
        let types =
            if let Some(t) = collection::get_collection(parser, Type::parse, Delimiter::RParen)? {
                parser.next_token();
                Ok(t)
            } else {
                parser.log_missing("type", "tuple element type");
                Err(ErrorsEmitted)
            }?;

        Ok(Type::Tuple(types))
    } else {
        let ty = Type::parse(parser)?;

        match parser.current_token().as_ref() {
            Some(Token::RParen { .. }) => {
                parser.next_token();
                Ok(Type::GroupedType(Box::new(ty)))
            }
            Some(Token::EOF) | None => {
                parser.log_missing_token("`)`");
                parser.log_unmatched_delimiter(&Delimiter::LParen);
                Err(ErrorsEmitted)
            }
            _ => {
                parser.log_unexpected_token("`)`");
                Err(ErrorsEmitted)
            }
        }
    }
}

// TODO
#[cfg(test)]
mod tests {}
