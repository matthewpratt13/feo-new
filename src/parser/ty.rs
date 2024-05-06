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
        parser.logger.log(
            LogLevel::Debug,
            LogMsg("entering `Type::parse()`".to_string()),
        );
        parser.log_current_token(false);

        let token = parser.next_token();

        match token {
            Some(Token::I32Type { .. }) => Ok(Type::I32(Int::I32(0_i32))),
            Some(Token::I64Type { .. }) => Ok(Type::I64(Int::I64(0_i64))),
            Some(Token::I128Type { .. }) => Ok(Type::I128(Int::I128(0_i128))),
            Some(Token::U8Type { .. }) => Ok(Type::U8(UInt::U8(0_u8))),
            Some(Token::U16Type { .. }) => Ok(Type::U16(UInt::U16(0_u16))),
            Some(Token::U32Type { .. }) => Ok(Type::U32(UInt::U32(0_u32))),
            Some(Token::U64Type { .. }) => Ok(Type::U64(UInt::U64(0_u64))),
            Some(Token::U128Type { .. }) => Ok(Type::U128(UInt::U128(0_u128))),
            Some(Token::U256Type { .. }) => Ok(Type::U256(BigUInt::U256(U256::zero()))),
            Some(Token::U512Type { .. }) => Ok(Type::U512(BigUInt::U512(U512::zero()))),
            Some(Token::ByteType { .. }) => Ok(Type::Byte(Byte(0_u8))),
            Some(Token::B2Type { .. }) => Ok(Type::B2(Bytes::B2(B2::zero()))),
            Some(Token::B4Type { .. }) => Ok(Type::B4(Bytes::B4(B4::zero()))),
            Some(Token::B8Type { .. }) => Ok(Type::B8(Bytes::B8(B8::zero()))),
            Some(Token::B16Type { .. }) => Ok(Type::B16(Bytes::B16(B16::zero()))),
            Some(Token::B32Type { .. }) => Ok(Type::B32(Bytes::B32(B32::zero()))),
            Some(Token::H160Type { .. }) => Ok(Type::H160(Hash::H160(H160::zero()))),
            Some(Token::H256Type { .. }) => Ok(Type::H256(Hash::H256(H256::zero()))),
            Some(Token::H512Type { .. }) => Ok(Type::H512(Hash::H512(H512::zero()))),
            Some(Token::StrType { .. }) => Ok(Type::Str(Str(Vec::<u8>::new()))),
            Some(Token::CharType { .. }) => Ok(Type::Char(Char('\0'))),
            Some(Token::BoolType { .. }) => Ok(Type::Bool(Bool(false))),
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
                match parser.next_token() {
                    Some(Token::LessThan { .. }) => (),
                    Some(_) => parser.log_unexpected_token("`<`"),
                    None => parser.log_missing_token("`<`"),
                }

                let ty = Type::parse(parser)?;

                match parser.next_token() {
                    Some(Token::GreaterThan { .. }) => (),
                    Some(_) => parser.log_unexpected_token("`>`"),
                    None => parser.log_missing_token("`>`"),
                }

                Ok(Type::Vec(Box::new(ty)))
            }

            Some(Token::MappingType { .. }) => {
                match parser.next_token() {
                    Some(Token::LessThan { .. }) => (),
                    Some(_) => parser.log_unexpected_token("`<`"),
                    None => parser.log_missing_token("`<`"),
                }
                let key_type = Box::new(Type::parse(parser)?);

                match parser.next_token() {
                    Some(Token::Comma { .. }) => (),
                    Some(_) => parser.log_unexpected_token("`,`"),
                    None => parser.log_missing_token("`,`"),
                }

                let value_type = Box::new(Type::parse(parser)?);

                match parser.next_token() {
                    Some(Token::GreaterThan { .. }) => (),
                    Some(_) => parser.log_unexpected_token("`>`"),
                    None => parser.log_missing_token("`>`"),
                }
                Ok(Type::Mapping {
                    key_type,
                    value_type,
                })
            }

            Some(Token::OptionType { .. }) => {
                match parser.next_token() {
                    Some(Token::LessThan { .. }) => (),
                    Some(_) => parser.log_unexpected_token("`<`"),
                    None => parser.log_missing_token("`<`"),
                }

                let ty = Type::parse(parser)?;

                match parser.next_token() {
                    Some(Token::GreaterThan { .. }) => (),
                    Some(_) => parser.log_unexpected_token("`<`"),
                    None => parser.log_missing_token("`<`"),
                }

                Ok(Type::Option {
                    inner_type: Box::new(ty),
                })
            }

            Some(Token::ResultType { .. }) => {
                match parser.next_token() {
                    Some(Token::LessThan { .. }) => (),
                    Some(_) => parser.log_unexpected_token("`<`"),
                    None => parser.log_missing_token("`<`"),
                }

                let ok = Box::new(Type::parse(parser)?);

                match parser.next_token() {
                    Some(Token::Comma { .. }) => (),
                    Some(_) => parser.log_unexpected_token("`,`"),
                    None => parser.log_missing_token("`,`"),
                }

                let err = Box::new(Type::parse(parser)?);

                match parser.next_token() {
                    Some(Token::GreaterThan { .. }) => (),
                    Some(_) => parser.log_unexpected_token("`>`"),
                    None => parser.log_missing_token("`>`"),
                }

                Ok(Type::Result { ok, err })
            }

            Some(Token::Identifier { name, .. }) => {
                if &name == "_" {
                    let ty = InferredType {
                        underscore: Identifier(name),
                    };

                    Ok(Type::InferredType(ty))
                } else {
                    let path = PathExpr::parse(parser, PathPrefix::Identifier(Identifier(name)))?;
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
        parser.log_unexpected_token("identifier");
        Err(ErrorsEmitted)
    }?;

    let open_paren = if let Some(Token::LParen { .. }) = parser.current_token() {
        parser.next_token();
        Ok(Delimiter::LParen)
    } else {
        parser.log_unexpected_token("`(`");
        Err(ErrorsEmitted)
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
        parser.log_unmatched_delimiter(open_paren.clone());
        Err(ErrorsEmitted)
    }?;

    let return_type_opt = if let Some(Token::ThinArrow { .. }) = parser.current_token() {
        parser.next_token();
        Some(Box::new(Type::parse(parser)?))
    } else {
        None
    };

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

    let num_elements = if let Some(Token::UIntLiteral { value, .. }) = parser.next_token() {
        Ok(value)
    } else {
        parser.log_unexpected_token("unsigned integer");
        Err(ErrorsEmitted)
    }?;

    match parser.next_token() {
        Some(Token::RBracket { .. }) => (),
        Some(_) => parser.log_unexpected_token("`]`"),
        None => parser.log_missing_token("`]`"),
    }

    Ok(Type::Array {
        element_type: Box::new(ty),
        num_elements,
    })
}

fn parse_tuple_type(parser: &mut Parser) -> Result<Type, ErrorsEmitted> {
    if let Some(Token::RParen { .. }) = parser.current_token() {
        parser.next_token();
        Ok(Type::UnitType(Unit))
    } else if let Some(Token::Comma { .. }) = parser.peek_ahead_by(1) {
        let types =
            if let Some(t) = collection::get_collection(parser, Type::parse, Delimiter::RParen)? {
                Ok(t)
            } else {
                parser.log_unexpected_token("type");
                Err(ErrorsEmitted)
            }?;

        Ok(Type::Tuple(types))
    } else {
        let ty = Type::parse(parser)?;

        if let Some(Token::RParen { .. }) = parser.current_token() {
            parser.next_token();
            Ok(Type::GroupedType(Box::new(ty)))
        } else {
            parser.log_missing_token("`)`");
            parser.log_unmatched_delimiter(Delimiter::LParen);
            Err(ErrorsEmitted)
        }
    }
}

// TODO
#[cfg(test)]
mod tests {}
