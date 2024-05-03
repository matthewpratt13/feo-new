use crate::{
    ast::{
        Delimiter, FunctionOrMethodParam, Identifier, PathExpr, PathPrefix, PrimitiveType,
        SelfType, Type,
    },
    error::ErrorsEmitted,
    token::Token,
};

use super::{collection, test_utils::log_token, Parser};

impl Type {
    /// Match a `Token` to a `Type` and return the `Type` or emit an error.
    pub(crate) fn parse(parser: &mut Parser) -> Result<Type, ErrorsEmitted> {
        log_token(parser, "enter `TypeExpr::parse()`", true);

        let token = parser.next_token();

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
            Some(Token::LParen { .. }) => parse_tuple_type(parser),
            Some(Token::LBracket { .. }) => parse_array_type(parser),
            Some(Token::Func { .. }) => parse_function_type(token, parser),
            Some(Token::Ampersand { .. } | Token::AmpersandMut { .. }) => {
                let inner_type = Box::new(Type::parse(parser)?);
                Ok(Type::Reference(inner_type))
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

                Ok(Type::Option(Box::new(ty)))
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

    if let Some(Token::LBrace { .. }) = parser.current_token() {
        parser.next_token();
    } else {
        parser.log_unexpected_token("`{`");
    };

    if let Some(Token::LParen { .. }) = parser.current_token() {
        parser.next_token();
    } else {
        parser.log_unexpected_token("`(`");
    }

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

    match parser.next_token() {
        Some(Token::RParen { .. }) => (),
        Some(_) => parser.log_unexpected_token("`)`"),
        None => parser.log_missing_token("`)`"),
    }

    let return_type_opt = if let Some(Token::ThinArrow { .. }) = parser.current_token() {
        parser.next_token();
        Some(Box::new(Type::parse(parser)?))
    } else {
        None
    };

    Ok(Type::Function {
        function_name,
        params_opt: {
            match params.is_empty() {
                true => None,
                false => Some(params),
            }
        },
        return_type_opt,
    })
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
        Ok(Type::UnitType)
    } else {
        let types =
            if let Some(t) = collection::get_collection(parser, Type::parse, Delimiter::RParen)? {
                Ok(t)
            } else {
                parser.log_unexpected_token("type");
                Err(ErrorsEmitted)
            }?;

        match parser.next_token() {
            Some(Token::RParen { .. }) => (),
            Some(_) => parser.log_unexpected_token("`)`"),
            None => parser.log_missing_token("`)`"),
        }

        Ok(Type::Tuple(types))
    }
}

// TODO
#[cfg(test)]
mod tests {}
