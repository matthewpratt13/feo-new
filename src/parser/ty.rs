mod type_path;
pub(crate) use type_path::{build_item_path, get_type_paths};

use super::{collection, Parser};

use crate::{
    ast::{
        BigUInt, Bool, Byte, Bytes, Char, Delimiter, Float, FunctionOrMethodParam, FunctionPtr,
        Hash, Identifier, InferredType, Int, ReferenceOp, SelfType, Str, Type, TypePath, UInt,
        UnitType,
    },
    error::ErrorsEmitted,
    span::Position,
    token::{Token, TokenType},
    B16, B2, B32, B4, B8, F32, F64, H160, H256, H512, U256, U512,
};

use core::fmt;

impl Type {
    /// Match a `Token` to a `Type` and return the `Type` or emit an error.
    pub(crate) fn parse(parser: &mut Parser) -> Result<Type, ErrorsEmitted> {
        parser.logger.debug("entering `Type::parse()`");
        parser.log_current_token(false);

        let token = parser.next_token();

        match &token {
            Some(Token::I32Type { .. }) => Ok(Type::I32(Int::I32(i32::default()))),
            Some(Token::I64Type { .. }) => Ok(Type::I64(Int::I64(i64::default()))),
            Some(Token::U8Type { .. }) => Ok(Type::U8(UInt::U8(u8::default()))),
            Some(Token::U16Type { .. }) => Ok(Type::U16(UInt::U16(u16::default()))),
            Some(Token::U32Type { .. }) => Ok(Type::U32(UInt::U32(u32::default()))),
            Some(Token::U64Type { .. }) => Ok(Type::U64(UInt::U64(u64::default()))),
            Some(Token::U256Type { .. }) => Ok(Type::U256(BigUInt::U256(U256::default()))),
            Some(Token::U512Type { .. }) => Ok(Type::U512(BigUInt::U512(U512::default()))),
            Some(Token::F32Type { .. }) => Ok(Type::F32(Float::F32(F32::default()))),
            Some(Token::F64Type { .. }) => Ok(Type::F64(Float::F64(F64::default()))),
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
            Some(Token::Func { .. }) => parse_function_ptr_type(parser),
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
                parser.expect_token(TokenType::LessThan)?;

                let ty = Type::parse(parser)?;

                match parser.current_token() {
                    Some(Token::GreaterThan { .. }) => {
                        parser.next_token();
                        Ok(Type::Vec {
                            element_type: Box::new(ty),
                        })
                    }
                    Some(Token::EOF) | None => {
                        parser.log_unexpected_eoi();
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
                parser.expect_token(TokenType::LessThan)?;

                let key_type = Box::new(Type::parse(parser)?);

                parser.expect_token(TokenType::Comma)?;

                let value_type = Box::new(Type::parse(parser)?);

                match parser.current_token() {
                    Some(Token::GreaterThan { .. }) => {
                        parser.next_token();

                        Ok(Type::Mapping {
                            key_type,
                            value_type,
                        })
                    }
                    Some(Token::EOF) | None => {
                        parser.log_unexpected_eoi();
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
                parser.expect_token(TokenType::LessThan)?;

                let ty = Type::parse(parser)?;

                match parser.current_token() {
                    Some(Token::GreaterThan { .. }) => {
                        parser.next_token();

                        Ok(Type::Option {
                            inner_type: Box::new(ty),
                        })
                    }

                    Some(Token::EOF) | None => {
                        parser.log_unexpected_eoi();
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
                parser.expect_token(TokenType::LessThan)?;

                let ok_type = Box::new(Type::parse(parser)?);

                parser.expect_token(TokenType::Comma)?;

                let err_type = Box::new(Type::parse(parser)?);

                match parser.current_token() {
                    Some(Token::GreaterThan { .. }) => {
                        parser.next_token();
                        Ok(Type::Result { ok_type, err_type })
                    }
                    Some(Token::EOF) | None => {
                        parser.log_unexpected_eoi();
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
                        name: Identifier::from(name),
                    };

                    Ok(Type::InferredType(ty))
                } else {
                    let path = TypePath::parse(parser, token)?;
                    Ok(Type::UserDefined(path))
                }
            }

            Some(Token::Lib { .. }) => {
                let path = TypePath::parse(parser, token)?;
                Ok(Type::UserDefined(path))
            }

            Some(Token::Super { .. }) => {
                let path = TypePath::parse(parser, token)?;
                Ok(Type::UserDefined(path))
            }

            Some(Token::SelfKeyword { .. }) => {
                let path = TypePath::parse(parser, token)?;
                Ok(Type::UserDefined(path))
            }

            Some(Token::SelfType { .. }) => match parser.peek_ahead_by(1) {
                Some(Token::DblColon { .. }) => {
                    let path = TypePath::parse(parser, token)?;
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

impl fmt::Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Type::I32(_) => write!(f, "i32"),
            Type::I64(_) => write!(f, "i64"),
            Type::U8(_) => write!(f, "u8"),
            Type::U16(_) => write!(f, "u16"),
            Type::U32(_) => write!(f, "u32"),
            Type::U64(_) => write!(f, "u64"),
            Type::U256(_) => write!(f, "u256"),
            Type::U512(_) => write!(f, "u512"),
            Type::Byte(_) => write!(f, "byte"),
            Type::F32(_) => write!(f, "f32"),
            Type::F64(_) => write!(f, "f64"),
            Type::B2(_) => write!(f, "b2"),
            Type::B4(_) => write!(f, "b4"),
            Type::B8(_) => write!(f, "b8"),
            Type::B16(_) => write!(f, "b16"),
            Type::B32(_) => write!(f, "b32"),
            Type::H160(_) => write!(f, "h160"),
            Type::H256(_) => write!(f, "h256"),
            Type::H512(_) => write!(f, "h512"),
            Type::Str(_) => write!(f, "str"),
            Type::Char(_) => write!(f, "char"),
            Type::Bool(_) => write!(f, "bool"),
            Type::UnitType(_) => write!(f, "()"),
            Type::GroupedType(g) => write!(f, "({})", *g),
            Type::Array {
                element_type,
                num_elements,
            } => write!(f, "[{}; {}]", *element_type, num_elements),
            Type::Tuple(t) => write!(f, "({:?})", t),
            Type::UserDefined(ud) => write!(f, "{}", ud),
            Type::FunctionPtr(fp) => write!(f, "{}", fp),
            Type::Reference {
                reference_op,
                inner_type,
            } => match reference_op {
                ReferenceOp::Borrow => write!(f, "{}{}", reference_op, *inner_type),
                ReferenceOp::MutableBorrow => write!(f, "{} {}", reference_op, *inner_type),
            },
            Type::SelfType(_) => write!(f, "Self"),
            Type::InferredType(_) => write!(f, "_"),
            Type::Vec { element_type } => write!(f, "Vec<{}>", *element_type),
            Type::Mapping {
                key_type,
                value_type,
            } => write!(f, "Mapping<{}, {}>", *key_type, *value_type),
            Type::Option { inner_type } => write!(f, "Option<{}>", *inner_type),
            Type::Result { ok_type, err_type } => {
                write!(f, "Result<{}, {}>", *ok_type, *err_type)
            }
        }
    }
}

impl fmt::Debug for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::I32(_) => f.debug_tuple("I32").finish(),
            Self::I64(_) => f.debug_tuple("I64").finish(),
            Self::U8(_) => f.debug_tuple("U8").finish(),
            Self::U16(_) => f.debug_tuple("U16").finish(),
            Self::U32(_) => f.debug_tuple("U32").finish(),
            Self::U64(_) => f.debug_tuple("U64").finish(),
            Self::U256(_) => f.debug_tuple("U256").finish(),
            Self::U512(_) => f.debug_tuple("U512").finish(),
            Self::F32(_) => f.debug_tuple("F32").finish(),
            Self::F64(_) => f.debug_tuple("F64").finish(),
            Self::Byte(_) => f.debug_tuple("Byte").finish(),
            Self::B2(_) => f.debug_tuple("B2").finish(),
            Self::B4(_) => f.debug_tuple("B4").finish(),
            Self::B8(_) => f.debug_tuple("B8").finish(),
            Self::B16(_) => f.debug_tuple("B16").finish(),
            Self::B32(_) => f.debug_tuple("B32").finish(),
            Self::H160(_) => f.debug_tuple("H160").finish(),
            Self::H256(_) => f.debug_tuple("H256").finish(),
            Self::H512(_) => f.debug_tuple("H512").finish(),
            Self::Str(_) => f.debug_tuple("Str").finish(),
            Self::Char(_) => f.debug_tuple("Char").finish(),
            Self::Bool(_) => f.debug_tuple("Bool").finish(),
            Self::UnitType(_) => f.debug_tuple("UnitType").finish(),
            Self::GroupedType(arg0) => f.debug_tuple("GroupedType").field(arg0).finish(),
            Self::Array {
                element_type,
                num_elements,
            } => f
                .debug_struct("Array")
                .field("element_type", element_type)
                .field("num_elements", num_elements)
                .finish(),
            Self::Tuple(arg0) => f.debug_tuple("Tuple").field(arg0).finish(),
            Self::UserDefined(arg0) => f.debug_tuple("UserDefined").field(arg0).finish(),
            Self::FunctionPtr(arg0) => f.debug_tuple("FunctionPtr").field(arg0).finish(),
            Self::Reference {
                reference_op,
                inner_type,
            } => f
                .debug_struct("Reference")
                .field("reference_op", reference_op)
                .field("inner_type", inner_type)
                .finish(),
            Self::SelfType(_) => f.debug_tuple("SelfType").finish(),
            Self::InferredType(arg0) => f.debug_tuple("InferredType").field(arg0).finish(),
            Self::Vec { element_type } => f
                .debug_struct("Vec")
                .field("element_type", element_type)
                .finish(),
            Self::Mapping {
                key_type,
                value_type,
            } => f
                .debug_struct("Mapping")
                .field("key_type", key_type)
                .field("value_type", value_type)
                .finish(),
            Self::Option { inner_type } => f
                .debug_struct("Option")
                .field("inner_type", inner_type)
                .finish(),
            Self::Result { ok_type, err_type } => f
                .debug_struct("Result")
                .field("ok_type", ok_type)
                .field("err_type", err_type)
                .finish(),
        }
    }
}

fn parse_function_ptr_type(parser: &mut Parser) -> Result<Type, ErrorsEmitted> {
    let mut params: Vec<FunctionOrMethodParam> = Vec::new();

    let open_paren = match parser.current_token() {
        Some(Token::LParen { .. }) => {
            let position = parser.current_position();
            parser.next_token();
            Ok(Delimiter::LParen { position })
        }
        Some(Token::EOF) | None => {
            parser.log_unexpected_eoi();
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
        collection::get_collection(parser, FunctionOrMethodParam::parse, &open_paren)?;

    if subsequent_params.is_some() {
        params.append(&mut subsequent_params.unwrap())
    };

    parser.expect_closing_paren()?;

    let return_type_opt = if let Some(Token::ThinArrow { .. }) = parser.current_token() {
        parser.next_token();

        if parser.current_token().is_some() {
            Ok(Some(Box::new(Type::parse(parser)?)))
        } else {
            parser.log_missing("type", "function return type");
            parser.next_token();
            Err(ErrorsEmitted)
        }
    } else {
        Ok(None)
    }?;

    let ty = FunctionPtr {
        params_opt: {
            if params.is_empty() {
                None
            } else {
                Some(params)
            }
        },
        return_type_opt,
    };

    Ok(Type::FunctionPtr(ty))
}

fn parse_array_type(parser: &mut Parser) -> Result<Type, ErrorsEmitted> {
    let ty = Type::parse(parser)?;

    parser.expect_token(TokenType::Semicolon)?;

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

    parser.expect_token(TokenType::RBracket)?;

    Ok(Type::Array {
        element_type: Box::new(ty),
        num_elements,
    })
}

fn parse_tuple_type(parser: &mut Parser) -> Result<Type, ErrorsEmitted> {
    let open_paren = Delimiter::LParen {
        position: Position::new(parser.current - 1, &parser.stream.span().input()),
    };

    if let Some(Token::RParen { .. }) = parser.current_token() {
        parser.next_token();
        Ok(Type::UnitType(UnitType))
    } else if let Some(Token::Comma { .. }) = parser.peek_ahead_by(1) {
        let types = if let Some(t) = collection::get_collection(parser, Type::parse, &open_paren)? {
            parser.next_token();
            Ok(t)
        } else {
            parser.log_missing("type", "tuple element type");
            parser.next_token();
            Err(ErrorsEmitted)
        }?;

        Ok(Type::Tuple(types))
    } else {
        let ty = Type::parse(parser)?;

        let _ = parser.get_parenthesized_item_span(None)?;

        Ok(Type::GroupedType(Box::new(ty)))
    }
}

// TODO: test `(T, U, V)`, `[T; n]` , `func(T) -> U`, `Result<T, E>`, `Option<T>`, `&T`, `&mut T`,
// TODO: `Mapping<K, V>`, `Vec<T>`, `()`, `Self` and `TypePath` (and `TypePath<T: TypePath, U>`)
// TODO: also test nested types; e.g., `Result<Option<Vec<Mapping<K, V>>>, E>` and
// TODO: `Mapping<h256, Mapping<K, V>>`
#[cfg(test)]
mod tests {}
