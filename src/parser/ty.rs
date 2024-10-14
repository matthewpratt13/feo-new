mod type_path;

use core::fmt;

use crate::{
    ast::{
        Delimiter, FunctionOrMethodParam, FunctionPtr, GenericParam, Identifier, InferredType,
        ReferenceOp, SelfType, Type, TypePath, UnitType,
    },
    error::ErrorsEmitted,
    log_trace,
    parser::parse_generic_param,
    semantic_analyser::{FormatObject, ToIdentifier},
    span::Position,
    token::{Token, TokenType},
};

pub(crate) use self::type_path::get_type_paths;

use super::{get_collection, Parser};

impl Type {
    pub(crate) const UNIT_TYPE: Type = Type::UnitType(UnitType);
    // pub(crate) const SELF_TYPE: Type = Type::SelfType(SelfType);

    /// Match a `Token` to a `Type` and return the `Type` or emit an error.
    pub(crate) fn parse(parser: &mut Parser) -> Result<Type, ErrorsEmitted> {
        log_trace!(parser.logger, "entering `Type::parse()` …");
        parser.log_current_token(false);

        let token = parser.next_token();

        match &token {
            Some(Token::I32Type { .. }) => Ok(Type::I32),
            Some(Token::I64Type { .. }) => Ok(Type::I64),
            Some(Token::U8Type { .. }) => Ok(Type::U8),
            Some(Token::U16Type { .. }) => Ok(Type::U16),
            Some(Token::U32Type { .. }) => Ok(Type::U32),
            Some(Token::U64Type { .. }) => Ok(Type::U64),
            Some(Token::U256Type { .. }) => Ok(Type::U256),
            Some(Token::U512Type { .. }) => Ok(Type::U512),
            Some(Token::F32Type { .. }) => Ok(Type::F32),
            Some(Token::F64Type { .. }) => Ok(Type::F64),
            Some(Token::ByteType { .. }) => Ok(Type::Byte),
            Some(Token::B2Type { .. }) => Ok(Type::B2),
            Some(Token::B4Type { .. }) => Ok(Type::B4),
            Some(Token::B8Type { .. }) => Ok(Type::B8),
            Some(Token::B16Type { .. }) => Ok(Type::B16),
            Some(Token::B32Type { .. }) => Ok(Type::B32),
            Some(Token::H160Type { .. }) => Ok(Type::H160),
            Some(Token::H256Type { .. }) => Ok(Type::H256),
            Some(Token::H512Type { .. }) => Ok(Type::H512),
            Some(Token::StrType { .. }) => Ok(Type::Str),
            Some(Token::CharType { .. }) => Ok(Type::Char),
            Some(Token::BoolType { .. }) => Ok(Type::Bool),
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
                        parser.emit_unexpected_eoi();
                        parser.warn_missing_token(&TokenType::LessThan.to_string());
                        Err(ErrorsEmitted)
                    }
                    _ => {
                        parser.emit_unexpected_token(&TokenType::LessThan.to_string());
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
                        parser.emit_unexpected_eoi();
                        parser.warn_missing_token(&TokenType::LessThan.to_string());
                        Err(ErrorsEmitted)
                    }
                    _ => {
                        parser.emit_unexpected_token(&TokenType::LessThan.to_string());
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
                        parser.emit_unexpected_eoi();
                        parser.warn_missing_token(&TokenType::LessThan.to_string());
                        Err(ErrorsEmitted)
                    }
                    _ => {
                        parser.emit_unexpected_token(&TokenType::LessThan.to_string());
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
                        parser.emit_unexpected_eoi();
                        parser.warn_missing_token(&TokenType::LessThan.to_string());
                        Err(ErrorsEmitted)
                    }
                    _ => {
                        parser.emit_unexpected_token(&TokenType::LessThan.to_string());
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
                    if name.len() == 1
                        && name
                            .as_str()
                            .chars()
                            .next()
                            .is_some_and(|c| c.is_uppercase())
                    {
                        let generic_param = parse_generic_param(parser)?;

                        Ok(Type::Generic(GenericParam {
                            name: generic_param.name,
                            type_bound_opt: generic_param.type_bound_opt,
                        }))
                    } else {
                        let path = TypePath::parse(parser, token)?;
                        Ok(Type::UserDefined(path))
                    }
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
                _ => Ok(Type::self_type(ReferenceOp::Owned)),
            },

            Some(Token::EOF) | None => {
                parser.emit_unexpected_eoi();
                Err(ErrorsEmitted)
            }

            _ => {
                parser.emit_unexpected_token("type annotation");
                Err(ErrorsEmitted)
            }
        }
    }

    pub(crate) fn inferred_type(name_str: &str) -> Type {
        Type::InferredType(InferredType {
            name: Identifier::from(name_str),
        })
    }

    pub(crate) fn self_type(reference_op: ReferenceOp) -> Type {
        Type::SelfType {
            reference_op,
            ty: SelfType,
        }
    }
}

impl FormatObject for Type {}

impl ToIdentifier for Type {}

impl fmt::Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Type::I32 => write!(f, "i32"),
            Type::I64 => write!(f, "i64"),
            Type::U8 => write!(f, "u8"),
            Type::U16 => write!(f, "u16"),
            Type::U32 => write!(f, "u32"),
            Type::U64 => write!(f, "u64"),
            Type::U256 => write!(f, "u256"),
            Type::U512 => write!(f, "u512"),
            Type::Byte => write!(f, "byte"),
            Type::F32 => write!(f, "f32"),
            Type::F64 => write!(f, "f64"),
            Type::B2 => write!(f, "b2"),
            Type::B4 => write!(f, "b4"),
            Type::B8 => write!(f, "b8"),
            Type::B16 => write!(f, "b16"),
            Type::B32 => write!(f, "b32"),
            Type::H160 => write!(f, "h160"),
            Type::H256 => write!(f, "h256"),
            Type::H512 => write!(f, "h512"),
            Type::Str => write!(f, "str"),
            Type::Char => write!(f, "char"),
            Type::Bool => write!(f, "bool"),
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
            } => write!(f, "{}{}", reference_op, *inner_type),
            Type::SelfType { reference_op, .. } => match reference_op {
                ReferenceOp::Borrow => write!(f, "&Self"),
                ReferenceOp::MutableBorrow => write!(f, "&mut Self"),
                ReferenceOp::Owned => write!(f, "Self"),
            },
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
            Type::Generic(GenericParam {
                name,
                type_bound_opt: bounds_opt,
            }) => write!(
                f,
                "{}: {}",
                name,
                bounds_opt
                    .clone()
                    .unwrap_or(TypePath::from(Identifier::from("_")))
            ),
        }
    }
}

impl fmt::Debug for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::I32 => write!(f, "i32"),
            Self::I64 => write!(f, "i64"),
            Self::U8 => write!(f, "u8"),
            Self::U16 => write!(f, "u16"),
            Self::U32 => write!(f, "u32"),
            Self::U64 => write!(f, "u64"),
            Self::U256 => write!(f, "u256"),
            Self::U512 => write!(f, "u512"),
            Self::F32 => write!(f, "f32"),
            Self::F64 => write!(f, "f64"),
            Self::Byte => write!(f, "byte"),
            Self::B2 => write!(f, "b2"),
            Self::B4 => write!(f, "b4"),
            Self::B8 => write!(f, "b8"),
            Self::B16 => write!(f, "b16"),
            Self::B32 => write!(f, "b32"),
            Self::H160 => write!(f, "h160"),
            Self::H256 => write!(f, "h256"),
            Self::H512 => write!(f, "h512"),
            Self::Str => write!(f, "str"),
            Self::Char => write!(f, "char"),
            Self::Bool => write!(f, "bool"),
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
            Self::SelfType { reference_op, ty } => f
                .debug_struct("SelfType")
                .field("reference_op", reference_op)
                .field("ty", ty)
                .finish(),
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
            Self::Generic(GenericParam {
                name,
                type_bound_opt,
            }) => f
                .debug_struct("Generic")
                .field("name", name)
                .field("type_bound_opt", type_bound_opt)
                .finish(),
        }
    }
}

fn parse_function_ptr_type(parser: &mut Parser) -> Result<Type, ErrorsEmitted> {
    let mut params: Vec<FunctionOrMethodParam> = Vec::new();

    let open_paren = parser.expect_open_paren()?;

    // `&self` and `&mut self` can only occur as the first parameter in a method
    if let Some(Token::Ampersand { .. } | Token::AmpersandMut { .. }) = parser.current_token() {
        let param = FunctionOrMethodParam::parse(parser)?;
        params.push(param);
    }

    let subsequent_params = get_collection(parser, FunctionOrMethodParam::parse, &open_paren)?;

    if subsequent_params.is_some() {
        params.append(&mut subsequent_params.unwrap())
    };

    parser.expect_closing_paren()?;

    let return_type_opt = if let Some(Token::ThinArrow { .. }) = parser.current_token() {
        parser.next_token();

        if parser.current_token().is_some() {
            Ok(Some(Box::new(Type::parse(parser)?)))
        } else {
            parser.emit_missing_node("type", "function return type");
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
            parser.emit_unexpected_eoi();
            Err(ErrorsEmitted)
        }
        _ => {
            parser.emit_unexpected_token("array length (unsigned integer)");
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
        Ok(Type::UNIT_TYPE)
    } else if let Some(Token::Comma { .. }) = parser.peek_ahead_by(1) {
        let types = if let Some(t) = get_collection(parser, Type::parse, &open_paren)? {
            parser.next_token();
            Ok(t)
        } else {
            parser.emit_missing_node("type", "tuple element type");
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
