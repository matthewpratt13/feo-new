use super::{symbol_table::Symbol, SemanticAnalyser};

use crate::{
    ast::{
        BigUInt, Bool, Byte, Bytes, Char, Float, Hash, Identifier, InferredType, Int, Keyword,
        LiteralPatt, Pattern, Str, Type, TypePath, UInt, UnitType,
    },
    error::SemanticErrorKind,
    B16, B2, B32, B4, B8, F32, F64, H160, H256, H512, U256, U512,
};

use std::collections::HashMap;

// TODO: alphabetize match arms

/// Analyse a pattern in the context of semantic analysis, returning the inferred type of
/// the pattern or an appropriate semantic error.
pub(crate) fn analyse_patt(
    analyser: &mut SemanticAnalyser,
    pattern: &Pattern,
) -> Result<Type, SemanticErrorKind> {
    match pattern {
        Pattern::IdentifierPatt(i) => match analyser.lookup(&TypePath::from(i.name.clone())) {
            Some(Symbol::Variable { var_type, .. }) => {
                // TODO: if an element is a generic type, resolve it,
                // TODO: including checking if it implements its bound trait (where applicable)
                Ok(var_type.clone())
            }
            Some(sym) => Err(SemanticErrorKind::UnexpectedSymbol {
                name: i.name.clone(),
                expected: "variable".to_string(),
                found: format!("`{sym}`"),
            }),
            _ => Err(SemanticErrorKind::UndefinedVariable {
                name: i.name.clone(),
            }),
        },

        Pattern::PathPatt(p) => {
            let path = TypePath::from(p.clone());

            if let Some(sym) = analyser.lookup(&path) {
                Ok(sym.symbol_type())
            } else {
                Err(SemanticErrorKind::UndefinedVariable {
                    name: path.type_name,
                })
            }
        }

        Pattern::LiteralPatt(l) => match l {
            LiteralPatt::Int { value } => match value {
                Int::I32(_) => Ok(Type::I32(Int::I32(i32::default()))),
                Int::I64(_) => Ok(Type::I64(Int::I64(i64::default()))),
            },
            LiteralPatt::UInt { value } => match value {
                UInt::U8(_) => Ok(Type::U8(UInt::U8(u8::default()))),
                UInt::U16(_) => Ok(Type::U16(UInt::U16(u16::default()))),
                UInt::U32(_) => Ok(Type::U32(UInt::U32(u32::default()))),
                UInt::U64(_) => Ok(Type::U64(UInt::U64(u64::default()))),
            },
            LiteralPatt::BigUInt { value } => match value {
                BigUInt::U256(_) => Ok(Type::U256(BigUInt::U256(U256::default()))),
                BigUInt::U512(_) => Ok(Type::U512(BigUInt::U512(U512::default()))),
            },
            LiteralPatt::Float { value } => match value {
                Float::F32(_) => Ok(Type::F32(Float::F32(F32::default()))),
                Float::F64(_) => Ok(Type::F64(Float::F64(F64::default()))),
            },
            LiteralPatt::Byte { .. } => Ok(Type::Byte(Byte::from(u8::default()))),
            LiteralPatt::Bytes { value } => match value {
                Bytes::B2(_) => Ok(Type::B2(Bytes::B2(B2::default()))),
                Bytes::B4(_) => Ok(Type::B4(Bytes::B4(B4::default()))),
                Bytes::B8(_) => Ok(Type::B8(Bytes::B8(B8::default()))),
                Bytes::B16(_) => Ok(Type::B16(Bytes::B16(B16::default()))),
                Bytes::B32(_) => Ok(Type::B32(Bytes::B32(B32::default()))),
            },
            LiteralPatt::Hash { value } => match value {
                Hash::H160(_) => Ok(Type::H160(Hash::H160(H160::default()))),
                Hash::H256(_) => Ok(Type::H256(Hash::H256(H256::default()))),
                Hash::H512(_) => Ok(Type::H512(Hash::H512(H512::default()))),
            },
            LiteralPatt::Str { .. } => Ok(Type::Str(Str::from(String::default().as_str()))),
            LiteralPatt::Char { .. } => Ok(Type::Char(Char::from(char::default()))),
            LiteralPatt::Bool { .. } => Ok(Type::Bool(Bool::from(bool::default()))),
        },

        Pattern::ReferencePatt(r) => Ok(Type::Reference {
            reference_op: r.reference_op,
            inner_type: Box::new(analyse_patt(analyser, &r.pattern)?),
        }),

        Pattern::GroupedPatt(g) => analyse_patt(analyser, &g.inner_pattern),

        Pattern::RangePatt(r) => match (&r.from_pattern_opt, &r.to_pattern_opt) {
            (None, None) => Ok(Type::UnitType(UnitType)),
            (None, Some(to)) => {
                let to_type = analyse_patt(analyser, &to.clone())?;

                match to_type {
                    Type::I32(_)
                    | Type::I64(_)
                    | Type::U8(_)
                    | Type::U16(_)
                    | Type::U32(_)
                    | Type::U64(_)
                    | Type::U256(_)
                    | Type::U512(_)
                    | Type::Byte(_)
                    | Type::Char(_) => Ok(to_type),
                    _ => Err(SemanticErrorKind::UnexpectedType {
                        expected: "numeric type, `byte` or `char`".to_string(),
                        found: to_type,
                    }),
                }
            }
            (Some(from), None) => {
                let from_type = analyse_patt(analyser, &from.clone())?;

                match from_type {
                    Type::I32(_)
                    | Type::I64(_)
                    | Type::U8(_)
                    | Type::U16(_)
                    | Type::U32(_)
                    | Type::U64(_)
                    | Type::U256(_)
                    | Type::U512(_)
                    | Type::Byte(_)
                    | Type::Char(_) => Ok(from_type),
                    _ => Err(SemanticErrorKind::UnexpectedType {
                        expected: "numeric type, `byte` or `char`".to_string(),
                        found: from_type,
                    }),
                }
            }
            (Some(from), Some(to)) => {
                let from_type = analyse_patt(analyser, &from.clone())?;

                match from_type {
                    Type::I32(_)
                    | Type::I64(_)
                    | Type::U8(_)
                    | Type::U16(_)
                    | Type::U32(_)
                    | Type::U64(_)
                    | Type::U256(_)
                    | Type::U512(_)
                    | Type::Byte(_)
                    | Type::Char(_) => (),
                    _ => {
                        return Err(SemanticErrorKind::UnexpectedType {
                            expected: "numeric type, `byte` or `char`".to_string(),
                            found: from_type,
                        })
                    }
                }

                let to_type = analyse_patt(analyser, &to.clone())?;

                match to_type {
                    Type::I32(_)
                    | Type::I64(_)
                    | Type::U8(_)
                    | Type::U16(_)
                    | Type::U32(_)
                    | Type::U64(_)
                    | Type::U256(_)
                    | Type::U512(_)
                    | Type::Byte(_)
                    | Type::Char(_) => (),
                    _ => {
                        return Err(SemanticErrorKind::UnexpectedType {
                            expected: "numeric type, `byte` or `char`".to_string(),
                            found: from_type,
                        })
                    }
                }

                if from_type == to_type {
                    Ok(to_type)
                } else {
                    Err(SemanticErrorKind::TypeMismatchValues {
                        expected: from_type,
                        found: to_type,
                    })
                }
            }
        },

        Pattern::TuplePatt(t) => {
            let mut element_types: Vec<Type> = Vec::new();

            for patt in t.tuple_patt_elements.elements.iter() {
                let ty = analyse_patt(analyser, patt)?;

                // TODO: if an element is a generic type, resolve it,
                // TODO: including checking if it implements its bound trait (where applicable)

                element_types.push(ty)
            }

            Ok(Type::Tuple(element_types))
        }

        Pattern::StructPatt(s) => {
            let type_path = TypePath::from(s.struct_path.clone());

            match analyser.lookup(&type_path).cloned() {
                Some(Symbol::Struct { struct_def, path }) => {
                    let mut field_map: HashMap<Identifier, Type> = HashMap::new();

                    let def_fields_opt = struct_def.fields_opt;
                    let patt_fields_opt = s.struct_fields_opt.clone();

                    if let Some(patt_fields) = patt_fields_opt {
                        for patt_field in patt_fields {
                            let field_name = patt_field.field_name.clone();
                            let field_value = patt_field.field_value.clone();
                            let field_type = analyse_patt(analyser, &field_value)?;

                            field_map.insert(field_name, field_type);
                        }
                    }

                    if let Some(def_fields) = def_fields_opt {
                        if field_map.len() > def_fields.len() {
                            return Err(SemanticErrorKind::StructArgCountMismatch {
                                struct_path: Identifier::from(type_path),
                                expected: def_fields.len(),
                                found: field_map.len(),
                            });
                        }

                        let field_names = def_fields
                            .iter()
                            .cloned()
                            .map(|sdf| sdf.field_name)
                            .collect::<Vec<_>>();

                        for (name, _) in field_map.iter() {
                            if !field_names.contains(name) {
                                return Err(SemanticErrorKind::UnexpectedStructField {
                                    field_name: struct_def.struct_name,
                                    found: name.clone(),
                                });
                            }
                        }

                        for def_field in def_fields.into_iter() {
                            match field_map.get_mut(&def_field.field_name) {
                                Some(patt_field_type) => {
                                    let mut symbol_table =
                                        if let Some(scope) = analyser.scope_stack.last() {
                                            scope.symbols.to_owned()
                                        } else {
                                            HashMap::new()
                                        };

                                    analyser.unify_types(
                                        &mut symbol_table,
                                        &*def_field.field_type,
                                        patt_field_type,
                                    )?;

                                    // if *patt_field_type != *def_field.field_type {
                                    //     return Err(SemanticErrorKind::TypeMismatchVariable {
                                    //         var_id: path.type_name,
                                    //         expected: format!("`{}`", *def_field.field_type),
                                    //         found: patt_field_type.clone(),
                                    //     });
                                    // }
                                }

                                None => {
                                    return Err(SemanticErrorKind::MissingStructField {
                                        expected: format!(
                                            "`{}: {}`",
                                            &def_field.field_name, *def_field.field_type
                                        ),
                                    })
                                }
                            }
                        }
                    }

                    Ok(Type::UserDefined(path))
                }

                None => Err(SemanticErrorKind::UndefinedStruct {
                    name: type_path.type_name,
                }),

                Some(sym) => Err(SemanticErrorKind::UnexpectedSymbol {
                    name: type_path.type_name,
                    expected: "struct".to_string(),
                    found: format!("`{sym}`"),
                }),
            }
        }

        Pattern::TupleStructPatt(ts) => {
            let type_path = TypePath::from(ts.struct_path.clone());

            match analyser.lookup(&type_path).cloned() {
                Some(Symbol::TupleStruct {
                    tuple_struct_def,
                    path,
                }) => {
                    let elements_opt = ts.struct_elements_opt.clone();
                    let fields_opt = tuple_struct_def.fields_opt;

                    match (elements_opt, fields_opt) {
                        (None, None) => Ok(Type::UnitType(UnitType)),

                        (None, Some(fields)) => Err(SemanticErrorKind::StructArgCountMismatch {
                            struct_path: Identifier::from(type_path),
                            expected: fields.len(),
                            found: 0,
                        }),

                        (Some(elements), None) => Err(SemanticErrorKind::StructArgCountMismatch {
                            struct_path: Identifier::from(type_path),
                            expected: 0,
                            found: elements.len(),
                        }),
                        (Some(elements), Some(fields)) => {
                            if &elements.len() != &fields.len() {
                                return Err(SemanticErrorKind::StructArgCountMismatch {
                                    struct_path: Identifier::from(type_path),
                                    expected: fields.len(),
                                    found: elements.len(),
                                });
                            }

                            for (elem, field) in elements.iter().zip(fields) {
                                let field_type = field.field_type;
                                // let field_type_clone = field_type.clone();
                                let mut elem_type = analyse_patt(analyser, &elem)?;
                                // let elem_type_clone = elem_type.clone();

                                let mut symbol_table =
                                    if let Some(scope) = analyser.scope_stack.last() {
                                        scope.symbols.to_owned()
                                    } else {
                                        HashMap::new()
                                    };

                                analyser.unify_types(
                                    &mut symbol_table,
                                    &*field_type,
                                    &mut elem_type,
                                )?;

                                // if elem_type_clone != field_type_clone {
                                //     return Err(SemanticErrorKind::TypeMismatchVariable {
                                //         var_id: tuple_struct_def.struct_name.clone(),
                                //         expected: format!("`{}`", field_type_clone),
                                //         found: elem_type_clone,
                                //     });
                                // }
                            }

                            Ok(Type::UserDefined(path))
                        }
                    }
                }

                None => Err(SemanticErrorKind::UndefinedStruct {
                    name: type_path.type_name,
                }),

                Some(sym) => Err(SemanticErrorKind::UnexpectedSymbol {
                    name: type_path.type_name,
                    expected: "struct".to_string(),
                    found: format!("`{sym}`"),
                }),
            }
        }

        Pattern::WildcardPatt(_) => Ok(Type::InferredType(InferredType {
            name: Identifier::from("_"),
        })),

        Pattern::RestPatt(_) => Ok(Type::InferredType(InferredType {
            name: Identifier::from(".."),
        })),

        Pattern::OrPatt(o) => {
            let first_patt_type = analyse_patt(analyser, &o.first_pattern.clone())?;

            for patt in o.subsequent_patterns.iter() {
                let subsequent_patt_type = analyse_patt(analyser, patt)?;

                if subsequent_patt_type != first_patt_type {
                    return Err(SemanticErrorKind::TypeMismatchOrPatt {
                        expected: first_patt_type,
                        found: subsequent_patt_type,
                    });
                }
            }

            Ok(first_patt_type)
        }

        Pattern::SomePatt(s) => {
            let ty = analyse_patt(analyser, &s.pattern.clone().inner_pattern)?;

            // TODO: if the inner type is a generic type, resolve it,
            // TODO: including checking if it implements its bound trait (where applicable)

            Ok(Type::Option {
                inner_type: Box::new(ty),
            })
        }

        Pattern::NonePatt(_) => Ok(Type::Option {
            inner_type: Box::new(Type::UnitType(UnitType)),
        }),

        Pattern::ResultPatt(r) => {
            let ty = analyse_patt(analyser, &r.pattern.clone().inner_pattern)?;

            // TODO: if any of the inner types is a generic type, resolve it,
            // TODO: including checking if it implements its bound trait (where applicable)

            match r.kw_ok_or_err {
                Keyword::Ok => Ok(Type::Result {
                    ok_type: Box::new(ty),
                    err_type: Box::new(Type::InferredType(InferredType {
                        name: Identifier::from("_"),
                    })),
                }),
                Keyword::Err => Ok(Type::Result {
                    ok_type: Box::new(Type::InferredType(InferredType {
                        name: Identifier::from("_"),
                    })),
                    err_type: Box::new(ty),
                }),
                keyword => Err(SemanticErrorKind::UnexpectedKeyword {
                    expected: "`Ok` or `Err`".to_string(),
                    found: keyword,
                }),
            }
        }
    }
}
