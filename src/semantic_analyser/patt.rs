use std::collections::HashMap;

use crate::{
    ast::{
        BigUInt, Bytes, Float, Hash, Identifier, Int, Keyword, LiteralPatt, Pattern, Type,
        TypePath, UInt,
    },
    error::SemanticErrorKind,
};

use super::{symbol_table::Symbol, FormatObject, SemanticAnalyser, ToIdentifier};

/// Analyse a pattern in the context of semantic analysis, returning the inferred type of
/// the pattern or an appropriate semantic error.
pub(crate) fn analyse_patt(
    analyser: &mut SemanticAnalyser,
    pattern: &Pattern,
) -> Result<Type, SemanticErrorKind> {
    match pattern {
        Pattern::GroupedPatt(g) => analyse_patt(analyser, &g.inner_pattern),

        Pattern::IdentifierPatt(i) => match analyser.lookup(&i.name.to_type_path()) {
            Some(Symbol::Variable { var_type, .. }) => Ok(var_type.clone()),
            Some(sym) => Err(SemanticErrorKind::UnexpectedSymbol {
                name: i.name.clone(),
                expected: "variable".to_string(),
                found: sym.to_backtick_string(),
            }),
            _ => Err(SemanticErrorKind::UndefinedVariable {
                name: i.name.clone(),
            }),
        },

        Pattern::LiteralPatt(l) => match l {
            LiteralPatt::Int { value } => match value {
                Int::I32(_) => Ok(Type::I32),
                Int::I64(_) => Ok(Type::I64),
            },
            LiteralPatt::UInt { value } => match value {
                UInt::U8(_) => Ok(Type::U8),
                UInt::U16(_) => Ok(Type::U16),
                UInt::U32(_) => Ok(Type::U32),
                UInt::U64(_) => Ok(Type::U64),
            },
            LiteralPatt::BigUInt { value } => match value {
                BigUInt::U256(_) => Ok(Type::U256),
                BigUInt::U512(_) => Ok(Type::U512),
            },
            LiteralPatt::Float { value } => match value {
                Float::F32(_) => Ok(Type::F32),
                Float::F64(_) => Ok(Type::F64),
            },
            LiteralPatt::Byte { .. } => Ok(Type::Byte),
            LiteralPatt::Bytes { value } => match value {
                Bytes::B2(_) => Ok(Type::B2),
                Bytes::B4(_) => Ok(Type::B4),
                Bytes::B8(_) => Ok(Type::B8),
                Bytes::B16(_) => Ok(Type::B16),
                Bytes::B32(_) => Ok(Type::B32),
            },
            LiteralPatt::Hash { value } => match value {
                Hash::H160(_) => Ok(Type::H160),
                Hash::H256(_) => Ok(Type::H256),
                Hash::H512(_) => Ok(Type::H512),
            },
            LiteralPatt::Str { .. } => Ok(Type::Str),
            LiteralPatt::Char { .. } => Ok(Type::Char),
            LiteralPatt::Bool { .. } => Ok(Type::Bool),
        },

        Pattern::NonePatt(_) => Ok(Type::Option {
            inner_type: Box::new(Type::UNIT_TYPE),
        }),

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

        Pattern::RangePatt(r) => match (&r.from_pattern_opt, &r.to_pattern_opt) {
            (None, None) => Ok(Type::UNIT_TYPE),
            (None, Some(to)) => {
                let to_type = analyse_patt(analyser, &to.clone())?;

                match to_type {
                    Type::I32
                    | Type::I64
                    | Type::U8
                    | Type::U16
                    | Type::U32
                    | Type::U64
                    | Type::U256
                    | Type::U512
                    | Type::Byte
                    | Type::Char => Ok(to_type),
                    _ => Err(SemanticErrorKind::UnexpectedType {
                        expected: "numeric type, `byte` or `char`".to_string(),
                        found: to_type,
                    }),
                }
            }
            (Some(from), None) => {
                let from_type = analyse_patt(analyser, &from.clone())?;

                match from_type {
                    Type::I32
                    | Type::I64
                    | Type::U8
                    | Type::U16
                    | Type::U32
                    | Type::U64
                    | Type::U256
                    | Type::U512
                    | Type::Byte
                    | Type::Char => Ok(from_type),
                    _ => Err(SemanticErrorKind::UnexpectedType {
                        expected: "numeric type, `byte` or `char`".to_string(),
                        found: from_type,
                    }),
                }
            }
            (Some(from), Some(to)) => {
                let from_type = analyse_patt(analyser, &from.clone())?;

                match from_type {
                    Type::I32
                    | Type::I64
                    | Type::U8
                    | Type::U16
                    | Type::U32
                    | Type::U64
                    | Type::U256
                    | Type::U512
                    | Type::Byte
                    | Type::Char => (),
                    _ => {
                        return Err(SemanticErrorKind::UnexpectedType {
                            expected: "numeric type, `byte` or `char`".to_string(),
                            found: from_type,
                        })
                    }
                }

                let to_type = analyse_patt(analyser, &to.clone())?;

                match to_type {
                    Type::I32
                    | Type::I64
                    | Type::U8
                    | Type::U16
                    | Type::U32
                    | Type::U64
                    | Type::U256
                    | Type::U512
                    | Type::Byte
                    | Type::Char => (),
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

        Pattern::ReferencePatt(r) => Ok(Type::Reference {
            reference_op: r.reference_op,
            inner_type: Box::new(analyse_patt(analyser, &r.pattern)?),
        }),

        Pattern::RestPatt(_) => Ok(Type::inferred_type("..")),

        Pattern::ResultPatt(r) => {
            let ty = analyse_patt(analyser, &r.pattern.clone().inner_pattern)?;

            match r.kw_ok_or_err {
                Keyword::Ok => Ok(Type::Result {
                    ok_type: Box::new(ty),
                    err_type: Box::new(Type::inferred_type("_")),
                }),
                Keyword::Err => Ok(Type::Result {
                    ok_type: Box::new(Type::inferred_type("_")),
                    err_type: Box::new(ty),
                }),
                keyword => Err(SemanticErrorKind::UnexpectedKeyword {
                    expected: "`Ok` or `Err`".to_string(),
                    found: keyword,
                }),
            }
        }

        Pattern::SomePatt(s) => {
            let ty = analyse_patt(analyser, &s.pattern.clone().inner_pattern)?;

            Ok(Type::Option {
                inner_type: Box::new(ty),
            })
        }

        Pattern::StructPatt(s) => {
            let type_path = TypePath::from(s.struct_path.clone());

            match analyser.lookup(&type_path).cloned() {
                Some(Symbol::Struct {
                    struct_def, path, ..
                }) => {
                    let mut field_map: HashMap<Identifier, Type> = HashMap::new();

                    let def_fields_opt = struct_def.fields_opt;
                    let patt_fields_opt = s.struct_fields_opt.clone();

                    if let Some(patt_fields) = patt_fields_opt {
                        for patt_field in patt_fields.iter() {
                            let field_name = patt_field.field_name.clone();
                            let field_value = patt_field.field_value.clone();
                            let field_type = analyse_patt(analyser, &field_value)?;

                            field_map.insert(field_name, field_type);
                        }
                    }

                    if let Some(def_fields) = def_fields_opt {
                        if field_map.len() > def_fields.len() {
                            return Err(SemanticErrorKind::StructArgCountMismatch {
                                struct_path: type_path.to_identifier(),
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
                                    struct_name: struct_def.struct_name,
                                    found: name.clone(),
                                });
                            }
                        }

                        for def_field in def_fields {
                            match field_map.get_mut(&def_field.field_name) {
                                Some(patt_field_type) => {
                                    analyser.check_types(
                                        &mut analyser.current_symbol_table(),
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
                    found: sym.to_backtick_string(),
                }),
            }
        }

        Pattern::TupleStructPatt(ts) => {
            let type_path = TypePath::from(ts.struct_path.clone());

            match analyser.lookup(&type_path).cloned() {
                Some(Symbol::TupleStruct {
                    tuple_struct_def,
                    path,
                    ..
                }) => {
                    let elements_opt = ts.struct_elements_opt.clone();
                    let fields_opt = tuple_struct_def.fields_opt;

                    match (elements_opt, fields_opt) {
                        (None, None) => Ok(Type::UNIT_TYPE),

                        (None, Some(fields)) => Err(SemanticErrorKind::StructArgCountMismatch {
                            struct_path: type_path.to_identifier(),
                            expected: fields.len(),
                            found: 0,
                        }),

                        (Some(elements), None) => Err(SemanticErrorKind::StructArgCountMismatch {
                            struct_path: type_path.to_identifier(),
                            expected: 0,
                            found: elements.len(),
                        }),
                        (Some(elements), Some(fields)) => {
                            if &elements.len() != &fields.len() {
                                return Err(SemanticErrorKind::StructArgCountMismatch {
                                    struct_path: type_path.to_identifier(),
                                    expected: fields.len(),
                                    found: elements.len(),
                                });
                            }

                            for (elem, field) in elements.iter().zip(fields) {
                                let field_type = field.field_type;
                                // let field_type_clone = field_type.clone();
                                let mut elem_type = analyse_patt(analyser, &elem)?;
                                // let elem_type_clone = elem_type.clone();

                                analyser.check_types(
                                    &mut analyser.current_symbol_table(),
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
                    found: sym.to_backtick_string(),
                }),
            }
        }

        Pattern::TuplePatt(t) => {
            let mut element_types: Vec<Type> = Vec::new();

            for patt in t.tuple_patt_elements.elements.iter() {
                let ty = analyse_patt(analyser, patt)?;

                element_types.push(ty)
            }

            Ok(Type::Tuple(element_types))
        }

        Pattern::WildcardPatt(_) => Ok(Type::inferred_type("_")),
    }
}
