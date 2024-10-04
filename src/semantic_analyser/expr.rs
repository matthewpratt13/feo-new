use super::{patt::analyse_patt, symbol_table::ScopeKind, SemanticAnalyser};

use crate::{
    ast::{
        BigUInt, Bool, Byte, Bytes, Char, ClosureParams, Expression, Float, FunctionOrMethodParam,
        FunctionParam, FunctionPtr, Hash, Identifier, Int, Keyword, Literal, PathExpr, PathRoot,
        Pattern, Statement, Str, Type, TypePath, UInt, UnaryOp, UnitType,
    },
    error::SemanticErrorKind,
    log_warn,
    semantic_analyser::symbol_table::Symbol,
    span::Spanned,
    B16, B2, B32, B4, B8, F32, F64, H160, H256, H512, U256, U512,
};

use core::fmt;
use std::collections::HashMap;

// TODO: alphabetize match arms

/// Analyse the given expression to determine its type within a specific context.
pub(crate) fn analyse_expr(
    analyser: &mut SemanticAnalyser,
    expression: &Expression,
    root: &TypePath,
) -> Result<Type, SemanticErrorKind> {
    match expression {
        Expression::Path(p) => {
            let path = match p.tree_opt.clone() {
                Some(mut segments) => {
                    if let Some(id) = segments.pop() {
                        let root = match &p.path_root {
                            PathRoot::Identifier(id) => id.to_type_path(),
                            PathRoot::SelfKeyword => TypePath::from(Identifier::from("self")),
                            PathRoot::SelfType(_) => TypePath::from(Identifier::from("Self")),

                            path_root => {
                                return Err(SemanticErrorKind::InvalidVariableIdentifier {
                                    name: Identifier::from(&path_root.to_string()),
                                })
                            }
                        };

                        root.join(TypePath {
                            associated_type_path_prefix_opt: Some(segments.to_vec()),
                            type_name: id,
                        })
                    } else {
                        match &p.path_root {
                            PathRoot::Identifier(i) => root.join(i.to_type_path()),
                            PathRoot::SelfType(_) => root.clone(),
                            PathRoot::SelfKeyword => root.clone(),

                            path_root => {
                                return Err(SemanticErrorKind::InvalidVariableIdentifier {
                                    name: Identifier::from(&path_root.to_string()),
                                })
                            }
                        }
                    }
                }

                _ => match &p.path_root {
                    PathRoot::Identifier(i) => i.to_type_path(),
                    PathRoot::SelfType(_) => root.clone(),
                    PathRoot::SelfKeyword => root.clone(),

                    path_root => {
                        return Err(SemanticErrorKind::InvalidVariableIdentifier {
                            name: Identifier::from(&path_root.to_string()),
                        })
                    }
                },
            };

            if let Some(sym) = analyser.lookup(&path) {
                Ok(sym.symbol_type())
            } else {
                Err(SemanticErrorKind::UndefinedVariable {
                    name: path.type_name,
                })
            }
        }

        Expression::Literal(l) => match l {
            Literal::Int { value, .. } => match value {
                Int::I32(_) => Ok(Type::I32(Int::I32(i32::default()))),
                Int::I64(_) => Ok(Type::I64(Int::I64(i64::default()))),
            },
            Literal::UInt { value, .. } => match value {
                UInt::U8(_) => Ok(Type::U8(UInt::U8(u8::default()))),
                UInt::U16(_) => Ok(Type::U16(UInt::U16(u16::default()))),
                UInt::U32(_) => Ok(Type::U32(UInt::U32(u32::default()))),
                UInt::U64(_) => Ok(Type::U64(UInt::U64(u64::default()))),
            },
            Literal::BigUInt { value, .. } => match value {
                BigUInt::U256(_) => Ok(Type::U256(BigUInt::U256(U256::default()))),
                BigUInt::U512(_) => Ok(Type::U512(BigUInt::U512(U512::default()))),
            },
            Literal::Float { value, .. } => match value {
                Float::F32(_) => Ok(Type::F32(Float::F32(F32::default()))),
                Float::F64(_) => Ok(Type::F64(Float::F64(F64::default()))),
            },
            Literal::Byte { .. } => Ok(Type::Byte(Byte::from(u8::default()))),
            Literal::Bytes { value, .. } => match value {
                Bytes::B2(_) => Ok(Type::B2(Bytes::B2(B2::default()))),
                Bytes::B4(_) => Ok(Type::B4(Bytes::B4(B4::default()))),
                Bytes::B8(_) => Ok(Type::B8(Bytes::B8(B8::default()))),
                Bytes::B16(_) => Ok(Type::B16(Bytes::B16(B16::default()))),
                Bytes::B32(_) => Ok(Type::B32(Bytes::B32(B32::default()))),
            },
            Literal::Hash { value, .. } => match value {
                Hash::H160(_) => Ok(Type::H160(Hash::H160(H160::default()))),
                Hash::H256(_) => Ok(Type::H256(Hash::H256(H256::default()))),
                Hash::H512(_) => Ok(Type::H512(Hash::H512(H512::default()))),
            },
            Literal::Str { .. } => Ok(Type::Str(Str::from(String::default().as_str()))),
            Literal::Char { .. } => Ok(Type::Char(Char::from(char::default()))),
            Literal::Bool { .. } => Ok(Type::Bool(Bool::from(bool::default()))),
        },

        Expression::MethodCall(mc) => {
            let receiver = wrap_into_expression(*mc.receiver.clone());
            let receiver_type = analyse_expr(analyser, &receiver, root)?;

            // convert receiver expression to path expression (i.e., check if receiver
            // is a valid path)
            let receiver_as_path_expr = PathExpr::from(receiver);
            let receiver_path = TypePath::from(receiver_as_path_expr);

            let symbol = analyser.lookup(&receiver_path).cloned();

            // check if path expression's type is that of an existing type and analyse
            match symbol {
                Some(
                    Symbol::Struct { path, .. }
                    | Symbol::TupleStruct { path, .. }
                    | Symbol::Enum { path, .. },
                ) => {
                    if Type::UserDefined(path.clone()) == receiver_type {
                        let method_path = path.join(mc.method_name.to_type_path());

                        analyse_call_or_method_call_expr(analyser, method_path, mc.args_opt.clone())
                    } else {
                        Err(SemanticErrorKind::TypeMismatchVariable {
                            var_id: receiver_path.type_name,
                            expected: format!("`{path}`"),
                            found: receiver_type,
                        })
                    }
                }

                None => Err(SemanticErrorKind::UndefinedType {
                    name: receiver_path.type_name,
                }),

                Some(sym) => Err(SemanticErrorKind::UnexpectedSymbol {
                    name: receiver_path.type_name,
                    expected: "struct".to_string(),
                    found: format!("`{sym}`"),
                }),
            }
        }

        Expression::FieldAccess(fa) => {
            let object = wrap_into_expression(*fa.object.clone());

            // convert object to path expression (i.e., check if object
            // is a valid path)
            let object_as_path_expr = PathExpr::from(object.clone());

            let object_path = TypePath::from(object_as_path_expr);
            let object_type = analyse_expr(analyser, &object, &object_path)?;

            match analyser.lookup(&object_path).cloned() {
                Some(Symbol::Struct { struct_def, .. }) => match &struct_def.fields_opt {
                    Some(fields) => match fields.iter().find(|f| f.field_name == fa.field_name) {
                        Some(sdf) => Ok(*sdf.field_type.clone()),
                        _ => Err(SemanticErrorKind::UndefinedField {
                            struct_path: Identifier::from(object_path),
                            field_name: fa.field_name.clone(),
                        }),
                    },
                    _ => Ok(Type::UnitType(UnitType)),
                },

                None => Err(SemanticErrorKind::UndefinedType {
                    name: Identifier::from(&object_type.to_string()),
                }),

                Some(sym) => Err(SemanticErrorKind::UnexpectedSymbol {
                    name: Identifier::from(&object_type.to_string()),
                    expected: "struct".to_string(),
                    found: format!("`{sym}`"),
                }),
            }
        }

        Expression::Call(c) => {
            let callee = wrap_into_expression(c.callee.clone());

            analyse_call_or_method_call_expr(
                analyser,
                TypePath::from(PathExpr::from(callee)),
                c.args_opt.clone(),
            )
        }

        Expression::Index(i) => {
            let array_type = analyse_expr(analyser, &wrap_into_expression(*i.array.clone()), root)?;

            let index_type = analyse_expr(analyser, &wrap_into_expression(*i.index.clone()), root)?;

            match &index_type {
                Type::U8(_) | Type::U16(_) | Type::U32(_) | Type::U64(_) => (),

                _ => {
                    return Err(SemanticErrorKind::UnexpectedType {
                        expected: "array index".to_string(),
                        found: index_type,
                    })
                }
            }

            match array_type {
                Type::Array { element_type, .. } => Ok(*element_type),
                _ => Err(SemanticErrorKind::UnexpectedType {
                    expected: "array".to_string(),
                    found: array_type,
                }),
            }
        }

        Expression::TupleIndex(ti) => {
            let tuple_type =
                analyse_expr(analyser, &wrap_into_expression(*ti.tuple.clone()), root)?;

            match tuple_type {
                Type::Tuple(elem_types) => {
                    if ti.index < UInt::from(elem_types.len()) {
                        Ok(Type::Tuple(elem_types))
                    } else {
                        Err(SemanticErrorKind::TupleIndexOutOfBounds {
                            len: ti.index.into(),
                            i: elem_types.len(),
                        })
                    }
                }
                Type::UserDefined(tp) => match analyser.lookup(&tp) {
                    Some(sym) => match sym {
                        Symbol::TupleStruct { .. } => Ok(Type::UserDefined(tp)),

                        _ => Err(SemanticErrorKind::UnexpectedSymbol {
                            name: tp.type_name,
                            expected: "tuple struct".to_string(),
                            found: format!("`{sym}`"),
                        }),
                    },
                    None => Err(SemanticErrorKind::UndefinedType { name: tp.type_name }),
                },
                _ => Err(SemanticErrorKind::UnexpectedType {
                    expected: "tuple or tuple struct".to_string(),
                    found: tuple_type,
                }),
            }
        }

        Expression::Unwrap(u) => {
            analyse_expr(analyser, &wrap_into_expression(*u.value_expr.clone()), root)
        }

        Expression::Unary(u) => {
            let expr_type =
                analyse_expr(analyser, &wrap_into_expression(*u.value_expr.clone()), root)?;

            match u.unary_op {
                UnaryOp::Negate => match &expr_type {
                    Type::I32(_)
                    | Type::I64(_)
                    | Type::U8(_)
                    | Type::U16(_)
                    | Type::U32(_)
                    | Type::U64(_)
                    | Type::U256(_)
                    | Type::U512(_)
                    | Type::F32(_)
                    | Type::F64(_) => Ok(expr_type),
                    _ => Err(SemanticErrorKind::UnexpectedType {
                        expected: "numeric value".to_string(),
                        found: expr_type,
                    }),
                },
                UnaryOp::Not => match &expr_type {
                    Type::Bool(_) => Ok(expr_type),
                    _ => Err(SemanticErrorKind::UnexpectedType {
                        expected: "boolean".to_string(),
                        found: expr_type,
                    }),
                },
            }
        }

        Expression::Reference(r) => {
            let reference_op = r.reference_op.clone();
            let expr_type = analyse_expr(analyser, &r.expression, root)?;

            Ok(Type::Reference {
                reference_op,
                inner_type: Box::new(expr_type),
            })
        }

        Expression::Dereference(d) => {
            match analyse_expr(
                analyser,
                &wrap_into_expression(d.assignee_expr.clone()),
                root,
            ) {
                Ok(Type::Reference { inner_type, .. }) => Ok(*inner_type),
                Ok(ty) => Ok(ty),
                Err(e) => Err(e),
            }
        }

        Expression::TypeCast(tc) => {
            let value_type =
                analyse_expr(analyser, &wrap_into_expression(*tc.value.clone()), root)?;

            let new_type = *tc.new_type.clone();

            match (&value_type, &new_type) {
                (
                    Type::I32(_) | Type::I64(_),
                    Type::I32(_)
                    | Type::I64(_)
                    | Type::U8(_)
                    | Type::U16(_)
                    | Type::U32(_)
                    | Type::U64(_)
                    | Type::U256(_)
                    | Type::U512(_)
                    | Type::F32(_)
                    | Type::F64(_),
                )
                | (
                    Type::U8(_) | Type::Byte(_),
                    Type::I32(_)
                    | Type::I64(_)
                    | Type::U8(_)
                    | Type::U16(_)
                    | Type::U32(_)
                    | Type::U64(_)
                    | Type::U256(_)
                    | Type::U512(_)
                    | Type::F32(_)
                    | Type::F64(_)
                    | Type::Byte(_)
                    | Type::Char(_),
                )
                | (
                    Type::U16(_) | Type::U32(_),
                    Type::I32(_)
                    | Type::I64(_)
                    | Type::U8(_)
                    | Type::U16(_)
                    | Type::U32(_)
                    | Type::U64(_)
                    | Type::U256(_)
                    | Type::U512(_)
                    | Type::F32(_)
                    | Type::F64(_)
                    | Type::Char(_),
                )
                | (
                    Type::U64(_),
                    Type::I32(_)
                    | Type::I64(_)
                    | Type::U8(_)
                    | Type::U16(_)
                    | Type::U32(_)
                    | Type::U64(_)
                    | Type::U256(_)
                    | Type::U512(_)
                    | Type::F32(_)
                    | Type::F64(_),
                )
                | (Type::U256(_) | Type::U512(_), Type::U256(_) | Type::U512(_))
                | (Type::F32(_) | Type::F64(_), Type::F32(_) | Type::F64(_))
                | (
                    Type::B2(_) | Type::B4(_) | Type::B16(_) | Type::B32(_),
                    Type::B2(_) | Type::B4(_) | Type::B16(_) | Type::B32(_),
                )
                | (
                    Type::H160(_) | Type::H256(_) | Type::H512(_),
                    Type::H160(_) | Type::H256(_) | Type::H512(_),
                )
                | (Type::Char(_), Type::U8(_) | Type::U16(_) | Type::U32(_) | Type::U64(_)) => {
                    Ok(new_type)
                }
                (t, u) => Err(SemanticErrorKind::TypeCastError {
                    from: t.clone(),
                    to: u.clone(),
                }),
            }
        }

        Expression::Binary(b) => {
            let lhs_type = analyse_expr(analyser, &wrap_into_expression(*b.lhs.clone()), root)?;

            let mut rhs_type = analyse_expr(analyser, &wrap_into_expression(*b.rhs.clone()), root)?;

            let mut symbol_table = if let Some(scope) = analyser.scope_stack.last() {
                scope.symbols.to_owned()
            } else {
                HashMap::new()
            };

            analyser.check_types(&mut symbol_table, &lhs_type, &mut rhs_type)?;

            Ok(rhs_type)
        }

        Expression::Comparison(c) => {
            let lhs_type = analyse_expr(analyser, &wrap_into_expression(c.lhs.clone()), root)?;

            let mut rhs_type = analyse_expr(analyser, &wrap_into_expression(c.rhs.clone()), root)?;

            let mut symbol_table = if let Some(scope) = analyser.scope_stack.last() {
                scope.symbols.to_owned()
            } else {
                HashMap::new()
            };

            analyser.check_types(&mut symbol_table, &lhs_type, &mut rhs_type)?;

            Ok(rhs_type)
        }

        Expression::Grouped(g) => analyse_expr(analyser, &g.inner_expression, root),

        Expression::Range(r) => match (&r.from_expr_opt, &r.to_expr_opt) {
            (None, None) => Ok(Type::UnitType(UnitType)),
            (None, Some(to)) => {
                let to_type = analyse_expr(analyser, &wrap_into_expression(*to.clone()), root)?;

                match to_type {
                    Type::I32(_)
                    | Type::I64(_)
                    | Type::U8(_)
                    | Type::U16(_)
                    | Type::U32(_)
                    | Type::U64(_)
                    | Type::U256(_)
                    | Type::U512(_) => Ok(to_type),
                    _ => Err(SemanticErrorKind::UnexpectedType {
                        expected: "numeric type".to_string(),
                        found: to_type,
                    }),
                }
            }
            (Some(from), None) => {
                let from_type = analyse_expr(analyser, &wrap_into_expression(*from.clone()), root)?;

                match &from_type {
                    Type::I32(_)
                    | Type::I64(_)
                    | Type::U8(_)
                    | Type::U16(_)
                    | Type::U32(_)
                    | Type::U64(_)
                    | Type::U256(_)
                    | Type::U512(_) => Ok(from_type),
                    _ => Err(SemanticErrorKind::UnexpectedType {
                        expected: "numeric type".to_string(),
                        found: from_type,
                    }),
                }
            }
            (Some(from), Some(to)) => {
                let from_type = analyse_expr(analyser, &wrap_into_expression(*from.clone()), root)?;

                match &from_type {
                    Type::I32(_)
                    | Type::I64(_)
                    | Type::U8(_)
                    | Type::U16(_)
                    | Type::U32(_)
                    | Type::U64(_)
                    | Type::U256(_)
                    | Type::U512(_) => (),
                    _ => {
                        return Err(SemanticErrorKind::UnexpectedType {
                            expected: "numeric type".to_string(),
                            found: from_type,
                        })
                    }
                }

                let to_type = analyse_expr(analyser, &wrap_into_expression(*to.clone()), root)?;

                match &to_type {
                    Type::I32(_)
                    | Type::I64(_)
                    | Type::U8(_)
                    | Type::U16(_)
                    | Type::U32(_)
                    | Type::U64(_)
                    | Type::U256(_)
                    | Type::U512(_) => (),
                    _ => {
                        return Err(SemanticErrorKind::UnexpectedType {
                            expected: "numeric type".to_string(),
                            found: to_type,
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

        Expression::Assignment(a) => {
            let assignee = wrap_into_expression(a.lhs.clone());
            let assignee_type = analyse_expr(analyser, &assignee, root)?;

            let mut value_type =
                analyse_expr(analyser, &wrap_into_expression(a.rhs.clone()), root)?;

            let mut symbol_table = if let Some(scope) = analyser.scope_stack.last() {
                scope.symbols.to_owned()
            } else {
                HashMap::new()
            };

            analyser.check_types(&mut symbol_table, &assignee_type, &mut value_type)?;

            let assignee_as_path_expr = PathExpr::from(assignee);

            let assignee_path = TypePath::from(assignee_as_path_expr);

            match analyser.lookup(&assignee_path).cloned() {
                Some(Symbol::Variable { var_type, .. }) => {
                    if var_type == value_type {
                        Ok(value_type)
                    } else {
                        Err(SemanticErrorKind::TypeMismatchVariable {
                            var_id: assignee_path.type_name,
                            expected: format!("`{assignee_type}`"),
                            found: var_type,
                        })
                    }
                }
                Some(Symbol::Constant { constant_name, .. }) => {
                    Err(SemanticErrorKind::ConstantReassignment {
                        name: constant_name,
                    })
                }
                Some(sym) => Err(SemanticErrorKind::UnexpectedSymbol {
                    name: assignee_path.type_name,
                    expected: format!("`{assignee_type}`"),
                    found: format!("`{sym}`"),
                }),
                _ => Err(SemanticErrorKind::UndefinedVariable {
                    name: assignee_path.type_name,
                }),
            }
        }

        Expression::CompoundAssignment(ca) => {
            let assignee = wrap_into_expression(ca.lhs.clone());
            let assignee_type = analyse_expr(analyser, &assignee, root)?;

            let mut value_type =
                analyse_expr(analyser, &wrap_into_expression(ca.rhs.clone()), root)?;

            let mut symbol_table = if let Some(scope) = analyser.scope_stack.last() {
                scope.symbols.to_owned()
            } else {
                HashMap::new()
            };

            analyser.check_types(&mut symbol_table, &assignee_type, &mut value_type)?;

            let assignee_as_path_expr = PathExpr::from(assignee);

            let assignee_path = TypePath::from(assignee_as_path_expr);

            match analyser.lookup(&assignee_path).cloned() {
                Some(Symbol::Variable { var_type, .. }) => {
                    if var_type == value_type {
                        Ok(value_type)
                    } else {
                        Err(SemanticErrorKind::TypeMismatchVariable {
                            var_id: assignee_path.type_name,
                            expected: format!("`{assignee_type}`"),
                            found: var_type,
                        })
                    }
                }

                Some(Symbol::Constant { constant_name, .. }) => {
                    Err(SemanticErrorKind::ConstantReassignment {
                        name: constant_name.clone(),
                    })
                }

                Some(sym) => Err(SemanticErrorKind::UnexpectedSymbol {
                    name: assignee_path.type_name,
                    expected: format!("`{assignee_type}`"),
                    found: format!("`{sym}`"),
                }),

                _ => Err(SemanticErrorKind::UndefinedVariable {
                    name: assignee_path.type_name,
                }),
            }
        }

        Expression::Return(r) => match &r.expression_opt {
            Some(expr) => analyse_expr(analyser, &expr.clone(), root),
            _ => Ok(Type::UnitType(UnitType)),
        },

        Expression::Break(_) => Ok(Type::UnitType(UnitType)),

        Expression::Continue(_) => Ok(Type::UnitType(UnitType)),

        Expression::Underscore(_) => Ok(Type::inferred_type("_")),

        Expression::Closure(c) => {
            let params_opt = match &c.closure_params {
                ClosureParams::Some(params) => {
                    let mut function_params: Vec<FunctionOrMethodParam> = Vec::new();

                    for param in params {
                        let param_type = param
                            .type_ann_opt
                            .clone()
                            .unwrap_or(Box::new(Type::inferred_type("_")));

                        let function_param = FunctionParam {
                            param_name: param.param_name.clone(),
                            param_type,
                        };

                        function_params.push(FunctionOrMethodParam::FunctionParam(function_param))
                    }

                    Some(function_params)
                }
                _ => None,
            };

            let return_type = match &c.return_type_opt {
                Some(ty) => Ok(*ty.clone()),
                _ => Ok(Type::UnitType(UnitType)),
            }?;

            let return_type_clone = return_type.clone();

            let mut expression_type = analyse_expr(analyser, &c.body_expression, root)?;

            if expression_type != return_type {
                if let Type::Result { ok_type, err_type } = expression_type.clone() {
                    if *ok_type == Type::inferred_type("_") || *err_type == Type::inferred_type("_")
                    {
                        ()
                    } else {
                        return Err(SemanticErrorKind::TypeMismatchReturnType {
                            expected: return_type_clone,
                            found: expression_type,
                        });
                    }
                }
            }

            let mut symbol_table = if let Some(scope) = analyser.scope_stack.last() {
                scope.symbols.to_owned()
            } else {
                HashMap::new()
            };

            analyser.check_types(&mut symbol_table, &return_type, &mut expression_type)?;

            let function_ptr = FunctionPtr {
                params_opt,
                return_type_opt: Some(Box::new(return_type_clone)),
            };

            Ok(Type::FunctionPtr(function_ptr))
        }

        Expression::Array(a) => match &a.elements_opt {
            Some(elements) => match elements.first() {
                Some(expr) => {
                    let mut element_count = 0u64;

                    let mut first_element_type = analyse_expr(analyser, expr, root)?;

                    element_count += 1;

                    for elem in elements.iter().skip(1) {
                        let element_type = analyse_expr(analyser, elem, root)?;

                        element_count += 1;

                        let mut symbol_table = if let Some(scope) = analyser.scope_stack.last() {
                            scope.symbols.to_owned()
                        } else {
                            HashMap::new()
                        };

                        analyser.check_types(
                            &mut symbol_table,
                            &element_type,
                            &mut first_element_type,
                        )?;

                        if element_type != first_element_type {
                            return Err(SemanticErrorKind::TypeMismatchArrayElems {
                                expected: format!("`{first_element_type}`"),
                                found: element_type,
                            });
                        }
                    }

                    Ok(Type::Array {
                        element_type: Box::new(first_element_type),
                        num_elements: UInt::U64(element_count),
                    })
                }
                _ => {
                    let element_type = Type::UnitType(UnitType);
                    let array = Type::Array {
                        element_type: Box::new(element_type),
                        num_elements: UInt::U64(0u64),
                    };

                    Ok(array)
                }
            },
            _ => Ok(Type::UnitType(UnitType)),
        },

        Expression::Tuple(t) => {
            let mut element_types: Vec<Type> = Vec::new();

            for elem in t.tuple_elements.elements.iter() {
                let ty = analyse_expr(analyser, elem, root)?;

                element_types.push(ty)
            }

            Ok(Type::Tuple(element_types))
        }

        Expression::Struct(s) => {
            let type_path = root.join(TypePath::from(s.struct_path.clone()));

            match analyser.lookup(&type_path).cloned() {
                Some(Symbol::Struct { struct_def, path }) => {
                    let hash_map = HashMap::new();
                    let mut field_map: HashMap<Identifier, Type> = hash_map;

                    let def_fields_opt = struct_def.fields_opt;
                    let obj_fields_opt = s.struct_fields_opt.clone();

                    if let Some(obj_fields) = obj_fields_opt {
                        for obj_field in obj_fields.iter() {
                            let field_name = obj_field.field_name.clone();
                            let field_value = *obj_field.field_value.clone();
                            let field_type = analyse_expr(analyser, &field_value, root)?;

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

                        for def_field in def_fields.iter() {
                            match field_map.get_mut(&def_field.field_name) {
                                Some(obj_field_type) => {
                                    let mut symbol_table =
                                        if let Some(scope) = analyser.scope_stack.last() {
                                            scope.symbols.to_owned()
                                        } else {
                                            HashMap::new()
                                        };

                                    analyser.check_types(
                                        &mut symbol_table,
                                        &*def_field.field_type,
                                        obj_field_type,
                                    )?;

                                    // if *obj_field_type != *def_field.field_type {
                                    //     return Err(SemanticErrorKind::TypeMismatchVariable {
                                    //         var_id: path.type_name,
                                    //         expected: format!("`{}`", *def_field.field_type),
                                    //         found: obj_field_type.clone(),
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

        Expression::TupleStruct(ts) => {
            let type_path = root.join(TypePath::from(ts.struct_path.clone()));

            match analyser.lookup(&type_path).cloned() {
                Some(Symbol::TupleStruct {
                    tuple_struct_def,
                    path,
                }) => {
                    let elements_opt = ts.struct_elements_opt.clone();
                    let fields_opt = tuple_struct_def.fields_opt;

                    match (&elements_opt, &fields_opt) {
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
                            if elements.len() != fields.len() {
                                return Err(SemanticErrorKind::StructArgCountMismatch {
                                    struct_path: Identifier::from(type_path),
                                    expected: fields.len(),
                                    found: elements.len(),
                                });
                            }

                            for (elem, field) in elements.iter().zip(fields) {
                                let field_type = *field.field_type.clone();

                                let mut elem_type = analyse_expr(
                                    analyser,
                                    &elem,
                                    &TypePath::from(Identifier::from("")),
                                )?;

                                let mut symbol_table =
                                    if let Some(scope) = analyser.scope_stack.last() {
                                        scope.symbols.to_owned()
                                    } else {
                                        HashMap::new()
                                    };

                                analyser.check_types(
                                    &mut symbol_table,
                                    &field_type,
                                    &mut elem_type,
                                )?;

                                if elem_type != field_type {
                                    return Err(SemanticErrorKind::TypeMismatchVariable {
                                        var_id: tuple_struct_def.struct_name,
                                        expected: format!("`{field_type}`"),
                                        found: elem_type,
                                    });
                                }
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
                    expected: "tuple struct".to_string(),
                    found: format!("`{sym}`"),
                }),
            }
        }

        Expression::Mapping(m) => match &m.pairs_opt {
            Some(pairs) => match pairs.first() {
                Some(pair) => {
                    let key_type = analyse_patt(analyser, &pair.k.clone())?;
                    let value_type = analyse_expr(analyser, &pair.v.clone(), root)?;

                    for pair in pairs.iter().skip(1) {
                        let mut pair_key_type = analyse_patt(analyser, &pair.k.clone())?;
                        let mut pair_value_type = analyse_expr(analyser, &pair.v.clone(), root)?;

                        let mut symbol_table = if let Some(scope) = analyser.scope_stack.last() {
                            scope.symbols.to_owned()
                        } else {
                            HashMap::new()
                        };

                        analyser.check_types(&mut symbol_table, &key_type, &mut pair_key_type)?;

                        analyser.check_types(
                            &mut symbol_table,
                            &value_type,
                            &mut pair_value_type,
                        )?;

                        if (&pair_key_type, &pair_value_type) != (&key_type, &value_type) {
                            return Err(SemanticErrorKind::UnexpectedType {
                                expected: (Type::Mapping {
                                    key_type: Box::new(key_type),
                                    value_type: Box::new(value_type),
                                })
                                .to_string(),
                                found: Type::Mapping {
                                    key_type: Box::new(pair_key_type),
                                    value_type: Box::new(pair_value_type),
                                },
                            });
                        }
                    }

                    Ok(Type::Mapping {
                        key_type: Box::new(key_type),
                        value_type: Box::new(value_type),
                    })
                }
                _ => {
                    let key_type = Box::new(Type::UnitType(UnitType));
                    let value_type = Box::new(Type::UnitType(UnitType));

                    Ok(Type::Mapping {
                        key_type,
                        value_type,
                    })
                }
            },
            _ => Ok(Type::UnitType(UnitType)),
        },

        Expression::Block(b) => match &b.statements_opt {
            Some(stmts) => {
                analyser.enter_scope(ScopeKind::LocalBlock);

                let mut cloned_iter = stmts.iter().peekable().clone();

                for stmt in stmts {
                    cloned_iter.next();

                    match stmt {
                        Statement::Expression(expr) => match expr {
                            Expression::Return(_)
                            | Expression::Break(_)
                            | Expression::Continue(_) => {
                                analyse_expr(analyser, expr, root)?;

                                match cloned_iter.peek() {
                                    Some(_) => log_warn!(analyser.logger, "unreachable code"),
                                    _ => (),
                                }
                            }
                            expression => match analyse_expr(analyser, expression, root) {
                                Ok(_) => (),
                                Err(err) => analyser.log_error(err, &expression.span()),
                            },
                        },
                        statement => analyser.analyse_stmt(statement, root.clone())?,
                    }
                }

                let ty = match stmts.last() {
                    Some(stmt) => match stmt {
                        Statement::Expression(expr) => analyse_expr(analyser, expr, root),
                        _ => Ok(Type::UnitType(UnitType)),
                    },
                    _ => Ok(Type::UnitType(UnitType)),
                };

                analyser.exit_scope();

                ty
            }

            _ => Ok(Type::UnitType(UnitType)),
        },

        Expression::If(i) => {
            analyse_expr(analyser, &Expression::Grouped(*i.condition.clone()), root)?;

            let if_block_type =
                analyse_expr(analyser, &Expression::Block(*i.if_block.clone()), root)?;

            let else_if_blocks_type = match &i.else_if_blocks_opt {
                Some(blocks) => match blocks.first() {
                    Some(_) => {
                        for block in blocks.iter() {
                            let block_type =
                                analyse_expr(analyser, &Expression::If(*block.clone()), root)?;

                            if block_type != if_block_type {
                                return Err(SemanticErrorKind::TypeMismatchValues {
                                    expected: if_block_type,
                                    found: block_type,
                                });
                            }
                        }

                        if_block_type.clone()
                    }
                    _ => Type::UnitType(UnitType),
                },
                _ => Type::UnitType(UnitType),
            };

            let trailing_else_block_type = match &i.trailing_else_block_opt {
                Some(block) => analyse_expr(analyser, &Expression::Block(block.clone()), root)?,
                _ => Type::UnitType(UnitType),
            };

            if i.else_if_blocks_opt.is_some() && else_if_blocks_type != if_block_type {
                return Err(SemanticErrorKind::TypeMismatchValues {
                    expected: if_block_type,
                    found: else_if_blocks_type,
                });
            }

            if trailing_else_block_type != if_block_type {
                return Err(SemanticErrorKind::TypeMismatchValues {
                    expected: if_block_type,
                    found: trailing_else_block_type,
                });
            }

            Ok(if_block_type)
        }

        Expression::Match(m) => {
            analyser.enter_scope(ScopeKind::MatchExpr);

            let scrutinee_type =
                analyse_expr(analyser, &wrap_into_expression(m.scrutinee.clone()), root)?;

            let mut patt_type = analyse_patt(analyser, &m.final_arm.matched_pattern.clone())?;

            if patt_type != scrutinee_type {
                if let Type::InferredType(_) = patt_type {
                    ()
                } else {
                    return Err(SemanticErrorKind::TypeMismatchMatchExpr {
                        loc: "scrutinee and matched pattern".to_string(),
                        expected: scrutinee_type,
                        found: patt_type,
                    });
                }
            }

            let expr_type = analyse_expr(analyser, &m.final_arm.arm_expression.clone(), root)?;

            if let Some(arms) = &m.match_arms_opt {
                for arm in arms.iter() {
                    let mut arm_patt_type = analyse_patt(analyser, &arm.matched_pattern)?;

                    let mut symbol_table = if let Some(scope) = analyser.scope_stack.last() {
                        scope.symbols.to_owned()
                    } else {
                        HashMap::new()
                    };

                    if patt_type == Type::inferred_type("_") {
                        analyser.check_types(&mut symbol_table, &arm_patt_type, &mut patt_type)?;
                    } else {
                        analyser.check_types(&mut symbol_table, &patt_type, &mut arm_patt_type)?;
                    }

                    if arm_patt_type != patt_type {
                        if let Type::InferredType(_) = patt_type {
                            ()
                        } else {
                            return Err(SemanticErrorKind::TypeMismatchMatchExpr {
                                loc: "matched pattern".to_string(),
                                expected: patt_type,
                                found: arm_patt_type,
                            });
                        }
                    }

                    let mut arm_expr_type =
                        analyse_expr(analyser, &arm.arm_expression.clone(), root)?;

                    analyser.check_types(&mut symbol_table, &expr_type, &mut arm_expr_type)?;

                    if arm_expr_type != expr_type {
                        return Err(SemanticErrorKind::TypeMismatchMatchExpr {
                            loc: "match arm expression".to_string(),
                            expected: expr_type,
                            found: arm_expr_type,
                        });
                    }
                }
            }

            analyser.exit_scope();

            Ok(expr_type)
        }

        Expression::ForIn(fi) => {
            analyser.enter_scope(ScopeKind::ForInLoop);

            let iter_type = analyse_expr(analyser, &fi.iterator.clone(), root)?;

            let element_type = match iter_type.clone() {
                Type::Array { element_type, .. } | Type::Vec { element_type, .. } => *element_type,

                Type::Reference { inner_type, .. } => match *inner_type {
                    Type::Array { element_type, .. } | Type::Vec { element_type, .. } => {
                        *element_type
                    }
                    _ => {
                        return Err(SemanticErrorKind::TypeMismatchArrayElems {
                            expected: "iterable type".to_string(),
                            found: iter_type,
                        })
                    }
                },

                _ => {
                    return Err(SemanticErrorKind::TypeMismatchArrayElems {
                        expected: "iterable type".to_string(),
                        found: iter_type,
                    });
                }
            };

            if let Pattern::IdentifierPatt(id) = *fi.pattern.clone() {
                analyser.insert(
                    id.name.to_type_path(),
                    Symbol::Variable {
                        name: id.name,
                        var_type: element_type,
                    },
                )?;
            }

            analyse_patt(analyser, &fi.pattern.clone())?;

            analyse_expr(analyser, &Expression::Block(fi.block.clone()), root)?;

            analyser.exit_scope();

            Ok(Type::UnitType(UnitType))
        }

        Expression::While(w) => {
            analyse_expr(analyser, &Expression::Grouped(*w.condition.clone()), root)?;
            analyse_expr(analyser, &Expression::Block(w.block.clone()), root)?;
            Ok(Type::UnitType(UnitType))
        }

        Expression::SomeExpr(s) => {
            let ty = analyse_expr(analyser, &s.expression.clone().inner_expression, root)?;

            let ty = if ty == Type::Tuple(Vec::new()) {
                Type::UnitType(UnitType)
            } else {
                ty
            };

            Ok(Type::Option {
                inner_type: Box::new(ty),
            })
        }

        Expression::NoneExpr(_) => Ok(Type::Option {
            inner_type: Box::new(Type::UnitType(UnitType)),
        }),

        Expression::ResultExpr(r) => {
            let ty = analyse_expr(analyser, &r.expression.clone().inner_expression, root)?;

            let ty = if ty == Type::Tuple(Vec::new()) {
                Type::UnitType(UnitType)
            } else {
                ty
            };

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
    }
}

/// Analyse a function or method call expression by resolving the provided path, validating
/// arguments and determining the return type.
fn analyse_call_or_method_call_expr(
    analyser: &mut SemanticAnalyser,
    path: TypePath,
    args_opt: Option<Vec<Expression>>,
) -> Result<Type, SemanticErrorKind> {
    match analyser.lookup(&path).cloned() {
        Some(Symbol::Function { function, .. }) => {
            let func_params = function.params_opt.clone();
            let func_def_return_type = function.return_type_opt.clone();

            match (&args_opt, &func_params) {
                (None, None) => match func_def_return_type {
                    Some(ty) => Ok(*ty),
                    _ => Ok(Type::UnitType(UnitType)),
                },
                (None, Some(params)) => {
                    let mut self_counter: usize = 0;
                    let mut func_param_counter: usize = 0;

                    for param in params {
                        if let Type::SelfType(_) = param.param_type() {
                            self_counter += 1;
                        } else {
                            func_param_counter += 1;
                        }
                    }

                    if self_counter > 1 {
                        return Err(SemanticErrorKind::MethodParamCountError);
                    }

                    if self_counter != params.len() {
                        return Err(SemanticErrorKind::FuncArgCountMismatch {
                            function_path: path.to_identifier(),
                            expected: self_counter,
                            found: self_counter + func_param_counter,
                        });
                    }

                    match func_def_return_type {
                        Some(ty) => Ok(*ty),
                        _ => Ok(Type::UnitType(UnitType)),
                    }
                }
                (Some(args), None) => Err(SemanticErrorKind::FuncArgCountMismatch {
                    function_path: path.to_identifier(),
                    expected: 0,
                    found: args.len(),
                }),
                (Some(args), Some(params)) => {
                    let mut self_counter: usize = 0;

                    for param in params {
                        if let Type::SelfType(_) = param.param_type() {
                            self_counter += 1;
                        }
                    }

                    if self_counter > 1 {
                        return Err(SemanticErrorKind::MethodParamCountError);
                    }

                    let num_func_params = params.len() - self_counter;

                    if args.len() != num_func_params {
                        return Err(SemanticErrorKind::FuncArgCountMismatch {
                            function_path: path.to_identifier(),
                            expected: params.len(),
                            found: args.len(),
                        });
                    }

                    for (arg, param) in args.iter().zip(params) {
                        let mut arg_type = analyse_expr(analyser, &arg, &path)?;

                        let param_type = param.param_type();

                        let mut symbol_table = if let Some(scope) = analyser.scope_stack.last() {
                            scope.symbols.to_owned()
                        } else {
                            HashMap::new()
                        };

                        analyser.check_types(&mut symbol_table, &param_type, &mut arg_type)?;

                        if arg_type != param_type {
                            return Err(SemanticErrorKind::TypeMismatchArg {
                                arg_id: function.function_name.clone(),
                                expected: param_type,
                                found: arg_type,
                            });
                        }
                    }

                    match func_def_return_type {
                        Some(ty) => Ok(*ty),
                        None => Ok(Type::UnitType(UnitType)),
                    }
                }
            }
        }

        None => Err(SemanticErrorKind::UndefinedFunc {
            name: path.type_name,
        }),
        Some(sym) => Err(SemanticErrorKind::UnexpectedSymbol {
            name: path.type_name,
            expected: "function".to_string(),
            found: format!("`{sym}`"),
        }),
    }
}

pub(crate) fn wrap_into_expression<T>(value: T) -> Expression
where
    T: Clone + fmt::Debug + TryFrom<Expression>,
    Expression: From<T>,
{
    Expression::from(value.clone())
}
