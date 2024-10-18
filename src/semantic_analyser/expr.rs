use std::collections::HashMap;

use crate::{
    ast::{
        BigUInt, Bytes, ClosureParams, EnumVariantKind, Expression, Float, FunctionOrMethodParam,
        FunctionParam, FunctionPtr, Hash, Identifier, Int, Keyword, Literal, PathExpr, PathRoot,
        Pattern, Statement, Type, TypePath, UInt, UnaryOp,
    },
    error::SemanticErrorKind,
    log_trace, log_warn,
    semantic_analyser::symbol_table::{Scope, Symbol},
    span::Spanned,
};

use super::{
    patt::analyse_patt, symbol_table::ScopeKind, FormatItem, SemanticAnalyser, ToExpression,
    ToIdentifier,
};

/// Analyse the given expression to determine its type within a specific context.
pub(crate) fn analyse_expr(
    analyser: &mut SemanticAnalyser,
    expression: &Expression,
    root: &TypePath,
) -> Result<Type, SemanticErrorKind> {
    match expression {
        Expression::Array(a) => match &a.elements_opt {
            Some(elements) => match elements.first() {
                Some(expr) => {
                    let mut element_count = 0u64;

                    let mut first_element_type = analyse_expr(analyser, expr, root)?;

                    element_count += 1;

                    for elem in elements.iter().skip(1) {
                        let element_type = analyse_expr(analyser, elem, root)?;

                        element_count += 1;

                        analyser.check_types(
                            &mut analyser.current_symbol_table(),
                            &element_type,
                            &mut first_element_type,
                        )?;

                        if element_type != first_element_type {
                            return Err(SemanticErrorKind::TypeMismatchArrayElems {
                                expected: first_element_type.to_backtick_string(),
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
                    let element_type = Type::UNIT_TYPE;
                    let array = Type::Array {
                        element_type: Box::new(element_type),
                        num_elements: UInt::U64(0u64),
                    };

                    Ok(array)
                }
            },
            _ => Ok(Type::UNIT_TYPE),
        },

        Expression::Assignment(a) => {
            let assignee = a.lhs.to_expression();
            let assignee_type = analyse_expr(analyser, &assignee, root)?;

            let mut value_type = analyse_expr(analyser, &a.rhs.to_expression(), root)?;

            analyser.check_types(
                &mut analyser.current_symbol_table(),
                &assignee_type,
                &mut value_type,
            )?;

            let assignee_as_path_expr = PathExpr::from(assignee);

            let assignee_path = TypePath::from(assignee_as_path_expr);

            match analyser.lookup(&assignee_path).cloned() {
                Some(Symbol::Variable { var_type, .. }) => {
                    if var_type == value_type {
                        Ok(value_type)
                    } else {
                        Err(SemanticErrorKind::TypeMismatchVariable {
                            var_id: assignee_path.type_name,
                            expected: assignee_type.to_backtick_string(),
                            found: var_type,
                        })
                    }
                }
                Some(Symbol::Constant { constant_name, .. }) => {
                    Err(SemanticErrorKind::ConstantReassignment { constant_name })
                }
                Some(sym) => Err(SemanticErrorKind::UnexpectedSymbol {
                    name: assignee_path.type_name,
                    expected: assignee_type.to_backtick_string(),
                    found: sym.to_backtick_string(),
                }),
                _ => Err(SemanticErrorKind::UndefinedVariable {
                    name: assignee_path.type_name,
                }),
            }
        }

        Expression::Binary(b) => {
            let lhs_type = analyse_expr(analyser, &b.lhs.to_expression(), root)?;

            let mut rhs_type = analyse_expr(analyser, &b.rhs.to_expression(), root)?;

            analyser.check_types(
                &mut analyser.current_symbol_table(),
                &lhs_type,
                &mut rhs_type,
            )?;

            Ok(rhs_type)
        }

        Expression::Block(b) => match &b.statements_opt {
            Some(stmts) => {
                match analyser.scope_stack.last() {
                    Some(
                        Scope {
                            scope_kind: ScopeKind::Public,
                            ..
                        }
                        | Scope {
                            scope_kind: ScopeKind::ProgramRoot,
                            ..
                        },
                    )
                    | None => todo!(), // TODO: error – block expression out of context

                    Some(Scope {
                        scope_kind: ScopeKind::FunctionDef(path),
                        ..
                    }) => analyser.enter_scope(ScopeKind::FunctionBody(path.clone())),

                    Some(_) => analyser.enter_scope(ScopeKind::LocalBlock),
                }

                log_trace!(analyser.logger, "analysing block …");

                let mut cloned_iter = stmts.iter().peekable().clone();

                for (i, stmt) in stmts.iter().enumerate() {
                    if i == stmts.len() - 1 {
                        break;
                    }

                    println!("analysing statement {} of {} …", i + 1, stmts.len());

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

                    println!("finished analysing statement {} of {}", i + 1, stmts.len());
                }

                // log_trace!(
                //     analyser.logger,
                //     "analysing final statement in block …"
                // );

                let ty = match stmts.last() {
                    Some(stmt) => match stmt {
                        Statement::Expression(expr) => match analyse_expr(analyser, expr, root) {
                            Ok(ty) => Ok(ty),
                            Err(err) => {
                                analyser.log_error(err.clone(), &expr.span());
                                Err(err)
                            }
                        },
                        _ => Ok(Type::UNIT_TYPE),
                    },
                    _ => Ok(Type::UNIT_TYPE),
                };

                log_trace!(analyser.logger, "block analysis complete");

                analyser.exit_scope();

                ty
            }

            _ => Ok(Type::UNIT_TYPE),
        },

        Expression::Break(_) => Ok(Type::UNIT_TYPE),

        Expression::Call(c) => analyse_call_or_method_call_expr(
            analyser,
            TypePath::from(PathExpr::from(c.callee.to_expression())),
            c.args_opt.clone(),
        ),

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
                _ => Ok(Type::UNIT_TYPE),
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

            analyser.check_types(
                &mut analyser.current_symbol_table(),
                &return_type,
                &mut expression_type,
            )?;

            let function_ptr = FunctionPtr {
                params_opt,
                return_type_opt: Some(Box::new(return_type_clone)),
            };

            Ok(Type::FunctionPtr(function_ptr))
        }

        Expression::Comparison(c) => {
            let lhs_type = analyse_expr(analyser, &c.lhs.to_expression(), root)?;

            let mut rhs_type = analyse_expr(analyser, &c.rhs.to_expression(), root)?;

            analyser.check_types(
                &mut analyser.current_symbol_table(),
                &lhs_type,
                &mut rhs_type,
            )?;

            Ok(rhs_type)
        }

        Expression::CompoundAssignment(ca) => {
            let assignee = ca.lhs.to_expression();
            let assignee_type = analyse_expr(analyser, &assignee, root)?;

            let mut value_type = analyse_expr(analyser, &ca.rhs.to_expression(), root)?;

            analyser.check_types(
                &mut analyser.current_symbol_table(),
                &assignee_type,
                &mut value_type,
            )?;

            let assignee_as_path_expr = PathExpr::from(assignee);

            let assignee_path = TypePath::from(assignee_as_path_expr);

            match analyser.lookup(&assignee_path).cloned() {
                Some(Symbol::Variable { var_type, .. }) => {
                    if var_type == value_type {
                        Ok(value_type)
                    } else {
                        Err(SemanticErrorKind::TypeMismatchVariable {
                            var_id: assignee_path.type_name,
                            expected: assignee_type.to_backtick_string(),
                            found: var_type,
                        })
                    }
                }

                Some(Symbol::Constant { constant_name, .. }) => {
                    Err(SemanticErrorKind::ConstantReassignment {
                        constant_name: constant_name.clone(),
                    })
                }

                Some(sym) => Err(SemanticErrorKind::UnexpectedSymbol {
                    name: assignee_path.type_name,
                    expected: assignee_type.to_backtick_string(),
                    found: sym.to_backtick_string(),
                }),

                _ => Err(SemanticErrorKind::UndefinedVariable {
                    name: assignee_path.type_name,
                }),
            }
        }

        Expression::Continue(_) => Ok(Type::UNIT_TYPE),

        Expression::Dereference(d) => {
            match analyse_expr(analyser, &d.assignee_expr.to_expression(), root) {
                Ok(Type::Reference { inner_type, .. }) => Ok(*inner_type),
                Ok(ty) => Ok(ty),
                Err(e) => Err(e),
            }
        }

        Expression::FieldAccess(fa) => {
            let object = fa.object.to_expression();

            // convert object to path expression (i.e., check if object
            // is a valid path)
            let object_as_path_expr = PathExpr::from(object.clone());

            let object_path = TypePath::from(object_as_path_expr);

            match analyser.lookup(&object_path).cloned() {
                Some(Symbol::Struct { struct_def, .. }) => match &struct_def.fields_opt {
                    Some(fields) => match fields.iter().find(|f| f.field_name == fa.field_name) {
                        Some(sdf) => Ok(*sdf.field_type.clone()),
                        _ => Err(SemanticErrorKind::UndefinedField {
                            struct_path: object_path.to_identifier(),
                            field_name: fa.field_name.clone(),
                        }),
                    },
                    _ => Ok(Type::UNIT_TYPE),
                },

                None => {
                    let object_type = analyse_expr(analyser, &object, &object_path)?;

                    Err(SemanticErrorKind::UndefinedType {
                        name: object_type.to_identifier(),
                    })
                }

                Some(sym) => {
                    let object_type = analyse_expr(analyser, &object, &object_path)?;

                    Err(SemanticErrorKind::UnexpectedSymbol {
                        name: object_type.to_identifier(),
                        expected: "struct".to_string(),
                        found: sym.to_backtick_string(),
                    })
                }
            }
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

            println!("analysing for-in loop with iterator type `{iter_type}` and element type `{element_type}` …");

            if let Pattern::IdentifierPatt(id) = *fi.pattern.clone() {
                analyser.insert(
                    id.name.to_type_path(),
                    Symbol::Variable {
                        name: id.name,
                        var_type: element_type,
                    },
                )?;
            }

            println!("for-in loop pattern: `{}`", fi.pattern);

            analyse_patt(analyser, &fi.pattern.clone())?;

            analyse_expr(analyser, &Expression::Block(fi.block.clone()), root)?;

            println!("finished analysing for-in loop");

            analyser.exit_scope();

            Ok(Type::UNIT_TYPE)
        }

        Expression::Grouped(g) => analyse_expr(analyser, &g.inner_expression, root),

        Expression::If(i) => {
            println!("entering `if` expression");

            analyse_expr(analyser, &Expression::Grouped(*i.condition.clone()), root)?;

            let if_block_type =
                analyse_expr(analyser, &Expression::Block(*i.if_block.clone()), root)?;

            println!("finished analysing `if` block with type: `{if_block_type}`");

            let else_if_blocks_type = match &i.else_if_blocks_opt {
                Some(blocks) => match blocks.first() {
                    Some(_) => {
                        println!("detected {} `else-if` block(s) …", blocks.len());

                        for block in blocks.iter() {
                            println!("analysing `else-if` block …");

                            let block_type =
                                analyse_expr(analyser, &Expression::If(*block.clone()), root)?;

                            if block_type != if_block_type {
                                return Err(SemanticErrorKind::TypeMismatchValues {
                                    expected: if_block_type,
                                    found: block_type,
                                });
                            }

                            println!(
                                "finished analysing `else-if` block with type: `{block_type}`"
                            );
                        }

                        if_block_type.clone()
                    }
                    _ => Type::UNIT_TYPE,
                },
                _ => Type::UNIT_TYPE,
            };

            let trailing_else_block_type = match &i.trailing_else_block_opt {
                Some(block) => analyse_expr(analyser, &Expression::Block(block.clone()), root)?,
                _ => Type::UNIT_TYPE,
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

            println!("finished analysing if expression");

            Ok(if_block_type)
        }

        Expression::Index(i) => {
            let array_type = analyse_expr(analyser, &i.array.to_expression(), root)?;

            let index_type = analyse_expr(analyser, &i.index.to_expression(), root)?;

            match &index_type {
                Type::U8 | Type::U16 | Type::U32 | Type::U64 => (),

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

        Expression::Literal(l) => match l {
            Literal::Int { value, .. } => match value {
                Int::I32(_) => Ok(Type::I32),
                Int::I64(_) => Ok(Type::I64),
            },
            Literal::UInt { value, .. } => match value {
                UInt::U8(_) => Ok(Type::U8),
                UInt::U16(_) => Ok(Type::U16),
                UInt::U32(_) => Ok(Type::U32),
                UInt::U64(_) => Ok(Type::U64),
            },
            Literal::BigUInt { value, .. } => match value {
                BigUInt::U256(_) => Ok(Type::U256),
                BigUInt::U512(_) => Ok(Type::U512),
            },
            Literal::Float { value, .. } => match value {
                Float::F32(_) => Ok(Type::F32),
                Float::F64(_) => Ok(Type::F64),
            },
            Literal::Byte { .. } => Ok(Type::Byte),
            Literal::Bytes { value, .. } => match value {
                Bytes::B2(_) => Ok(Type::B2),
                Bytes::B4(_) => Ok(Type::B4),
                Bytes::B8(_) => Ok(Type::B8),
                Bytes::B16(_) => Ok(Type::B16),
                Bytes::B32(_) => Ok(Type::B32),
            },
            Literal::Hash { value, .. } => match value {
                Hash::H160(_) => Ok(Type::H160),
                Hash::H256(_) => Ok(Type::H256),
                Hash::H512(_) => Ok(Type::H512),
            },
            Literal::Str { .. } => Ok(Type::Str),
            Literal::Char { .. } => Ok(Type::Char),
            Literal::Bool { .. } => Ok(Type::Bool),
        },

        Expression::Mapping(m) => match &m.pairs_opt {
            Some(pairs) => match pairs.first() {
                Some(pair) => {
                    let key_type = analyse_patt(analyser, &pair.k.clone())?;
                    let value_type = analyse_expr(analyser, &pair.v.clone(), root)?;

                    for pair in pairs.iter().skip(1) {
                        let mut pair_key_type = analyse_patt(analyser, &pair.k.clone())?;
                        let mut pair_value_type = analyse_expr(analyser, &pair.v.clone(), root)?;

                        analyser.check_types(
                            &mut analyser.current_symbol_table(),
                            &key_type,
                            &mut pair_key_type,
                        )?;

                        analyser.check_types(
                            &mut analyser.current_symbol_table(),
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
                    let key_type = Box::new(Type::UNIT_TYPE);
                    let value_type = Box::new(Type::UNIT_TYPE);

                    Ok(Type::Mapping {
                        key_type,
                        value_type,
                    })
                }
            },
            _ => Ok(Type::UNIT_TYPE),
        },

        Expression::Match(m) => {
            analyser.enter_scope(ScopeKind::MatchExpr);

            let scrutinee_type = analyse_expr(analyser, &m.scrutinee.to_expression(), root)?;

            println!("analysing match expression with scrutinee type: `{scrutinee_type}` …");

            let mut matched_patt_type =
                analyse_patt(analyser, &m.final_arm.matched_pattern.clone())?;

            // Q: what if `matched_patt_type` is a `u64` and `scrutinee_type` is an `i64`?
            // A: we shouldn't throw an error as they are compatible; hence this code is wrong
            // if matched_patt_type != scrutinee_type {
            //     if let Type::InferredType(_) = matched_patt_type {
            //         ()
            //     } else {
            //         return Err(SemanticErrorKind::TypeMismatchMatchExpr {
            //             loc: "scrutinee and matched pattern".to_string(),
            //             expected: scrutinee_type,
            //             found: matched_patt_type,
            //         });
            //     }
            // }

            let expr_type = analyse_expr(analyser, &m.final_arm.arm_expression.clone(), root)?;

            if let Some(arms) = &m.match_arms_opt {
                for arm in arms.iter() {
                    let mut arm_patt_type = analyse_patt(analyser, &arm.matched_pattern)?;

                    if matched_patt_type == Type::inferred_type("_") {
                        analyser.check_types(
                            &mut analyser.current_symbol_table(),
                            &scrutinee_type,
                            &mut matched_patt_type,
                        )?;
                    } else {
                        analyser.check_types(
                            &mut analyser.current_symbol_table(),
                            &matched_patt_type,
                            &mut arm_patt_type,
                        )?;
                    }

                    // Q: what if `arm_patt_type` is a `u64` and `matched_patt_type` is an `i64`?
                    // A: we shouldn't throw an error as they are compatible; hence this code is wrong
                    // if arm_patt_type != matched_patt_type {
                    //     if let Type::InferredType(_) = matched_patt_type {
                    //         ()
                    //     } else {
                    //         return Err(SemanticErrorKind::TypeMismatchMatchExpr {
                    //             loc: "matched pattern".to_string(),
                    //             expected: matched_patt_type,
                    //             found: arm_patt_type,
                    //         });
                    //     }
                    // }

                    let mut arm_expr_type =
                        analyse_expr(analyser, &arm.arm_expression.clone(), root)?;

                    analyser.check_types(
                        &mut analyser.current_symbol_table(),
                        &expr_type,
                        &mut arm_expr_type,
                    )?;

                    if arm_expr_type != expr_type {
                        return Err(SemanticErrorKind::TypeMismatchMatchExpr {
                            loc: "match arm expression".to_string(),
                            expected: expr_type,
                            found: arm_expr_type,
                        });
                    }
                }
            }

            println!("finished analysing match expression with type: `{expr_type}`");

            analyser.exit_scope();

            Ok(expr_type)
        }

        Expression::MethodCall(mc) => {
            let receiver = mc.receiver.to_expression();
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
                            expected: path.to_backtick_string(),
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
                    found: sym.to_backtick_string(),
                }),
            }
        }

        Expression::NoneExpr(_) => Ok(Type::Option {
            inner_type: Box::new(Type::UNIT_TYPE),
        }),

        Expression::Path(p) => {
            let path = match p.tree_opt.clone() {
                Some(mut segments) => {
                    if let Some(id) = segments.pop() {
                        let root = match &p.path_root {
                            PathRoot::Identifier(id) => id.to_type_path(),
                            PathRoot::SelfKeyword => Identifier::from("self").to_type_path(),
                            PathRoot::SelfType(_) => Identifier::from("Self").to_type_path(),

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

            if let Some(symbol) = analyser.lookup(&path).cloned() {
                match &symbol {
                    Symbol::Variable { name, var_type } => {
                        if let Type::UserDefined(_) = var_type {
                            let mut variable_type_path = name.to_type_path();

                            let associated_type_path = &mut variable_type_path.strip_suffix();

                            if let Some(sym) = analyser.lookup(associated_type_path) {
                                match sym {
                                    Symbol::Enum {
                                        path: enum_path, ..
                                    } => Ok(Type::UserDefined(enum_path.clone())),

                                    _ => Ok(symbol.symbol_type()),
                                }
                            } else {
                                Err(SemanticErrorKind::UndefinedType {
                                    name: associated_type_path.to_identifier(),
                                })
                            }
                        } else {
                            Ok(symbol.symbol_type())
                        }
                    }
                    _ => Ok(symbol.symbol_type()),
                }
            } else {
                Err(SemanticErrorKind::UndefinedVariable {
                    name: path.type_name,
                })
            }
        }

        Expression::Range(r) => match (&r.from_expr_opt, &r.to_expr_opt) {
            (None, None) => Ok(Type::UNIT_TYPE),
            (None, Some(to)) => {
                let to_type = analyse_expr(analyser, &to.to_expression(), root)?;

                match to_type {
                    Type::I32
                    | Type::I64
                    | Type::U8
                    | Type::U16
                    | Type::U32
                    | Type::U64
                    | Type::U256
                    | Type::U512 => Ok(to_type),
                    _ => Err(SemanticErrorKind::UnexpectedType {
                        expected: "numeric type".to_string(),
                        found: to_type,
                    }),
                }
            }
            (Some(from), None) => {
                let from_type = analyse_expr(analyser, &from.to_expression(), root)?;

                match &from_type {
                    Type::I32
                    | Type::I64
                    | Type::U8
                    | Type::U16
                    | Type::U32
                    | Type::U64
                    | Type::U256
                    | Type::U512 => Ok(from_type),
                    _ => Err(SemanticErrorKind::UnexpectedType {
                        expected: "numeric type".to_string(),
                        found: from_type,
                    }),
                }
            }
            (Some(from), Some(to)) => {
                let from_type = analyse_expr(analyser, &from.to_expression(), root)?;

                match &from_type {
                    Type::I32
                    | Type::I64
                    | Type::U8
                    | Type::U16
                    | Type::U32
                    | Type::U64
                    | Type::U256
                    | Type::U512 => (),
                    _ => {
                        return Err(SemanticErrorKind::UnexpectedType {
                            expected: "numeric type".to_string(),
                            found: from_type,
                        })
                    }
                }

                let to_type = analyse_expr(analyser, &to.to_expression(), root)?;

                match &to_type {
                    Type::I32
                    | Type::I64
                    | Type::U8
                    | Type::U16
                    | Type::U32
                    | Type::U64
                    | Type::U256
                    | Type::U512 => (),
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

        Expression::Reference(r) => {
            let reference_op = r.reference_op.clone();
            let expr_type = analyse_expr(analyser, &r.expression, root)?;

            Ok(Type::Reference {
                reference_op,
                inner_type: Box::new(expr_type),
            })
        }

        Expression::ResultExpr(r) => {
            let ty = analyse_expr(analyser, &r.expression.clone().inner_expression, root)?;

            let ty = if ty == Type::Tuple(Vec::new()) {
                Type::UNIT_TYPE
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

        Expression::Return(r) => match &r.expression_opt {
            Some(expr) => analyse_expr(analyser, &expr.clone(), root),
            _ => Ok(Type::UNIT_TYPE),
        },

        Expression::SomeExpr(s) => {
            let ty = analyse_expr(analyser, &s.expression.clone().inner_expression, root)?;

            let ty = if ty == Type::Tuple(Vec::new()) {
                Type::UNIT_TYPE
            } else {
                ty
            };

            Ok(Type::Option {
                inner_type: Box::new(ty),
            })
        }

        Expression::Struct(s) => {
            let type_path = root.join(TypePath::from(s.struct_path.clone()));
            let obj_fields_opt = s.struct_fields_opt.clone();

            let hash_map = HashMap::new();
            let mut field_map: HashMap<Identifier, Type> = hash_map;

            if let Some(obj_fields) = obj_fields_opt {
                for obj_field in obj_fields.iter() {
                    let field_name = obj_field.field_name.clone();
                    let field_value = *obj_field.field_value.clone();
                    let field_type = analyse_expr(analyser, &field_value, root)?;

                    field_map.insert(field_name, field_type);
                }
            }

            match analyser.lookup(&type_path).cloned() {
                Some(Symbol::Struct {
                    struct_def, path, ..
                }) => {
                    let def_fields_opt = struct_def.fields_opt;

                    if let Some(def_fields) = def_fields_opt {
                        if field_map.len() != def_fields.len() {
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

                        for def_field in def_fields.iter() {
                            match field_map.get_mut(&def_field.field_name) {
                                Some(obj_field_type) => {
                                    analyser.check_types(
                                        &mut analyser.current_symbol_table(),
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

                Some(Symbol::Variable { name, .. }) => {
                    let mut variable_type_path = name.to_type_path();

                    let associated_type_path = &mut variable_type_path.strip_suffix();

                    match analyser.lookup(associated_type_path).cloned() {
                        Some(Symbol::Enum { path, enum_def, .. }) => {
                            for variant in enum_def.variants.iter() {
                                if variant.variant_path == variable_type_path {
                                    match &variant.variant_kind {
                                        EnumVariantKind::Struct(struct_variant) => {
                                            if field_map.len() != struct_variant.struct_fields.len()
                                            {
                                                return Err(
                                                    SemanticErrorKind::StructArgCountMismatch {
                                                        struct_path: variant
                                                            .variant_path
                                                            .to_identifier(),
                                                        expected: struct_variant
                                                            .struct_fields
                                                            .len(),
                                                        found: field_map.len(),
                                                    },
                                                );
                                            }

                                            let field_names = struct_variant
                                                .struct_fields
                                                .iter()
                                                .cloned()
                                                .map(|sdf| sdf.field_name)
                                                .collect::<Vec<_>>();

                                            for (name, _) in field_map.iter() {
                                                if !field_names.contains(name) {
                                                    return Err(
                                                        SemanticErrorKind::UnexpectedStructField {
                                                            struct_name: variant
                                                                .variant_path
                                                                .to_identifier(),
                                                            found: name.clone(),
                                                        },
                                                    );
                                                }
                                            }

                                            for def_field in struct_variant.struct_fields.iter() {
                                                match field_map.get_mut(&def_field.field_name) {
                                                    Some(obj_field_type) => {
                                                        analyser.check_types(
                                                            &mut analyser.current_symbol_table(),
                                                            &*def_field.field_type,
                                                            &mut *obj_field_type,
                                                        )?;
                                                    }

                                                    None => {
                                                        return Err(
                                                            SemanticErrorKind::MissingStructField {
                                                                expected: format!(
                                                                    "`{}: {}`",
                                                                    &def_field.field_name,
                                                                    *def_field.field_type
                                                                ),
                                                            },
                                                        )
                                                    }
                                                }
                                            }
                                        }

                                        EnumVariantKind::Standard => {
                                            return Err(SemanticErrorKind::UnexpectedSymbol {
                                                name,
                                                expected: "struct enum variant".to_string(),
                                                found: "standard enum variant".to_string(),
                                            })
                                        }

                                        EnumVariantKind::TupleStruct(_) => {
                                            return Err(SemanticErrorKind::UnexpectedSymbol {
                                                name,
                                                expected: "struct enum variant".to_string(),
                                                found: "tuple struct enum variant".to_string(),
                                            })
                                        }
                                    }
                                }
                            }

                            Ok(Type::UserDefined(path.clone()))
                        }

                        Some(sym) => Err(SemanticErrorKind::UnexpectedSymbol {
                            name: type_path.type_name,
                            expected: "struct".to_string(),
                            found: sym.to_backtick_string(),
                        }),

                        _ => Err(SemanticErrorKind::MissingItem {
                            expected: "associated enum".to_string(),
                        }),
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

        Expression::TupleIndex(ti) => {
            let tuple_type = analyse_expr(analyser, &ti.tuple.to_expression(), root)?;

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
                            found: sym.to_backtick_string(),
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

        Expression::TupleStruct(ts) => {
            let type_path = root.join(TypePath::from(ts.struct_path.clone()));
            let elements_opt = ts.struct_elements_opt.clone();

            match analyser.lookup(&type_path).cloned() {
                Some(Symbol::TupleStruct {
                    tuple_struct_def,
                    path,
                    ..
                }) => {
                    let fields_opt = tuple_struct_def.fields_opt;

                    match (&elements_opt, &fields_opt) {
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
                            if elements.len() != fields.len() {
                                return Err(SemanticErrorKind::StructArgCountMismatch {
                                    struct_path: type_path.to_identifier(),
                                    expected: fields.len(),
                                    found: elements.len(),
                                });
                            }

                            for (elem, field) in elements.iter().zip(fields) {
                                let field_type = *field.field_type.clone();

                                let mut elem_type = analyse_expr(
                                    analyser,
                                    &elem,
                                    &Identifier::from("").to_type_path(),
                                )?;

                                analyser.check_types(
                                    &mut analyser.current_symbol_table(),
                                    &field_type,
                                    &mut elem_type,
                                )?;

                                if elem_type != field_type {
                                    return Err(SemanticErrorKind::TypeMismatchVariable {
                                        var_id: tuple_struct_def.struct_name,
                                        expected: field_type.to_backtick_string(),
                                        found: elem_type,
                                    });
                                }
                            }

                            Ok(Type::UserDefined(path))
                        }
                    }
                }

                Some(Symbol::Variable { name, .. }) => {
                    let mut variable_type_path = name.to_type_path();

                    let associated_type_path = &mut variable_type_path.strip_suffix();

                    match analyser.lookup(associated_type_path).cloned() {
                        Some(Symbol::Enum { path, enum_def, .. }) => {
                            for variant in enum_def.variants.iter() {
                                if variant.variant_path == variable_type_path {
                                    match &variant.variant_kind {
                                        EnumVariantKind::TupleStruct(tuple_struct_variant) => {
                                            if let Some(elems) = elements_opt.clone() {
                                                if elems.len()
                                                    != tuple_struct_variant.element_types.len()
                                                {
                                                    return Err(
                                                        SemanticErrorKind::StructArgCountMismatch {
                                                            struct_path: name,
                                                            expected: tuple_struct_variant
                                                                .element_types
                                                                .len(),
                                                            found: elems.len(),
                                                        },
                                                    );
                                                }

                                                for (elem, ty) in elems
                                                    .iter()
                                                    .zip(tuple_struct_variant.element_types.clone())
                                                {
                                                    let mut elem_type = analyse_expr(
                                                        analyser,
                                                        &elem,
                                                        &Identifier::from("").to_type_path(),
                                                    )?;

                                                    analyser.check_types(
                                                        &mut analyser.current_symbol_table(),
                                                        &ty,
                                                        &mut elem_type,
                                                    )?;

                                                    if elem_type != ty {
                                                        return Err(SemanticErrorKind::TypeMismatchVariable {
                                                            var_id: name,
                                                            expected: ty.to_backtick_string(),
                                                            found: elem_type,
                                                        });
                                                    }
                                                }
                                            } else {
                                                return Err(
                                                    SemanticErrorKind::StructArgCountMismatch {
                                                        struct_path: name,
                                                        expected: tuple_struct_variant
                                                            .element_types
                                                            .len(),
                                                        found: 0,
                                                    },
                                                );
                                            }
                                        }

                                        EnumVariantKind::Standard => {
                                            return Err(SemanticErrorKind::UnexpectedSymbol {
                                                name,
                                                expected: "tuple struct enum variant".to_string(),
                                                found: "standard enum variant".to_string(),
                                            })
                                        }

                                        EnumVariantKind::Struct(_) => {
                                            return Err(SemanticErrorKind::UnexpectedSymbol {
                                                name,
                                                expected: "tuple struct enum variant".to_string(),
                                                found: "struct enum variant".to_string(),
                                            })
                                        }
                                    }
                                }
                            }

                            Ok(Type::UserDefined(path))
                        }
                        _ => todo!(),
                    }
                }

                None => Err(SemanticErrorKind::UndefinedStruct {
                    name: type_path.type_name,
                }),

                Some(sym) => Err(SemanticErrorKind::UnexpectedSymbol {
                    name: type_path.type_name,
                    expected: "tuple struct".to_string(),
                    found: sym.to_backtick_string(),
                }),
            }
        }

        Expression::Tuple(t) => {
            let mut element_types: Vec<Type> = Vec::new();

            for elem in t.tuple_elements.elements.iter() {
                let ty = analyse_expr(analyser, elem, root)?;

                element_types.push(ty)
            }

            Ok(Type::Tuple(element_types))
        }

        Expression::TypeCast(tc) => {
            let value_type = analyse_expr(analyser, &tc.value.to_expression(), root)?;

            let new_type = *tc.new_type.clone();

            match (&value_type, &new_type) {
                (
                    Type::I32 | Type::I64,
                    Type::I32
                    | Type::I64
                    | Type::U8
                    | Type::U16
                    | Type::U32
                    | Type::U64
                    | Type::U256
                    | Type::U512
                    | Type::F32
                    | Type::F64,
                )
                | (
                    Type::U8 | Type::Byte,
                    Type::I32
                    | Type::I64
                    | Type::U8
                    | Type::U16
                    | Type::U32
                    | Type::U64
                    | Type::U256
                    | Type::U512
                    | Type::F32
                    | Type::F64
                    | Type::Byte
                    | Type::Char,
                )
                | (
                    Type::U16 | Type::U32,
                    Type::I32
                    | Type::I64
                    | Type::U8
                    | Type::U16
                    | Type::U32
                    | Type::U64
                    | Type::U256
                    | Type::U512
                    | Type::F32
                    | Type::F64
                    | Type::Char,
                )
                | (
                    Type::U64,
                    Type::I32
                    | Type::I64
                    | Type::U8
                    | Type::U16
                    | Type::U32
                    | Type::U64
                    | Type::U256
                    | Type::U512
                    | Type::F32
                    | Type::F64,
                )
                | (Type::U256 | Type::U512, Type::U256 | Type::U512)
                | (Type::F32 | Type::F64, Type::F32 | Type::F64)
                | (
                    Type::B2 | Type::B4 | Type::B16 | Type::B32,
                    Type::B2 | Type::B4 | Type::B16 | Type::B32,
                )
                | (Type::H160 | Type::H256 | Type::H512, Type::H160 | Type::H256 | Type::H512)
                | (Type::Char, Type::U8 | Type::U16 | Type::U32 | Type::U64) => Ok(new_type),
                (t, u) => Err(SemanticErrorKind::TypeCastError {
                    from: t.clone(),
                    to: u.clone(),
                }),
            }
        }

        Expression::Unary(u) => {
            let expr_type = analyse_expr(analyser, &u.value_expr.to_expression(), root)?;

            match u.unary_op {
                UnaryOp::Negate => match &expr_type {
                    Type::I32
                    | Type::I64
                    | Type::U8
                    | Type::U16
                    | Type::U32
                    | Type::U64
                    | Type::U256
                    | Type::U512
                    | Type::F32
                    | Type::F64 => Ok(expr_type),
                    _ => Err(SemanticErrorKind::UnexpectedType {
                        expected: "numeric value".to_string(),
                        found: expr_type,
                    }),
                },
                UnaryOp::Not => match &expr_type {
                    Type::Bool => Ok(expr_type),
                    _ => Err(SemanticErrorKind::UnexpectedType {
                        expected: "boolean".to_string(),
                        found: expr_type,
                    }),
                },
            }
        }

        Expression::Underscore(_) => Ok(Type::inferred_type("_")),

        Expression::Unwrap(u) => analyse_expr(analyser, &u.value_expr.to_expression(), root),

        Expression::While(w) => {
            println!(
                "analysing while loop with condition `{}` …",
                Expression::Grouped(*w.condition.clone())
            );

            analyse_expr(analyser, &Expression::Grouped(*w.condition.clone()), root)?;
            analyse_expr(analyser, &Expression::Block(w.block.clone()), root)?;

            println!("finished analysing while loop");

            Ok(Type::UNIT_TYPE)
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
                    _ => Ok(Type::UNIT_TYPE),
                },
                (None, Some(params)) => {
                    let mut self_counter: usize = 0;
                    let mut func_param_counter: usize = 0;

                    for param in params {
                        if let Type::SelfType { .. } = param.param_type() {
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
                        _ => Ok(Type::UNIT_TYPE),
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
                        if let Type::SelfType { .. } = param.param_type() {
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

                        analyser.check_types(
                            &mut analyser.current_symbol_table(),
                            &param_type,
                            &mut arg_type,
                        )?;

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
                        None => Ok(Type::UNIT_TYPE),
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
            found: sym.to_backtick_string(),
        }),
    }
}
