#![allow(dead_code)]

mod symbol_table;

use crate::{
    ast::{
        BigUInt, Bool, Byte, Bytes, Char, Expression, Float, Hash, Identifier, InferredType, Int,
        Literal, PathRoot, Statement, Str, Type, UInt, Unit,
    },
    error::{CompilerError, ErrorsEmitted, SemanticErrorKind},
    parser::Module,
    span::Spanned,
    B16, B2, B32, B4, B8, F32, F64, H160, H256, H512, U256, U512,
};

use self::symbol_table::SymbolTable;

struct SemanticAnalyzer {
    symbol_table: SymbolTable,
    errors: Vec<CompilerError<SemanticErrorKind>>,
}

impl SemanticAnalyzer {
    fn new() -> Self {
        SemanticAnalyzer {
            symbol_table: SymbolTable::new(),
            errors: Vec::new(),
        }
    }

    fn analyze(&mut self, module: &Module) -> Result<(), ErrorsEmitted> {
        for s in &module.statements {
            self.analyze_stmt(s)?;
        }
        Ok(())
    }

    fn analyze_stmt(&mut self, statement: &Statement) -> Result<(), ErrorsEmitted> {
        match statement {
            Statement::Let(s) => {
                let name = s.assignee.name.clone();
                let value = s.value_opt.clone().unwrap();

                let expr_type = self.analyze_expr(&value).map_err(|e| {
                    self.log_error(e, &value);
                    ErrorsEmitted
                })?;

                self.symbol_table.insert(name, expr_type);
            }

            Statement::Item(_) => (),

            Statement::Expression(expr) => match self.analyze_expr(expr) {
                Ok(_) => (),
                Err(err) => {
                    self.log_error(err, expr);
                    return Err(ErrorsEmitted);
                }
            },
        }

        Ok(())
    }

    fn analyze_expr(&mut self, expression: &Expression) -> Result<Type, SemanticErrorKind> {
        match expression {
            Expression::Path(p) => {
                let name = match &p.tree_opt {
                    Some(v) => match v.last() {
                        Some(i) => i.clone(),
                        _ => match &p.path_root {
                            PathRoot::Identifier(i) => i.clone(),

                            PathRoot::SelfType(_) => Identifier::from("Self"),

                            PathRoot::SelfKeyword => Identifier::from("self"),

                            PathRoot::Package => {
                                return Err(SemanticErrorKind::InvalidPathIdentifier {
                                    name: Identifier::from("package"),
                                })
                            }

                            PathRoot::Super => {
                                return Err(SemanticErrorKind::InvalidPathIdentifier {
                                    name: Identifier::from("super"),
                                })
                            }
                        },
                    },

                    _ => match &p.path_root {
                        PathRoot::Identifier(i) => i.clone(),

                        PathRoot::SelfType(_) => Identifier::from("Self"),

                        PathRoot::SelfKeyword => Identifier::from("self"),

                        PathRoot::Package => {
                            return Err(SemanticErrorKind::InvalidPathIdentifier {
                                name: Identifier::from("package"),
                            })
                        }

                        PathRoot::Super => {
                            return Err(SemanticErrorKind::InvalidPathIdentifier {
                                name: Identifier::from("super"),
                            })
                        }
                    },
                };

                match self.symbol_table.get(&name) {
                    Some(t) => Ok(t.clone()),
                    _ => Err(SemanticErrorKind::UndefinedPath { name }),
                }
            }

            Expression::Literal(l) => match l {
                Literal::Int { value, .. } => match value {
                    Int::I32(_) => Ok(Type::I32(Int::I32(i32::default()))),
                    Int::I64(_) => Ok(Type::I64(Int::I64(i64::default()))),
                    Int::I128(_) => Ok(Type::I128(Int::I128(i128::default()))),
                },

                Literal::UInt { value, .. } => match value {
                    UInt::U8(_) => Ok(Type::U8(UInt::U8(u8::default()))),
                    UInt::U16(_) => Ok(Type::U16(UInt::U16(u16::default()))),
                    UInt::U32(_) => Ok(Type::U32(UInt::U32(u32::default()))),
                    UInt::U64(_) => Ok(Type::U64(UInt::U64(u64::default()))),
                    UInt::U128(_) => Ok(Type::U128(UInt::U128(u128::default()))),
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

            Expression::MethodCall(_) => todo!(),

            Expression::FieldAccess(_) => todo!(),

            Expression::Call(_) => todo!(),

            Expression::Index(_) => todo!(),

            Expression::TupleIndex(_) => todo!(),

            Expression::Unwrap(u) => {
                self.analyze_expr(&Expression::try_from(*u.value_expr.clone()).map_err(|_| {
                    SemanticErrorKind::ConversionError {
                        from: format!("{:?}", u),
                        into: "`Expression`".to_string(),
                    }
                })?)
            }

            Expression::Unary(_) => todo!(),

            Expression::Reference(r) => {
                let reference_op = r.reference_op.clone();
                let inner_type = self.analyze_expr(&*r.expression)?;

                Ok(Type::Reference {
                    reference_op,
                    inner_type: Box::new(inner_type),
                })
            }

            Expression::Dereference(d) => {
                self.analyze_expr(&Expression::try_from(d.assignee_expr.clone()).map_err(|_| {
                    SemanticErrorKind::ConversionError {
                        from: format!("`{:?}`", d),
                        into: "`Expression`".to_string(),
                    }
                })?)
            }

            Expression::TypeCast(tc) => Ok(*tc.new_type.clone()),

            Expression::Binary(b) => {
                let lhs_clone = *b.lhs.clone();
                let rhs_clone = *b.rhs.clone();

                let lhs_type =
                    self.analyze_expr(&Expression::try_from(*b.lhs.clone()).map_err(|_| {
                        SemanticErrorKind::ConversionError {
                            from: format!("`{:?}`", &lhs_clone),
                            into: "`Expression`".to_string(),
                        }
                    })?)?;

                let rhs_type =
                    self.analyze_expr(&Expression::try_from(*b.rhs.clone()).map_err(|_| {
                        SemanticErrorKind::ConversionError {
                            from: format!("`{:?}`", &rhs_clone),
                            into: "`Expression`".to_string(),
                        }
                    })?)?;

                match (&lhs_type, &rhs_type) {
                    (Type::I32(_), Type::I32(_)) => Ok(Type::I32(Int::I32(i32::default()))),

                    (Type::I32(_), t) => Err(SemanticErrorKind::TypeMismatchBinaryExpr {
                        expected: "`i32`".to_string(),
                        found: t.to_string(),
                    }),

                    (Type::I64(_), Type::I32(_) | Type::I64(_)) => {
                        Ok(Type::I64(Int::I64(i64::default())))
                    }

                    (Type::I64(_), t) => Err(SemanticErrorKind::TypeMismatchBinaryExpr {
                        expected: "`i64` or `i32`".to_string(),
                        found: t.to_string(),
                    }),

                    (Type::I128(_), Type::I32(_) | Type::I64(_) | Type::I128(_)) => {
                        Ok(Type::I128(Int::I128(i128::default())))
                    }

                    (Type::I128(_), t) => Err(SemanticErrorKind::TypeMismatchBinaryExpr {
                        expected: "`i128` or smaller signed integer".to_string(),
                        found: t.to_string(),
                    }),

                    (Type::U8(_), Type::U8(_)) => Ok(Type::U8(UInt::U8(u8::default()))),

                    (Type::U8(_), t) => Err(SemanticErrorKind::TypeMismatchBinaryExpr {
                        expected: "`u8`".to_string(),
                        found: t.to_string(),
                    }),

                    (Type::U16(_), Type::U8(_) | Type::U16(_)) => {
                        Ok(Type::U16(UInt::U16(u16::default())))
                    }

                    (Type::U16(_), t) => Err(SemanticErrorKind::TypeMismatchBinaryExpr {
                        expected: "`u16` or `u8`".to_string(),
                        found: t.to_string(),
                    }),

                    (Type::U32(_), Type::U8(_) | Type::U16(_) | Type::U32(_)) => {
                        Ok(Type::U32(UInt::U32(u32::default())))
                    }

                    (Type::U32(_), t) => Err(SemanticErrorKind::TypeMismatchBinaryExpr {
                        expected: "`u32` or smaller unsigned integer".to_string(),
                        found: t.to_string(),
                    }),

                    (Type::U64(_), Type::U8(_) | Type::U16(_) | Type::U32(_) | Type::U64(_)) => {
                        Ok(Type::U64(UInt::U64(u64::default())))
                    }

                    (Type::U64(_), t) => Err(SemanticErrorKind::TypeMismatchBinaryExpr {
                        expected: "`u64` or smaller unsigned integer".to_string(),
                        found: t.to_string(),
                    }),

                    (
                        Type::U128(_),
                        Type::U8(_) | Type::U16(_) | Type::U32(_) | Type::U64(_) | Type::U128(_),
                    ) => Ok(Type::U128(UInt::U128(u128::default()))),

                    (Type::U128(_), t) => Err(SemanticErrorKind::TypeMismatchBinaryExpr {
                        expected: "`u128` or smaller unsigned integer".to_string(),
                        found: t.to_string(),
                    }),

                    (Type::U256(_), Type::U256(_)) => {
                        Ok(Type::U256(BigUInt::U256(U256::default())))
                    }

                    (Type::U256(_), t) => Err(SemanticErrorKind::TypeMismatchBinaryExpr {
                        expected: "`u256`".to_string(),
                        found: t.to_string(),
                    }),

                    (Type::U512(_), Type::U256(_) | Type::U512(_)) => {
                        Ok(Type::U512(BigUInt::U512(U512::default())))
                    }

                    (Type::U512(_), t) => Err(SemanticErrorKind::TypeMismatchBinaryExpr {
                        expected: "`u512` or `u256`".to_string(),
                        found: t.to_string(),
                    }),

                    (Type::F32(_), Type::F32(_)) => Ok(Type::F32(Float::F32(F32::default()))),

                    (Type::F32(_), t) => Err(SemanticErrorKind::TypeMismatchBinaryExpr {
                        expected: "`f32`".to_string(),
                        found: t.to_string(),
                    }),

                    (Type::F64(_), Type::F32(_) | Type::F64(_)) => {
                        Ok(Type::F64(Float::F64(F64::default())))
                    }

                    (Type::F64(_), t) => Err(SemanticErrorKind::TypeMismatchBinaryExpr {
                        expected: "`f64` or `f32`".to_string(),
                        found: t.to_string(),
                    }),

                    _ => Err(SemanticErrorKind::TypeMismatch {
                        expected: "matching numeric types".to_string(),
                        found: format!("`({:?}, {:?})`", &lhs_type, &rhs_type),
                    }),
                }
            }

            Expression::Comparison(c) => {
                let lhs_clone = c.lhs.clone();
                let rhs_clone = c.rhs.clone();

                let lhs_type =
                    self.analyze_expr(&Expression::try_from(c.lhs.clone()).map_err(|_| {
                        SemanticErrorKind::ConversionError {
                            from: format!("`{:?}`", &lhs_clone),
                            into: "`Expression`".to_string(),
                        }
                    })?)?;

                let rhs_type =
                    self.analyze_expr(&Expression::try_from(c.rhs.clone()).map_err(|_| {
                        SemanticErrorKind::ConversionError {
                            from: format!("`{:?}`", &rhs_clone),
                            into: "`Expression`".to_string(),
                        }
                    })?)?;

                match (&lhs_type, &rhs_type) {
                    (Type::I32(_), Type::I32(_)) => Ok(Type::I32(Int::I32(i32::default()))),

                    (Type::I32(_), t) => Err(SemanticErrorKind::TypeMismatchBinaryExpr {
                        expected: "`i32`".to_string(),
                        found: t.to_string(),
                    }),

                    (Type::I64(_), Type::I64(_)) => Ok(Type::I64(Int::I64(i64::default()))),

                    (Type::I64(_), t) => Err(SemanticErrorKind::TypeMismatchBinaryExpr {
                        expected: "`i64`".to_string(),
                        found: t.to_string(),
                    }),

                    (Type::I128(_), Type::I128(_)) => Ok(Type::I128(Int::I128(i128::default()))),

                    (Type::I128(_), t) => Err(SemanticErrorKind::TypeMismatchBinaryExpr {
                        expected: "`i128`".to_string(),
                        found: t.to_string(),
                    }),

                    (Type::U8(_), Type::U8(_)) => Ok(Type::U8(UInt::U8(u8::default()))),

                    (Type::U8(_), t) => Err(SemanticErrorKind::TypeMismatchBinaryExpr {
                        expected: "`u8`".to_string(),
                        found: t.to_string(),
                    }),

                    (Type::U16(_), Type::U16(_)) => Ok(Type::U16(UInt::U16(u16::default()))),

                    (Type::U16(_), t) => Err(SemanticErrorKind::TypeMismatchBinaryExpr {
                        expected: "`u16`".to_string(),
                        found: t.to_string(),
                    }),

                    (Type::U32(_), Type::U32(_)) => Ok(Type::U32(UInt::U32(u32::default()))),

                    (Type::U32(_), t) => Err(SemanticErrorKind::TypeMismatchBinaryExpr {
                        expected: "`u32`".to_string(),
                        found: t.to_string(),
                    }),

                    (Type::U64(_), Type::U64(_)) => Ok(Type::U64(UInt::U64(u64::default()))),

                    (Type::U64(_), t) => Err(SemanticErrorKind::TypeMismatchBinaryExpr {
                        expected: "`u64`".to_string(),
                        found: t.to_string(),
                    }),

                    (Type::U128(_), Type::U128(_)) => Ok(Type::U128(UInt::U128(u128::default()))),

                    (Type::U128(_), t) => Err(SemanticErrorKind::TypeMismatchBinaryExpr {
                        expected: "`u128`".to_string(),
                        found: t.to_string(),
                    }),

                    (Type::U256(_), Type::U256(_)) => {
                        Ok(Type::U256(BigUInt::U256(U256::default())))
                    }

                    (Type::U256(_), t) => Err(SemanticErrorKind::TypeMismatchBinaryExpr {
                        expected: "`u256`".to_string(),
                        found: t.to_string(),
                    }),

                    (Type::U512(_), Type::U512(_)) => {
                        Ok(Type::U512(BigUInt::U512(U512::default())))
                    }

                    (Type::U512(_), t) => Err(SemanticErrorKind::TypeMismatchBinaryExpr {
                        expected: "`u512`".to_string(),
                        found: t.to_string(),
                    }),

                    (Type::F32(_), Type::F32(_)) => Ok(Type::F32(Float::F32(F32::default()))),

                    (Type::F32(_), t) => Err(SemanticErrorKind::TypeMismatchBinaryExpr {
                        expected: "`f32`".to_string(),
                        found: t.to_string(),
                    }),

                    (Type::F64(_), Type::F64(_)) => Ok(Type::F64(Float::F64(F64::default()))),

                    (Type::F64(_), t) => Err(SemanticErrorKind::TypeMismatchBinaryExpr {
                        expected: "`f64`".to_string(),
                        found: t.to_string(),
                    }),

                    _ => Err(SemanticErrorKind::TypeMismatch {
                        expected: "matching numeric types".to_string(),
                        found: format!("`({:?}, {:?})`", &lhs_type, &rhs_type),
                    }),
                }
            }

            Expression::Grouped(g) => self.analyze_expr(&g.inner_expression),

            Expression::Range(_) => todo!(),

            Expression::Assignment(a) => {
                let lhs_clone = a.lhs.clone();
                let rhs_clone = a.rhs.clone();

                let lhs_type =
                    self.analyze_expr(&Expression::try_from(a.lhs.clone()).map_err(|_| {
                        SemanticErrorKind::ConversionError {
                            from: format!("`{:?}`", &lhs_clone),
                            into: "`Expression`".to_string(),
                        }
                    })?)?;

                let rhs_type =
                    self.analyze_expr(&Expression::try_from(a.rhs.clone()).map_err(|_| {
                        SemanticErrorKind::ConversionError {
                            from: format!("`{:?}`", &rhs_clone),
                            into: "`Expression`".to_string(),
                        }
                    })?)?;

                match (&lhs_type, &rhs_type) {
                    (Type::I32(_), Type::I32(_)) => Ok(Type::I32(Int::I32(i32::default()))),

                    (Type::I32(_), t) => Err(SemanticErrorKind::TypeMismatchBinaryExpr {
                        expected: "`i32`".to_string(),
                        found: t.to_string(),
                    }),

                    (Type::I64(_), Type::I64(_)) => Ok(Type::I64(Int::I64(i64::default()))),

                    (Type::I64(_), t) => Err(SemanticErrorKind::TypeMismatchBinaryExpr {
                        expected: "`i64`".to_string(),
                        found: t.to_string(),
                    }),

                    (Type::I128(_), Type::I128(_)) => Ok(Type::I128(Int::I128(i128::default()))),

                    (Type::I128(_), t) => Err(SemanticErrorKind::TypeMismatchBinaryExpr {
                        expected: "`i128`".to_string(),
                        found: t.to_string(),
                    }),

                    (Type::U8(_), Type::U8(_)) => Ok(Type::U8(UInt::U8(u8::default()))),

                    (Type::U8(_), t) => Err(SemanticErrorKind::TypeMismatchBinaryExpr {
                        expected: "`u8`".to_string(),
                        found: t.to_string(),
                    }),

                    (Type::U16(_), Type::U16(_)) => Ok(Type::U16(UInt::U16(u16::default()))),

                    (Type::U16(_), t) => Err(SemanticErrorKind::TypeMismatchBinaryExpr {
                        expected: "`u16`".to_string(),
                        found: t.to_string(),
                    }),

                    (Type::U32(_), Type::U32(_)) => Ok(Type::U32(UInt::U32(u32::default()))),

                    (Type::U32(_), t) => Err(SemanticErrorKind::TypeMismatchBinaryExpr {
                        expected: "`u32`".to_string(),
                        found: t.to_string(),
                    }),

                    (Type::U64(_), Type::U64(_)) => Ok(Type::U64(UInt::U64(u64::default()))),

                    (Type::U64(_), t) => Err(SemanticErrorKind::TypeMismatchBinaryExpr {
                        expected: "`u64`".to_string(),
                        found: t.to_string(),
                    }),

                    (Type::U128(_), Type::U128(_)) => Ok(Type::U128(UInt::U128(u128::default()))),

                    (Type::U128(_), t) => Err(SemanticErrorKind::TypeMismatchBinaryExpr {
                        expected: "`u128`".to_string(),
                        found: t.to_string(),
                    }),

                    (Type::U256(_), Type::U256(_)) => {
                        Ok(Type::U256(BigUInt::U256(U256::default())))
                    }

                    (Type::U256(_), t) => Err(SemanticErrorKind::TypeMismatchBinaryExpr {
                        expected: "`u256`".to_string(),
                        found: t.to_string(),
                    }),

                    (Type::U512(_), Type::U512(_)) => {
                        Ok(Type::U512(BigUInt::U512(U512::default())))
                    }

                    (Type::U512(_), t) => Err(SemanticErrorKind::TypeMismatchBinaryExpr {
                        expected: "`u512`".to_string(),
                        found: t.to_string(),
                    }),

                    (Type::F32(_), Type::F32(_)) => Ok(Type::F32(Float::F32(F32::default()))),

                    (Type::F32(_), t) => Err(SemanticErrorKind::TypeMismatchBinaryExpr {
                        expected: "`f32`".to_string(),
                        found: t.to_string(),
                    }),

                    (Type::F64(_), Type::F64(_)) => Ok(Type::F64(Float::F64(F64::default()))),

                    (Type::F64(_), t) => Err(SemanticErrorKind::TypeMismatchBinaryExpr {
                        expected: "`f64`".to_string(),
                        found: t.to_string(),
                    }),

                    _ => Err(SemanticErrorKind::TypeMismatch {
                        expected: "matching numeric types".to_string(),
                        found: format!("`({:?}, {:?})`", &lhs_type, &rhs_type),
                    }),
                }
            }

            Expression::CompoundAssignment(ca) => {
                let lhs_clone = ca.lhs.clone();
                let rhs_clone = ca.rhs.clone();

                let lhs_type =
                    self.analyze_expr(&Expression::try_from(ca.lhs.clone()).map_err(|_| {
                        SemanticErrorKind::ConversionError {
                            from: format!("`{:?}`", &lhs_clone),
                            into: "`Expression`".to_string(),
                        }
                    })?)?;

                let rhs_type =
                    self.analyze_expr(&Expression::try_from(ca.rhs.clone()).map_err(|_| {
                        SemanticErrorKind::ConversionError {
                            from: format!("`{:?}`", &rhs_clone),
                            into: "`Expression`".to_string(),
                        }
                    })?)?;

                match (&lhs_type, &rhs_type) {
                    (Type::I32(_), Type::I32(_)) => Ok(Type::I32(Int::I32(i32::default()))),

                    (Type::I32(_), t) => Err(SemanticErrorKind::TypeMismatchBinaryExpr {
                        expected: "`i32`".to_string(),
                        found: t.to_string(),
                    }),

                    (Type::I64(_), Type::I64(_)) => Ok(Type::I64(Int::I64(i64::default()))),

                    (Type::I64(_), t) => Err(SemanticErrorKind::TypeMismatchBinaryExpr {
                        expected: "`i64`".to_string(),
                        found: t.to_string(),
                    }),

                    (Type::I128(_), Type::I128(_)) => Ok(Type::I128(Int::I128(i128::default()))),

                    (Type::I128(_), t) => Err(SemanticErrorKind::TypeMismatchBinaryExpr {
                        expected: "`i128`".to_string(),
                        found: t.to_string(),
                    }),

                    (Type::U8(_), Type::U8(_)) => Ok(Type::U8(UInt::U8(u8::default()))),

                    (Type::U8(_), t) => Err(SemanticErrorKind::TypeMismatchBinaryExpr {
                        expected: "`u8`".to_string(),
                        found: t.to_string(),
                    }),

                    (Type::U16(_), Type::U16(_)) => Ok(Type::U16(UInt::U16(u16::default()))),

                    (Type::U16(_), t) => Err(SemanticErrorKind::TypeMismatchBinaryExpr {
                        expected: "`u16`".to_string(),
                        found: t.to_string(),
                    }),

                    (Type::U32(_), Type::U32(_)) => Ok(Type::U32(UInt::U32(u32::default()))),

                    (Type::U32(_), t) => Err(SemanticErrorKind::TypeMismatchBinaryExpr {
                        expected: "`u32`".to_string(),
                        found: t.to_string(),
                    }),

                    (Type::U64(_), Type::U64(_)) => Ok(Type::U64(UInt::U64(u64::default()))),

                    (Type::U64(_), t) => Err(SemanticErrorKind::TypeMismatchBinaryExpr {
                        expected: "`u64`".to_string(),
                        found: t.to_string(),
                    }),

                    (Type::U128(_), Type::U128(_)) => Ok(Type::U128(UInt::U128(u128::default()))),

                    (Type::U128(_), t) => Err(SemanticErrorKind::TypeMismatchBinaryExpr {
                        expected: "`u128`".to_string(),
                        found: t.to_string(),
                    }),

                    (Type::U256(_), Type::U256(_)) => {
                        Ok(Type::U256(BigUInt::U256(U256::default())))
                    }

                    (Type::U256(_), t) => Err(SemanticErrorKind::TypeMismatchBinaryExpr {
                        expected: "`u256`".to_string(),
                        found: t.to_string(),
                    }),

                    (Type::U512(_), Type::U512(_)) => {
                        Ok(Type::U512(BigUInt::U512(U512::default())))
                    }

                    (Type::U512(_), t) => Err(SemanticErrorKind::TypeMismatchBinaryExpr {
                        expected: "`u512`".to_string(),
                        found: t.to_string(),
                    }),

                    (Type::F32(_), Type::F32(_)) => Ok(Type::F32(Float::F32(F32::default()))),

                    (Type::F32(_), t) => Err(SemanticErrorKind::TypeMismatchBinaryExpr {
                        expected: "`f32`".to_string(),
                        found: t.to_string(),
                    }),

                    (Type::F64(_), Type::F64(_)) => Ok(Type::F64(Float::F64(F64::default()))),

                    (Type::F64(_), t) => Err(SemanticErrorKind::TypeMismatchBinaryExpr {
                        expected: "`f64`".to_string(),
                        found: t.to_string(),
                    }),

                    _ => Err(SemanticErrorKind::TypeMismatch {
                        expected: "matching numeric types".to_string(),
                        found: format!("`({:?}, {:?})`", &lhs_type, &rhs_type),
                    }),
                }
            }

            Expression::Return(r) => match &r.expression_opt {
                Some(e) => self.analyze_expr(&*e.clone()),
                None => Ok(Type::UnitType(Unit)),
            },

            Expression::Break(_) => Ok(Type::UnitType(Unit)),

            Expression::Continue(_) => Ok(Type::UnitType(Unit)),

            Expression::Underscore(_) => Ok(Type::InferredType(InferredType {
                underscore: Identifier::from("_"),
            })),

            Expression::Closure(c) => {
                let return_type = match &c.return_type_opt {
                    Some(t) => Ok(*t.clone()),
                    None => Ok(Type::UnitType(Unit)),
                }?;

                let expression_type = self.analyze_expr(&*c.body_expression)?;

                if return_type != expression_type {
                    return Err(SemanticErrorKind::TypeMismatch {
                        expected: return_type.to_string(),
                        found: expression_type.to_string(),
                    });
                }

                Ok(return_type)
            }

            Expression::Array(a) => match &a.elements_opt {
                Some(v) => match v.first() {
                    Some(e) => {
                        let mut elem_count = 0u128;

                        let first_elem_type = self.analyze_expr(e)?;

                        elem_count += 1;

                        for elem in v.iter().skip(1) {
                            let elem_type = self.analyze_expr(elem)?;

                            elem_count += 1;

                            if elem_type != first_elem_type {
                                return Err(SemanticErrorKind::TypeMismatch {
                                    expected: first_elem_type.to_string(),
                                    found: elem_type.to_string(),
                                });
                            }
                        }

                        Ok(Type::Array {
                            element_type: Box::new(first_elem_type),
                            num_elements: UInt::U128(elem_count),
                        })
                    }

                    None => {
                        let element_type = Type::UnitType(Unit);
                        let array = Type::Array {
                            element_type: Box::new(element_type),
                            num_elements: UInt::U128(0u128),
                        };

                        Ok(array)
                    }
                },
                None => Ok(Type::UnitType(Unit)),
            },

            Expression::Tuple(t) => {
                let mut element_types: Vec<Type> = Vec::new();

                for expr in t.tuple_elements.elements.iter() {
                    let ty = self.analyze_expr(expr)?;
                    element_types.push(ty)
                }

                Ok(Type::Tuple(element_types))
            }

            Expression::Struct(_) => todo!(),

            Expression::Mapping(m) => match &m.pairs_opt {
                Some(v) => match v.first() {
                    Some(p) => {
                        let key_type = match self.symbol_table.get(&p.key.name.clone()) {
                            Some(t) => Ok(t.clone()),
                            _ => Err(SemanticErrorKind::UndefinedVariable {
                                name: p.key.name.clone(),
                            }),
                        }?;

                        let value_type = self.analyze_expr(&*p.value.clone())?;

                        for pair in v.iter().skip(1) {
                            let pair_key_type = match self.symbol_table.get(&pair.key.name.clone())
                            {
                                Some(t) => Ok(t.clone()),
                                _ => Err(SemanticErrorKind::UndefinedVariable {
                                    name: p.key.name.clone(),
                                }),
                            }?;

                            let pair_value_type = self.analyze_expr(&*pair.value.clone())?;

                            if (&pair_key_type, &pair_value_type) != (&key_type, &value_type) {
                                return Err(SemanticErrorKind::TypeMismatch {
                                    expected: format!(
                                        "{{ key: `{}`, value: `{}` }}",
                                        &key_type.to_string(),
                                        &value_type.to_string()
                                    ),
                                    found: format!(
                                        "{{ key: `{}`, value: `{}` }}",
                                        &pair_key_type.to_string(),
                                        &pair_value_type.to_string()
                                    ),
                                });
                            }
                        }

                        Ok(Type::Mapping {
                            key_type: Box::new(key_type),
                            value_type: Box::new(value_type),
                        })
                    }

                    None => {
                        let key_type = Box::new(Type::UnitType(Unit));
                        let value_type = Box::new(Type::UnitType(Unit));

                        Ok(Type::Mapping {
                            key_type,
                            value_type,
                        })
                    }
                },
                None => Ok(Type::UnitType(Unit)),
            },

            Expression::Block(b) => match &b.statements_opt {
                Some(v) => match v.last() {
                    Some(s) => match s {
                        Statement::Let(ls) => match &ls.value_opt {
                            Some(e) => self.analyze_expr(e),
                            None => Ok(Type::UnitType(Unit)),
                        },

                        Statement::Item(_) => Ok(Type::UnitType(Unit)),

                        Statement::Expression(e) => self.analyze_expr(e),
                    },

                    None => Ok(Type::UnitType(Unit)),
                },

                None => Ok(Type::UnitType(Unit)),
            },

            Expression::If(_) => todo!(),

            Expression::Match(_) => todo!(),

            Expression::ForIn(_) => todo!(),

            Expression::While(_) => todo!(),

            Expression::SomeExpr(s) => self.analyze_expr(&*s.expression.clone().inner_expression),

            Expression::NoneExpr(_) => Ok(Type::UnitType(Unit)),

            Expression::ResultExpr(r) => self.analyze_expr(&*r.expression.clone().inner_expression),
        }
    }

    fn log_error(&mut self, error_kind: SemanticErrorKind, expression: &Expression) {
        let span = expression.span();

        let error = CompilerError::new(error_kind, span.start(), &span.input());

        self.errors.push(error);
    }
}
