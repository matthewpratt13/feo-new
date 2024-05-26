#![allow(dead_code)]

use crate::{
    ast::{
        BigUInt, Bytes, Expression, Float, Hash, Identifier, Int, Literal, PathRoot, Statement,
        Type, UInt,
    },
    error::{CompilerError, ErrorsEmitted, SemanticErrorKind},
    parser::Module,
    span::Spanned,
};

use self::symbol_table::SymbolTable;

mod symbol_table;

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
            Statement::Item(_) => todo!(),
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
                        None => match &p.path_root {
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

                    _ => Identifier::from(""),
                };

                match self.symbol_table.get(&name) {
                    Some(t) => Ok(t.clone()),
                    None => Err(SemanticErrorKind::UndefinedPath { name }),
                }
            }
            Expression::Literal(l) => match l {
                Literal::Int { value, .. } => match value {
                    Int::I32(_) => Ok(Type::I32(*value)),
                    Int::I64(_) => Ok(Type::I64(*value)),
                    Int::I128(_) => Ok(Type::I128(*value)),
                },
                Literal::UInt { value, .. } => match value {
                    UInt::U8(_) => Ok(Type::U8(*value)),
                    UInt::U16(_) => Ok(Type::U16(*value)),
                    UInt::U32(_) => Ok(Type::U32(*value)),
                    UInt::U64(_) => Ok(Type::U64(*value)),
                    UInt::U128(_) => Ok(Type::U128(*value)),
                },
                Literal::BigUInt { value, .. } => match value {
                    BigUInt::U256(_) => Ok(Type::U256(*value)),
                    BigUInt::U512(_) => Ok(Type::U512(*value)),
                },
                Literal::Float { value, .. } => match value {
                    Float::F32(_) => Ok(Type::F32(*value)),
                    Float::F64(_) => Ok(Type::F64(*value)),
                },
                Literal::Byte { value, .. } => Ok(Type::Byte(*value)),
                Literal::Bytes { value, .. } => match value {
                    Bytes::B2(_) => Ok(Type::B2(*value)),
                    Bytes::B4(_) => Ok(Type::B4(*value)),
                    Bytes::B8(_) => Ok(Type::B8(*value)),
                    Bytes::B16(_) => Ok(Type::B16(*value)),
                    Bytes::B32(_) => Ok(Type::B32(*value)),
                },
                Literal::Hash { value, .. } => match value {
                    Hash::H160(_) => Ok(Type::H160(*value)),
                    Hash::H256(_) => Ok(Type::H256(*value)),
                    Hash::H512(_) => Ok(Type::H512(*value)),
                },
                Literal::Str { value, .. } => Ok(Type::Str(value.clone())),
                Literal::Char { value, .. } => Ok(Type::Char(*value)),
                Literal::Bool { value, .. } => Ok(Type::Bool(*value)),
            },
            Expression::MethodCall(_) => todo!(),
            Expression::FieldAccess(_) => todo!(),
            Expression::Call(_) => todo!(),
            Expression::Index(_) => todo!(),
            Expression::TupleIndex(_) => todo!(),
            Expression::Unwrap(_) => todo!(),
            Expression::Unary(_) => todo!(),
            Expression::Reference(_) => todo!(),
            Expression::Dereference(_) => todo!(),
            Expression::TypeCast(_) => todo!(),
            Expression::Binary(b) => {
                let lhs_clone = *b.lhs.clone();
                let rhs_clone = *b.rhs.clone();

                let lhs_type =
                    self.analyze_expr(&Expression::try_from(*b.lhs.clone()).map_err(|_| {
                        SemanticErrorKind::ConversionError {
                            from: format!("{:?}", lhs_clone),
                            into: "`Expression`".to_string(),
                        }
                    })?)?;
                let rhs_type =
                    self.analyze_expr(&Expression::try_from(*b.rhs.clone()).map_err(|_| {
                        SemanticErrorKind::ConversionError {
                            from: format!("{:?}", rhs_clone),
                            into: "`Expression`".to_string(),
                        }
                    })?)?;
                match (&lhs_type, &rhs_type) {
                    (
                        Type::I32(i) | Type::I64(i) | Type::I128(i),
                        Type::I32(_) | Type::I64(_) | Type::I128(_),
                    ) => Ok(Type::I128(*i)),
                    (
                        Type::U8(u) | Type::U16(u) | Type::U32(u) | Type::U64(u) | Type::U128(u),
                        Type::U8(_) | Type::U16(_) | Type::U32(_) | Type::U64(_) | Type::U128(_),
                    ) => Ok(Type::U128(*u)),
                    (Type::U256(ui) | Type::U512(ui), Type::U256(_) | Type::U512(_)) => {
                        Ok(Type::U512(*ui))
                    }
                    (Type::F32(f) | Type::F64(f), Type::F32(_) | Type::F64(_)) => Ok(Type::F64(*f)),

                    _ => Err(SemanticErrorKind::TypeMismatch {
                        expected: "matching numeric operands".to_string(),
                        found: format!("`({:?}, {:?})`", lhs_type, rhs_type),
                    }),
                }
            }
            Expression::Comparison(_) => todo!(),
            Expression::Grouped(_) => todo!(),
            Expression::Range(_) => todo!(),
            Expression::Assignment(_) => todo!(),
            Expression::CompoundAssignment(_) => todo!(),
            Expression::Return(_) => todo!(),
            Expression::Break(_) => todo!(),
            Expression::Continue(_) => todo!(),
            Expression::Underscore(_) => todo!(),
            Expression::Closure(_) => todo!(),
            Expression::Array(_) => todo!(),
            Expression::Tuple(_) => todo!(),
            Expression::Struct(_) => todo!(),
            Expression::Mapping(_) => todo!(),
            Expression::Block(_) => todo!(),
            Expression::If(_) => todo!(),
            Expression::Match(_) => todo!(),
            Expression::ForIn(_) => todo!(),
            Expression::While(_) => todo!(),
            Expression::SomeExpr(_) => todo!(),
            Expression::NoneExpr(_) => todo!(),
            Expression::ResultExpr(_) => todo!(),
        }
    }

    fn log_error(&mut self, error_kind: SemanticErrorKind, expression: &Expression) {
        let span = expression.span();

        let error = CompilerError::new(error_kind, span.start(), &span.input());

        self.errors.push(error);
    }
}
