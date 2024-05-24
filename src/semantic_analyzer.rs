use crate::{
    ast::{BigUInt, Bytes, Expression, Float, Hash, Int, Literal, Statement, Type, UInt},
    parser::Module,
};

use self::symbol_table::SymbolTable;

mod symbol_table;

struct SemanticAnalyzer {
    symbol_table: SymbolTable,
}

impl SemanticAnalyzer {
    fn new() -> Self {
        SemanticAnalyzer {
            symbol_table: SymbolTable::new(),
        }
    }

    fn analyze(&mut self, module: &Module) -> Result<(), String> {
        for Statement in &module.statements {
            self.analyze_stmt(Statement)?;
        }
        Ok(())
    }

    fn analyze_stmt(&mut self, statement: &Statement) -> Result<(), String> {
        match statement {
            Statement::Let(s) => {
                let name = s.assignee.name.clone();
                let value = s.value_opt.clone().unwrap();
                let expr_type = self.analyze_expr(&value)?;
                self.symbol_table.insert(name, expr_type);
            }
            Statement::Item(i) => todo!(),
            Statement::Expression(e) => {
                self.analyze_expr(e)?;
            }
        }
        Ok(())
    }

    fn analyze_expr(&mut self, expression: &Expression) -> Result<Type, String> {
        match expression {
            // Expression::(_) => Ok(Type::Integer),
            // Expression::Variable(name) => match self.symbol_table.get(name) {
            //     Some(var_type) => Ok(var_type.clone()),
            //     None => Err(format!("Undefined variable '{}'", name)),
            // },
            // Expression::BinaryOp(lhs, op, rhs) => {
            //     let lhs_type = self.analyze_expr(lhs)?;
            //     let rhs_type = self.analyze_expr(rhs)?;
            //     if lhs_type == Type::Integer && rhs_type == Type::Integer {
            //         Ok(Type::Integer)
            //     } else {
            //         Err(format!("Type error in binary operation"))
            //     }
            // }
            Expression::Literal(l) => match l {
                Literal::Int(i) => match i {
                    Int::I32(_) => Ok(Type::I32(*i)),
                    Int::I64(_) => Ok(Type::I64(*i)),
                    Int::I128(_) => Ok(Type::I128(*i)),
                },
                Literal::UInt(ui) => match ui {
                    UInt::U8(_) => Ok(Type::U8(*ui)),
                    UInt::U16(_) => Ok(Type::U16(*ui)),
                    UInt::U32(_) => Ok(Type::U32(*ui)),
                    UInt::U64(_) => Ok(Type::U64(*ui)),
                    UInt::U128(_) => Ok(Type::U128(*ui)),
                },
                Literal::BigUInt(bui) => match bui {
                    BigUInt::U256(_) => Ok(Type::U256(*bui)),
                    BigUInt::U512(_) => Ok(Type::U512(*bui)),
                },
                Literal::Float(f) => match f {
                    Float::F32(_) => Ok(Type::F32(*f)),
                    Float::F64(_) => Ok(Type::F64(*f)),
                },
                Literal::Byte(by) => Ok(Type::Byte(*by)),
                Literal::Bytes(bb) => match bb {
                    Bytes::B2(_) => Ok(Type::B2(*bb)),
                    Bytes::B4(_) => Ok(Type::B4(*bb)),
                    Bytes::B8(_) => Ok(Type::B8(*bb)),
                    Bytes::B16(_) => Ok(Type::B16(*bb)),
                    Bytes::B32(_) => Ok(Type::B32(*bb)),
                },
                Literal::Hash(h) => match h {
                    Hash::H160(_) => Ok(Type::H160(*h)),
                    Hash::H256(_) => Ok(Type::H256(*h)),
                    Hash::H512(_) => Ok(Type::H512(*h)),
                },
                Literal::Str(s) => Ok(Type::Str(s.clone())),
                Literal::Char(c) => Ok(Type::Char(*c)),
                Literal::Bool(b) => Ok(Type::Bool(*b)),
            },
            Expression::Path(_) => todo!(),
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
            Expression::Binary(_) => todo!(),
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
}
