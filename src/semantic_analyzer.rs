use crate::{
    ast::{Expression, Statement, Type},
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
            Expression::Literal(l) => todo!(),
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
