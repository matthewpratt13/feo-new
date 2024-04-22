use crate::{
    ast::{AssignmentExpr, AssignmentOp, CompoundAssignmentExpr, CompoundAssignmentOp, Expression, PlaceExpr},
    error::ErrorsEmitted,
};

use super::{Parser, Precedence};

impl AssignmentExpr {
    pub(crate) fn parse(
        parser: &mut Parser,
        left_expr: Expression,
    ) -> Result<AssignmentExpr, ErrorsEmitted> {
        {
            let right_expr = parser.parse_expression(Precedence::Assignment)?;
            Ok(AssignmentExpr {
                lhs: Box::new(left_expr),
                op: AssignmentOp(()),
                rhs: Box::new(right_expr),
            })
        }
    }
}

impl CompoundAssignmentExpr {
    pub(crate) fn parse(
        parser: &mut Parser,
        left_expr: PlaceExpr,
        op: CompoundAssignmentOp,
    ) -> Result<CompoundAssignmentExpr, ErrorsEmitted> {
        match op {
            CompoundAssignmentOp::AddAssign => {
                let right_expr = parser.parse_expression(Precedence::CompoundAssignment)?;
                Ok(CompoundAssignmentExpr {
                    lhs: left_expr,
                    op,
                    rhs: Box::new(right_expr),
                })
            }
            CompoundAssignmentOp::SubtractAssign => {
                let right_expr = parser.parse_expression(Precedence::CompoundAssignment)?;
                Ok(CompoundAssignmentExpr {
                    lhs: left_expr,
                    op,
                    rhs: Box::new(right_expr),
                })
            }
            CompoundAssignmentOp::MultiplyAssign => {
                let right_expr = parser.parse_expression(Precedence::CompoundAssignment)?;
                Ok(CompoundAssignmentExpr {
                    lhs: left_expr,
                    op,
                    rhs: Box::new(right_expr),
                })
            }
            CompoundAssignmentOp::DivideAssign => {
                let right_expr = parser.parse_expression(Precedence::CompoundAssignment)?;
                Ok(CompoundAssignmentExpr {
                    lhs: left_expr,
                    op,
                    rhs: Box::new(right_expr),
                })
            }
            CompoundAssignmentOp::ModulusAssign => {
                let right_expr = parser.parse_expression(Precedence::CompoundAssignment)?;
                Ok(CompoundAssignmentExpr {
                    lhs: left_expr,
                    op,
                    rhs: Box::new(right_expr),
                })
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::parser::test_utils;

    #[test]
    fn parse_assign_expr() -> Result<(), ()> {
        let input = r#"x = 5"#;

        let mut parser = test_utils::get_parser(input, false);

        let expressions = parser.parse();

        match expressions {
            Ok(t) => Ok(println!("{:#?}", t)),
            Err(_) => Err(println!("{:#?}", parser.errors())),
        }
    }
}
